# Bootstrap stability of wearing-position bias
#
# Reusable helpers for estimating how the sampling variability of the mean
# wearing-position bias changes with the number of participants and the number
# of participant-days per participant. Bias is calculated on each metric's
# model scale and normalized to the absolute model-scale glasses reference.

# Derive each metric's model scale and the transform needed to bring stored
# values onto that scale, matching the metric-comparison notebook.
derive_metric_scales <- function(metric_info) {
  required <- c(
    "name",
    "metric_family",
    "metric_type",
    "engine",
    "pre.transformed",
    "response",
    "note"
  )
  missing <- setdiff(required, names(metric_info))
  if (length(missing) > 0L) {
    stop(
      "`metric_info` is missing required columns: ",
      paste(missing, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  metric_info |>
    dplyr::mutate(
      response = stringr::str_replace(response, "metric", "value")
    ) |>
    dplyr::transmute(
      metric = name,
      metric_family,
      metric_class = metric_type,
      scaling = dplyr::case_when(
        pre.transformed == "log_zero_inflated" ~ "log10",
        engine == "glmmTMB" ~ "log",
        response == "log_zero_inflated(value)" ~ "log10",
        response == "qlogis(value)" ~ "logit",
        stringr::str_detect(dplyr::coalesce(note, ""), "poisson") ~ "log",
        .default = "linear"
      ),
      already_scaled = !is.na(pre.transformed) & pre.transformed != "",
      transform = dplyr::case_when(
        already_scaled ~ "identity",
        scaling %in% c("log10", "log") ~ "log_zero_inflated",
        scaling == "logit" ~ "qlogis",
        .default = "identity"
      )
    )
}

# Build a long bias table on the model scale. Positions are aligned within
# participant-day before comparison with the glasses reference.
compute_bias <- function(
  metrics,
  scales,
  positions = c("chest", "wrist"),
  reference = "glasses"
) {
  required <- c("metric", "site", "Id", "Date", "position", "value")
  missing <- setdiff(required, names(metrics))
  if (length(missing) > 0L) {
    stop(
      "`metrics` is missing required columns: ",
      paste(missing, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  missing_scales <- setdiff(unique(metrics$metric), scales$metric)
  if (length(missing_scales) > 0L) {
    stop(
      "No model-scale metadata found for: ",
      paste(missing_scales, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  ms <- metrics |>
    dplyr::left_join(
      dplyr::select(
        scales,
        metric,
        metric_family,
        metric_class,
        transform
      ),
      by = "metric"
    )

  ms$value_ms <- ms$value
  is_log <- ms$transform == "log_zero_inflated"
  ms$value_ms[is_log] <- LightLogR::log_zero_inflated(ms$value[is_log])
  is_logit <- ms$transform == "qlogis"
  ms$value_ms[is_logit] <- stats::qlogis(ms$value[is_logit])

  reference_levels <- ms |>
    dplyr::filter(position == reference) |>
    dplyr::group_by(metric) |>
    dplyr::summarise(
      ref = abs(mean(value_ms, na.rm = TRUE)),
      .groups = "drop"
    )

  invalid_reference <- reference_levels |>
    dplyr::filter(!is.finite(ref) | ref <= 0)
  if (nrow(invalid_reference) > 0L) {
    stop(
      "The model-scale glasses reference must be finite and positive for every metric.",
      call. = FALSE
    )
  }

  bias <- ms |>
    dplyr::filter(position %in% c(reference, positions)) |>
    dplyr::select(
      metric,
      metric_family,
      metric_class,
      site,
      Id,
      Date,
      position,
      value_ms
    ) |>
    tidyr::pivot_wider(names_from = position, values_from = value_ms) |>
    tidyr::pivot_longer(
      dplyr::all_of(positions),
      names_to = "position",
      values_to = "value_position"
    ) |>
    dplyr::mutate(bias = value_position - .data[[reference]]) |>
    dplyr::filter(is.finite(bias)) |>
    dplyr::left_join(reference_levels, by = "metric") |>
    dplyr::select(
      metric,
      metric_family,
      metric_class,
      position,
      site,
      Id,
      Date,
      bias,
      ref
    )

  missing_bias <- setdiff(unique(metrics$metric), unique(bias$metric))
  if (length(missing_bias) > 0L) {
    stop(
      "No finite paired wearing-position bias could be computed for: ",
      paste(missing_bias, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  bias
}

.as_positive_integers <- function(x, arg) {
  if (
    length(x) == 0L ||
      anyNA(x) ||
      any(!is.finite(x)) ||
      any(x < 1) ||
      any(x != as.integer(x))
  ) {
    stop("`", arg, "` must contain positive integers.", call. = FALSE)
  }
  as.integer(x)
}

.split_bias_by_id <- function(data) {
  by_id <- split(data$bias, data$Id, drop = TRUE)
  by_id <- lapply(by_id, function(x) x[is.finite(x)])
  by_id[lengths(by_id) > 0L]
}

# For each selected participant index, sample `n_days` observations from that
# participant's empirical daily-bias distribution. Index sampling avoids the
# special behavior of sample(x) when a participant has only one numeric value.
.sample_days <- function(by_id, selected_id, n_days) {
  sampled_days <- matrix(
    NA_real_,
    nrow = length(selected_id),
    ncol = n_days
  )

  for (id_index in seq_along(by_id)) {
    rows <- which(selected_id == id_index)
    if (length(rows) == 0L) {
      next
    }
    values <- by_id[[id_index]]
    sampled_days[rows, ] <- matrix(
      values[
        sample.int(
          length(values),
          size = length(rows) * n_days,
          replace = TRUE
        )
      ],
      nrow = length(rows),
      ncol = n_days
    )
  }

  sampled_days
}

.cumulative_rows <- function(x) {
  out <- x
  if (ncol(out) > 1L) {
    for (column in 2:ncol(out)) {
      out[, column] <- out[, column - 1L] + x[, column]
    }
  }
  out
}

# Bootstrap one complete N-participant curve efficiently. Each replicate is a
# nested sequence up to max(n_range), so all requested N share random draws.
.participant_curve <- function(by_id, n_range, B, n_days) {
  max_n <- max(n_range)
  selected_id <- sample.int(
    length(by_id),
    size = B * max_n,
    replace = TRUE
  )
  sampled_days <- .sample_days(by_id, selected_id, n_days)
  participant_means <- matrix(
    rowMeans(sampled_days),
    nrow = B,
    ncol = max_n
  )
  cumulative_means <- .cumulative_rows(participant_means) |>
    sweep(2, seq_len(max_n), "/")

  tibble::tibble(
    n = n_range,
    sd_raw = vapply(
      n_range,
      function(n) stats::sd(cumulative_means[, n]),
      numeric(1)
    )
  )
}

# Participant axis: vary N while fixing the number of participant-days sampled
# for each participant. The default compares three and seven days per person.
boot_participants <- function(
  bias_df,
  n_range = 1:200,
  B = 1000,
  days_per_participant = c(3, 7)
) {
  n_range <- .as_positive_integers(n_range, "n_range")
  B <- .as_positive_integers(B, "B")
  days_per_participant <- .as_positive_integers(
    days_per_participant,
    "days_per_participant"
  )
  if (length(B) != 1L || B < 2L) {
    stop("`B` must be one integer greater than one.", call. = FALSE)
  }

  bias_df |>
    dplyr::group_by(metric, metric_family, metric_class, position) |>
    dplyr::group_modify(function(.x, .y) {
      ref <- dplyr::first(.x$ref)
      by_id <- .split_bias_by_id(.x)
      if (length(by_id) == 0L) {
        stop(
          "A metric-position group has no finite participant biases.",
          call. = FALSE
        )
      }

      purrr::map_dfr(days_per_participant, function(n_days) {
        .participant_curve(by_id, n_range, B, n_days) |>
          dplyr::transmute(
            n,
            sd = sd_raw * 100 / ref,
            boot_type = "participants",
            days_per_participant = n_days,
            n_participants = NA_integer_,
            scenario = paste0("N participants\n", n_days, " days each")
          )
      })
    }) |>
    dplyr::ungroup()
}

# Bootstrap one complete days-per-participant curve for a fixed study size.
.participant_day_curve <- function(by_id, day_range, B, n_participants) {
  max_days <- max(day_range)
  selected_id <- sample.int(
    length(by_id),
    size = B * n_participants,
    replace = TRUE
  )
  sampled_days <- .sample_days(by_id, selected_id, max_days)
  cumulative_days <- .cumulative_rows(sampled_days)

  tibble::tibble(
    n = day_range,
    sd_raw = vapply(
      day_range,
      function(n_days) {
        participant_means <- matrix(
          cumulative_days[, n_days] / n_days,
          nrow = B,
          ncol = n_participants
        )
        stats::sd(rowMeans(participant_means))
      },
      numeric(1)
    )
  )
}

# Participant-day axis: vary days per participant while fixing the number of
# participants. The default compares studies with 25 and 50 participants.
boot_days <- function(
  bias_df,
  day_range = 1:7,
  B = 1000,
  n_participants = c(25, 50)
) {
  day_range <- .as_positive_integers(day_range, "day_range")
  B <- .as_positive_integers(B, "B")
  n_participants <- .as_positive_integers(n_participants, "n_participants")
  if (length(B) != 1L || B < 2L) {
    stop("`B` must be one integer greater than one.", call. = FALSE)
  }

  bias_df |>
    dplyr::group_by(metric, metric_family, metric_class, position) |>
    dplyr::group_modify(function(.x, .y) {
      ref <- dplyr::first(.x$ref)
      by_id <- .split_bias_by_id(.x)
      if (length(by_id) == 0L) {
        stop(
          "A metric-position group has no finite participant biases.",
          call. = FALSE
        )
      }

      purrr::map_dfr(n_participants, function(study_n) {
        .participant_day_curve(by_id, day_range, B, study_n) |>
          dplyr::transmute(
            n,
            sd = sd_raw * 100 / ref,
            boot_type = "participant-days",
            days_per_participant = NA_integer_,
            n_participants = study_n,
            scenario = paste0(
              "Days per participant\n",
              study_n,
              " participants"
            )
          )
      })
    }) |>
    dplyr::ungroup()
}

# Run both bootstrap axes and return metric-level curves.
bootstrap_stability <- function(
  bias_df,
  n_range = 1:200,
  day_range = 1:7,
  B = 1000,
  days_per_participant = c(3, 7),
  day_axis_participants = c(25, 50)
) {
  dplyr::bind_rows(
    boot_participants(
      bias_df,
      n_range = n_range,
      B = B,
      days_per_participant = days_per_participant
    ),
    boot_days(
      bias_df,
      day_range = day_range,
      B = B,
      n_participants = day_axis_participants
    )
  )
}

# Take the pointwise median of normalized metric-specific SD curves within each
# metric class. The returned `n_metrics` records the class denominator.
median_metric_classes <- function(results) {
  required <- c(
    "metric",
    "metric_class",
    "position",
    "n",
    "sd",
    "boot_type",
    "days_per_participant",
    "n_participants",
    "scenario"
  )
  missing <- setdiff(required, names(results))
  if (length(missing) > 0L) {
    stop(
      "`results` is missing required columns: ",
      paste(missing, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  class_medians <- results |>
    dplyr::group_by(
      metric_class,
      position,
      boot_type,
      days_per_participant,
      n_participants,
      scenario,
      n
    ) |>
    dplyr::summarise(
      sd = stats::median(sd),
      n_metrics = dplyr::n_distinct(metric),
      .groups = "drop"
    )

  inconsistent_counts <- class_medians |>
    dplyr::distinct(metric_class, n_metrics) |>
    dplyr::count(metric_class) |>
    dplyr::filter(n > 1L)
  if (nrow(inconsistent_counts) > 0L) {
    stop(
      "At least one metric class has an inconsistent metric denominator.",
      call. = FALSE
    )
  }

  class_medians
}

# Smallest sample size at which a class-median bias SD reaches the tolerance.
crossing_table <- function(stability, tol = 5, id = "metric_class") {
  if (!is.numeric(tol) || length(tol) != 1L || !is.finite(tol) || tol <= 0) {
    stop("`tol` must be one finite positive number.", call. = FALSE)
  }
  if (!id %in% names(stability)) {
    stop("`id` must name a column in `stability`.", call. = FALSE)
  }

  grouping <- c(
    id,
    "position",
    "boot_type",
    "days_per_participant",
    "n_participants",
    "scenario"
  )
  if ("n_metrics" %in% names(stability)) {
    grouping <- c(grouping, "n_metrics")
  }

  stability |>
    dplyr::group_by(dplyr::across(dplyr::all_of(grouping))) |>
    dplyr::arrange(n, .by_group = TRUE) |>
    dplyr::summarise(
      n_within = {
        within <- which(sd <= tol)
        if (length(within) > 0L) n[within[1L]] else NA_integer_
      },
      .groups = "drop"
    )
}

# Colorblind-friendly, well-separated palette for the six metric classes.
metric_class_palette <- c(
  "#E69F00",
  "#56B4E9",
  "#009E73",
  "#0072B2",
  "#D55E00",
  "#CC79A7"
)

# Convert stored metric names to concise labels for direct annotations.
format_stability_metric <- function(metric) {
  metric |>
    stringr::str_replace_all("_", " ") |>
    stringr::str_replace_all("\\b1000\\b", "1,000") |>
    stringr::str_replace_all("\\b([0-9]+)h\\b", "\\1 h") |>
    stringr::str_to_sentence()
}

# Eight-panel figure: wearing positions in rows and the four study-design
# scenarios in columns. Faint lines show individual metrics and thick lines
# show pointwise class-median SD curves.
plot_stability <- function(
  metric_results,
  class_results,
  position_colors,
  metric_class_labels,
  tol = 5,
  ylim = c(0, 20),
  draws
) {
  scenario_levels <- c(
    "N participants\n3 days each",
    "N participants\n7 days each",
    "Days per participant\n25 participants",
    "Days per participant\n50 participants"
  )

  metric_plot_data <- metric_results |>
    dplyr::mutate(
      metric_class = factor(
        metric_class,
        levels = names(metric_class_labels),
        labels = unname(metric_class_labels)
      ),
      position = stringr::str_to_title(position),
      scenario = factor(scenario, levels = scenario_levels)
    )

  class_plot_data <- class_results |>
    dplyr::mutate(
      metric_class = factor(
        metric_class,
        levels = names(metric_class_labels),
        labels = unname(metric_class_labels)
      ),
      position = stringr::str_to_title(position),
      scenario = factor(scenario, levels = scenario_levels)
    )

  annotated_metric_keys <- metric_results |>
    dplyr::filter(
      scenario == scenario_levels[[1L]],
      is.finite(sd)
    ) |>
    dplyr::group_by(position) |>
    dplyr::filter(n == max(n)) |>
    dplyr::slice_max(sd, n = 3L, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::select(position, metric, metric_class)

  annotation_data <- metric_results |>
    dplyr::filter(
      scenario == scenario_levels[[1L]],
      is.finite(sd)
    ) |>
    dplyr::semi_join(
      annotated_metric_keys,
      by = c("position", "metric", "metric_class")
    ) |>
    dplyr::group_by(position, metric) |>
    dplyr::mutate(
      x_min = min(n),
      x_max = max(n),
      target_x = x_min + 0.75 * (x_max - x_min)
    ) |>
    dplyr::slice_min(abs(n - target_x), n = 1L, with_ties = FALSE) |>
    dplyr::group_by(position) |>
    dplyr::arrange(dplyr::desc(sd), .by_group = TRUE) |>
    dplyr::mutate(
      label_rank = dplyr::row_number(),
      label_offset = pmax(0.8, 3.2 - 0.7 * (label_rank - 1))
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      metric_class = factor(
        metric_class,
        levels = names(metric_class_labels),
        labels = unname(metric_class_labels)
      ),
      position = stringr::str_to_title(position),
      label = format_stability_metric(metric),
      label_x = x_min + 0.60 * (x_max - x_min),
      label_y = pmin(sd + label_offset, max(ylim) - 1),
      segment_x = x_min + 0.62 * (x_max - x_min),
      segment_y = label_y
    ) |>
    dplyr::select(
      position,
      metric_class,
      n,
      sd,
      label,
      label_x,
      label_y,
      segment_x,
      segment_y
    )

  plot <- ggplot2::ggplot() +
    ggplot2::geom_hline(
      yintercept = tol,
      linetype = "dashed",
      linewidth = 0.45,
      colour = "grey35"
    ) +
    ggplot2::geom_line(
      data = metric_plot_data,
      mapping = ggplot2::aes(
        n,
        sd,
        colour = metric_class,
        group = metric
      ),
      linewidth = 0.28,
      alpha = 0.18,
      lineend = "round",
      show.legend = FALSE
    ) +
    ggplot2::geom_line(
      data = class_plot_data,
      mapping = ggplot2::aes(
        n,
        sd,
        colour = metric_class,
        group = metric_class
      ),
      linewidth = 0.95,
      lineend = "round"
    ) +
    ggplot2::layer(
      data = annotation_data,
      mapping = ggplot2::aes(
        x = segment_x,
        y = segment_y,
        xend = n,
        yend = sd,
        colour = metric_class
      ),
      geom = "segment",
      stat = "identity",
      position = "identity",
      show.legend = FALSE,
      inherit.aes = FALSE,
      layout = c(1L, 5L),
      params = list(
        na.rm = FALSE,
        arrow = grid::arrow(
          angle = 25,
          length = grid::unit(4, "pt"),
          type = "open"
        ),
        lineend = "round",
        linejoin = "round",
        linewidth = 0.45
      )
    ) +
    ggplot2::layer(
      data = annotation_data,
      mapping = ggplot2::aes(
        x = label_x,
        y = label_y,
        label = label,
        colour = metric_class
      ),
      geom = "text",
      stat = "identity",
      position = "identity",
      show.legend = FALSE,
      inherit.aes = FALSE,
      layout = c(1L, 5L),
      params = list(
        na.rm = FALSE,
        parse = FALSE,
        check_overlap = FALSE,
        size.unit = "mm",
        size = 2.8,
        hjust = 1,
        vjust = 0.5,
        fontface = "bold"
      )
    ) +
    ggplot2::facet_grid(position ~ scenario, scales = "free_x") +
    ggplot2::scale_colour_manual(
      values = stats::setNames(metric_class_palette, metric_class_labels),
      name = "Metric class"
    ) +
    ggplot2::scale_x_continuous(
      breaks = function(limits) {
        if (max(limits, na.rm = TRUE) <= 8) {
          1:7
        } else {
          c(1, 50, 100, 150, 200)
        }
      },
      expand = ggplot2::expansion(mult = c(0.01, 0.025))
    ) +
    ggplot2::coord_cartesian(ylim = ylim) +
    ggplot2::labs(
      x = "Sample size (participants or days per participant)",
      y = "SD of bias (% of glasses reference)",
      caption = paste0(
        "Dashed: ",
        tol,
        "% tolerance. Thin curves: individual metrics; thick curves: ",
        "class medians (B = ",
        format(draws, big.mark = ","),
        "). Labels mark the three highest endpoint metrics in leftmost facets. ",
        "Display clipped at ",
        max(ylim),
        "%."
      )
    ) +
    cowplot::theme_cowplot(font_size = 10) +
    ggplot2::theme(
      legend.position = "bottom",
      legend.title = ggplot2::element_text(face = "bold"),
      strip.background = ggplot2::element_rect(
        fill = "grey94",
        colour = "grey75",
        linewidth = 0.35
      ),
      strip.text = ggplot2::element_text(face = "bold", size = 9),
      panel.grid.major.y = ggplot2::element_line(
        colour = "grey90",
        linewidth = 0.35
      ),
      panel.spacing = grid::unit(7, "pt"),
      plot.caption = ggplot2::element_text(
        hjust = 0,
        colour = "grey30",
        size = 8.5,
        margin = ggplot2::margin(t = 8)
      )
    ) +
    ggplot2::guides(
      colour = ggplot2::guide_legend(
        nrow = 2,
        byrow = TRUE,
        override.aes = list(linewidth = 1.1)
      )
    )

  cowplot::ggdraw(color_strips_grid(plot, position_colors))
}

# Recolor the right-hand facet strips by wearing position.
color_strips_grid <- function(plot, position_colors, side = "strip-r") {
  grob <- ggplot2::ggplotGrob(plot)
  for (index in grep(side, grob$layout$name)) {
    label <- grob$grobs[[index]]$grobs[[1]]$children[[2]]$children[[1]]$label
    fill <- position_colors[[tolower(label)]]
    if (!is.null(fill) && !is.na(fill)) {
      grob$grobs[[index]]$grobs[[1]]$children[[1]]$gp$fill <- fill
    }
  }
  grob
}
