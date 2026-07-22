# Bootstrap stability of wearing-position bias
#
# Reusable helpers for estimating the magnitude and uncertainty of the
# wearing-position bias (a comparison position relative to the glasses reference)
# and how that uncertainty shrinks as (a) the number of participants or (b) the
# number of participant-days per participant increases.
#
# Bias is computed on each metric's MODEL SCALE (as used in the comparison
# notebook) and expressed in percent of the model-scale glasses reference level,
# matching the "outcome-level bias (%)" convention. At each sample size the
# bootstrap distribution of the mean bias is summarised by its standard deviation
# (in percentage points). Requires LightLogR for `log_zero_inflated()`.

# Derive each metric's model scale and the transform needed to bring stored
# values onto that scale, replicating the comparison notebook's `case_when`.
# Metrics already stored on their model scale (pre.transformed) get "identity".
derive_metric_scales <- function(metric_info) {
  metric_info |>
    dplyr::mutate(response = stringr::str_replace(response, "metric", "value")) |>
    dplyr::transmute(
      metric = name,
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

# Build a long bias table on the model scale. For each metric, stored values are
# transformed to the model scale, positions are aligned per participant-day, and
# bias = (comparison position) - (glasses reference). A per-metric `ref` column
# holds |mean model-scale glasses level|, the denominator used to express bias in
# percent. Transforms are applied by row subset (not case_when) so domain-limited
# transforms never touch values outside their domain.
compute_bias <- function(metrics, scales,
                         positions = c("chest", "wrist"),
                         reference = "glasses") {
  ms <- dplyr::inner_join(metrics, dplyr::select(scales, metric, transform),
                          by = "metric")
  ms$value_ms <- ms$value
  i_log <- ms$transform == "log_zero_inflated"
  ms$value_ms[i_log] <- LightLogR::log_zero_inflated(ms$value[i_log])
  i_logit <- ms$transform == "qlogis"
  ms$value_ms[i_logit] <- stats::qlogis(ms$value[i_logit])

  ref <- ms |>
    dplyr::filter(position == reference) |>
    dplyr::group_by(metric) |>
    dplyr::summarise(ref = abs(mean(value_ms, na.rm = TRUE)), .groups = "drop")

  ms |>
    dplyr::filter(position %in% c(reference, positions)) |>
    dplyr::select(metric, site, Id, Date, position, value_ms) |>
    tidyr::pivot_wider(names_from = position, values_from = value_ms) |>
    tidyr::pivot_longer(dplyr::all_of(positions), names_to = "position",
                        values_to = "value_pos") |>
    dplyr::mutate(bias = value_pos - .data[[reference]]) |>
    dplyr::filter(!is.na(bias)) |>
    dplyr::left_join(ref, by = "metric") |>
    dplyr::select(metric, position, site, Id, Date, bias, ref)
}

# Summarise a bootstrap vector of mean biases by its standard deviation, in
# percentage points of the metric reference level.
.summarise_boot <- function(est, ref, n) {
  tibble::tibble(n = n, sd = stats::sd(est) * 100 / ref)
}

# Panel A driver: bootstrap over participants. Each participant is collapsed to
# their mean bias, then N participants are resampled with replacement (N may
# exceed the true participant count, i.e. an empirical-distribution projection).
boot_participants <- function(bias_df, n_range = 1:200, B = 1000) {
  bias_df |>
    dplyr::group_by(metric, position) |>
    dplyr::group_modify(\(d, key) {
      ref <- d$ref[1]
      per_id <- d |>
        dplyr::group_by(Id) |>
        dplyr::summarise(mb = mean(bias), .groups = "drop") |>
        dplyr::pull(mb)
      purrr::map_dfr(n_range, \(n) {
        est <- replicate(B, mean(sample(per_id, n, replace = TRUE)))
        .summarise_boot(est, ref, n)
      })
    }) |>
    dplyr::ungroup() |>
    dplyr::mutate(boot_type = "participants")
}

# Panel B driver: bootstrap over participant-days. Each replicate draws
# `n_participants` participants with replacement; for each, `n_days` of their days
# are resampled with replacement and averaged, then averaged across the drawn
# participants. This holds the study size fixed while varying days per person.
boot_days <- function(bias_df, day_range = 1:7, B = 1000, n_participants = 50) {
  bias_df |>
    dplyr::group_by(metric, position) |>
    dplyr::group_modify(\(d, key) {
      ref <- d$ref[1]
      by_id <- split(d$bias, d$Id)
      ids <- names(by_id)
      purrr::map_dfr(day_range, \(nd) {
        est <- replicate(B, {
          samp <- sample(ids, n_participants, replace = TRUE)
          mean(vapply(samp, \(id) mean(sample(by_id[[id]], nd, replace = TRUE)), numeric(1)))
        })
        .summarise_boot(est, ref, nd)
      })
    }) |>
    dplyr::ungroup() |>
    dplyr::mutate(boot_type = "participant-days")
}

# Run both bootstrap axes and return one tidy tibble
# (metric, position, n, sd, boot_type), the SD in percentage points. The
# participant-days axis is based on `days_n_participants` participants.
bootstrap_stability <- function(bias_df, n_range = 1:200, day_range = 1:7,
                                B = 1000, days_n_participants = 50) {
  dplyr::bind_rows(
    boot_participants(bias_df, n_range, B),
    boot_days(bias_df, day_range, B, n_participants = days_n_participants)
  )
}

# For each metric x position x axis, the smallest sample size at which the bias
# SD falls to or below `tol` (percentage points); NA means it never does.
crossing_table <- function(stability, tol = 5) {
  stability |>
    dplyr::group_by(metric, position, boot_type) |>
    dplyr::arrange(n, .by_group = TRUE) |>
    dplyr::summarise(
      n_within = {
        ok <- which(sd <= tol)
        if (length(ok)) n[ok[1]] else NA_integer_
      },
      .groups = "drop"
    )
}

# Four-panel figure: the standard deviation of the bias (percentage points) per
# metric, faceted by wearing position (rows) x bootstrap axis (columns). A dashed
# reference marks the `tol` (5%) SD tolerance. The participant-days column uses
# integer breaks 1:7; a caption records the participant count behind it.
# Returns a ggdraw object.
# Colorblind-friendly, well-separated qualitative palette (Okabe-Ito) for metrics.
metric_palette <- c("#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

plot_stability <- function(results, position_colors, metric_labels,
                           tol = 5, ylim = c(0, 20), days_n_participants = 50,
                           draws) {
  p <- results |>
    dplyr::mutate(
      metric = factor(metric, levels = names(metric_labels), labels = metric_labels),
      position = stringr::str_to_title(position),
      boot_type = factor(boot_type,
                         levels = c("participants", "participant-days"),
                         labels = c("N participants", "N participant-days"))
    ) |>
    ggplot2::ggplot(ggplot2::aes(n, sd, colour = metric)) +
    ggplot2::geom_hline(yintercept = tol, linetype = "dashed", colour = "grey40") +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::facet_grid(position ~ boot_type, scales = "free_x") +
    ggplot2::scale_colour_manual(values = metric_palette, name = "Metric") +
    ggplot2::scale_x_continuous(
      breaks = function(lims) if (max(lims, na.rm = TRUE) <= 8) 1:7 else scales::extended_breaks()(lims)
    ) +
    ggplot2::coord_cartesian(ylim = ylim) +
    ggplot2::labs(
      x = "Sample size",
      y = "SD of bias (% of glasses level)",
      caption = paste0("Participant-days panels are based on N = ",
                       days_n_participants, " participants (resampled).",
                       "Bootstrap with N=", draws ," draws")
    ) +
    cowplot::theme_cowplot() +
    ggplot2::theme(strip.background = ggplot2::element_rect(fill = "grey90"))
  cowplot::ggdraw(color_strips_grid(p, position_colors))
}

# Recolor the right-hand (position) facet strips by wearing position.
color_strips_grid <- function(plot, position_colors, side = "strip-r") {
  g <- ggplot2::ggplotGrob(plot)
  for (i in grep(side, g$layout$name)) {
    label <- g$grobs[[i]]$grobs[[1]]$children[[2]]$children[[1]]$label
    fill  <- position_colors[[tolower(label)]]
    if (!is.null(fill) && !is.na(fill))
      g$grobs[[i]]$grobs[[1]]$children[[1]]$gp$fill <- fill
  }
  g
}
