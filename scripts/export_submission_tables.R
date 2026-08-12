# Export the manuscript's image-based gt tables as Word documents.

project_root <- normalizePath(".", mustWork = TRUE)

if (!file.exists(file.path(project_root, "WearingPositionError_2026.Rproj"))) {
  stop("Run this script from the project root.", call. = FALSE)
}

cache_stem <- file.path(
  project_root,
  "notebooks",
  "JZ_error_stabilisation_cache",
  "html",
  "full-run_933d055dd6476396f53afe86ff289049"
)

notebook_jobs <- list(
  list(
    input = "notebooks/JZ_metric_comparison.qmd",
    outputs = c(
      "output/tables/Table_metrics.docx",
      "output/tables/Table_metrics_summary.docx"
    )
  ),
  list(
    input = "notebooks/JZ_metric_comparison_nonwear.qmd",
    outputs = "output/tables/Table_metrics_summary_nonwear.docx"
  ),
  list(
    input = "notebooks/JZ_metric_comparison_wake_only.qmd",
    outputs = "output/tables/Table_metrics_summary_wake_only.docx"
  ),
  list(
    input = "notebooks/JZ_error_stabilisation.qmd",
    outputs = "output/tables/Table_crossing.docx",
    bootstrap_cache = cache_stem
  )
)

job_filter <- Sys.getenv("SUBMISSION_TABLE_JOB", unset = "")
if (nzchar(job_filter)) {
  keep_job <- vapply(
    notebook_jobs,
    function(job) grepl(job_filter, job$input, fixed = TRUE),
    logical(1)
  )
  notebook_jobs <- notebook_jobs[keep_job]

  if (length(notebook_jobs) == 0L) {
    stop("No notebook matched `SUBMISSION_TABLE_JOB`.", call. = FALSE)
  }
}

export_state_path <- Sys.getenv("SUBMISSION_TABLE_STATE", unset = "")

display_only_labels <- c(
  "tbl-model-specs",
  "tbl-unformatted-reference",
  "tbl-metric-class-counts"
)

skip_assignments <- c(
  "diagnostics = map(H1_model, check_model),"
)

evaluate_notebook_until_outputs <- function(job) {
  input <- file.path(project_root, job$input)
  outputs <- file.path(project_root, job$outputs)

  if (!file.exists(input)) {
    stop("Notebook not found: `", input, "`.", call. = FALSE)
  }

  existing_outputs <- outputs[file.exists(outputs)]
  if (length(existing_outputs) > 0L && !all(file.remove(existing_outputs))) {
    stop("Could not replace an existing DOCX export.", call. = FALSE)
  }

  extracted_code <- tempfile(fileext = ".R")
  on.exit(unlink(extracted_code), add = TRUE)

  knitr::purl(
    input = input,
    output = extracted_code,
    documentation = 1L,
    quiet = TRUE
  )

  extracted_lines <- readLines(extracted_code, warn = FALSE)
  chunk_starts <- grep("^## -{5,}[[:space:]]*$", extracted_lines)

  if (length(chunk_starts) == 0L) {
    stop("No executable notebook chunks were extracted.", call. = FALSE)
  }

  chunk_ends <- c(chunk_starts[-1L] - 1L, length(extracted_lines))
  chunks <- Map(
    function(first, last) extracted_lines[first:last],
    chunk_starts,
    chunk_ends
  )

  notebook_environment <- if (is.null(job$bootstrap_cache)) {
    new.env(parent = globalenv())
  } else {
    globalenv()
  }
  notebook_environment$params <- list(draws = 10000L)

  message("Executing `", job$input, "` until its requested DOCX exports exist.")

  for (chunk in chunks) {
    chunk_text <- paste(chunk, collapse = "\n")
    label_match <- regmatches(
      chunk_text,
      regexec("#\\| label:[[:space:]]*([^[:space:]]+)", chunk_text)
    )[[1L]]
    chunk_label <- if (length(label_match) >= 2L) label_match[[2L]] else NA_character_

    if (!is.na(chunk_label) && chunk_label %in% display_only_labels) {
      next
    }

    has_docx_export <- grepl("\\.docx", chunk_text, ignore.case = TRUE)
    has_figure_export <- grepl(
      "\\.(pdf|png|jpe?g|svg)[\"']",
      chunk_text,
      ignore.case = TRUE
    )

    if (has_figure_export && !has_docx_export) {
      next
    }

    expressions <- parse(text = chunk_text, keep.source = FALSE)

    for (expression in expressions) {
      expression_text <- paste(deparse(expression), collapse = "\n")
      for (assignment in skip_assignments) {
        expression_text <- sub(assignment, "", expression_text, fixed = TRUE)
      }
      expression <- parse(text = expression_text, keep.source = FALSE)[[1L]]
      is_file_export <- grepl("gtsave\\(|ggsave\\(", expression_text)
      is_docx_export <- grepl("\\.docx", expression_text)

      if (is_file_export && !is_docx_export) {
        next
      }

      if (
        !is.null(job$bootstrap_cache) &&
          grepl(
            "metric_stability <- bootstrap_stability",
            expression_text,
            fixed = TRUE
          )
      ) {
        if (!all(file.exists(paste0(job$bootstrap_cache, c(".rdx", ".rdb"))))) {
          stop("The verified production bootstrap cache is unavailable.", call. = FALSE)
        }

        lazyLoad(job$bootstrap_cache, envir = notebook_environment)
        next
      }

      eval(expression, envir = notebook_environment)
    }

    if (all(file.exists(outputs))) {
      break
    }
  }

  missing_outputs <- outputs[!file.exists(outputs)]
  if (length(missing_outputs) > 0L) {
    stop(
      "Notebook execution ended before creating: `",
      paste(missing_outputs, collapse = "`, `"),
      "`.",
      call. = FALSE
    )
  }

  if (any(file.info(outputs)$size <= 0)) {
    stop("At least one DOCX export is empty.", call. = FALSE)
  }

  if (nzchar(export_state_path)) {
    state_objects <- intersect(
      c("df", "df_summary", "table", "table_summary", "crossing_wide", "crossing_gt"),
      ls(envir = notebook_environment, all.names = TRUE)
    )
    save(
      list = state_objects,
      file = export_state_path,
      envir = notebook_environment,
      compress = "xz"
    )
  }

  invisible(outputs)
}

exported_files <- unlist(
  lapply(notebook_jobs, evaluate_notebook_until_outputs),
  use.names = FALSE
)

message("Created raw DOCX exports:")
message(paste0("- ", exported_files, collapse = "\n"))
