#' Log a Structured JSON Summary
#'
#' This function logs a structured summary as a JSON file with a timestamped filename.
#'
#' @param summary A list or data structure to be serialized into JSON.
#' @param config A list containing logging configuration. Must include a `logging` sublist with:
#'   \describe{
#'     \item{enabled}{Logical. Whether logging is enabled.}
#'     \item{summary_log_dir}{Character. Directory where JSON logs will be saved.}
#'     \item{log_to_console}{Logical. Whether to print a message to the console upon successful logging.}
#'   }
#'
#' @return Invisibly returns the path to the written JSON file.
#' @examples
#' summary <- list(status = "success", metrics = list(accuracy = 0.95))
#' config <- list(logging = list(enabled = TRUE, summary_log_dir = "logs/summaries", log_to_console = TRUE))
#' log_summary(summary, config)
#'
#' @export
#'

log_summary <- function(summary) {
  library(yaml)
  config <- yaml::read_yaml("config.yml")
  log_cfg <- config$logging

  if (!isTRUE(log_cfg$enabled)) {
    return(invisible(NULL))
  }

  if (!dir.exists(log_cfg$summary_log_dir)) {
    dir.create(log_cfg$summary_log_dir, recursive = TRUE)
  }

  timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
  log_file <- file.path(
    log_cfg$summary_log_dir,
    paste0("quality_log_", timestamp, ".json")
  )

  tryCatch(
    {
      jsonlite::write_json(
        summary,
        path = log_file,
        pretty = TRUE,
        auto_unbox = TRUE
      )
      if (isTRUE(log_cfg$log_to_console)) {
        message("Structured log written to: ", log_file)
      }
    },
    error = function(e) {
      warning("Failed to write structured log: ", e$message)
    }
  )

  invisible(log_file)
}
