#' Log an Event Message
#'
#' Logs a timestamped message to a file and optionally to the console.
#'
#' Logging behaviour is controlled by the `logging` section of the
#' configuration returned by `get_config()`.
#'
#' @param message Character string containing the message to log.
#'
#' @return Invisibly returns NULL.
#' @export
log_event <- function(message) {
  cfg <- get_config()

  log_cfg <- cfg$logging
  if (!isTRUE(log_cfg$enabled)) {
    return(invisible(NULL))
  }

  log_path <- log_cfg$event_log_path
  log_dir <- dirname(log_path)

  if (!dir.exists(log_dir)) {
    dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
  }

  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  log_line <- sprintf("[%s] %s", timestamp, message)

  cat(log_line, file = log_path, append = TRUE, sep = "\n")

  if (isTRUE(log_cfg$log_to_console)) {
    message(log_line)
  }

  invisible(NULL)
}
