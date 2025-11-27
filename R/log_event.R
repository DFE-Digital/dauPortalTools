#' Log an Event Message
#'
#' This function logs a simple timestamped message to a specified file and optionally to the console.
#'
#' @param message A character string containing the message to log.
#' @param config A list containing logging configuration. Must include a `logging` sublist with:
#'   \describe{
#'     \item{enabled}{Logical. Whether logging is enabled.}
#'     \item{event_log_path}{Character. Path to the event log file.}
#'     \item{log_to_console}{Logical. Whether to also print the log message to the console.}
#'   }
#'
#' @return Invisibly returns `NULL`. Writes the log message to the specified file.
#' @examples
#' config <- list(logging = list(enabled = TRUE, event_log_path = "logs/events.log", log_to_console = TRUE))
#' log_event("Process started", config)
#'
#' @export
#'

log_event <- function(message, config) {
  library(yaml)
  config <- yaml::read_yaml("config.yml", eval.expr = TRUE)
  log_cfg <- config$logging

  if (!isTRUE(log_cfg$enabled)) {
    return(invisible(NULL))
  }

  if (!dir.exists(dirname(log_cfg$event_log_path))) {
    dir.create(dirname(log_cfg$event_log_path), recursive = TRUE)
  }

  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  log_line <- sprintf("[%s] %s\n", timestamp, message)

  cat(log_line, file = log_cfg$event_log_path, append = TRUE)

  if (isTRUE(log_cfg$log_to_console)) message(log_line)
}
