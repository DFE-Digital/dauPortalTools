#' Log an Event Message
#'
#' Logs a timestamped message to a file and optionally to the console.
#'
#' @param message Character string containing the message to log.
#' @return Invisibly returns NULL.
#' @export

.log_con <- NULL

log_event <- function(message) {
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

  con <- file(log_path, open = "a")
  writeLines(log_line, con)
  flush(con)
  close(con)

  if (isTRUE(log_cfg$log_to_console)) {
    cat(log_line, "\n")
    flush.console()
  }

  invisible(NULL)
}
