#' Log an Event Message
#'
#' @param message Character message to log
#' @param debug Logical, only log if debug_toggle = TRUE
#' @export
log_event <- function(message, debug = FALSE) {
  cfg <- get_config()
  log_cfg <- cfg$logging %||% list(enabled = FALSE)

  # ---- master switch ----
  if (!isTRUE(log_cfg$enabled)) {
    return(invisible(NULL))
  }

  # ---- debug filter ----
  if (isTRUE(debug) && !isTRUE(log_cfg$debug_toggle)) {
    return(invisible(NULL))
  }

  log_path <- log_cfg$log_path %||% "logs/events.log"

  if (!dir.exists(dirname(log_path))) {
    dir.create(dirname(log_path), recursive = TRUE, showWarnings = FALSE)
  }

  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  level <- if (debug) "DEBUG" else "INFO"

  log_line <- sprintf("[%s][%s] %s", timestamp, level, message)

  write(log_line, file = log_path, append = TRUE)

  if (isTRUE(log_cfg$log_to_console)) {
    cat(log_line, "\n")
    flush.console()
  }

  invisible(NULL)
}
