#' Log an Event Message
#'
#' Logs a timestamped message to a file and optionally to the console.
#'
#' @param message Character string containing the message to log.
#' @return Invisibly returns NULL.
#' @export

.log_group_id <- NULL
.log_last_time <- NULL
.log_counter <- 0L

log_event <- function(message, debug = FALSE) {
  cfg <- get_config()
  log_cfg <- cfg$logging %||% list(enabled = FALSE)

  if (!isTRUE(log_cfg$enabled)) {
    return(invisible(NULL))
  }

  if (isTRUE(debug) && !isTRUE(log_cfg$debug_toggle)) {
    return(invisible(NULL))
  }

  now <- Sys.time()

  new_group <- FALSE

  if (is.null(.log_last_time)) {
    new_group <- TRUE
  } else {
    diff <- as.numeric(difftime(now, .log_last_time, units = "secs"))
    if (is.na(diff) || diff > 0.5) {
      new_group <- TRUE
    }
  }

  if (new_group) {
    .log_counter <<- .log_counter + 1L

    .log_group_id <<- paste0(
      format(now, "%H%M%S"),
      "-",
      Sys.getpid(),
      "-",
      .log_counter
    )
  }

  .log_last_time <<- now

  timestamp <- format(now, "%Y-%m-%d %H:%M:%OS3")
  level <- if (debug) "DEBUG" else "INFO"

  log_line <- paste0(
    "[",
    timestamp,
    "]",
    "[",
    level,
    "]",
    "[G:",
    .log_group_id,
    "] ",
    message
  )

  log_path <- log_cfg$log_path %||% "logs/events.log"

  if (!dir.exists(dirname(log_path))) {
    dir.create(dirname(log_path), recursive = TRUE, showWarnings = FALSE)
  }

  write(log_line, file = log_path, append = TRUE)

  if (isTRUE(log_cfg$log_to_console)) {
    cat(log_line, "\n")
  }

  invisible(NULL)
}
