#' Log an Event Message
#'
#' Writes a timestamped log message to file and optionally to the console.
#'
#' @param message Character scalar. Message to log.
#' @param debug Logical scalar. If `TRUE`, message is only logged when
#'   `logging$debug_toggle` is enabled in the configuration.
#'
#' @details
#' Logging behaviour is controlled via the configuration returned by
#' [get_config()]. The following fields are used:
#' \itemize{
#'   \item \code{logging$enabled} – master switch for all logging
#'   \item \code{logging$debug_toggle} – enables debug-level messages
#'   \item \code{logging$log_path} – file path for log output
#'   \item \code{logging$log_to_console} – whether to also print to console
#' }
#'
#' If logging is disabled, the function exits silently.
#'
#' @section Side Effects:
#' \itemize{
#'   \item Writes log entries to a file
#'   \item Creates log directories if required
#'   \item Optionally writes to console output
#' }
#'
#' @return Invisibly returns `NULL`.
#'
#' @seealso [get_config()]
#'
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

#' Deprecated Summary Logger
#'
#' This function is deprecated. Use [log_event()] instead.
#'
#' @param ... Ignored.
#'
#' @return Invisibly returns `NULL`.
#'
#' @export

log_summary <- function(...) {
  .Deprecated(
    "log_event",
    msg = "log_summary() is deprecated; use log_event() instead"
  )
  invisible(NULL)
}

#' Log a Transaction to the Database
#'
#' Inserts a transaction record into the central transaction table and logs
#' a corresponding event message.
#'
#' @param action Character scalar. Description of the action performed.
#' @param record_id Integer or `NULL`. Identifier of the affected record.
#' @param record_type Character scalar. Type of record affected.
#' @param performed_by Character scalar. User performing the action.
#' @param status Character scalar. Outcome of the action (e.g. `"Success"` or `"Failure"`).
#' @param notes Character scalar. Additional context or details.
#'
#' @details
#' The function:
#' \itemize{
#'   \item Reads configuration from `config.yml`
#'   \item Inserts a record into the `transaction_table`
#'   \item Logs a structured summary via deprecated [log_summary()]
#'   \item Logs a formatted message via [log_event()]
#' }
#'
#' If the database operation fails, an error is logged and the function
#' returns `FALSE`.
#'
#' @section Side Effects:
#' \itemize{
#'   \item Writes to the database
#'   \item Reads configuration from disk
#'   \item Writes log entries via [log_event()]
#' }
#'
#' @return Logical scalar. Returns `TRUE` on success, `FALSE` on failure
#'   (invisibly).
#'
#' @examples
#' \dontrun{
#' log_transaction(
#'   action = "Update Delivery Officer",
#'   record_id = 123,
#'   record_type = "twn_all_notices",
#'   performed_by = "admin",
#'   status = "Success",
#'   notes = "Updated delivery officer"
#' )
#' }
#'
#' @seealso [log_event()]
#'
#' @export

log_transaction <- function(
  action,
  record_id,
  record_type,
  performed_by,
  status,
  notes
) {
  # Load confuration from YAML file
  library(yaml)
  conf <- yaml::read_yaml("config.yml")
  app_id <- conf$app_details$app_id

  conn <- sql_manager("dit")

  tryCatch(
    {
      query <- glue::glue_sql(
        "
      INSERT INTO {`conf$database`}.{`conf$schemas$db_schema_01a`}.[transaction_table] (
        app_id, action, record_id, record_type, performed_by, performed_on, status, notes, created_on
      ) VALUES (
        {app_id},
        {action},
        {if (is.null(record_id)) DBI::SQL('NULL') else record_id},
        {record_type},
        {performed_by},
        {timestamp},
        {status},
        {notes},
        SYSUTCDATETIME()
      );
    ",
        .con = conn
      )

      DBI::dbExecute(conn, query)

      # Log structured summary
      summary <- list(
        app_id = app_id,
        action = action,
        record_id = record_id,
        record_type = record_type,
        performed_by = performed_by,
        status = status,
        notes = notes,
        timestamp = timestamp
      )
      log_summary(summary)

      # Log event
      log_event(
        glue::glue("[{status}] {action} on {record_type} (ID: {record_id})"),
        conf = NULL
      )

      invisible(TRUE)
    },
    error = function(e) {
      log_event(
        glue::glue("Transaction logging failed: {e$message}"),
        conf = NULL
      )
      invisible(FALSE)
    }
  )
}
