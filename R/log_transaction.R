#' Log a Transaction to the Central SQL Table
#'
#' This function inserts a transaction record into the central transaction table
#' and also logs a structured summary and event message
#'
#' @param action Character. The action performed (e.g., "Update Delivery Officer").
#' @param record_id Integer or NULL. The ID of the affected record.
#' @param record_type Character. The type of record (e.g., "twn_all_notices").
#' @param performed_by Character. The user performing the action.
#' @param status Character. Status of the action ("Success" or "Failure").
#' @param notes Character. Additional notes or context.
#'
#' @return Invisibly returns TRUE if successful, FALSE otherwise.
#' @examples
#' log_transaction(action = "Update Delivery Officer", record_id = 123,
#'                 record_type = "twn_all_notices", performed_by = "admin",
#'                 status = "Success", notes = "Updated delivery officer")
#'
#' @export
#'

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
  conf <- yaml::read_yaml("conf.yml")
  app_id <- conf$app_details$app_id

  conn <- sql_manager("dit")
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

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
        {timestamp}
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
