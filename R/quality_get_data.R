#' Retrieve Quality Issue Records from AIDT Tables
#'
#' This function queries the quality_list table for unresolved quality issues (`quality_status = 0`)
#' associated with a specific application ID, optionally filtered by a record ID. It joins with the
#' quality_check and app_list tables to enrich the results.
#'
#' @param record Optional. A numeric record ID to filter the results. If `NULL`, all records for the app are returned.
#'
#' @return A data frame containing quality issue records with the following columns:
#' \describe{
#'   \item{Quality Concern}{Name of the quality issue.}
#'   \item{Description}{Detailed description of the issue.}
#'   \item{Date Identified}{Date the issue was created.}
#'   \item{Last Reviewed}{Date the issue was last checked.}
#' }
#'
#' @examples
#' \dontrun{
#' # Retrieve all unresolved quality issues for the current app
#' get_aidt_quality_data()
#'
#' # Retrieve issues for a specific record ID
#' get_aidt_quality_data(record = 12345)
#' }
#'
#' @export
#'

quality_get_data <- function(record = NULL) {
  # Log the start of the function
  log_event("Starting function quality_get_data")

  app_id <- conf$app_details$app_id
  db_schema_01a <- DBI::SQL(conf$schemas$db_schema_01a)

  # Log the received record ID and app ID
  log_event(glue::glue("Received {record} id for app id: {app_id}."))

  record <- if (!is.null(record)) {
    glue::glue_sql(
      " AND l.record_id = '{record}'",
      .con = conn
    )
  } else {
    DBI::SQL("")
  }

  # Construct the SQL query to retrieve quality data

  sql_command <- glue::glue_sql(
    "
    SELECT c.quality_name AS 'Quality Concern',
           c.quality_description AS 'Description',
           [date_created] AS 'Date Identified',
           [last_checked] AS 'Last Reviewed'
    FROM {db_schema_01a}.[quality_list] l
    LEFT JOIN {db_schema_01a}.[quality_check] c 
        ON c.quality_check_id = l.error_id
    LEFT JOIN {db_schema_01a}.[app_list] a 
        ON l.app_id = a.app_id
    WHERE l.app_id = {app_id}
      AND quality_status = 0
    {record}  ",
    .con = conn
  )

  summary_data <- DBI::dbGetQuery(conn, sql_command)

  # Log the completion of data retrieval
  log_event("Finished retrieving records")

  # Log a structured summary of the retrieved data
  log_summary(quality_records)

  # Return the data frame of quality records
  return(quality_records)
}
