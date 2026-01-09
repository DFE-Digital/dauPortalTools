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
#'   \item{Resolution Status}{Status code (0 = unresolved).}
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

quality_tests_get_data <- function() {
  conn <- sql_manager("dit")

  log_event("Starting function quality_tests_get_data")
  log_event(glue::glue("Received app_id: {app_id}"))

  schema_01a <- DBI::SQL(conf$schemas$db_schema_01a)

  # Construct SQL
  sql_command <- glue::glue_sql(
    "
    SELECT [quality_name],
           [quality_description]
    FROM {schema_01a}.[quality_check]
  ",
    .con = conn
  )

  log_event(glue::glue("Executing SQL: {sql_command}"), conf)

  summary_data <- DBI::dbGetQuery(conn, sql_command)

  summary_data <- summary_data |>
    dplyr::rename(
      "Quality Concern" = quality_name,
      "Description" = quality_description
    )

  log_event("Finished retrieving records")
  log_summary(summary_data)

  return(summary_data)
}
