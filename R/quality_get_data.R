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

get_aidt_quality_data <- function(record = NULL) {
  # Log the start of the function
  log_event("Starting function get_aidt_quality_data", config)

  # Load configuration from YAML file
  library(yaml)
  config <- yaml::read_yaml("config.yml")
  app_id <- config$app_details$app_id

  # Log the received record ID and app ID
  log_event(glue::glue("Received {record} id for app id: {app_id}."), config)

  # Validate and format the record filter if provided
  if (!is.null("record") & is.numeric(record)) {
    record <- paste0(" AND l.record_id = '", record, "'")
  } else {
    record <- ""
  }

  # Construct the SQL query to retrieve quality data

  sql_command <- glue::glue_sql(
    "
    SELECT c.quality_name,
           c.quality_description,
           [quality_status],
           [date_created],
           [last_checked]
    FROM {`config$database`}.{`config$schema$db_schema_01a`}.[quality_list] l
    LEFT JOIN {`config$database`}.{`config$schema$db_schema_01a`}.[quality_check] c 
        ON c.quality_check_id = l.error_id
    LEFT JOIN {`config$database`}.{`config$schema$db_schema_01a`}.[app_list] a 
        ON l.app_id = a.app_id
    WHERE l.app_id = {app_id}
      AND quality_status = 0
    {record}  ",
    .con = conn
  )

  summary_data <- tryCatch(
    {
      DBI::dbGetQuery(conn, sql_command)
    },
    error = function(e) {
      dauPortalTools::log_event(glue::glue(
        "Error fetching summary: {e$message}"
      ))
      return(data.frame(
        total_live_records = NA,
        updated_records = NA,
        quality_issues = NA
      ))
    }
  )

  summary_data <- summary_data |>
    dplyr::rename(
      "Quality Concern" = quality_name,
      "Description" = quality_description,
      "Resolution Status" = quality_status,
      "Date Identified" = date_created,
      "Last Reviewed" = last_checked
    )

  # Log the completion of data retrieval
  log_event("Finished retrieving records")

  # Log a structured summary of the retrieved data
  log_summary(quality_records)

  # Return the data frame of quality records
  return(quality_records)
}
