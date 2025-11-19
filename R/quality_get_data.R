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

quality_get_data <- function(record = NULL) {
  conf <- yaml::read_yaml("config.yml", eval.expr = TRUE)
  app_id <- conf$app_details$app_id
  conn <- sql_manager("dit")

  log_event("Starting function quality_get_data", conf)
  log_event(glue::glue("Received record: {record}, app_id: {app_id}"), conf)

  # Build record filter
  record_filter <- if (!is.null(record) && is.numeric(record)) {
    glue::glue_sql(" AND l.record_id = {record}", .con = conn)
  } else {
    DBI::SQL("")
  }

  schema_01a <- DBI::SQL(conf$schemas$db_schema_01a)

  # Construct SQL
  sql_command <- glue::glue_sql(
    "
    SELECT c.[quality_name],
           c.[quality_description],
           l.[quality_status],
           l.[date_created],
           l.[last_checked]
    FROM {schema_01a}.[quality_list] l
    LEFT JOIN {schema_01a}.[quality_check] c 
        ON c.quality_check_id = l.error_id
    LEFT JOIN {schema_01a}.[app_list] a 
        ON l.app_id = a.app_id
    WHERE l.app_id = {app_id}
      AND l.quality_status = 0
      {record_filter}
  ",
    .con = conn
  )

  log_event(glue::glue("Executing SQL: {sql_command}"), conf)

  summary_data <- tryCatch(
    {
      DBI::dbGetQuery(conn, sql_command)
    },
    error = function(e) {
      dauPortalTools::log_event(glue::glue(
        "Error fetching summary: {e$message}"
      ))
      return(data.frame(
        "Quality Concern" = NA,
        "Description" = NA,
        "Resolution Status" = NA,
        "Date Identified" = NA,
        "Last Reviewed" = NA
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

  log_event("Finished retrieving records", conf)
  log_summary(summary_data)

  return(summary_data)
}

get_aidt_quality_data <- function(record = NULL) {
  conf <- yaml::read_yaml("config.yml", eval.expr = TRUE)

  log_event(
    "This function has been renamed too quality_get_data, please update before this function is removed",
    conf
  )
  quality_get_data(record)
}
