#' Retrieve Quality Issue Records from AIDT Tables
#'
#' Retrieves unresolved quality issues for the current application,
#' optionally filtered by a specific record ID.
#'
#' @param record Optional integer. Record ID to filter results.
#' @param db_get_query Function used to execute SQL queries.
#'
#' @return A data frame of quality issue records.
#' @export
quality_get_data <- function(
  record = NULL,
  db_get_query = utils_db_get_query
) {
  log_event("Starting quality_get_data")

  app_id <- utils_get_app_id()

  log_event(
    glue::glue("Received record_id = {record} for app_id = {app_id}")
  )

  conn <- sql_manager("dit")
  on.exit(
    {
      try(DBI::dbDisconnect(conn), silent = TRUE)
      log_event(
        glue::glue(
          "Finished quality_get_data (rows_returned = {nrow(result)})"
        )
      )
    },
    add = TRUE
  )

  record_filter <- if (!is.null(record)) {
    glue_sql(
      "AND l.record_id = {record}",
      .con = conn
    )
  } else {
    DBI::SQL("")
  }

  query <- glue_sql(
    "
    SELECT
      c.quality_name        AS [Quality Concern],
      c.quality_description AS [Description],
      l.date_created        AS [Date Identified],
      l.last_checked        AS [Last Reviewed]
    FROM {utils_resolve_schema('01a')}.[quality_list] l
    LEFT JOIN {utils_resolve_schema('01a')}.[quality_check] c
           ON c.quality_check_id = l.error_id
    WHERE l.app_id = {app_id}
      AND l.quality_status = 0
    {record_filter}
    ",
    .con = conn
  )

  result <- db_get_query(conn, query)

  result
}
