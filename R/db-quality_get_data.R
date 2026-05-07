#' Retrieve Active Quality Issue Records
#'
#' Returns unresolved quality issues for the current application, with an
#' optional filter for a specific record ID.
#'
#' @param record Integer scalar or `NULL`. Optional record ID used to filter
#'   results. If `NULL` (default), all matching records are returned.
#'
#' @details
#' The current application ID is resolved using [utils_get_app_id()]. Only
#' records with `quality_status = 0` (i.e. unresolved issues) are retrieved.
#'
#' When a `record` value is supplied, results are further restricted to that
#' specific record ID.
#'
#' The query joins:
#' \itemize{
#'   \item `quality_list` (issue instances)
#'   \item `quality_check` (issue definitions)
#' }
#'
#' The database schema is resolved using [utils_resolve_schema()], and the
#' query is executed using [utils_db_get_query()].
#'
#' Database connections are managed internally and safely closed using
#' `on.exit()`. Logging is performed via [log_event()] at the start and
#' end of execution.
#'
#' @section Side Effects:
#' \itemize{
#'   \item Opens and closes a database connection.
#'   \item Writes log entries via [log_event()].
#' }
#'
#' @return A [`data.frame`] with the following columns:
#' \describe{
#'   \item{Quality Concern}{Character name of the quality issue.}
#'   \item{Description}{Character description of the issue.}
#'   \item{Date Identified}{Datetime the issue was created.}
#'   \item{Last Reviewed}{Datetime the issue was last checked.}
#' }
#'
#' @examples
#' \dontrun{
#' # All unresolved issues
#' issues <- quality_get_data()
#'
#' # Filter for specific record
#' issues <- quality_get_data(record = 1234)
#' }
#'
#' @seealso [quality_add_log()]
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
    FROM {utils_resolve_schema('db_schema_01a')}.[quality_list] l
    LEFT JOIN {utils_resolve_schema('db_schema_01a')}.[quality_check] c
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
