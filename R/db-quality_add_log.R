#' Insert a Quality Check Log Entry
#'
#' Records the execution of a quality check by inserting a new row into the
#' `quality_check_log` table. The entry captures the number of live, new, and
#' closed issues at the time of execution.
#'
#' @param quality_check_id Integer scalar. Identifier for the quality check.
#' @param live_issues Integer scalar or `NA`. Number of currently active issues.
#'   Defaults to `0`. Missing or `NA` values are coerced to `0`.
#' @param new_issues Integer scalar or `NA`. Number of newly identified issues.
#'   Defaults to `0`. Missing or `NA` values are coerced to `0`.
#' @param closed_issues Integer scalar or `NA`. Number of issues closed since
#'   the previous run. Defaults to `0`. Missing or `NA` values are coerced to `0`.
#'
#' @details
#' All count inputs are normalised to integer scalars. Values that are `NULL`,
#' length-zero, or `NA` are converted to `0`.
#'
#' The database schema is resolved using [utils_resolve_schema()], and the
#' query is executed using [utils_db_execute()]. The `last_run` timestamp is
#' recorded using `SYSUTCDATETIME()` at the database level.
#'
#' Database connections are managed internally and safely closed using
#' `on.exit()`. Logging is performed via [log_event()] at the start and
#' end of execution.
#'
#' @section Side Effects:
#' \itemize{
#'   \item Inserts a new row into the database.
#'   \item Opens and closes a database connection.
#'   \item Writes log entries via [log_event()].
#' }
#'
#' @return Invisible numeric scalar indicating the number of rows affected
#'   (typically `1L`).
#'
#' @examples
#' \dontrun{
#' quality_add_log(
#'   quality_check_id = 42,
#'   live_issues = 10,
#'   new_issues = 3,
#'   closed_issues = 2
#' )
#' }
#'
#' @seealso [utils_db_execute()], [log_event()]
#' @export
quality_add_log <- function(
  quality_check_id,
  live_issues = 0,
  new_issues = 0,
  closed_issues = 0,
  db_execute = utils_db_execute
) {
  log_event("Starting quality_add_log")

  # normalise scalars
  scalar_zero <- function(x) {
    if (length(x) == 0 || is.na(x)) 0L else as.integer(x)
  }

  quality_check_id <- as.integer(quality_check_id)
  live_issues <- scalar_zero(live_issues)
  new_issues <- scalar_zero(new_issues)
  closed_issues <- scalar_zero(closed_issues)

  conn <- sql_manager("dit")
  on.exit(
    {
      try(DBI::dbDisconnect(conn), silent = TRUE)
      log_event("Finished quality_add_log")
    },
    add = TRUE
  )

  query <- glue_sql(
    "
    INSERT INTO {utils_resolve_schema('db_schema_01a')}.[quality_check_log] (
      quality_check_id,
      last_run,
      live_issues,
      new_issues,
      closed_issues
    ) VALUES (
      {quality_check_id},
      SYSUTCDATETIME(),
      {live_issues},
      {new_issues},
      {closed_issues}
    );
    ",
    .con = conn
  )

  db_execute(conn, query)
}
