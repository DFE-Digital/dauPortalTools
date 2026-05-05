#' Insert a New Quality Check Log Entry
#'
#' Logs the execution of a quality check, recording counts of live, new,
#' and closed issues.
#'
#' @param quality_check_id Integer scalar.
#' @param live_issues Integer scalar or NULL/NA.
#' @param new_issues Integer scalar or NULL/NA.
#' @param closed_issues Integer scalar or NULL/NA.
#' @param db_execute Function used to execute SQL (dependency injection).
#'
#' @return Invisibly returns number of rows affected.
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
    INSERT INTO {utils_resolve_schema('01a')}.[quality_check_log] (
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
