#' Insert a New Quality Check Log Entry
#'
#' Adds a new row to the `[01_AIDT].[quality_check_log]` table, recording when a
#' quality check was executed and how many live, new, and closed issues were
#' identified. The log timestamp is generated in SQL Server using `SYSDATETIME()`.
#'
#' @param quality_check_id Integer scalar.
#'   The ID of the quality check being logged. Must correspond to an existing
#'   record in the `[quality_check]` table.
#'
#' @param live_issues Integer scalar.
#'   The number of live issues after the check has run. If `NULL`, `NA`, or
#'   length-0, this is treated as `0`.
#'
#' @param new_issues Integer scalar.
#'   The number of newly identified issues found during this run. If `NULL`, `NA`,
#'   or length-0, this is treated as `0`.
#'
#' @param closed_issues Integer scalar.
#'   The number of issues that were closed/resolved during this run. If `NULL`,
#'   `NA`, or length-0, this is treated as `0`.
#'
#' @return Invisibly returns the number of rows affected (typically `1L`) as
#'   returned by [DBI::dbExecute()].
#'
#' @details
#' This function:
#' \itemize{
#'   \item Connects to SQL Server using `dauPortalTools::sql_manager("dit")`.
#'   \item Reads configuration from `config.yml` to obtain the target schema
#'         (`conf$schemas$db_schema_01a`).
#'   \item Normalises `live_issues`, `new_issues`, and `closed_issues` so that
#'         `NULL`, `NA`, or length-0 values are converted to `0` (avoids empty
#'         SQL generation in `glue_sql()`).
#'   \item Constructs a parameterised `INSERT` statement using [glue::glue_sql()].
#'         The schema identifier is wrapped with [DBI::SQL()] to ensure it is
#'         treated as SQL (not quoted as a string).
#'   \item Uses SQL Server `SYSDATETIME()` to populate `[last_run]`.
#' }
#'
#' @examples
#' \dontrun{
#' # Log a quality check run
#' quality_add_log(
#'   quality_check_id = 21,
#'   live_issues = 10,
#'   new_issues = 0,
#'   closed_issues = 0
#' )
#'
#' # Safe with NULL/NA (normalised to 0)
#' quality_add_log(
#'   quality_check_id = 21,
#'   live_issues = NULL,
#'   new_issues = NA,
#'   closed_issues = numeric(0)
#' )
#' }
#'
#' @seealso
#' [DBI::dbExecute()], [glue::glue_sql()], [DBI::SQL()]
#'
#' @export

quality_add_log <- function(
  quality_check_id,
  live_issues = 0,
  new_issues = 0,
  closed_issues = 0
) {
  scalar_zero <- function(x) {
    if (length(x) == 0 || is.na(x)) 0 else as.integer(x)
  }

  live_issues <- scalar_zero(live_issues)
  new_issues <- scalar_zero(new_issues)
  closed_issues <- scalar_zero(closed_issues)

  conn <- dauPortalTools::sql_manager("dit")
  conf <- yaml::read_yaml("config.yml")

  schema <- DBI::SQL(conf$schemas$db_schema_01a)

  query <- glue::glue_sql(
    "
      INSERT INTO {schema}.[quality_check_log] (
        [quality_check_id],
        [last_run],
        [live_issues],
        [new_issues],
        [closed_issues]
      ) VALUES (
        {quality_check_id},
        SYSDATETIME(),
        {live_issues},
        {new_issues},
        {closed_issues}
      );
    ",
    .con = conn
  )

  DBI::dbExecute(conn, query)
}
