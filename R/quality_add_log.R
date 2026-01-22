#' Insert a New Quality Check Log Entry
#'
#' Adds a new row to the `quality_check_log` table, recording when a quality
#' check was executed and how many new and closed issues were identified.
#' The function automatically timestamps the entry using the system time.
#'
#' @param quality_check_id Integer.
#'   The ID of the quality check being logged. Must correspond to an existing
#'   record in the `[quality_check]` table.
#'
#' @param live_issues Integer.
#'    The number of live issues after the test has ran.
#'
#' @param new_issues Integer.
#'   The number of newly identified issues found during this run.
#'
#' @param closed_issues Integer.
#'   The number of issues that were closed or resolved during this run.
#'
#' @return Invisibly returns the number of rows affected (typically `1L`)
#'   as returned by `DBI::dbExecute()`.
#'
#' @details
#' This function:
#' - Connects to SQL Server using `sql_manager("dit")`.
#' - Generates a timestamp in `YYYY-MM-DD HH:MM:SS` format.
#' - Constructs a fully parameterised SQL `INSERT` statement using
#'   `glue::glue_sql()` for safe interpolation.
#' - Inserts a new log entry into:
#'
#' ```
#' {conf$database}.{conf$schemas$db_schema_01a}.quality_check_log
#' ```
#'
#'
#' @examples
#' \dontrun{
#' # Log a quality check run
#' quality_add_log(
#'   quality_check_id = 12,
#'   new_issues = 3,
#'   closed_issues = 1
#' )
#' }
#'
#' @seealso
#' \code{\link[DBI]{dbExecute}},
#' \code{\link[glue]{glue_sql}}
#'
#' @export

quality_add_log <- function(
  quality_check_id,
  live_issues,
  new_issues,
  closed_issues
) {
  conn <- sql_manager("dit")
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

  query <- glue::glue_sql(
    "
      INSERT INTO {`conf$database`}.{`conf$schemas$db_schema_01a`}.[quality_check_log] (
        [quality_check_id],
        [last_run],
        [live_issues],
        [new_issues],
        [closed_issues]
      ) VALUES (
        {quality_check_id},
        {timestamp},
        {live_issues},
        {new_issues},
        {closed_issues}
      );
    ",
    .con = conn
  )

  DBI::dbExecute(conn, query)
}
