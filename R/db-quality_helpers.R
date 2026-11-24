# ==============================================================================
# File: R/db-quality.R
# Purpose: Database query and mutation functions for Data Quality checks & logs
#
# Functions:
#   - db_quality_get_all()               : Fetch all active quality issues
#   - db_quality_get_data()              : Fetch unresolved quality issues for an app
#   - db_quality_get_dashboard_records() : Fetch active quality issues for dashboard display
#   - db_quality_insert_log()            : Record a quality check execution summary
#
# Dependencies: DBI, glue, internal
# ==============================================================================

#' Fetch all unresolved quality issue records
#'
#' Retrieves all active, unresolved quality issue records (`quality_status = 0`)
#' across all applications from `[quality_list]`, joined to `[quality_check]`
#' for issue definitions.
#'
#' @details
#' Queries `[quality_list]` and `[quality_check]` within the schema resolved by
#' `utils_resolve_schema("db_schema_01a")`.
#'
#' @section Side Effects:
#' \itemize{
#'   \item Opens and closes a database connection via [sql_manager()]
#'   \item Writes debug start/completion entries and error logs via [log_event()]
#' }
#'
#' @param conn_name Character scalar. Database configuration alias passed to
#'   [sql_manager()]. Defaults to `"dit"`.
#'
#' @return A `data.frame` with 9 columns, or `NULL` if a database connection or
#'   query error occurs:
#'   \describe{
#'     \item{`record_id`}{Integer. Internal record primary key.}
#'     \item{`app_id`}{Integer. Originating application context ID.}
#'     \item{`urn`}{Integer/Character. School Unique Reference Number.}
#'     \item{`region`}{Character. Target geographic region.}
#'     \item{`with_rcs`}{Logical/Integer. Indicator for RCS status assignment.}
#'     \item{`quality_concern`}{Character. Name of the quality check rule.}
#'     \item{`description`}{Character. Description of the quality issue.}
#'     \item{`date_identified`}{POSIXct. Timestamp when the issue was created.}
#'     \item{`last_reviewed`}{POSIXct. Timestamp when the issue was last checked.}
#'   }
#'
#' @family quality database functions
#' @seealso [db_quality_get_data()], [db_quality_get_dashboard_records()],
#'   [db_quality_insert_log()], [sql_manager()], [utils_resolve_schema()]
#'
#' @export
#' @examples
#' \dontrun{
#' all_issues <- db_quality_get_all()
#' }
db_quality_get_all <- function(conn_name = "dit") {
  fn_name <- as.character(match.call()[[1]])
  log_event(paste("Starting", fn_name), debug = TRUE)
  on.exit(log_event(paste("Finished", fn_name)), add = TRUE)

  schema <- utils_resolve_schema("db_schema_01a")

  tryCatch(
    {
      conn <- sql_manager(conn_name)
      on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

      query <- glue::glue_sql(
        "
        SELECT
          l.[record_id],
          l.[app_id],
          l.[urn],
          l.[region],
          l.[with_rcs],
          c.[quality_name]        AS [quality_concern],
          c.[quality_description] AS [description],
          l.[date_created]        AS [date_identified],
          l.[last_checked]        AS [last_reviewed]
        FROM {`schema`}.[quality_list] l
        LEFT JOIN {`schema`}.[quality_check] c
          ON c.[quality_check_id] = l.[error_id]
        WHERE l.[quality_status] = 0;
        ",
        .con = conn
      )

      utils_db_get_query(conn, query)
    },
    error = function(e) {
      log_event(paste0(fn_name, " failed: ", e$message), type = 2)
      NULL
    }
  )
}

#' Fetch active quality issue records for an application
#'
#' Returns unresolved quality issues (`quality_status = 0`) for the target
#' application, with an optional filter for a specific `record_id`.
#'
#' @details
#' Queries `[quality_list]` joined to `[quality_check]` within
#' `utils_resolve_schema("db_schema_01a")`. Defaults `app_id` to
#' [utils_get_app_id()].
#'
#' @section Side Effects:
#' \itemize{
#'   \item Opens and closes a database connection via [sql_manager()]
#'   \item Writes debug start/completion entries and error logs via [log_event()]
#' }
#'
#' @param record Optional integer scalar. Record ID used to filter results.
#'   If `NULL` (default), all unresolved issues for `app_id` are returned.
#' @param app_id Integer scalar. Target application ID scope. Defaults to
#'   [utils_get_app_id()].
#' @param conn_name Character scalar. Database configuration alias passed to
#'   [sql_manager()]. Defaults to `"dit"`.
#'
#' @return A `data.frame` with 4 columns, or `NULL` on invalid input or
#'   database error:
#'   \describe{
#'     \item{`quality_concern`}{Character. Name of the quality issue.}
#'     \item{`description`}{Character. Description of the issue.}
#'     \item{`date_identified`}{POSIXct. Timestamp when the issue was created.}
#'     \item{`last_reviewed`}{POSIXct. Timestamp when the issue was last checked.}
#'   }
#'
#' @family quality database functions
#' @seealso [db_quality_get_all()], [db_quality_get_dashboard_records()],
#'   [db_quality_insert_log()], [utils_get_app_id()]
#'
#' @export
#' @examples
#' \dontrun{
#' # All unresolved issues for the current app
#' issues <- db_quality_get_data()
#'
#' # Filter for a specific record
#' record_issues <- db_quality_get_data(record = 1234L)
#' }
db_quality_get_data <- function(
  record = NULL,
  app_id = utils_get_app_id(),
  conn_name = "dit"
) {
  fn_name <- as.character(match.call()[[1]])
  log_event(paste("Starting", fn_name), debug = TRUE)
  on.exit(log_event(paste("Finished", fn_name)), add = TRUE)

  aid <- suppressWarnings(as.integer(app_id))
  if (length(aid) != 1L || is.na(aid)) {
    log_event(paste0(fn_name, " failed: invalid app_id"), type = 2)
    return(NULL)
  }

  rec_id <- NULL
  if (!is.null(record)) {
    rec_id <- suppressWarnings(as.integer(record))
    if (length(rec_id) != 1L || is.na(rec_id)) {
      log_event(paste0(fn_name, " failed: invalid record"), type = 2)
      return(NULL)
    }
  }

  schema <- utils_resolve_schema("db_schema_01a")

  tryCatch(
    {
      conn <- sql_manager(conn_name)
      on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

      record_filter <- if (!is.null(rec_id)) {
        glue::glue_sql("AND l.[record_id] = {rec_id}", .con = conn)
      } else {
        DBI::SQL("")
      }

      query <- glue::glue_sql(
        "
        SELECT
          c.[quality_name]        AS [quality_concern],
          c.[quality_description] AS [description],
          l.[date_created]        AS [date_identified],
          l.[last_checked]        AS [last_reviewed]
        FROM {`schema`}.[quality_list] l
        LEFT JOIN {`schema`}.[quality_check] c
          ON c.[quality_check_id] = l.[error_id]
        WHERE l.[app_id] = {aid}
          AND l.[quality_status] = 0
          {record_filter};
        ",
        .con = conn
      )

      res <- utils_db_get_query(conn, query)
      log_event(
        glue::glue("{fn_name} retrieved {nrow(res)} row(s) (app_id = {aid})"),
        debug = TRUE
      )
      res
    },
    error = function(e) {
      log_event(paste0(fn_name, " failed: ", e$message), type = 2)
      NULL
    }
  )
}

#' Fetch quality issues for dashboard display
#'
#' Fetches active, unresolved quality issues (`quality_status = 0`) including
#' URN, geographic region, and application/record identifiers to support
#' cross-portal dashboard filtering and contextual deep-linking.
#'
#' @details
#' Queries `[quality_list]` joined to `[quality_check]` within
#' `utils_resolve_schema("db_schema_01a")`. If `app_id` is `NULL`, records
#' across all applications are returned.
#'
#' @section Side Effects:
#' \itemize{
#'   \item Opens and closes a database connection via [sql_manager()]
#'   \item Writes debug start/completion entries and error logs via [log_event()]
#' }
#'
#' @param app_id Optional integer scalar. Application identifier used to filter
#'   results. If `NULL` (default), returns active issues across all apps.
#' @param conn_name Character scalar. Database configuration alias passed to
#'   [sql_manager()]. Defaults to `"dit"`.
#'
#' @return A `data.frame` with 6 columns, or `NULL` on invalid input or
#'   database error:
#'   \describe{
#'     \item{`urn`}{Integer/Character. Unique school reference number.}
#'     \item{`region`}{Character. Target geographic region.}
#'     \item{`quality_issue`}{Character. Textual description of the quality concern.}
#'     \item{`app_id`}{Integer. Originating application context ID.}
#'     \item{`record_id`}{Integer. Internal record primary key.}
#'     \item{`with_rcs`}{Logical/Integer. Indicator for RCS status assignment (`0` or `1`).}
#'   }
#'
#' @family quality database functions
#' @seealso [db_quality_get_all()], [db_quality_get_data()],
#'   [db_quality_insert_log()]
#'
#' @export
#' @examples
#' \dontrun{
#' # All active quality issues across apps
#' dash_all <- db_quality_get_dashboard_records()
#'
#' # Filtered to a single app
#' dash_app <- db_quality_get_dashboard_records(app_id = 1L)
#' }
db_quality_get_dashboard_records <- function(app_id = NULL, conn_name = "dit") {
  fn_name <- as.character(match.call()[[1]])
  log_event(paste("Starting", fn_name), debug = TRUE)
  on.exit(log_event(paste("Finished", fn_name)), add = TRUE)

  aid <- NULL
  if (!is.null(app_id)) {
    aid <- suppressWarnings(as.integer(app_id))
    if (length(aid) != 1L || is.na(aid)) {
      log_event(paste0(fn_name, " failed: invalid app_id"), type = 2)
      return(NULL)
    }
  }

  schema <- utils_resolve_schema("db_schema_01a")

  tryCatch(
    {
      conn <- sql_manager(conn_name)
      on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

      app_filter <- if (!is.null(aid)) {
        glue::glue_sql("AND l.[app_id] = {aid}", .con = conn)
      } else {
        DBI::SQL("")
      }

      query <- glue::glue_sql(
        "
        SELECT
          l.[urn]                 AS [urn],
          l.[region]              AS [region],
          c.[quality_description] AS [quality_issue],
          l.[app_id]              AS [app_id],
          l.[record_id]           AS [record_id],
          l.[with_rcs]            AS [with_rcs]
        FROM {`schema`}.[quality_list] l
        LEFT JOIN {`schema`}.[quality_check] c
          ON c.[quality_check_id] = l.[error_id]
        WHERE l.[quality_status] = 0
          {app_filter};
        ",
        .con = conn
      )

      res <- utils_db_get_query(conn, query)
      log_event(
        glue::glue("{fn_name} retrieved {nrow(res)} row(s)"),
        debug = TRUE
      )
      res
    },
    error = function(e) {
      log_event(paste0(fn_name, " failed: ", e$message), type = 2)
      NULL
    }
  )
}

#' Insert a quality check execution log entry
#'
#' Records the execution of a quality check by inserting a new row into
#' `[quality_check_log]`, capturing live, new, and closed issue counts at the
#' current UTC database timestamp (`SYSUTCDATETIME()`).
#'
#' @details
#' Inserts into `[quality_check_log]` within
#' `utils_resolve_schema("db_schema_01a")`. Count inputs (`live_issues`,
#' `new_issues`, `closed_issues`) that are `NULL`, length-zero, or `NA` are
#' coerced to `0L`.
#'
#' @section Side Effects:
#' \itemize{
#'   \item Inserts a row into `[quality_check_log]`
#'   \item Opens and closes a database connection via [sql_manager()]
#'   \item Writes debug start/completion entries and error logs via [log_event()]
#' }
#'
#' @param quality_check_id Integer scalar. Identifier for the quality check.
#' @param live_issues Integer scalar or `NA`. Number of currently active issues.
#'   Defaults to `0L`.
#' @param new_issues Integer scalar or `NA`. Number of newly identified issues.
#'   Defaults to `0L`.
#' @param closed_issues Integer scalar or `NA`. Number of issues closed since
#'   the previous run. Defaults to `0L`.
#' @param conn_name Character scalar. Database configuration alias passed to
#'   [sql_manager()]. Defaults to `"dit"`.
#'
#' @return Integer scalar (invisibly) indicating the number of rows affected
#'   (`1L` on success, `0L` on validation failure or database error).
#'
#' @family quality database functions
#' @seealso [db_quality_get_all()], [db_quality_get_data()],
#'   [utils_db_execute()], [log_event()]
#'
#' @export
#' @examples
#' \dontrun{
#' db_quality_insert_log(
#'   quality_check_id = 42L,
#'   live_issues = 10L,
#'   new_issues = 3L,
#'   closed_issues = 2L
#' )
#' }
db_quality_insert_log <- function(
  quality_check_id,
  live_issues = 0L,
  new_issues = 0L,
  closed_issues = 0L,
  conn_name = "dit"
) {
  fn_name <- as.character(match.call()[[1]])
  log_event(paste("Starting", fn_name), debug = TRUE)
  on.exit(log_event(paste("Finished", fn_name)), add = TRUE)

  qcid <- suppressWarnings(as.integer(quality_check_id))
  if (length(qcid) != 1L || is.na(qcid)) {
    log_event(paste0(fn_name, " failed: invalid quality_check_id"), type = 2)
    return(invisible(0L))
  }

  scalar_zero <- function(x) {
    val <- suppressWarnings(as.integer(x))
    if (length(val) != 1L || is.na(val)) 0L else val
  }

  live_n <- scalar_zero(live_issues)
  new_n <- scalar_zero(new_issues)
  closed_n <- scalar_zero(closed_issues)

  schema <- utils_resolve_schema("db_schema_01a")

  tryCatch(
    {
      conn <- sql_manager(conn_name)
      on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

      query <- glue::glue_sql(
        "
        INSERT INTO {`schema`}.[quality_check_log] (
          [quality_check_id],
          [last_run],
          [live_issues],
          [new_issues],
          [closed_issues]
        ) VALUES (
          {qcid},
          SYSUTCDATETIME(),
          {live_n},
          {new_n},
          {closed_n}
        );
        ",
        .con = conn
      )

      rows <- as.integer(utils_db_execute(conn, query))
      invisible(rows)
    },
    error = function(e) {
      log_event(paste0(fn_name, " failed: ", e$message), type = 2)
      invisible(0L)
    }
  )
}
