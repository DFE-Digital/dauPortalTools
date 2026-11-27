# ==============================================================================
# File: R/db-analytics.R
# Purpose: Database query and mutation functions for App Analytics & Audit Logs
#
# Functions:
#   - db_analytics_get_all()          : Fetch app analytics telemetry records
#   - db_analytics_get_summary()      : Fetch aggregated usage metrics for an app
#   - db_analytics_insert()           : Insert a standard clickstream telemetry event
#   - db_analytics_record_download()  : Record a file download event
#   - db_analytics_record_login()     : Record a user login / initial load event
#   - db_analytics_insert_audit_log() : Write a centralised system audit log entry
#
# Dependencies: DBI, glue, internal
# ==============================================================================

#' Fetch application analytics telemetry records
#'
#' Retrieves telemetry events from `[app_analytics]`, ordered most recent
#' first. Optionally filters to a specific `app_id`.
#'
#' @details
#' Queries `[app_analytics]` within the schema resolved by
#' `utils_resolve_schema("db_schema_01sr")`.
#'
#' @section Side Effects:
#' \itemize{
#'   \item Opens and closes a database connection via [sql_manager()]
#'   \item Writes debug start/completion entries and error logs via [log_event()]
#' }
#'
#' @param app_id Optional integer scalar. Application identifier used to filter
#'   results. If `NULL` (default), returns events across all applications.
#' @param conn_name Character scalar. Database configuration alias passed to
#'   [sql_manager()]. Defaults to `"dit"`.
#'
#' @return A `data.frame` ordered by `event_timestamp` descending with 7 columns,
#'   or `NULL` on invalid input or database error:
#'   \describe{
#'     \item{`user_id`}{Integer. Canonical user ID.}
#'     \item{`app_id`}{Integer. Originating application ID.}
#'     \item{`env_id`}{Integer. Environment identifier (1 = dev, 2 = test, 3 = prod).}
#'     \item{`event_timestamp`}{POSIXct. UTC timestamp of the event.}
#'     \item{`page_name`}{Character. Application page where the event occurred.}
#'     \item{`action_type`}{Character. High-level action category (e.g., `"Load"`, `"Download"`).}
#'     \item{`action_sub_type`}{Character. Action detail or downloaded file name.}
#'   }
#'
#' @family analytics database functions
#' @seealso [db_analytics_get_summary()], [db_analytics_insert()],
#'   [db_analytics_record_login()], [db_analytics_record_download()],
#'   [db_analytics_insert_audit_log()], [sql_manager()]
#'
#' @export
#' @examples
#' \dontrun{
#' events <- db_analytics_get_all()
#' app_events <- db_analytics_get_all(app_id = 1L)
#' }
db_analytics_get_all <- function(app_id = NULL, conn_name = "dit") {
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

  schema <- utils_resolve_schema("db_schema_01sr")

  tryCatch(
    {
      conn <- sql_manager(conn_name)
      on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

      where_sql <- if (!is.null(aid)) {
        glue::glue_sql("WHERE [app_id] = {aid}", .con = conn)
      } else {
        DBI::SQL("")
      }

      query <- glue::glue_sql(
        "
        SELECT
          [user_id],
          [app_id],
          [env_id],
          [event_timestamp],
          [page_name],
          [action_type],
          [action_sub_type]
        FROM {`schema`}.[app_analytics]
        {where_sql}
        ORDER BY [event_timestamp] DESC;
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

#' Fetch aggregated application usage summary
#'
#' Generates high-level usage metrics (distinct users, total actions, and most
#' visited page) from `[app_analytics]` over a trailing window of `days_back`
#' days for a target application.
#'
#' @details
#' Queries `[app_analytics]` within `utils_resolve_schema("db_schema_01sr")`,
#' filtering to `app_id` and `event_timestamp >= DATEADD(day, -days_back, SYSUTCDATETIME())`.
#' Supports passing an existing `DBIConnection` via `conn`.
#'
#' @section Side Effects:
#' \itemize{
#'   \item Opens and closes a database connection via [sql_manager()] when `conn` is `NULL`
#'   \item Writes debug start/completion entries and error logs via [log_event()]
#' }
#'
#' @param app_id Integer scalar. Target application ID scope. Defaults to
#'   [utils_get_app_id()].
#' @param days_back Integer scalar. Trailing lookback window in days. Defaults
#'   to `30L`.
#' @param conn Optional `DBIConnection` object. If `NULL` (default), a new
#'   connection is opened via [sql_manager()] and closed on exit.
#' @param conn_name Character scalar. Database configuration alias passed to
#'   [sql_manager()] when `conn` is `NULL`. Defaults to `"dit"`.
#'
#' @return A 1-row `data.frame` with 3 columns, or `NULL` on invalid input or
#'   database error:
#'   \describe{
#'     \item{`unique_users`}{Integer. Count of distinct `user_id`s in the period.}
#'     \item{`total_actions`}{Integer. Total count of recorded telemetry events.}
#'     \item{`most_visited_page`}{Character. Name of the most frequently visited page (`NA` if no events).}
#'   }
#'
#' @family analytics database functions
#' @seealso [db_analytics_get_all()], [db_analytics_insert()],
#'   [utils_get_app_id()]
#'
#' @export
#' @examples
#' \dontrun{
#' summary_30d <- db_analytics_get_summary()
#' summary_7d <- db_analytics_get_summary(app_id = 1L, days_back = 7L)
#' }
db_analytics_get_summary <- function(
  app_id = utils_get_app_id(),
  days_back = 30L,
  conn = NULL,
  conn_name = "dit"
) {
  fn_name <- as.character(match.call()[[1]])
  log_event(paste("Starting", fn_name), debug = TRUE)
  on.exit(log_event(paste("Finished", fn_name)), add = TRUE)

  aid <- suppressWarnings(as.integer(app_id))
  days <- suppressWarnings(as.integer(days_back))
  if (
    length(aid) != 1L ||
      is.na(aid) ||
      length(days) != 1L ||
      is.na(days) ||
      days < 1L
  ) {
    log_event(paste0(fn_name, " failed: invalid app_id or days_back"), type = 2)
    return(NULL)
  }

  schema <- utils_resolve_schema("db_schema_01sr")

  tryCatch(
    {
      if (is.null(conn)) {
        conn <- sql_manager(conn_name)
        on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)
      }

      query <- glue::glue_sql(
        "
        SELECT
          COUNT(DISTINCT [user_id]) AS [unique_users],
          COUNT([analytics_id])     AS [total_actions],
          (
            SELECT TOP 1 [page_name]
            FROM {`schema`}.[app_analytics]
            WHERE [app_id] = {aid}
              AND [event_timestamp] >= DATEADD(day, -{days}, SYSUTCDATETIME())
            GROUP BY [page_name]
            ORDER BY COUNT(*) DESC
          ) AS [most_visited_page]
        FROM {`schema`}.[app_analytics]
        WHERE [app_id] = {aid}
          AND [event_timestamp] >= DATEADD(day, -{days}, SYSUTCDATETIME());
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

#' Insert a standard clickstream telemetry event
#'
#' Writes a normalised event record directly to `[app_analytics]` with
#' `event_timestamp = SYSUTCDATETIME()`.
#'
#' @details
#' Inserts into `[app_analytics]` within `utils_resolve_schema("db_schema_01sr")`.
#' Supports passing an existing `DBIConnection` via `conn`.
#'
#' @section Side Effects:
#' \itemize{
#'   \item Inserts a row into `[app_analytics]`
#'   \item Opens and closes a database connection via [sql_manager()] when `conn` is `NULL`
#'   \item Writes debug start/completion entries and error logs via [log_event()]
#' }
#'
#' @param user_id Integer scalar. Canonical primary key user identifier.
#' @param page_name Character scalar. Standardised view or page name.
#' @param action_type Character scalar. Action verb (e.g., `"Load"`, `"Click"`, `"Download"`).
#' @param action_sub_type Optional character scalar. Supplemental metadata detail.
#'   Defaults to `NULL` (stored as SQL `NULL`).
#' @param app_id Integer scalar. Application ID scope. Defaults to [utils_get_app_id()].
#' @param env_id Integer scalar. Hosting environment ID (`1` = dev, `2` = test, `3` = prod).
#'   Defaults to [utils_resolve_env_id()].
#' @param conn Optional `DBIConnection` object. If `NULL` (default), a new
#'   connection is opened via [sql_manager()] and closed on exit.
#' @param conn_name Character scalar. Database configuration alias passed to
#'   [sql_manager()] when `conn` is `NULL`. Defaults to `"dit"`.
#'
#' @return Integer scalar indicating the number of rows affected (`1L` on
#'   success, `0L` on validation failure or database error).
#'
#' @family analytics database functions
#' @seealso [db_analytics_record_login()], [db_analytics_record_download()],
#'   [db_analytics_get_all()]
#'
#' @export
#' @examples
#' \dontrun{
#' db_analytics_insert(
#'   user_id = 42L,
#'   page_name = "Caseload Overview",
#'   action_type = "Click",
#'   action_sub_type = "Filter: Over 9 months"
#' )
#' }
db_analytics_insert <- function(
  user_id,
  page_name,
  action_type,
  action_sub_type = NULL,
  app_id = utils_get_app_id(),
  env_id = utils_resolve_env_id(),
  conn = NULL,
  conn_name = "dit"
) {
  fn_name <- as.character(match.call()[[1]])
  log_event(paste("Starting", fn_name), debug = TRUE)
  on.exit(log_event(paste("Finished", fn_name)), add = TRUE)

  uid <- suppressWarnings(as.integer(user_id))
  aid <- suppressWarnings(as.integer(app_id))
  eid <- suppressWarnings(as.integer(env_id))

  if (
    length(uid) != 1L ||
      is.na(uid) ||
      length(aid) != 1L ||
      is.na(aid) ||
      length(eid) != 1L ||
      is.na(eid) ||
      !is.character(page_name) ||
      length(page_name) != 1L ||
      !nzchar(trimws(page_name)) ||
      !is.character(action_type) ||
      length(action_type) != 1L ||
      !nzchar(trimws(action_type))
  ) {
    log_event(paste0(fn_name, " failed: invalid required parameters"), type = 2)
    return(0L)
  }

  sub_type_sql <- if (
    !is.null(action_sub_type) &&
      length(action_sub_type) == 1L &&
      !is.na(action_sub_type)
  ) {
    as.character(action_sub_type)
  } else {
    NA_character_
  }

  schema <- utils_resolve_schema("db_schema_01sr")

  tryCatch(
    {
      if (is.null(conn)) {
        conn <- sql_manager(conn_name)
        on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)
      }

      query <- glue::glue_sql(
        "
        INSERT INTO {`schema`}.[app_analytics] (
          [user_id],
          [app_id],
          [env_id],
          [event_timestamp],
          [page_name],
          [action_type],
          [action_sub_type]
        )
        VALUES (
          {uid},
          {aid},
          {eid},
          SYSUTCDATETIME(),
          {page_name},
          {action_type},
          {sub_type_sql}
        );
        ",
        .con = conn
      )

      as.integer(utils_db_execute(conn, query))
    },
    error = function(e) {
      log_event(paste0(fn_name, " failed: ", e$message), type = 2)
      0L
    }
  )
}

#' Record a file download telemetry event
#'
#' Inserts a `"Download"` event into `[app_analytics]`, resolving the raw
#' username or email token into a canonical integer `user_id` via
#' [db_user_create()].
#'
#' @details
#' Resolves `app_id` via [utils_get_app_id()], `env_id` via
#' [utils_resolve_env_id()], and `user_id` via [db_user_create()], then inserts
#' into `[app_analytics]` within `utils_resolve_schema("db_schema_01sr")`
#' with `event_timestamp = SYSUTCDATETIME()`.
#'
#' @section Side Effects:
#' \itemize{
#'   \item Provisions/resolves user record via [db_user_create()]
#'   \item Inserts a row into `[app_analytics]`
#'   \item Opens and closes a database connection via [sql_manager()]
#'   \item Writes debug start/completion entries and error logs via [log_event()]
#' }
#'
#' @param page_name Character scalar. Name of the page where the download occurred.
#' @param file_name Character scalar. Name of the downloaded file.
#' @param user Character scalar. Username or email token executing the action.
#'   Defaults to `"Guest"`.
#' @param conn_name Character scalar. Database configuration alias passed to
#'   [sql_manager()]. Defaults to `"dit"`.
#'
#' @return Logical scalar (invisibly). `TRUE` if the download event was recorded,
#'   `FALSE` on validation failure or database error.
#'
#' @family analytics database functions
#' @seealso [db_analytics_record_login()], [db_analytics_insert()],
#'   [db_user_create()], [utils_get_app_id()], [utils_resolve_env_id()]
#'
#' @export
#' @examples
#' \dontrun{
#' db_analytics_record_download(
#'   page_name = "Caseload Overview",
#'   file_name = "dao_extract_2026-01.xlsx",
#'   user = "bsmith7"
#' )
#' }
db_analytics_record_download <- function(
  page_name,
  file_name,
  user = "Guest",
  conn_name = "dit"
) {
  fn_name <- as.character(match.call()[[1]])
  log_event(paste("Starting", fn_name), debug = TRUE)
  on.exit(log_event(paste("Finished", fn_name)), add = TRUE)

  if (
    !is.character(page_name) ||
      length(page_name) != 1L ||
      !nzchar(trimws(page_name)) ||
      !is.character(file_name) ||
      length(file_name) != 1L ||
      !nzchar(trimws(file_name))
  ) {
    log_event(
      paste0(fn_name, " failed: invalid page_name or file_name"),
      type = 2
    )
    return(invisible(FALSE))
  }

  schema <- utils_resolve_schema("db_schema_01sr")

  tryCatch(
    {
      app_id <- as.integer(utils_get_app_id())
      env_id <- as.integer(utils_resolve_env_id())
      target_user_id <- as.integer(db_user_create(user))

      log_event(
        glue::glue(
          "Recording download: user_id={target_user_id}, app_id={app_id}, ",
          "env_id={env_id}, page={page_name}, file={file_name}"
        ),
        debug = TRUE
      )

      conn <- sql_manager(conn_name)
      on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

      query <- glue::glue_sql(
        "
        INSERT INTO {`schema`}.[app_analytics] (
          [user_id],
          [app_id],
          [env_id],
          [event_timestamp],
          [page_name],
          [action_type],
          [action_sub_type]
        )
        VALUES (
          {target_user_id},
          {app_id},
          {env_id},
          SYSUTCDATETIME(),
          {page_name},
          'Download',
          {file_name}
        );
        ",
        .con = conn
      )

      rows_affected <- utils_db_execute(conn, query)
      invisible(rows_affected > 0L)
    },
    error = function(e) {
      log_event(paste0(fn_name, " failed: ", e$message), type = 2)
      invisible(FALSE)
    }
  )
}

#' Record a user login / initial load event
#'
#' Inserts a `"Load"` / `"Initial Load"` telemetry record into `[app_analytics]`
#' and returns the user's canonical primary key `user_id` resolved via
#' [db_user_create()].
#'
#' @details
#' Resolves `app_id` via [utils_get_app_id()], `env_id` via
#' [utils_resolve_env_id()], and `user_id` via [db_user_create()], then inserts
#' into `[app_analytics]` within `utils_resolve_schema("db_schema_01sr")`.
#'
#' @section Side Effects:
#' \itemize{
#'   \item Provisions/resolves user record via [db_user_create()]
#'   \item Inserts a row into `[app_analytics]`
#'   \item Opens and closes a database connection via [sql_manager()]
#'   \item Writes debug start/completion entries and error logs via [log_event()]
#' }
#'
#' @param user Character scalar. Username or email token of the user accessing
#'   the application. Defaults to `"Guest"`.
#' @param conn_name Character scalar. Database configuration alias passed to
#'   [sql_manager()]. Defaults to `"dit"`.
#'
#' @return Integer scalar containing the canonical primary key `user_id` for the
#'   logged-in user, or `NULL` if an error occurs.
#'
#' @family analytics database functions
#' @seealso [db_analytics_record_download()], [db_analytics_insert()],
#'   [db_user_create()]
#'
#' @export
#' @examples
#' \dontrun{
#' uid <- db_analytics_record_login(user = "bsmith7")
#' }
db_analytics_record_login <- function(user = "Guest", conn_name = "dit") {
  fn_name <- as.character(match.call()[[1]])
  log_event(paste("Starting", fn_name), debug = TRUE)
  on.exit(log_event(paste("Finished", fn_name)), add = TRUE)

  schema <- utils_resolve_schema("db_schema_01sr")

  tryCatch(
    {
      app_id <- as.integer(utils_get_app_id())
      env_id <- as.integer(utils_resolve_env_id())
      target_user_id <- as.integer(db_user_create(user))

      log_event(
        glue::glue(
          "Recording login: user_id={target_user_id}, app_id={app_id}, env_id={env_id}"
        ),
        debug = TRUE
      )

      conn <- sql_manager(conn_name)
      on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

      query <- glue::glue_sql(
        "
        INSERT INTO {`schema`}.[app_analytics] (
          [user_id],
          [app_id],
          [env_id],
          [event_timestamp],
          [page_name],
          [action_type],
          [action_sub_type]
        )
        VALUES (
          {target_user_id},
          {app_id},
          {env_id},
          SYSUTCDATETIME(),
          'Home page',
          'Load',
          'Initial Load'
        );
        ",
        .con = conn
      )

      utils_db_execute(conn, query)
      target_user_id
    },
    error = function(e) {
      log_event(paste0(fn_name, " failed: ", e$message), type = 2)
      NULL
    }
  )
}

#' Insert a centralised system audit log entry
#'
#' Records a data-mutation audit event into `[audit_logs]`, capturing the
#' originating application, environment, user, target table, optional record
#' identifier, and narrative summary.
#'
#' @details
#' Defaults `app_id` via [utils_get_app_id()] and `env_id` via
#' [utils_resolve_env_id()], then inserts into `[audit_logs]` within
#' `utils_resolve_schema("db_schema_01sr")` with `created_date = SYSUTCDATETIME()`.
#' Supports passing an existing `DBIConnection` via `conn`.
#'
#' @section Side Effects:
#' \itemize{
#'   \item Inserts a row into `[audit_logs]`
#'   \item Opens and closes a database connection via [sql_manager()] when `conn` is `NULL`
#'   \item Writes debug start/completion entries and error logs via [log_event()]
#' }
#'
#' @param user_id Integer scalar. Canonical user ID executing the action.
#' @param action_type Character scalar. Action classification (e.g.,
#'   `"INSERT"`, `"UPDATE"`, `"DELETE"`).
#' @param target_table Character scalar. Physical database table altered.
#' @param action_summary Character scalar. Narrative log detailing the change.
#' @param record_id Optional character or integer scalar. Primary key tracking
#'   the altered row. Defaults to `NULL` (stored as SQL `NULL`).
#' @param app_id Integer scalar. Application ID scope. Defaults to [utils_get_app_id()].
#' @param env_id Integer scalar. Hosting environment ID (`1` = dev, `2` = test, `3` = prod).
#'   Defaults to [utils_resolve_env_id()].
#' @param conn Optional `DBIConnection` object. If `NULL` (default), a new
#'   connection is opened via [sql_manager()] and closed on exit.
#' @param conn_name Character scalar. Database configuration alias passed to
#'   [sql_manager()] when `conn` is `NULL`. Defaults to `"dit"`.
#'
#' @return Integer scalar indicating the number of rows affected (`1L` on
#'   success, `0L` on validation failure or database error).
#'
#' @family analytics database functions
#' @seealso [db_analytics_get_all()], [utils_get_app_id()],
#'   [utils_resolve_env_id()]
#'
#' @export
#' @examples
#' \dontrun{
#' db_analytics_insert_audit_log(
#'   user_id = 42L,
#'   action_type = "UPDATE",
#'   target_table = "tracker",
#'   record_id = 105L,
#'   action_summary = "Updated decision status to Approved"
#' )
#' }
db_analytics_insert_audit_log <- function(
  user_id,
  action_type,
  target_table,
  action_summary,
  record_id = NULL,
  app_id = utils_get_app_id(),
  env_id = utils_resolve_env_id(),
  conn = NULL,
  conn_name = "dit"
) {
  fn_name <- as.character(match.call()[[1]])
  log_event(paste("Starting", fn_name), debug = TRUE)
  on.exit(log_event(paste("Finished", fn_name)), add = TRUE)

  uid <- suppressWarnings(as.integer(user_id))
  aid <- suppressWarnings(as.integer(app_id))
  eid <- suppressWarnings(as.integer(env_id))

  if (
    length(uid) != 1L ||
      is.na(uid) ||
      length(aid) != 1L ||
      is.na(aid) ||
      length(eid) != 1L ||
      is.na(eid) ||
      !is.character(action_type) ||
      length(action_type) != 1L ||
      !nzchar(trimws(action_type)) ||
      !is.character(target_table) ||
      length(target_table) != 1L ||
      !nzchar(trimws(target_table)) ||
      !is.character(action_summary) ||
      length(action_summary) != 1L ||
      !nzchar(trimws(action_summary))
  ) {
    log_event(paste0(fn_name, " failed: invalid required parameters"), type = 2)
    return(0L)
  }

  schema <- utils_resolve_schema("db_schema_01sr")

  tryCatch(
    {
      if (is.null(conn)) {
        conn <- sql_manager(conn_name)
        on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)
      }

      safe_record_sql <- if (
        !is.null(record_id) && length(record_id) == 1L && !is.na(record_id)
      ) {
        as.character(record_id)
      } else {
        DBI::SQL("NULL")
      }

      query <- glue::glue_sql(
        "
        INSERT INTO {`schema`}.[audit_logs] (
          [app_id],
          [user_id],
          [env_id],
          [action_type],
          [target_table],
          [record_id],
          [action_summary],
          [created_date]
        )
        VALUES (
          {aid},
          {uid},
          {eid},
          {action_type},
          {target_table},
          {safe_record_sql},
          {action_summary},
          SYSUTCDATETIME()
        );
        ",
        .con = conn
      )

      as.integer(utils_db_execute(conn, query))
    },
    error = function(e) {
      log_event(paste0(fn_name, " failed: ", e$message), type = 2)
      0L
    }
  )
}
