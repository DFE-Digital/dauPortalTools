# ==============================================================================
# File: R/db-messages.R
# Purpose: Database query and mutation functions for Portal Messages & Banners
#
# Functions:
#   - db_messages_get_all()        : Fetch all portal messages across apps
#   - db_messages_get_active()     : Fetch active messages for current & global app
#   - db_messages_insert()         : Insert a new portal message
#   - db_messages_update()         : Update text, priority, scope, or active status
#   - db_messages_deactivate()     : Soft-delete an active portal message
#
# Dependencies: DBI, glue, internal
# ==============================================================================

#' Fetch all portal messages
#'
#' Retrieves all portal messages (both active and inactive) across all
#' applications from `[portal_messages]`, left-joined against `[users]` to
#' resolve author usernames.
#'
#' @details
#' Queries `[portal_messages]` joined to `[users]` within the schema resolved by
#' `utils_resolve_schema("db_schema_01sr")`, ordered by `is_active` descending,
#' `priority` ascending, and `message_date` descending.
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
#' @return A `data.frame` with 7 columns, or `NULL` if a database connection or
#'   query error occurs:
#'   \describe{
#'     \item{`message_id`}{Integer. Primary key message identifier.}
#'     \item{`message_date`}{POSIXct. UTC timestamp when the message was created or last modified.}
#'     \item{`message_text`}{Character. Message body (HTML permitted).}
#'     \item{`app_id`}{Integer. Target application ID (`1L` indicates global catch-all).}
#'     \item{`priority`}{Integer. Display priority (lower values appear first).}
#'     \item{`is_active`}{Logical/Integer. Whether the message is currently active.}
#'     \item{`ad_username`}{Character. Username of creator/last editor, defaulting to `"System"`.}
#'   }
#'
#' @family messages database functions
#' @seealso [db_messages_get_active()], [db_messages_insert()],
#'   [db_messages_update()], [db_messages_deactivate()], [sql_manager()]
#'
#' @export
#' @examples
#' \dontrun{
#' all_msgs <- db_messages_get_all()
#' }
db_messages_get_all <- function(conn_name = "dit") {
  fn_name <- as.character(match.call()[[1]])
  log_event(paste("Starting", fn_name), debug = TRUE)
  on.exit(log_event(paste("Finished", fn_name)), add = TRUE)

  schema <- utils_resolve_schema("db_schema_01sr")

  tryCatch(
    {
      conn <- sql_manager(conn_name)
      on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

      query <- glue::glue_sql(
        "
        SELECT
          m.[message_id],
          m.[message_date],
          m.[message_text],
          m.[app_id],
          m.[priority],
          m.[is_active],
          COALESCE(u.[username], 'System') AS [ad_username]
        FROM {`schema`}.[portal_messages] m
        LEFT JOIN {`schema`}.[users] u
          ON m.[user_id] = u.[user_id]
        ORDER BY
          m.[is_active] DESC,
          m.[priority] ASC,
          m.[message_date] DESC;
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

#' Fetch active portal messages for an application
#'
#' Returns all active portal messages (`is_active = 1`) scoped either globally
#' (`app_id = 1L`) or to the target `app_id`, with user identifiers resolved to
#' usernames.
#'
#' @details
#' Queries `[portal_messages]` joined to `[users]` within
#' `utils_resolve_schema("db_schema_01sr")`, filtering to `is_active = 1` and
#' `app_id IN (1, app_id)`. Defaults `app_id` to [utils_get_app_id()].
#'
#' @section Side Effects:
#' \itemize{
#'   \item Opens and closes a database connection via [sql_manager()]
#'   \item Writes debug start/completion entries and error logs via [log_event()]
#' }
#'
#' @param app_id Integer scalar. Target application ID scope. Defaults to
#'   [utils_get_app_id()].
#' @param conn_name Character scalar. Database configuration alias passed to
#'   [sql_manager()]. Defaults to `"dit"`.
#'
#' @return A `data.frame` ordered by `priority` ascending then `message_date`
#'   descending with 6 columns, or `NULL` on invalid input or database error:
#'   \describe{
#'     \item{`message_id`}{Integer. Primary key message identifier.}
#'     \item{`message_date`}{POSIXct. UTC timestamp when the message was created or last modified.}
#'     \item{`message_text`}{Character. Message body (HTML permitted).}
#'     \item{`app_id`}{Integer. Target application ID (`1L` indicates global catch-all).}
#'     \item{`priority`}{Integer. Display priority (lower values appear first).}
#'     \item{`ad_username`}{Character. Username of creator/last editor, defaulting to `"System"`.}
#'   }
#'
#' @family messages database functions
#' @seealso [db_messages_get_all()], [db_messages_insert()],
#'   [db_messages_update()], [db_messages_deactivate()], [utils_get_app_id()]
#'
#' @export
#' @examples
#' \dontrun{
#' msgs <- db_messages_get_active()
#' }
db_messages_get_active <- function(
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

  schema <- utils_resolve_schema("db_schema_01sr")

  tryCatch(
    {
      conn <- sql_manager(conn_name)
      on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

      query <- glue::glue_sql(
        "
        SELECT
          m.[message_id],
          m.[message_date],
          m.[message_text],
          m.[app_id],
          m.[priority],
          COALESCE(u.[username], 'System') AS [ad_username]
        FROM {`schema`}.[portal_messages] m
        LEFT JOIN {`schema`}.[users] u
          ON m.[user_id] = u.[user_id]
        WHERE m.[is_active] = 1
          AND m.[app_id] IN (1, {aid})
        ORDER BY
          m.[priority] ASC,
          m.[message_date] DESC;
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

#' Insert a new portal message
#'
#' Inserts an active message into `[portal_messages]`, scoped either to the
#' current application via [utils_get_app_id()] or globally (`app_id = 1L`)
#' when `force_catch_all = TRUE`.
#'
#' @details
#' Inserts into `[portal_messages]` within
#' `utils_resolve_schema("db_schema_01sr")` with `is_active = 1` and
#' `message_date = SYSUTCDATETIME()`.
#'
#' @section Side Effects:
#' \itemize{
#'   \item Inserts a row into `[portal_messages]`
#'   \item Opens and closes a database connection via [sql_manager()]
#'   \item Writes debug start/completion entries and error logs via [log_event()]
#' }
#'
#' @param message_text Character scalar. The message body to display (HTML permitted).
#' @param user_id Integer scalar. Unique identity registry key of the creator.
#' @param priority Integer scalar. Message priority where lower values appear
#'   first. Defaults to `1L`.
#' @param force_catch_all Logical scalar. If `TRUE`, assigns the message to the
#'   global catch-all application (`app_id = 1L`). Defaults to `FALSE`.
#' @param conn_name Character scalar. Database configuration alias passed to
#'   [sql_manager()]. Defaults to `"dit"`.
#'
#' @return Logical scalar (invisibly). `TRUE` if the message was inserted,
#'   `FALSE` on validation failure or database error.
#'
#' @family messages database functions
#' @seealso [db_messages_get_active()], [db_messages_update()],
#'   [db_messages_deactivate()], [utils_get_app_id()]
#'
#' @export
#' @examples
#' \dontrun{
#' db_messages_insert(
#'   message_text = "<strong>Notice:</strong> Scheduled maintenance at 18:00.",
#'   user_id = 42L,
#'   priority = 1L,
#'   force_catch_all = TRUE
#' )
#' }
db_messages_insert <- function(
  message_text,
  user_id,
  priority = 1L,
  force_catch_all = FALSE,
  conn_name = "dit"
) {
  fn_name <- as.character(match.call()[[1]])
  log_event(paste("Starting", fn_name), debug = TRUE)
  on.exit(log_event(paste("Finished", fn_name)), add = TRUE)

  uid <- suppressWarnings(as.integer(user_id))
  prio <- suppressWarnings(as.integer(priority))
  aid <- if (isTRUE(force_catch_all)) {
    1L
  } else {
    suppressWarnings(as.integer(utils_get_app_id()))
  }

  if (
    !is.character(message_text) ||
      length(message_text) != 1L ||
      !nzchar(trimws(message_text)) ||
      length(uid) != 1L ||
      is.na(uid) ||
      length(prio) != 1L ||
      is.na(prio) ||
      length(aid) != 1L ||
      is.na(aid)
  ) {
    log_event(paste0(fn_name, " failed: invalid parameters"), type = 2)
    return(invisible(FALSE))
  }

  schema <- utils_resolve_schema("db_schema_01sr")

  tryCatch(
    {
      conn <- sql_manager(conn_name)
      on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

      query <- glue::glue_sql(
        "
        INSERT INTO {`schema`}.[portal_messages] (
          [message_text],
          [app_id],
          [priority],
          [user_id],
          [message_date],
          [is_active]
        )
        VALUES (
          {message_text},
          {aid},
          {prio},
          {uid},
          SYSUTCDATETIME(),
          1
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

#' Update an existing portal message
#'
#' Selectively updates the text, priority, application scope, or active status
#' of an existing record in `[portal_messages]`, stamping `user_id` and
#' `message_date = SYSUTCDATETIME()`. Any optional field left as `NULL` retains
#' its current database value via SQL `COALESCE()`.
#'
#' @details
#' Updates `[portal_messages]` within `utils_resolve_schema("db_schema_01sr")`.
#' `NULL` arguments are translated to SQL `NULL` so `COALESCE()` preserves
#' existing column values safely.
#'
#' @section Side Effects:
#' \itemize{
#'   \item Updates a row in `[portal_messages]`
#'   \item Opens and closes a database connection via [sql_manager()]
#'   \item Writes debug start/completion entries and error logs via [log_event()]
#' }
#'
#' @param message_id Integer scalar. Primary key identifier of the message to modify.
#' @param user_id Integer scalar. Identity registry key of the administrator modifying the record.
#' @param message_text Optional character scalar. Updated message body. If `NULL` (default), unchanged.
#' @param priority Optional integer scalar. Updated priority level. If `NULL` (default), unchanged.
#' @param force_catch_all Optional logical scalar. If `TRUE`, scopes globally (`app_id = 1L`);
#'   if `FALSE`, scopes to [utils_get_app_id()]; if `NULL` (default), scope is unchanged.
#' @param is_active Optional logical scalar. Updated visibility status. If `NULL` (default), unchanged.
#' @param conn_name Character scalar. Database configuration alias passed to
#'   [sql_manager()]. Defaults to `"dit"`.
#'
#' @return Logical scalar (invisibly). `TRUE` if a matching message row was
#'   updated, `FALSE` on validation failure, missing ID, or database error.
#'
#' @family messages database functions
#' @seealso [db_messages_get_active()], [db_messages_insert()],
#'   [db_messages_deactivate()]
#'
#' @export
#' @examples
#' \dontrun{
#' db_messages_update(
#'   message_id = 12L,
#'   user_id = 42L,
#'   priority = 2L,
#'   force_catch_all = FALSE
#' )
#' }
db_messages_update <- function(
  message_id,
  user_id,
  message_text = NULL,
  priority = NULL,
  force_catch_all = NULL,
  is_active = NULL,
  conn_name = "dit"
) {
  fn_name <- as.character(match.call()[[1]])
  log_event(paste("Starting", fn_name), debug = TRUE)
  on.exit(log_event(paste("Finished", fn_name)), add = TRUE)

  mid <- suppressWarnings(as.integer(message_id))
  uid <- suppressWarnings(as.integer(user_id))
  if (length(mid) != 1L || is.na(mid) || length(uid) != 1L || is.na(uid)) {
    log_event(
      paste0(fn_name, " failed: invalid message_id or user_id"),
      type = 2
    )
    return(invisible(FALSE))
  }

  sql_text <- if (!is.null(message_text)) {
    as.character(message_text)[1L]
  } else {
    DBI::SQL("NULL")
  }
  sql_prio <- if (!is.null(priority)) {
    as.integer(priority)[1L]
  } else {
    DBI::SQL("NULL")
  }
  sql_active <- if (!is.null(is_active)) {
    as.integer(isTRUE(is_active))
  } else {
    DBI::SQL("NULL")
  }
  sql_app <- if (!is.null(force_catch_all)) {
    if (isTRUE(force_catch_all)) 1L else as.integer(utils_get_app_id())
  } else {
    DBI::SQL("NULL")
  }

  schema <- utils_resolve_schema("db_schema_01sr")

  tryCatch(
    {
      conn <- sql_manager(conn_name)
      on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

      query <- glue::glue_sql(
        "
        UPDATE {`schema`}.[portal_messages]
        SET
          [message_text] = COALESCE({sql_text}, [message_text]),
          [priority]     = COALESCE({sql_prio}, [priority]),
          [app_id]       = COALESCE({sql_app}, [app_id]),
          [is_active]    = COALESCE({sql_active}, [is_active]),
          [user_id]      = {uid},
          [message_date] = SYSUTCDATETIME()
        WHERE [message_id] = {mid};
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

#' Deactivate a portal message (Soft-Delete)
#'
#' Sets `is_active = 0` on a portal message record to hide it from application
#' banners while recording the administrator `user_id` and UTC timestamp.
#'
#' @details
#' Updates `[portal_messages]` within `utils_resolve_schema("db_schema_01sr")`.
#'
#' @section Side Effects:
#' \itemize{
#'   \item Updates `[is_active] = 0` in `[portal_messages]`
#'   \item Opens and closes a database connection via [sql_manager()]
#'   \item Writes debug start/completion entries and error logs via [log_event()]
#' }
#'
#' @param message_id Integer scalar. Primary key of the target message.
#' @param user_id Integer scalar. Identity registry key of the administrator
#'   deactivating the message.
#' @param conn_name Character scalar. Database configuration alias passed to
#'   [sql_manager()]. Defaults to `"dit"`.
#'
#' @return Logical scalar (invisibly). `TRUE` if the message was deactivated,
#'   `FALSE` on validation failure, missing ID, or database error.
#'
#' @family messages database functions
#' @seealso [db_messages_get_active()], [db_messages_update()]
#'
#' @export
#' @examples
#' \dontrun{
#' db_messages_deactivate(message_id = 12L, user_id = 42L)
#' }
db_messages_deactivate <- function(
  message_id,
  user_id,
  conn_name = "dit"
) {
  fn_name <- as.character(match.call()[[1]])
  log_event(paste("Starting", fn_name), debug = TRUE)
  on.exit(log_event(paste("Finished", fn_name)), add = TRUE)

  mid <- suppressWarnings(as.integer(message_id))
  uid <- suppressWarnings(as.integer(user_id))
  if (length(mid) != 1L || is.na(mid) || length(uid) != 1L || is.na(uid)) {
    log_event(
      paste0(fn_name, " failed: invalid message_id or user_id"),
      type = 2
    )
    return(invisible(FALSE))
  }

  schema <- utils_resolve_schema("db_schema_01sr")

  tryCatch(
    {
      conn <- sql_manager(conn_name)
      on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

      query <- glue::glue_sql(
        "
        UPDATE {`schema`}.[portal_messages]
        SET
          [is_active]    = 0,
          [user_id]      = {uid},
          [message_date] = SYSUTCDATETIME()
        WHERE [message_id] = {mid};
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
