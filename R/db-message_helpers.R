#' Retrieve Active Portal Messages
#'
#' Returns all active portal messages relevant to the current application,
#' including global ("catch-all") messages, resolving user identifiers.
#'
#' @details
#' The current application ID is resolved using [utils_get_app_id()].
#' The database schema is resolved via [utils_resolve_schema()], and the
#' query is executed using [utils_db_get_query()].
#'
#' @return A [`data.frame`] containing active portal messages.
#' @export
db_get_portal_messages <- function() {
  log_event("Starting db_get_portal_messages")

  app_id <- utils_get_app_id()

  conn <- sql_manager("dit")
  on.exit(
    {
      try(DBI::dbDisconnect(conn), silent = TRUE)
      log_event("Finished db_get_portal_messages")
    },
    add = TRUE
  )

  query <- glue::glue_sql(
    "
    SELECT
      m.[message_date],
      m.[message_text],
      m.[app_id],
      m.[priority],
      COALESCE(u.[username], 'System') AS [ad_username]
    FROM {utils_resolve_schema('db_schema_01sr')}.[portal_messages] m
    LEFT JOIN {utils_resolve_schema('db_schema_01sr')}.[users] u 
      ON m.[user_id] = u.[user_id]
    WHERE
      m.[is_active] = 1
      AND m.[app_id] IN (1, {app_id})
    ORDER BY
      m.[priority] ASC,
      m.[message_date] DESC;
    ",
    .con = conn
  )

  tryCatch(
    utils_db_get_query(conn, query),
    error = function(e) {
      warning("db_get_portal_messages failed: ", e$message)
      data.frame()
    }
  )
}

#' Add a Portal Message
#'
#' Inserts a new message into the `portal_messages` table, linking it to a
#' normalized user identifier.
#'
#' @param message_text Character scalar. The message body to display. HTML is permitted.
#' @param priority Integer scalar. Message priority, where lower values appear first. Defaults to `1`.
#' @param user_id Integer scalar. The unique identity registry key of the creator.
#' @param force_catch_all Logical scalar. If `TRUE`, the message is assigned to the global app (`app_id = 1L`).
#' @return Logical scalar. Returns `TRUE` invisibly on successful insertion, or `FALSE` on failure.
#' @export
db_add_portal_message <- function(
  message_text,
  priority = 1,
  user_id,
  force_catch_all = FALSE
) {
  log_event("Starting db_add_portal_message")

  app_id <- if (isTRUE(force_catch_all)) {
    1L
  } else {
    utils_get_app_id()
  }

  conn <- sql_manager("dit")
  on.exit(
    {
      try(DBI::dbDisconnect(conn), silent = TRUE)
      log_event("Finished db_add_portal_message")
    },
    add = TRUE
  )

  query <- glue::glue_sql(
    "
    INSERT INTO {utils_resolve_schema('db_schema_01sr')}.[portal_messages] (
      [message_text],
      [app_id],
      [priority],
      [user_id],
      [message_date],
      [is_active]
    )
    VALUES (
      {message_text},
      {as.integer(app_id)},
      {as.integer(priority)},
      {as.integer(user_id)},
      SYSUTCDATETIME(),
      1
    );
    ",
    .con = conn
  )

  rows_affected <- tryCatch(
    utils_db_execute(conn, query),
    error = function(e) {
      warning("db_add_portal_message failed: ", e$message)
      0L
    }
  )

  invisible(rows_affected > 0L)
}

#' Edit an Existing Portal Message
#'
#' Updates the text, priority, visibility scope, or active status of a specific
#' portal message record.
#'
#' @param message_id Integer scalar. The primary key identifier of the message being modified.
#' @param message_text Character scalar. The updated message content. If NULL, text remains unchanged.
#' @param priority Integer scalar. The updated priority level. If NULL, priority remains unchanged.
#' @param force_catch_all Logical scalar. If TRUE, scopes the message globally (app_id = 1). If FALSE, scopes it to the active app ID.
#' @param is_active Logical scalar. Flips the visibility status of the message. If NULL, status remains unchanged.
#' @param user_id Integer scalar. The unique identity registry key of the administrator modifying the record.
#' @return Logical scalar. Returns TRUE invisibly if the database mutation affected rows, FALSE otherwise.
#' @export
db_edit_portal_message <- function(
  message_id,
  message_text = NULL,
  priority = NULL,
  force_catch_all = NULL,
  is_active = NULL,
  user_id
) {
  log_event("Starting db_edit_portal_message")

  shiny::req(conn, message_id, user_id)

  app_id <- if (!is.null(force_catch_all)) {
    if (isTRUE(force_catch_all)) 1L else utils_get_app_id()
  } else {
    NULL
  }

  conn <- sql_manager("dit")
  on.exit(
    {
      try(DBI::dbDisconnect(conn), silent = TRUE)
      log_event("Finished db_edit_portal_message")
    },
    add = TRUE
  )

  query <- glue::glue_sql(
    "
    UPDATE {utils_resolve_schema('db_schema_01sr')}.[portal_messages]
    SET 
      [message_text] = COALESCE({message_text}, [message_text]),
      [priority]     = COALESCE({as.integer(priority)}, [priority]),
      [app_id]       = COALESCE({as.integer(app_id)}, [app_id]),
      [is_active]    = COALESCE({as.logical(is_active)}, [is_active]),
      [user_id]      = {as.integer(user_id)},
      [message_date] = SYSUTCDATETIME()
    WHERE [message_id] = {as.integer(message_id)};
    ",
    .con = conn
  )

  rows_affected <- tryCatch(
    utils_db_execute(conn, query),
    error = function(e) {
      warning("db_edit_portal_message failed: ", e$message)
      0L
    }
  )

  invisible(rows_affected > 0L)
}

#' Deactivate a Portal Message (Soft-Delete)
#'
#' Flips the active flag of a portal message to hide it from all application views
#' while preserving its audit history.
#'
#' @param message_id Integer scalar. The primary key of the target message.
#' @param user_id Integer scalar. The identifier of the administrator pulling the message.
#' @return Logical scalar. TRUE invisibly if successful, FALSE otherwise.
#' @export
db_deactivate_portal_message <- function(message_id, user_id) {
  log_event("Starting db_deactivate_portal_message")

  shiny::req(message_id, user_id)

  conn <- sql_manager("dit")
  on.exit(
    {
      try(DBI::dbDisconnect(conn), silent = TRUE)
      log_event("Finished db_deactivate_portal_message")
    },
    add = TRUE
  )

  query <- glue::glue_sql(
    "
    UPDATE {utils_resolve_schema('db_schema_01sr')}.[portal_messages]
    SET 
      [is_active]    = 0,
      [user_id]      = {as.integer(user_id)},
      [message_date] = SYSUTCDATETIME()
    WHERE [message_id] = {as.integer(message_id)};
    ",
    .con = conn
  )

  rows_affected <- tryCatch(
    utils_db_execute(conn, query),
    error = function(e) {
      warning("db_deactivate_portal_message failed: ", e$message)
      0L
    }
  )

  invisible(rows_affected > 0L)
}
