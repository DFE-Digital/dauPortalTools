#' Add a portal message
#'
#' Inserts a new message into the portal_messages table, either scoped to the
#' current application or globally (catch‑all) if requested.
#'
#' @param message_text Character. Message body (HTML allowed).
#' @param priority Integer. Lower numbers display first.
#' @param ad_username Character. Username of the creator.
#' @param force_catch_all Logical. If TRUE, message applies to all apps.
#'
#' @return TRUE on success.
#' @export
db_add_portal_message <- function(
  message_text,
  priority = 1,
  ad_username,
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

  query <- glue_sql(
    "
    INSERT INTO {utils_resolve_schema('01s')}.[portal_messages] (
      message_text,
      app_id,
      priority,
      ad_username
    )
    VALUES (
      {message_text},
      {app_id},
      {priority},
      {ad_username}
    )
    ",
    .con = conn
  )

  dbExecute(conn, query)

  TRUE
}
