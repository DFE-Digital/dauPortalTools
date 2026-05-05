#' Retrieve active portal messages
#'
#' Returns active portal messages for the current application, including
#' global (catch‑all) messages.
#'
#' @return A data frame of portal messages.
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

  query <- glue_sql(
    "
    SELECT
      message_date,
      message_text,
      app_id,
      priority,
      ad_username
    FROM {utils_resolve_schema('db_schema_01sr')}.[portal_messages]
    WHERE
      is_active = 1
      AND app_id IN (1, {app_id})
    ORDER BY
      priority ASC,
      message_date DESC
    ",
    .con = conn
  )

  dbGetQuery(conn, query)
}
