#' Retrieve Active Portal Messages
#'
#' Returns all active portal messages relevant to the current application,
#' including global ("catch-all") messages.
#'
#' Messages are filtered to those marked as active (`is_active = 1`) and
#' belonging either to the current application or to the global scope
#' (`app_id = 1`).
#'
#' @details
#' The current application ID is resolved using [utils_get_app_id()].
#' The database schema is resolved via [utils_resolve_schema()], and the
#' query is executed using [dbGetQuery()].
#'
#' Results are ordered by:
#' \itemize{
#'   \item `priority` (ascending; lower values indicate higher importance)
#'   \item `message_date` (descending; most recent messages first)
#' }
#'
#' Database connections are managed internally and safely closed using
#' `on.exit()`. Logging is performed via [log_event()] at the start and
#' end of execution.
#'
#' @section Side Effects:
#' \itemize{
#'   \item Opens and closes a database connection.
#'   \item Writes log entries via [log_event()].
#' }
#'
#' @return A [`data.frame`] containing active portal messages with the
#' following columns:
#' \describe{
#'   \item{message_date}{Datetime the message was created.}
#'   \item{message_text}{Character message content (may include HTML).}
#'   \item{app_id}{Integer application identifier.}
#'   \item{priority}{Integer priority (lower = higher importance).}
#'   \item{ad_username}{Character username of the message creator.}
#' }
#'
#' @examples
#' \dontrun{
#' msgs <- db_get_portal_messages()
#'
#' # Preview messages
#' head(msgs)
#'
#' # Filter high priority messages
#' subset(msgs, priority == 1)
#' }
#'
#' @seealso [db_add_portal_message()], [utils_get_app_id()]
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
      AND app_id IN (0, {app_id})
    ORDER BY
      priority ASC,
      message_date DESC
    ",
    .con = conn
  )

  dbGetQuery(conn, query)
}

#' Add a Portal Message
#'
#' Inserts a new message into the `portal_messages` table. The message can be
#' scoped to the current application or applied globally ("catch-all") depending
#' on the `force_catch_all` flag.
#'
#' Messages are stored with a priority value, where lower numbers indicate
#' higher importance and are displayed first in the UI.
#'
#' @param message_text Character scalar. The message body to display. HTML is
#'   permitted and will be rendered in the UI.
#' @param priority Integer scalar. Message priority, where lower values appear
#'   first. Defaults to `1`.
#' @param ad_username Character scalar. Username of the user creating the
#'   message (typically Active Directory username).
#' @param force_catch_all Logical scalar. If `TRUE`, the message is assigned to
#'   the global application (`app_id = 1L`), making it visible across all
#'   applications. If `FALSE`, the message is assigned to the current
#'   application via [utils_get_app_id()].
#'
#' @details
#' The function resolves the database schema using
#' [utils_resolve_schema()] and writes directly to the
#' `portal_messages` table.
#'
#' Database connections are managed internally and safely closed on exit
#' using `on.exit()`. Logging is performed via [log_event()] at the start
#' and end of execution.
#'
#' @section Side Effects:
#' \itemize{
#'   \item Writes a new record to the database.
#'   \item Opens and closes a database connection.
#'   \item Writes log entries via [log_event()].
#' }
#'
#' @return Logical scalar. Returns `TRUE` invisibly on successful insertion.
#'
#' @examples
#' \dontrun{
#' db_add_portal_message(
#'   message_text = "<b>System maintenance tonight</b>",
#'   priority = 1,
#'   ad_username = "BSMITH7"
#' )
#'
#' db_add_portal_message(
#'   message_text = "Global announcement",
#'   priority = 2,
#'   ad_username = "ADMIN",
#'   force_catch_all = TRUE
#' )
#' }
#'
#' @seealso [db_get_portal_messages()], [utils_get_app_id()]
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
    INSERT INTO {utils_resolve_schema('db_schema_01sr')}.[portal_messages] (
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
