#' Record a User Login Event
#' Inserts a record into the `tools_analytics` table to track when a user
#' accesses the application.
#'
#' @param user Character scalar. Username of the user performing the login.
#'   Defaults to `"Guest"`.
#'
#' @details
#' The current application ID is resolved using [utils_get_app_id()]. The
#' event is recorded with an `action_type` of `"Load"` and a fixed
#' `action_sub_type` of `"Initial Load"` to indicate the initial entry
#' point into the application.
#'
#' The database schema is resolved using [utils_resolve_schema()], and the
#' query is executed using [utils_db_execute()].
#'
#' The timestamp is generated using `SYSUTCDATETIME()` at the database level.
#'
#' Database connections are managed internally and safely closed using
#' `on.exit()`. Logging is performed via [log_event()] at the start and
#' end of execution.
#'
#' @section Side Effects:
#' \itemize{
#'   \item Inserts a new row into the database.
#'   \item Opens and closes a database connection.
#'   \item Writes log entries via [log_event()].
#' }
#'
#' @return `NULL`, invisibly.
#'
#' @examples
#' \dontrun{
#' db_record_login(user = "BSMITH7")
#' }
#'
#' @seealso [db_record_download()]
#' @export

db_record_login <- function(
  user = "Guest",
  db_execute = utils_db_execute
) {
  log_event("Starting db_record_login")

  app_id <- utils_get_app_id()

  log_event(
    glue::glue(
      "Recording login: user={user}, app_id={app_id}"
    )
  )

  conn <- sql_manager("dit")
  on.exit(
    {
      try(dbDisconnect(conn), silent = TRUE)
      log_event("Finished db_record_login")
    },
    add = TRUE
  )

  query <- glue_sql(
    "
    INSERT INTO {utils_resolve_schema('db_schema_01a')}.[tools_analytics]
      (date_time_visited,
       page_name,
       action_type,
       ad_username,
       action_sub_type,
       app_id)
    VALUES
      (SYSUTCDATETIME(),
       'Home page',
       'Load',
       {user},
       'Initial Load',
       {app_id})
    ",
    .con = conn
  )

  utils_db_execute(conn, query)

  invisible(NULL)
}

#' @export
record_login <- function(user = "Guest") {
  .Deprecated(
    "db_record_login",
    msg = "record_login() is deprecated; use db_record_login() instead"
  )

  db_record_login(user = user)
}
