#' Record User Login Activity
#'
#' Logs a user's visit to the application by inserting a record into the
#' tools_analytics table.
#'
#' @param user Character. The username to record. Defaults to "Guest".
#' @param db_execute Function used to execute SQL (dependency injection).
#'
#' @return Invisibly returns NULL.
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
      try(DBI::dbDisconnect(conn), silent = TRUE)
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

  db_execute(conn, query)

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
