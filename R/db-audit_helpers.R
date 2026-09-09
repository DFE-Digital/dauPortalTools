#' Record a File Download Event
#'
#' Inserts a record into the app analytics table to track file download actions,
#' translating raw usernames into robust user_id records.
#'
#' @param user Character scalar. Username or email token executing the action. Defaults to `"Guest"`.
#' @param page_name Character scalar. Name of the page where the download occurred.
#' @param file_name Character scalar. Name of the downloaded file.
#' @return `NULL`, invisibly.
#' @export
db_record_download <- function(user = "Guest", page_name, file_name) {
  log_event("Starting record_download", debug = TRUE)

  app_id <- utils_get_app_id()
  env_id <- utils_resolve_env_id()
  target_user_id <- db_user_create(user)

  log_event(
    glue::glue(
      "Recording download: user_id={target_user_id}, app_id={app_id}, env_id={env_id}, page={page_name}, file={file_name}"
    )
  )

  conn <- sql_manager("dit")
  on.exit(
    {
      try(DBI::dbDisconnect(conn), silent = TRUE)
      log_event("Finished record_download")
    },
    add = TRUE
  )

  query <- glue::glue_sql(
    "
    INSERT INTO {utils_resolve_schema('db_schema_01sr')}.[app_analytics]
      ([user_id],
       [app_id],
       [env_id],
       [event_timestamp],
       [page_name],
       [action_type],
       [action_sub_type])
    VALUES
      ({as.integer(target_user_id)},
       {as.integer(app_id)},
       {as.integer(env_id)},
       SYSUTCDATETIME(),
       {page_name},
       'Download',
       {file_name})
    ",
    .con = conn
  )

  utils_db_execute(conn, query)
  invisible(NULL)
}

#' @export
record_download <- function(user = "Guest", page_name, file_name) {
  .Deprecated(
    new = "db_record_download",
    package = "dauPortalTools",
    msg = "record_download() is deprecated; switch over to use db_record_download() which handles integer user_ids."
  )

  db_record_download(
    user = user,
    page_name = page_name,
    file_name = file_name
  )
}

#' Record a User Login Event
#'
#' Inserts a record into the app analytics table to track when a user
#' accesses the application, resolving their unique integer ID dynamically.
#'
#' @param user Character scalar. Username or email of the user performing the login. Defaults to `"Guest"`.
#' @return Integer scalar. The canonical primary key 'user_id' representing the logged-in user.
#' @export
db_record_login <- function(user = "Guest") {
  log_event("Starting db_record_login", debug = TRUE)

  app_id <- utils_get_app_id()
  env_id <- utils_resolve_env_id()
  target_user_id <- db_user_create(user)

  log_event(
    glue::glue(
      "Recording login: user_id={target_user_id}, app_id={app_id}, env_id={env_id}"
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

  analytics_query <- glue::glue_sql(
    "
    INSERT INTO {utils_resolve_schema('db_schema_01sr')}.[app_analytics]
      ([user_id],
       [app_id],
       [env_id],
       [event_timestamp],
       [page_name],
       [action_type],
       [action_sub_type])
    VALUES
      ({as.integer(target_user_id)},
       {as.integer(app_id)},
       {as.integer(env_id)},
       SYSUTCDATETIME(),
       'Home page',
       'Load',
       'Initial Load')
    ",
    .con = conn
  )

  utils_db_execute(conn, analytics_query)
  return(target_user_id)
}

#' @export
record_login <- function(user = "Guest") {
  .Deprecated(
    new = "db_record_login",
    package = "dauPortalTools",
    msg = "record_login() is deprecated; use db_record_login() instead"
  )

  db_record_login(user = user)
}

#' Write Centralised System Audit Log Entry
#'
#' Injects an audit event record into the shared central log directory. This function
#' is app-agnostic and relies on dynamic environment and application resolution.
#'
#' @param user_id Integer scalar. The unique profile user key executing the action.
#' @param action_type Character scalar. Classification action slice (e.g., 'INSERT', 'UPDATE', 'DELETE').
#' @param target_table Character scalar. Direct physical database table altered.
#' @param record_id Character or Integer scalar. Primary identifier key tracking altered row.
#' @param action_summary Character scalar. Narrative log detailing the transaction specifics.
#' @return Integer scalar. Count of affected transactional database rows (typically 1L on success).
#' @export
db_write_audit_log <- function(
  user_id,
  action_type,
  target_table,
  record_id = NULL,
  action_summary
) {
  log_event("Starting db_write_audit_log", debug = TRUE)
  on.exit(log_event("Finished db_write_audit_log"), add = TRUE)

  shiny::req(user_id, action_type, target_table, action_summary)

  app_id <- utils_get_app_id()
  env_id <- utils_resolve_env_id()

  conn <- sql_manager("dit")
  on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

  safe_record_str <- if (!is.null(record_id)) as.character(record_id) else NULL

  query <- glue::glue_sql(
    "INSERT INTO {utils_resolve_schema('db_schema_01sr')}.[audit_logs] 
       ([app_id], [user_id], [env_id], [action_type], [target_table], [record_id], [action_summary], [created_date])
     VALUES (
        {as.integer(app_id)},
        {as.integer(user_id)}, 
        {as.integer(env_id)}, 
        {action_type}, 
        {target_table}, 
        {safe_record_str}, 
        {action_summary}, 
        SYSUTCDATETIME()
     );",
    .con = conn
  )

  tryCatch(
    {
      utils_db_execute(conn, query)
    },
    error = function(e) {
      log_event(
        paste0("db_write_audit_log failed: ", e$message),
        2
      )
      warning("Global system audit logger dropped transaction: ", e$message)
      return(0L)
    }
  )
}
