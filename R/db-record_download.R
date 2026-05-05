#' Record a File Download Event
#'
#' Logs a user's download action into the tools_analytics table.
#'
#' @param user Character. Active Directory username. Default is "Guest".
#' @param page_name Character. Page where the download occurred.
#' @param file_name Character. Name of the file downloaded.
#' @param db_execute Function used to execute SQL (dependency injection).
#'
#' @return Invisibly returns NULL.
#' @export
db_record_download <- function(
  user = "Guest",
  page_name,
  file_name,
  db_execute = utils_db_execute
) {
  log_event("Starting record_download")

  app_id <- utils_get_app_id()

  log_event(
    glue::glue(
      "Recording download: user={user}, app_id={app_id}, page={page_name}, file={file_name}"
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
       {page_name},
       'Download',
       {user},
       {file_name},
       {app_id})
    ",
    .con = conn
  )

  db_execute(conn, query)

  invisible(NULL)
}


record_download <- function(
  user = "Guest",
  page_name,
  file_name
) {
  .Deprecated(
    "db_record_download",
    msg = "record_download() is deprecated; use db_record_download() instead"
  )

  db_record_download(
    user = user,
    page_name = page_name,
    file_name = file_name
  )
}
