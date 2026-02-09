#' Record a File Download Event
#'
#' Logs a user's download action into the `tools_analytics` table, capturing
#' timestamp, page name, action type, username, downloaded file name, and app ID.
#'
#' @param user Character. Active Directory username. Default is `"Guest"`.
#' @param page_name Character. Name of the page or module where the download occurred.
#' @param file_name Character. Name of the file being downloaded (e.g. `"school_data_20240209.csv"`).
#'
#' @details
#' Reads configuration from `config.yml` to obtain the application ID and schema
#' settings. An SQL `INSERT` is performed into the `tools_analytics` table.
#' This function mirrors the `record_login()` structure for consistency across
#' DAU Portal Tools.
#'
#' @return None. Called for its side effect of logging analytics.
#'
#' @examples
#' \dontrun{
#' record_download(
#'   user = "ben.smith",
#'   page_name = "School Details",
#'   file_name = "school_data_20240208.csv"
#' )
#' }
#'
#' @export
record_download <- function(
  user = "Guest",
  page_name,
  file_name
) {
  log_event("Starting function record_download")

  app_id <- conf$app_details$app_id
  date_time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  conn <- sql_manager("dit")

  log_event(
    glue::glue(
      "Recording download by {user} for app id {app_id}. Page: {page_name}, File: {file_name}"
    ),
    conf
  )

  schema_01a <- DBI::SQL(conf$schemas$db_schema_01a)

  sql_command <- glue::glue_sql(
    "
    INSERT INTO {schema_01a}.[tools_analytics]
      (date_time_visited,
       page_name,
       action_type,
       ad_username,
       action_sub_type,
       app_id)
    VALUES
      ({date_time},
       {page_name},
       'Download',
       {user},
       {file_name},
       {app_id})
    ",
    .con = conn
  )

  DBI::dbExecute(conn, sql_command)

  log_event("Finished recording download")
}
