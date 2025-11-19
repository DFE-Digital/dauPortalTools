#' Record User Login Activity
#'
#' This function logs a user's visit to the application by inserting a record into the
#' `tools_analytics` table. It captures details such as the timestamp, page name, action type,
#' username, and associated application ID.
#'
#' @param user Character. The username to record. Defaults to `"Guest"`.
#'
#' @details
#' The function reads configuration details from `config.yml` to determine the application ID
#' and database schema. It then constructs and executes an SQL `INSERT` statement to log the
#' visit. Logging messages are generated at the start and end of the process.
#'
#' @return No return value. The function is called for its side effect of recording the login.
#'
#' @examples
#' \dontrun{
#' # Record a login for the default user
#' record_login()
#'
#' # Record a login for a specific user
#' record_login(user = "ben.smith")
#' }
#'
#' @export

record_login <- function(user = "Guest") {
  # Log the start of the function
  log_event("Starting function record_login", conf)

  # Load confuration from YAML file
  library(yaml)
  conf <- yaml::read_yaml("config.yml", eval.expr = TRUE)
  app_id <- conf$app_details$app_id
  date <- Sys.time()
  conn <- sql_manager("dit")

  # Log the received record ID and app ID
  log_event(glue::glue("Received {user} for app id: {app_id}."), conf)

  schema_01a <- DBI::SQL(conf$schemas$db_schema_01a)

  # Construct the SQL query to retrieve quality data
  sql_command <- glue::glue_sql(
    "
    INSERT INTO {schema_01a}.[tools_analytics]
    (date_time_visited,
           page_name,
           action_type,
           ad_username,
           action_sub_type,
           app_id)
    VALUES ({date},'Home page','Load',{user},'Initial Load',{app_id}) 
    ",
    .con = conn
  )

  DBI::dbExecute(conn, sql_command)

  # Log the completion of data retrieval
  log_event("Finished recording login")
}
