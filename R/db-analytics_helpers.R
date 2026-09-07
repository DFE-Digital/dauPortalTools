#' Insert Standard App Clickstream Telemetry
#'
#' Writes a normalized event record directly to the application analytics ledger.
#'
#' @param conn A DBI database connection object.
#' @param user_id Integer. The canonical unique identity tracking key.
#' @param app_id Integer. Normalized application ID.
#' @param env_id Integer/Byte. The hosting environment target ID (1 = dev, 2 = test, 3 = prod).
#' @param page_name Character scalar. Standardized view target location.
#' @param action_type Character scalar. Standard action verb ('Load', 'Click', 'Download').
#' @param action_sub_type Character scalar. Supplemental metadata payload context.
#' @return Integer scalar. Count of affected transactional database rows.
#' @export
db_insert_app_analytics <- function(
  user_id,
  app_id,
  env_id,
  page_name,
  action_type,
  action_sub_type = NULL
) {
  log_event("Starting db_insert_app_analytics")
  on.exit(log_event("Finished db_insert_app_analytics"), add = TRUE)

  conn <- sql_manager("dit")

  shiny::req(user_id, app_id, env_id, page_name, action_type)

  query <- glue::glue_sql(
    "INSERT INTO {utils_resolve_schema('db_schema_01sr')}.[app_analytics] 
       ([user_id], [app_id], [env_id], [event_timestamp], [page_name], [action_type], [action_sub_type])
     VALUES 
       ({as.integer(user_id)}, {as.integer(app_id)}, {as.integer(env_id)}, SYSUTCDATETIME(), {page_name}, {action_type}, {action_sub_type});",
    .con = conn
  )

  tryCatch(
    utils_db_execute(conn, query),
    error = function(e) {
      warning("db_insert_app_analytics failed to log telemetry: ", e$message)
      0L
    }
  )
}

#' Insert Platform System Audit Log Entry
#'
#' Writes a structured modification footprint tracking user mutations directly to the audit log.
#'
#' @param conn A DBI database connection object.
#' @param app_id Integer. Normalized application ID.
#' @param user_id Integer. The canonical unique identity tracking key.
#' @param env_id Integer/Byte. The hosting environment target ID (1 = dev, 2 = test, 3 = prod).
#' @param action_type Character scalar. Database mutation operation slice ('INSERT', 'UPDATE', 'DELETE').
#' @param target_table Character scalar. Direct physical table modified.
#' @param record_id Character/Integer scalar. Primary identifier key tracking altered row.
#' @param action_summary Character scalar. Narrative log detailing the transaction specifics.
#' @return Integer scalar. Count of affected transactional database rows.
#' @export
db_insert_audit_log <- function(
  conn,
  app_id,
  user_id,
  env_id,
  action_type,
  target_table,
  record_id = NULL,
  action_summary
) {
  log_event("Starting db_insert_audit_log")
  on.exit(log_event("Finished db_insert_audit_log"), add = TRUE)

  conn <- sql_manager("dit")

  shiny::req(
    conn,
    app_id,
    user_id,
    env_id,
    action_type,
    target_table,
    action_summary
  )

  safe_record_str <- if (!is.null(record_id)) as.character(record_id) else NULL

  query <- glue::glue_sql(
    "INSERT INTO {utils_resolve_schema('db_schema_01sr')}.[audit_logs] 
       ([app_id], [user_id], [env_id], [action_type], [target_table], [record_id], [action_summary], [created_date])
     VALUES 
       ({as.integer(app_id)}, {as.integer(user_id)}, {as.integer(env_id)}, {action_type}, {target_table}, {safe_record_str}, {action_summary}, SYSUTCDATETIME());",
    .con = conn
  )

  tryCatch(
    utils_db_execute(conn, query),
    error = function(e) {
      warning("db_insert_audit_log failed to write log entry: ", e$message)
      0L
    }
  )
}

#' Fetch Aggregated Application Usage Summary
#'
#' Generates summary metrics from the analytics table for reporting dashboards.
#'
#' @param conn A DBI database connection object.
#' @param app_id Integer. The app ID to filter summaries against.
#' @param days_back Integer. Timespan to calculate metrics over.
#' @return A data frame with unique users, total hits, and top active pages.
#' @export
db_get_analytics_summary <- function(conn, app_id, days_back = 30) {
  log_event("Starting db_get_analytics_summary")
  on.exit(log_event("Finished db_get_analytics_summary"), add = TRUE)

  conn <- sql_manager("dit")

  shiny::req(conn, app_id)

  query <- glue::glue_sql(
    "SELECT 
        COUNT(DISTINCT [user_id]) AS unique_users,
        COUNT([analytics_id]) AS total_actions,
        (SELECT TOP 1 [page_name] FROM {utils_resolve_schema('db_schema_01sr')}.[app_analytics] 
         WHERE [app_id] = {as.integer(app_id)} AND [event_timestamp] >= DATEADD(day, -{as.integer(days_back)}, SYSUTCDATETIME())
         GROUP BY [page_name] ORDER BY COUNT(*) DESC) AS most_visited_page
     FROM {utils_resolve_schema('db_schema_01sr')}.[app_analytics]
     WHERE [app_id] = {as.integer(app_id)}
       AND [event_timestamp] >= DATEADD(day, -{as.integer(days_back)}, SYSUTCDATETIME());",
    .con = conn
  )

  tryCatch(
    utils_db_get_query(conn, query),
    error = function(e) {
      warning("db_get_analytics_summary failed: ", e$message)
      data.frame()
    }
  )
}
