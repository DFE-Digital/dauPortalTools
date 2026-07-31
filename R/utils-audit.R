#' Write Centralised System Audit Log Entry
#'
#' Injects an audit change record into the shared central log files, handling state evaluation.
#'
#' @param user_id Integer. The unique profile user key executing mutations.
#' @param action_type Character scalar. Classification action ('INSERT', 'UPDATE', 'DELETE').
#' @param target_table Character scalar. Direct table context modified.
#' @param record_id Character or Integer scalar. Primary unique row entry modified identifier.
#' @param action_summary Character scalar. Clear narrative string describing what changed.
#' @return Integer scalar. Rows affected from downstream audit write.
#' @export
db_write_audit_log <- function(
  user_id,
  action_type,
  target_table,
  record_id = NULL,
  action_summary
) {
  log_event("Starting db_write_audit_log orchestrator")
  on.exit(log_event("Finished db_write_audit_log orchestrator"), add = TRUE)

  shiny::req(user_id, action_type, target_table, action_summary)

  app_id <- utils_get_app_id()
  env_id <- utils_resolve_env_id()

  # Low-level auditor handles connection management internally
  db_insert_audit_log(
    app_id = app_id,
    user_id = user_id,
    env_id = env_id,
    action_type = action_type,
    target_table = target_table,
    record_id = record_id,
    action_summary = action_summary
  )
}

#' Generate App Analytics Summary Card
#'
#' High-level wrapper that pulls calculated analytical traffic statistics for the calling app.
#'
#' @param days_back Integer. The scope of days to summarize. Defaults to 30.
#' @return A list containing calculated statistics blocks ready for UI cards.
#' @export
get_app_analytics_summary <- function(days_back = 30) {
  log_event("Starting get_app_analytics_summary")
  on.exit(log_event("Finished get_app_analytics_summary"), add = TRUE)

  app_id <- utils_get_app_id()
  log_event(sprintf(
    "Gathering analytics metrics card data for app_id %d across previous %d days",
    app_id,
    days_back
  ))

  # This core function handles its internal sql_manager connection safely on its own
  metrics <- db_get_analytics_summary(app_id = app_id, days_back = days_back)

  if (nrow(metrics) == 0) {
    log_event(
      "No analytical logs recovered for this execution window. Emitting fallback payload block."
    )
    return(list(unique_users = 0, total_hits = 0, top_page = "N/A"))
  }

  log_event(
    "Analytics metrics data successfully aggregated. Populating list infrastructure fields."
  )
  list(
    unique_users = as.integer(metrics$unique_users[1]),
    total_hits = as.integer(metrics$total_actions[1]),
    top_page = as.character(metrics$most_visited_page[1])
  )
}
