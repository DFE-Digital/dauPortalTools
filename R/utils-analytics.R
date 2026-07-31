#' Record a User Login Event (Universal)
#'
#' High-level wrapper deployed at shiny server startup to drop an 'Initial Load'
#' timestamp trace and register the active user_id.
#'
#' @param user Character scalar. Raw identity signature token from session header maps.
#' @return Integer scalar. The fully resolved unique integer primary key [user_id].
#' @export
db_record_login <- function(user = "Guest") {
  log_event("Starting db_record_login orchestrator")
  on.exit(log_event("Finished db_record_login orchestrator"), add = TRUE)

  # 1. Coordinate and resolve unique user_id (manages its own connection)
  target_user_id <- db_user_create(user)

  app_id <- utils_get_app_id()
  env_id <- utils_resolve_env_id()

  # 2. Write standard telemetry load via internal DB execution layer function (manages connection)
  db_insert_app_analytics(
    user_id = target_user_id,
    app_id = app_id,
    env_id = env_id,
    page_name = "Home page",
    action_type = "Load",
    action_sub_type = "Initial Load"
  )

  return(target_user_id)
}
