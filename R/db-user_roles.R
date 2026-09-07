#' Query Active User Privileges and Roles by App
#'
#' Retrieves active role assignments for a given user within an application context.
#'
#' @param user_id Integer scalar. Resolved user profile identifier.
#' @param app_id Integer scalar. App scope key context.
#' @return Character vector of active role names, or character(0) if none found.
#' @export
db_get_user_roles_by_app <- function(user_id, app_id) {
  log_event("Starting db_get_user_roles_by_app")
  on.exit(log_event("Finished db_get_user_roles_by_app"), add = TRUE)

  shiny::req(user_id, app_id)

  conn <- sql_manager("dit")
  on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

  query <- glue::glue_sql(
    "SELECT r.[role_name] 
     FROM {utils_resolve_schema('db_schema_01sr')}.[user_role_assignments] a
     INNER JOIN {utils_resolve_schema('db_schema_01sr')}.[roles_config] r 
       ON a.[role_id] = r.[role_id]
     WHERE a.[user_id] = {as.integer(user_id)} 
       AND a.[app_id] = {as.integer(app_id)} 
       AND a.[is_active] = 1;",
    .con = conn
  )

  res <- tryCatch(
    utils_db_get_query(conn, query),
    error = function(e) {
      log_event(paste0("db_get_user_roles_by_app error: ", e$message))
      warning("db_get_user_roles_by_app failed: ", e$message)
      NULL
    }
  )

  if (!is.null(res) && nrow(res) > 0) {
    return(as.character(res$role_name))
  }
  character(0)
}
