#' Query Dynamic User Permissions
#'
#' Evaluates a user's active permissions by crawling the assignment matrix through
#' assigned roles down to explicit feature configurations.
#'
#' @param conn A DBI database connection object.
#' @param user_id Integer. Canonical unique profile identity key.
#' @param app_id Integer. Target application scope context.
#' @return Character vector of active permission strings (e.g., 'can_delete_records').
#' @export
db_get_user_permissions <- function(conn, user_id, app_id) {
  log_event("Starting db_get_user_permissions")
  on.exit(log_event("Finished db_get_user_permissions"), add = TRUE)

  shiny::req(conn, user_id, app_id)

  query <- glue::glue_sql(
    "SELECT DISTINCT p.[permission_name]
     FROM {utils_resolve_schema('db_schema_01sr')}.[user_role_assignments] a
     INNER JOIN {utils_resolve_schema('db_schema_01sr')}.[role_permissions] rp ON a.[role_id] = rp.[role_id]
     INNER JOIN {utils_resolve_schema('db_schema_01sr')}.[permissions_config] p ON rp.[permission_id] = p.[permission_id]
     WHERE a.[user_id] = {as.integer(user_id)}
       AND a.[app_id] = {as.integer(app_id)}
       AND a.[is_active] = 1;",
    .con = conn
  )

  res <- tryCatch(
    utils_db_get_query(conn, query),
    error = function(e) {
      warning("db_get_user_permissions failed: ", e$message)
      data.frame()
    }
  )

  if (nrow(res) > 0) {
    return(as.character(res$permission_name))
  }
  return(character(0))
}
