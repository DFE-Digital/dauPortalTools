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

#' Retrieve Application User Assignments
#'
#' @param app_id Integer scalar. Application identifier.
#' @return Data frame of users and assigned roles.
#' @export
db_get_app_users <- function(app_id) {
  log_event(paste0("Starting db_get_app_users for app_id: ", app_id))
  on.exit(log_event("Finished db_get_app_users"), add = TRUE)

  shiny::req(app_id)

  conn <- sql_manager("dit")
  on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

  query <- glue::glue_sql(
    "SELECT 
       u.[user_id],
       u.[username],
       u.[email],
       r.[role_id],
       r.[role_name],
       a.[assigned_at]
     FROM {utils_resolve_schema('db_schema_01sr')}.[users] u
     LEFT JOIN {utils_resolve_schema('db_schema_01sr')}.[user_role_assignments] a 
       ON u.[user_id] = a.[user_id] AND a.[app_id] = {as.integer(app_id)} AND a.[is_active] = 1
     LEFT JOIN {utils_resolve_schema('db_schema_01sr')}.[roles_config] r 
       ON a.[role_id] = r.[role_id];",
    .con = conn
  )

  tryCatch(
    utils_db_get_query(conn, query),
    error = function(e) {
      log_event(paste0("db_get_app_users error: ", e$message))
      data.frame()
    }
  )
}

#' Update or Insert Application User Role Assignment
#'
#' @param user_id Integer scalar. Target user ID.
#' @param role_id Integer scalar. New role ID.
#' @param app_id Integer scalar. Application ID.
#' @param assigned_by Integer scalar. Admin user ID making the change.
#' @return Integer scalar. Rows affected.
#' @export
db_update_user_role <- function(user_id, role_id, app_id, assigned_by) {
  log_event(sprintf(
    "Starting db_update_user_role for user_id: %s, role_id: %s",
    user_id,
    role_id
  ))
  on.exit(log_event("Finished db_update_user_role"), add = TRUE)

  shiny::req(user_id, role_id, app_id)

  conn <- sql_manager("dit")
  on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

  # Deactivate previous active roles for this app, then insert the new role
  query <- glue::glue_sql(
    "BEGIN TRANSACTION;
     UPDATE {utils_resolve_schema('db_schema_01sr')}.[user_role_assignments]
     SET [is_active] = 0
     WHERE [user_id] = {as.integer(user_id)} AND [app_id] = {as.integer(app_id)};

     INSERT INTO {utils_resolve_schema('db_schema_01sr')}.[user_role_assignments]
       ([user_id], [role_id], [app_id], [is_active], [assigned_by], [assigned_at])
     VALUES
       ({as.integer(user_id)}, {as.integer(role_id)}, {as.integer(app_id)}, 1, {as.integer(assigned_by)}, SYSUTCDATETIME());
     COMMIT;",
    .con = conn
  )

  tryCatch(
    utils_db_execute(conn, query),
    error = function(e) {
      log_event(paste0("db_update_user_role error: ", e$message))
      0L
    }
  )
}
