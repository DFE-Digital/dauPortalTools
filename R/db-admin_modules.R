#' Query Access Control Matrix Configuration Directory
#'
#' Returns full list profiles of all users who have an active role configuration profile row.
#'
#' @param conn A DBI database connection object.
#' @param app_id Integer. Application context filter frame.
#' @return Data frame listing assigned users, current roles, and assignees.
#' @export
db_get_app_users_matrix <- function(conn, app_id) {
  log_event("Starting db_get_app_users_matrix", debug = TRUE)
  on.exit(log_event("Finished db_get_app_users_matrix"), add = TRUE)

  shiny::req(conn, app_id)

  query <- glue::glue_sql(
    "SELECT 
        u.[user_id], u.[username], u.[email],
        rc.[friendly_name] AS [assigned_role],
        a.[assigned_at],
        admin.[username] AS [assigned_by_user]
     FROM {utils_resolve_schema('db_schema_01sr')}.[user_role_assignments] a
     INNER JOIN {utils_resolve_schema('db_schema_01sr')}.[users] u ON a.[user_id] = u.[user_id]
     INNER JOIN {utils_resolve_schema('db_schema_01sr')}.[roles_config] rc ON a.[role_id] = rc.[role_id]
     INNER JOIN {utils_resolve_schema('db_schema_01sr')}.[users] admin ON a.[assigned_by] = admin.[user_id]
     WHERE a.[app_id] = {as.integer(app_id)} 
       AND a.[is_active] = 1;",
    .con = conn
  )

  tryCatch(
    utils_db_get_query(conn, query),
    error = function(e) {
      log_event(
        paste0("db_get_app_users_matrix failed: ", e$message),
        2
      )
      warning("db_get_app_users_matrix failed: ", e$message)
      data.frame()
    }
  )
}

#' Query Reference Table Lists of System Config Roles
#'
#' @param conn A DBI database connection object.
#' @return Data frame containing available configurations under roles_config lookup.
#' @export
db_get_available_roles <- function(conn) {
  log_event("Starting db_get_available_roles", debug = TRUE)
  on.exit(log_event("Finished db_get_available_roles"), add = TRUE)

  shiny::req(conn)

  query <- glue::glue_sql(
    "SELECT [role_id], [role_name], [friendly_name] 
     FROM {utils_resolve_schema('db_schema_01sr')}.[roles_config]
     ORDER BY [friendly_name];",
    .con = conn
  )

  tryCatch(
    utils_db_get_query(conn, query),
    error = function(e) {
      log_event(
        paste0("db_get_available_roles failed: ", e$message),
        2
      )
      warning("db_get_available_roles failed: ", e$message)
      data.frame()
    }
  )
}

#' Retrieve Historical Role Assignment Audit Trail
#'
#' Returns a complete audit history of role assignments (both active and inactive)
#' for a specific application scope.
#'
#' @param app_id Integer scalar. The target application scope context.
#' @return A [`data.frame`] containing the assignment history ledger.
#' @export
db_get_user_role_history <- function(app_id) {
  log_event("Starting db_get_user_role_history", debug = TRUE)

  shiny::req(app_id)
  conn <- sql_manager("dit")
  on.exit(
    {
      try(DBI::dbDisconnect(conn), silent = TRUE)
      log_event("Finished db_get_user_role_history")
    },
    add = TRUE
  )

  query <- glue::glue_sql(
    "
    SELECT 
      u.[username] AS [target_user],
      rc.[friendly_name] AS [role_name],
      a.[is_active],
      a.[assigned_at],
      admin_in.[username] AS [assigned_by_user],
      a.[revoked_at],
      admin_out.[username] AS [revoked_by_user]
    FROM {utils_resolve_schema('db_schema_01sr')}.[user_role_assignments] a
    INNER JOIN {utils_resolve_schema('db_schema_01sr')}.[users] u ON a.[user_id] = u.[user_id]
    INNER JOIN {utils_resolve_schema('db_schema_01sr')}.[roles_config] rc ON a.[role_id] = rc.[role_id]
    INNER JOIN {utils_resolve_schema('db_schema_01sr')}.[users] admin_in ON a.[assigned_by] = admin_in.[user_id]
    LEFT JOIN {utils_resolve_schema('db_schema_01sr')}.[users] admin_out ON a.[revoked_by] = admin_out.[user_id]
    WHERE a.[app_id] = {as.integer(app_id)}
    ORDER BY COALESCE(a.[revoked_at], a.[assigned_at]) DESC;
    ",
    .con = conn
  )

  tryCatch(
    utils_db_get_query(conn, query),
    error = function(e) {
      log_event(
        paste0("db_get_user_role_history failed: ", e$message),
        2
      )
      warning("db_get_user_role_history failed: ", e$message)
      data.frame()
    }
  )
}
