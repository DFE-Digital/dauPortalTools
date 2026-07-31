#' Get User System Role Privileges Ledger
#'
#' Retrieves the global collection of system configuration roles from the registry.
#'
#' @return A [`data.frame`] containing available roles.
#' @export
db_get_roles <- function() {
  log_event("Starting db_get_roles")
  on.exit(log_event("Finished db_get_roles"), add = TRUE)

  conn <- sql_manager("dit")
  on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

  query <- glue::glue_sql(
    "SELECT [role_id], [role_name], [friendly_name] 
     FROM {utils_resolve_schema('db_schema_01sr')}.[roles_config]
     ORDER BY [friendly_name];",
    .con = conn
  )

  tryCatch(
    utils_db_get_query(conn, query),
    error = function(e) {
      warning("db_get_roles failed: ", e$message)
      data.frame()
    }
  )
}

#' Update Targeted Account Security Application Profile
#'
#' Updates or reactivates an access profile row for a specific user within the active application scope.
#'
#' @param user_id Integer scalar. The structural internal user account tracking key identifier.
#' @param role_id Integer scalar. Targeted access matrix scope key.
#' @param assigned_by Integer scalar. Identifier of the user executing the change.
#' @return Integer scalar. Rows affected.
#' @export
db_update_user_role <- function(user_id, role_id, assigned_by) {
  log_event("Starting db_update_user_role")
  on.exit(log_event("Finished db_update_user_role"), add = TRUE)

  shiny::req(user_id, role_id, assigned_by)
  app_id <- utils_get_app_id()

  conn <- sql_manager("dit")
  on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

  query <- glue::glue_sql(
    "MERGE {utils_resolve_schema('db_schema_01sr')}.[user_role_assignments] AS target
     USING (SELECT {as.integer(user_id)} AS user_id, {as.integer(app_id)} AS app_id) AS source
     ON (target.user_id = source.user_id AND target.app_id = source.app_id)
     WHEN MATCHED THEN
        UPDATE SET 
          [role_id] = {as.integer(role_id)}, 
          [is_active] = 1, 
          [assigned_at] = SYSUTCDATETIME(), 
          [assigned_by] = {as.integer(assigned_by)}, 
          [revoked_at] = NULL, 
          [revoked_by] = NULL
     WHEN NOT MATCHED THEN
        INSERT ([user_id], [role_id], [app_id], [is_active], [assigned_at], [assigned_by])
        VALUES (source.user_id, {as.integer(role_id)}, source.app_id, 1, SYSUTCDATETIME(), {as.integer(assigned_by)});",
    .con = conn
  )

  tryCatch(
    utils_db_execute(conn, query),
    error = function(e) {
      warning("db_update_user_role failed: ", e$message)
      0L
    }
  )
}
