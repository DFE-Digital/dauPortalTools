#' Assign a System Role to a User
#'
#' Inserts a new role assignment or flips an inactive assignment back to active.
#'
#' @param conn A DBI database connection object.
#' @param user_id Integer. The target user ID receiving the role.
#' @param role_id Integer. The ID of the role being assigned.
#' @param app_id Integer. The target application ID scope.
#' @param assigned_by Integer. The user ID of the administrator granting access.
#' @return Integer scalar. Rows affected.
#' @export
db_insert_role_assignment <- function(
  conn,
  user_id,
  role_id,
  app_id,
  assigned_by
) {
  log_event("Starting db_insert_role_assignment")
  on.exit(log_event("Finished db_insert_role_assignment"), add = TRUE)

  shiny::req(conn, user_id, role_id, app_id, assigned_by)

  query <- glue::glue_sql(
    "MERGE {utils_resolve_schema('db_schema_01sr')}.[user_role_assignments] AS target
     USING (SELECT {as.integer(user_id)} AS user_id, {as.integer(role_id)} AS role_id, {as.integer(app_id)} AS app_id) AS source
     ON (target.user_id = source.user_id AND target.role_id = source.role_id AND target.app_id = source.app_id)
     WHEN MATCHED THEN
        UPDATE SET [is_active] = 1, [assigned_at] = SYSUTCDATETIME(), [assigned_by] = {as.integer(assigned_by)}, [revoked_at] = NULL, [revoked_by] = NULL
     WHEN NOT MATCHED THEN
        INSERT ([user_id], [role_id], [app_id], [is_active], [assigned_at], [assigned_by])
        VALUES (source.user_id, source.role_id, source.app_id, 1, SYSUTCDATETIME(), {as.integer(assigned_by)});",
    .con = conn
  )

  tryCatch(
    utils_db_execute(conn, query),
    error = function(e) {
      warning("db_insert_role_assignment failed: ", e$message)
      0L
    }
  )
}

#' Revoke a System Role from a User (Soft-Delete)
#'
#' Rather than deleting the row, this marks the assignment as inactive and records who revoked it.
#'
#' @param conn A DBI database connection object.
#' @param user_id Integer. The target user ID losing the role.
#' @param role_id Integer. The ID of the role being revoked.
#' @param app_id Integer. The target application ID scope.
#' @param revoked_by Integer. The user ID of the administrator pulling access.
#' @return Integer scalar. Rows affected.
#' @export
db_revoke_role_assignment <- function(
  conn,
  user_id,
  role_id,
  app_id,
  revoked_by
) {
  log_event("Starting db_revoke_role_assignment")
  on.exit(log_event("Finished db_revoke_role_assignment"), add = TRUE)

  shiny::req(conn, user_id, role_id, app_id, revoked_by)

  query <- glue::glue_sql(
    "UPDATE {utils_resolve_schema('db_schema_01sr')}.[user_role_assignments]
     SET [is_active] = 0, [revoked_at] = SYSUTCDATETIME(), [revoked_by] = {as.integer(revoked_by)}
     WHERE [user_id] = {as.integer(user_id)} 
       AND [role_id] = {as.integer(role_id)} 
       AND [app_id] = {as.integer(app_id)} 
       AND [is_active] = 1;",
    .con = conn
  )

  tryCatch(
    utils_db_execute(conn, query),
    error = function(e) {
      warning("db_revoke_role_assignment failed: ", e$message)
      0L
    }
  )
}
