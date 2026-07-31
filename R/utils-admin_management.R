#' Grant Application Access Permission
#'
#' High-level wrapper to assign roles to users. Automatically extracts who is performing
#' the authorization from their active user session.
#'
#' @param target_user_id Integer. The user ID to grant permission to.
#' @param role_name Character scalar. The system name string of the role (e.g., 'admin').
#' @param admin_user_id Integer. The user ID of the admin assigning the role.
#' @return Logical. TRUE if the write succeeded, FALSE otherwise.
#' @export
assign_user_role <- function(target_user_id, role_name, admin_user_id) {
  log_event("Starting assign_user_role")
  on.exit(log_event("Finished assign_user_role"), add = TRUE)

  shiny::req(target_user_id, role_name, admin_user_id)
  app_id <- utils_get_app_id()

  log_event(sprintf(
    "Attempting to assign role '%s' to user_id %d by admin_id %d under app_id %d",
    role_name,
    target_user_id,
    admin_user_id,
    app_id
  ))

  # Low-level DB query requires a connection context, so we establish it only for this lookup
  conn <- sql_manager("dit")
  on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

  role_query <- glue::glue_sql(
    "SELECT [role_id] FROM {utils_resolve_schema('db_schema_01sr')}.[roles_config] WHERE [role_name] = {role_name};",
    .con = conn
  )

  role_res <- tryCatch(
    utils_db_get_query(conn, role_query),
    error = function(e) {
      warning("Failed to look up role identity: ", e$message)
      NULL
    }
  )

  if (is.null(role_res) || nrow(role_res) == 0) {
    warning("Role '", role_name, "' does not exist in roles_config.")
    return(FALSE)
  }

  role_id <- as.integer(role_res$role_id[1])

  # Safely disconnect the local query connection before invoking downstream connection managers
  try(DBI::dbDisconnect(conn), silent = TRUE)

  log_event(sprintf(
    "Resolved role '%s' to role_id %d. Invoking database mutation matrix wrapper.",
    role_name,
    role_id
  ))

  # These helpers open/close their own internal database connection handles safely
  rows_changed <- db_insert_role_assignment(
    user_id = target_user_id,
    role_id = role_id,
    app_id = app_id,
    assigned_by = admin_user_id
  )

  if (rows_changed > 0) {
    log_event(
      "Database merge completed successfully. Generating system transactional audit footprint track entry."
    )
    db_write_audit_log(
      user_id = admin_user_id,
      action_type = "INSERT",
      target_table = "user_role_assignments",
      record_id = target_user_id,
      action_summary = sprintf(
        "Assigned role '%s' to user_id %d.",
        role_name,
        target_user_id
      )
    )
    return(TRUE)
  }

  warning(
    "db_insert_role_assignment reported 0 rows changed. Assignment operation skipped."
  )
  return(FALSE)
}

#' Revoke Application Access Permission
#'
#' Soft-deletes a user assignment row and files a system audit trace.
#'
#' @param target_user_id Integer. The user ID losing access privileges.
#' @param role_name Character scalar. The role string to strip.
#' @param admin_user_id Integer. The executing administrator ID.
#' @return Logical. TRUE if the write succeeded, FALSE otherwise.
#' @export
revoke_user_role <- function(target_user_id, role_name, admin_user_id) {
  log_event("Starting revoke_user_role")
  on.exit(log_event("Finished revoke_user_role"), add = TRUE)

  shiny::req(target_user_id, role_name, admin_user_id)
  app_id <- utils_get_app_id()

  log_event(sprintf(
    "Attempting to revoke role '%s' from user_id %d by admin_id %d under app_id %d",
    role_name,
    target_user_id,
    admin_user_id,
    app_id
  ))

  conn <- sql_manager("dit")
  on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

  role_query <- glue::glue_sql(
    "SELECT [role_id] FROM {utils_resolve_schema('db_schema_01sr')}.[roles_config] WHERE [role_name] = {role_name};",
    .con = conn
  )

  role_res <- tryCatch(
    utils_db_get_query(conn, role_query),
    error = function(e) {
      warning("Failed to look up role identity for revocation: ", e$message)
      NULL
    }
  )

  if (is.null(role_res) || nrow(role_res) == 0) {
    warning("Role '", role_name, "' does not exist in roles_config context.")
    return(FALSE)
  }

  role_id <- as.integer(role_res$role_id[1])

  try(DBI::dbDisconnect(conn), silent = TRUE)

  log_event(sprintf(
    "Resolved role '%s' to role_id %d. Invoking soft-delete revocation wrapper row.",
    role_name,
    role_id
  ))

  rows_changed <- db_revoke_role_assignment(
    user_id = target_user_id,
    role_id = role_id,
    app_id = app_id,
    revoked_by = admin_user_id
  )

  if (rows_changed > 0) {
    log_event(
      "Database revocation update completed successfully. Writing system transactional audit trail entry."
    )
    db_write_audit_log(
      user_id = admin_user_id,
      action_type = "UPDATE",
      target_table = "user_role_assignments",
      record_id = target_user_id,
      action_summary = sprintf(
        "Revoked role '%s' from user_id %d.",
        role_name,
        target_user_id
      )
    )
    return(TRUE)
  }

  warning(
    "db_revoke_role_assignment reported 0 rows changed. Revocation operation skipped."
  )
  return(FALSE)
}
