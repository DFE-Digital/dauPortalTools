# R/db-portal_user_admin_helpers.R

#' Fetch application users and their assigned role
#' @export
db_get_app_users <- function(
  app_id,
  db_get_query = utils_db_get_query
) {
  log_event("Starting db_get_app_users")

  conn <- sql_manager("dit")
  on.exit(
    {
      try(DBI::dbDisconnect(conn), silent = TRUE)
      log_event("Finished db_get_app_users")
    },
    add = TRUE
  )

  query <- glue_sql(
    "
    SELECT
      u.user_id,
      u.username,
      u.email,
      ISNULL(r.role_name, 'User') AS role_name,
      ur.role_id
    FROM {utils_resolve_schema('01a')}.[users] u
    LEFT JOIN {utils_resolve_schema('01a')}.[user_roles] ur
      ON ur.user_id = u.user_id
     AND ur.app_id  = {app_id}
    LEFT JOIN {utils_resolve_schema('01a')}.[roles] r
      ON ur.role_id = r.role_id
    ORDER BY u.username;
    ",
    .con = conn
  )

  db_get_query(conn, query)
}


#' Retrieve roles
#' @export
db_get_roles <- function(
  db_get_query = utils_db_get_query
) {
  log_event("Starting db_get_roles")

  conn <- sql_manager("dit")
  on.exit(
    {
      try(DBI::dbDisconnect(conn), silent = TRUE)
      log_event("Finished db_get_roles")
    },
    add = TRUE
  )

  query <- glue_sql(
    "
    SELECT role_id, role_name
    FROM {utils_resolve_schema('01a')}.[roles]
    ORDER BY role_name;
    ",
    .con = conn
  )

  db_get_query(conn, query)
}


#' Update user role
#' @export
db_update_user_role <- function(
  user_id,
  role_id,
  app_id,
  assigned_by,
  db_execute = utils_db_execute
) {
  log_event("Starting db_update_user_role")

  user_id <- as.integer(user_id)
  role_id <- as.integer(role_id)
  app_id <- as.integer(app_id)
  assigned_by <- as.integer(assigned_by)

  conn <- sql_manager("dit")
  on.exit(
    {
      try(DBI::dbDisconnect(conn), silent = TRUE)
      log_event("Finished db_update_user_role")
    },
    add = TRUE
  )

  query <- glue_sql(
    "
    MERGE {utils_resolve_schema('01a')}.[user_roles] AS tgt
    USING (
      SELECT
        {user_id} AS user_id,
        {role_id} AS role_id,
        {app_id}  AS app_id
    ) AS src
       ON tgt.user_id = src.user_id
      AND tgt.app_id  = src.app_id

    WHEN MATCHED THEN
      UPDATE SET
        role_id     = src.role_id,
        assigned_at = SYSUTCDATETIME(),
        assigned_by = {assigned_by}

    WHEN NOT MATCHED THEN
      INSERT (user_id, role_id, app_id, assigned_by)
      VALUES (src.user_id, src.role_id, src.app_id, {assigned_by});
    ",
    .con = conn
  )

  db_execute(conn, query)
}
