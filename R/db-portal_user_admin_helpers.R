#' Fetch app users and their role (default "User" if none)
#' @export
db_get_app_users <- function(conn, app_id) {
  sql <- "
    SELECT
      u.user_id,
      u.username,
      u.email,
      ISNULL(r.role_name, 'User') AS role_name,
      ur.role_id
    FROM [01_AIDT].[users] u
    LEFT JOIN [01_AIDT].[user_roles] ur
      ON ur.user_id = u.user_id AND ur.app_id = ?
    LEFT JOIN [01_AIDT].[roles] r
      ON ur.role_id = r.role_id
    ORDER BY u.username;
  "
  DBI::dbGetQuery(conn, sql, params = list(app_id))
}

#' Get all available roles
#' @export
db_get_roles <- function(conn) {
  DBI::dbGetQuery(
    conn,
    "SELECT role_id, role_name FROM [01_AIDT].[roles] ORDER BY role_name"
  )
}

#' Insert or update user role for an app
#' @export
db_update_user_role <- function(conn, user_id, role_id, app_id, assigned_by) {
  sql <- "
    MERGE [01_AIDT].[user_roles] AS tgt
    USING (SELECT ? AS user_id, ? AS role_id, ? AS app_id) AS src
      ON tgt.user_id = src.user_id AND tgt.app_id = src.app_id
    WHEN MATCHED THEN
      UPDATE SET role_id = src.role_id,
                 assigned_at = SYSUTCDATETIME(),
                 assigned_by = ?
    WHEN NOT MATCHED THEN
      INSERT (user_id, role_id, app_id, assigned_by)
      VALUES (src.user_id, src.role_id, src.app_id, ?);
  "
  DBI::dbExecute(
    conn,
    sql,
    params = list(
      user_id,
      role_id,
      app_id,
      assigned_by,
      assigned_by
    )
  )
}
