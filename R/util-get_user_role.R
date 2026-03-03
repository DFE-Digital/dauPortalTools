get_user_role <- function(username) {
  if (!nzchar(username)) {
    return(NULL)
  }

  app_id <- tryCatch(
    config::get("app_details")$app_id,
    error = function(e) NULL
  )
  if (is.null(app_id)) {
    stop("No app_id defined in config under app_details$app_id")
  }

  conn <- sql_manager("dit")

  on.exit(DBI::dbDisconnect(conn), add = TRUE)

  sql <- "
    SELECT r.role_name
    FROM [01_AIDT].[users] u
    JOIN [01_AIDT].[user_roles] ur
      ON u.user_id = ur.user_id
    JOIN [01_AIDT].[roles] r
      ON ur.role_id = r.role_id
    WHERE u.username = ?
      AND ur.app_id = ?
  "

  res <- DBI::dbGetQuery(conn, sql, params = list(username, app_id))

  if (nrow(res) == 0) {
    return(NULL)
  }

  res$role_name[[1]]
}
