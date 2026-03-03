#' Retrieve a User's Role for the Current Application
#'
#' Looks up the role assigned to a given username for the application
#' identified by `app_id` in the package's configuration file. The function
#' automatically creates and closes a database connection using `sql_manager()`,
#' queries the user/role mapping tables, and returns the associated role name.
#'
#' @param username A character string representing the username whose role
#'   should be retrieved. This should match the `username` field in the
#'   `[01_AIDT].[users]` table.
#'
#' @return A character string containing the user's role name (e.g., `"admin"`),
#'   or `NULL` if the user has no assigned role for the configured application.
#'
#' @details
#' This function expects the package configuration (`config.yml`) to contain
#' an `app` block with an `app_id` value, for example:
#'
#' ```
#' app_details:
#'   app_id: 1
#' ```
#'
#' The role lookup is performed by joining:
#'
#' * `[01_AIDT].[users]`
#' * `[01_AIDT].[user_roles]`
#' * `[01_AIDT].[roles]`
#'
#' Only one role per user per application is permitted due to the database
#' constraint `uq_user_role_app (user_id, role_id, app_id)`.
#'
#' A database connection is created internally via `sql_manager()`, and is
#' automatically closed when the function exits.
#'
#' @examples
#' \dontrun{
#' # Returns the role for a given user
#' get_user_role("bsmith7")
#'
#' # Typical output:
#' # [1] "admin"
#' }
#'
#' @export

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
