#' Get Current Portal Username
#'
#' Retrieves the username from the active Shiny (or Posit Connect) session.
#' If unavailable (e.g. local development), falls back to a configured or
#' provided default value.
#'
#' @param session Optional Shiny session object. If `NULL`, the function
#'   attempts to detect the current session automatically.
#' @param fallback Character scalar. Username to return if no session or
#'   emulated user is available. Defaults to `"Guest"`.
#'
#' @details
#' The function resolves the username in the following order:
#' \itemize{
#'   \item `session$user`, if available
#'   \item `config::get("emulate_user")`, if defined
#'   \item `fallback`
#' }
#'
#' This allows consistent behaviour across production and local development.
#'
#' @return Character scalar representing the resolved username.
#'
#' @seealso [config::get()]
#'
#' @export

get_user <- function(session = NULL, fallback = "Guest") {
  if (is.null(session)) {
    session <- tryCatch(
      shiny::getDefaultReactiveDomain(),
      error = function(e) NULL
    )
  }

  if (!is.null(session) && !is.null(session$user) && nzchar(session$user)) {
    return(session$user)
  }

  emulate_user <- tryCatch(
    config::get("emulate_user"),
    error = function(e) NULL
  )

  if (!is.null(emulate_user) && nzchar(emulate_user)) {
    return(emulate_user)
  }

  fallback
}

#' Retrieve User Role for Current Application
#'
#' Looks up the role assigned to a user for the current application.
#'
#' @param username Character scalar. Username to retrieve the role for.
#'
#' @details
#' The function:
#' \itemize{
#'   \item Retrieves `app_id` from configuration via [get_config()]
#'   \item Creates a database connection using [sql_manager()]
#'   \item Queries user, role, and mapping tables
#' }
#'
#' The lookup joins:
#' \itemize{
#'   \item `[01_AIDT].[users]`
#'   \item `[01_AIDT].[user_roles]`
#'   \item `[01_AIDT].[roles]`
#' }
#'
#' The database connection is automatically closed on exit.
#'
#' @return Character scalar containing the user's role name, or `NULL`
#'   if no role is found.
#'
#' @section Side Effects:
#' \itemize{
#'   \item Opens and closes a database connection via [sql_manager()]
#' }
#'
#' @examples
#' \dontrun{
#' get_user_role("bsmith7")
#' }
#'
#' @seealso [get_config()], [sql_manager()]
#'
#' @export

get_user_role <- function(username) {
  if (!nzchar(username)) {
    return(NULL)
  }

  conf <- get_config()
  app_id <- conf$app_details$app_id

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

#' Retrieve User ID from Username#' Retrieve User ID from Username user's numeric `user_id` in the AIDT users table.
#'
#' @param conn A `DBIConnection` object created by [sql_manager()].
#' @param username Character scalar. Username to look up.
#'
#' @details
#' The function queries the `[01_AIDT].[users]` table and returns the
#' corresponding `user_id` if found.
#'
#' @return Integer scalar `user_id`, or `NA_integer_` if the user is not found
#'   or the input is invalid.
#'
#' @section Side Effects:
#' \itemize{
#'   \item Executes a database query via [DBI::dbGetQuery()]
#' }
#'
#' @examples
#' \dontrun{
#' conn <- sql_manager("dit")
#' get_user_id(conn, "bsmith7")
#' }
#'
#' @seealso [sql_manager()]
#'
#' @export

get_user_id <- function(conn, username) {
  if (is.null(username) || is.na(username) || username == "") {
    return(NA_integer_)
  }

  result <- DBI::dbGetQuery(
    conn,
    "
      SELECT user_id
      FROM [Data_Insight_Team].[01_AIDT].[users]
      WHERE username = ?
    ",
    params = list(username)
  )

  if (nrow(result) == 0) {
    return(NA_integer_)
  }

  return(as.integer(result$user_id[1]))
}
