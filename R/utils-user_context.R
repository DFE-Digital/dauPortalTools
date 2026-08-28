#' Get Current Portal Username Token
#'
#' Retrieves the network username signature token from the active Shiny session
#' or Posit Connect server headers. If unavailable (e.g. local development),
#' falls back to local configuration overrides or a guest default.
#'
#' @param session Optional Shiny session object. If `NULL`, attempts to auto-detect
#'   the active reactive domain context.
#' @param fallback Character scalar. Default identity string if no session or emulated
#'   token is present. Defaults to `"Guest"`.
#'
#' @details
#' Resolves user identity tokens in the following hierarchy:
#' \itemize{
#'   \item `session$user` (Posit Connect / LDAP header string)
#'   \item `config::get("emulate_user")` (Local dev override in config.yml)
#'   \item `fallback` ("Guest")
#' }
#'
#' @return Character scalar representing the raw username identity token.
#' @export
get_user <- function(session = NULL, fallback = "Guest") {
  log_event("Starting get_user session token resolution")

  if (is.null(session)) {
    session <- tryCatch(
      shiny::getDefaultReactiveDomain(),
      error = function(e) NULL
    )
  }

  # 1. Check Posit Connect / Server HTTP Header User
  if (!is.null(session) && !is.null(session$user) && nzchar(session$user)) {
    log_event(paste0(
      "Resolved user token directly from session$user: ",
      session$user
    ))
    return(session$user)
  }

  # 2. Check local development emulation token in config.yml
  emulate_user <- tryCatch(
    config::get("emulate_user"),
    error = function(e) NULL
  )

  if (!is.null(emulate_user) && nzchar(emulate_user)) {
    log_event(paste0(
      "Resolved user token from emulate_user config: ",
      emulate_user
    ))
    return(emulate_user)
  }

  # 3. Fallback
  log_event(paste0(
    "No active user session or emulation config found. Falling back to: ",
    fallback
  ))
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
    FROM [01_SSSR].[users] u
    JOIN [01_SSSR].[user_roles] ur
      ON u.user_id = ur.user_id
    JOIN [01_SSSR].[roles] r
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

get_user_id <- function(username) {
  if (is.null(username) || is.na(username) || username == "") {
    return(NA_integer_)
  }

  conn <- sql_manager("dit")
  on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

  result <- DBI::dbGetQuery(
    conn,
    "
      SELECT user_id
      FROM [Data_Insight_Team].[01_SSSR].[users]
      WHERE username = ?
    ",
    params = list(username)
  )

  if (nrow(result) == 0) {
    return(NA_integer_)
  }

  as.integer(result$user_id[1])
}
