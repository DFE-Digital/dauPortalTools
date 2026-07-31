#' Resolve or Create User Profile from Token
#'
#' Utility pipeline that wraps the fast-to-slow identity lookup cascade.
#' This function evaluates session headers, manages connections, and runs provisioning scripts.
#'
#' @param login_token Character scalar. Shorthand network username token or email address.
#' @return Integer scalar. The canonical primary key [user_id].
#' @export
db_user_create <- function(login_token) {
  log_event("Starting db_user_create")
  on.exit(log_event("Finished db_user_create"), add = TRUE)

  if (
    is.null(login_token) ||
      !nzchar(login_token) ||
      identical(login_token, "Guest")
  ) {
    log_event(
      "Login token is missing or 'Guest'. Defaulting to guest user_id = 1L."
    )
    return(1L)
  }

  conn <- sql_manager("dit")
  on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

  token_clean <- tolower(trimws(login_token))

  # 1. Fast Path Check
  log_event(paste0("Attempting Fast Path Check for token: ", token_clean))
  fast_profile <- db_get_user_by_token(conn, token_clean)
  if (nrow(fast_profile) > 0) {
    target_uid <- honest_uid <- as.integer(fast_profile$user_id[1])
    log_event(paste0(
      "Fast Path Match successful. Resolved user_id: ",
      target_uid
    ))
    return(target_uid)
  }

  # 2. Slower Alias Resolution Path
  log_event(paste0(
    "Fast Path failed. Checking user aliases for token: ",
    token_clean
  ))
  target_email <- db_get_user_email_alias(conn, token_clean)

  if (is.null(target_email)) {
    log_event(
      "No explicit alias found. Constructing canonical fallback address string."
    )
    target_email <- if (grepl("@", token_clean, fixed = TRUE)) {
      token_clean
    } else {
      paste0(token_clean, "@education.gov.uk")
    }
  }

  # 3. Check for existence under canonical email address
  log_event(paste0(
    "Evaluating database registry for canonical email: ",
    target_email
  ))
  email_profile <- db_get_user_by_email(conn, target_email)
  if (nrow(email_profile) > 0) {
    target_uid <- as.integer(email_profile$user_id[1])
    log_event(paste0(
      "Canonical email matched existing record. Resolved user_id: ",
      target_uid
    ))
    return(target_uid)
  }

  # 4. Slowest Path: Just-in-Time Provisioning
  log_event(paste0(
    "User not registered. Executing Just-In-Time Provisioning for: ",
    target_email
  ))
  username_prefix <- strsplit(target_email, "@")[[1]][1]
  new_uid <- db_insert_new_user(conn, username_prefix, target_email)

  log_event(paste0(
    "JIT Provisioning complete. New user created with user_id: ",
    new_uid
  ))
  return(new_uid)
}

#' Retrieve App-Specific User Role Assignments
#'
#' Higher-level utility that dynamically identifies the calling application ID and checks
#' the user's active database roles.
#'
#' @param user_id Integer scalar. The unique profile user identity key.
#' @return Character vector. The active authorization role assigned to the user.
#' @export
get_user_role <- function(user_id) {
  log_event("Starting get_user_role")
  on.exit(log_event("Finished get_user_role"), add = TRUE)

  if (is.null(user_id) || identical(user_id, 1L)) {
    log_event("Null or guest user_id provided. Access role context skipped.")
    return(NULL)
  }

  app_id <- utils_get_app_id()
  log_event(paste0(
    "Resolving active roles for user_id: ",
    user_id,
    " under app_id: ",
    app_id
  ))

  conn <- sql_manager("dit")
  on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

  roles <- db_get_user_roles_by_app(conn, user_id, app_id)
  log_event(paste0(
    "Role resolution complete. Total active role profiles found: ",
    length(roles)
  ))

  return(roles)
}
