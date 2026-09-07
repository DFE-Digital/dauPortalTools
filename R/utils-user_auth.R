#' Convert Email to AD Username Heuristic
#'
#' Handles:
#'   - Optional digits: 'ben.smith' -> 'bsmith', 'ben7.smith' -> 'bsmith7'
#'   - Hyphenated surnames: 'ben.smith-jones' -> 'bsmith-jones'
#'   - Hyphenated forenames: 'mary-jane.watson' -> 'mwatson'
#'   - Apostrophes: "siobhan.o'connor" -> 'soconnor'
#'
#' @param email Character scalar.
#' @return Character scalar or NULL if format cannot be parsed.
#' @export
utils_convert_email_to_ad_token <- function(email) {
  if (is.null(email) || !nzchar(email) || !grepl("@", email, fixed = TRUE)) {
    return(NULL)
  }

  local_part <- strsplit(email, "@")[[1]][1]
  local_part <- gsub("['`]", "", local_part)

  pattern <- "^([a-z][a-z0-9_-]*?)([0-9]*)\\.([a-z0-9_-]+)$"

  if (grepl(pattern, local_part, perl = TRUE)) {
    first_char <- substr(sub(pattern, "\\1", local_part, perl = TRUE), 1, 1)
    digits <- sub(pattern, "\\2", local_part, perl = TRUE)
    surname <- sub(pattern, "\\3", local_part, perl = TRUE)

    return(paste0(first_char, surname, digits))
  }

  NULL
}

#' Resolve or Provision User Identity from Session Token
#'
#' Resolves an authenticated user token into an internal integer user_id:
#' 1. Looks up existing profile by canonical email.
#' 2. Evaluates explicit aliases or AD heuristics (e.g., bsmith7) and links email if matched.
#' 3. Just-in-time provisions new profiles if neither exists.
#'
#' @param login_token Character scalar. Authenticated email address or token.
#' @return Integer scalar. Canonical primary key [user_id].
#' @export
utils_resolve_user <- function(login_token) {
  log_event("Starting utils_resolve_user")
  on.exit(log_event("Finished utils_resolve_user"), add = TRUE)

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

  login_email <- tolower(trimws(login_token))

  # --------------------------------------------------------------------------
  # 1. Check if user already exists by Email
  # --------------------------------------------------------------------------
  log_event(paste0("Checking user table for existing email: ", login_email))
  email_profile <- db_get_user_by_email(login_email)

  if (nrow(email_profile) > 0) {
    target_uid <- as.integer(email_profile$user_id[1])
    log_event(paste0("Found existing record by email. user_id: ", target_uid))

    # =========================================================================
    # TODO: Downstream verified email login tasks (claims sync, session auditing)
    # =========================================================================

    return(target_uid)
  }

  # --------------------------------------------------------------------------
  # 2. Check Alias table or AD Username heuristic (e.g. bsmith7)
  # --------------------------------------------------------------------------
  log_event("Email not registered. Evaluating alias and AD heuristics.")
  ad_token_guess <- utils_convert_email_to_ad_token(login_email)

  if (!is.null(ad_token_guess)) {
    log_event(paste0("Derived AD username heuristic: ", ad_token_guess))
    ad_profile <- db_get_user_by_username(ad_token_guess)

    if (nrow(ad_profile) > 0) {
      target_uid <- as.integer(ad_profile$user_id[1])
      log_event(paste0(
        "Matched legacy AD record. Linking email to user_id: ",
        target_uid
      ))

      db_update_user_email(user_id = target_uid, email = login_email)

      # =======================================================================
      # TODO: Downstream legacy AD migration tasks
      # =======================================================================

      return(target_uid)
    }
  }

  # --------------------------------------------------------------------------
  # 3. Neither exists: Just-in-Time Provisioning
  # --------------------------------------------------------------------------
  log_event(paste0(
    "No existing account found. JIT provisioning user for: ",
    login_email
  ))

  default_username <- if (!is.null(ad_token_guess)) {
    ad_token_guess
  } else {
    strsplit(login_email, "@")[[1]][1]
  }

  new_uid <- db_insert_new_user(
    username = default_username,
    email = login_email
  )
  log_event(paste0("JIT Provisioning complete. New user_id: ", new_uid))

  # =========================================================================
  # TODO: Downstream new account tasks (default permissions/notifications)
  # =========================================================================

  return(new_uid)
}

#' Get Current Portal Username Token
#'
#' Retrieves identity from Posit Connect headers, config override, or fallback.
#'
#' @param session Optional Shiny session object.
#' @param fallback Character scalar. Defaults to `"Guest"`.
#' @return Character scalar representing the active identity token.
#' @export
utils_get_user <- function(session = NULL, fallback = "Guest") {
  log_event("Starting utils_get_user session resolution")

  if (is.null(session)) {
    session <- tryCatch(shiny::getDefaultReactiveDomain(), error = function(e) {
      NULL
    })
  }

  if (!is.null(session) && !is.null(session$user) && nzchar(session$user)) {
    return(session$user)
  }

  emulate_user <- tryCatch(config::get("emulate_user"), error = function(e) {
    NULL
  })
  if (!is.null(emulate_user) && nzchar(emulate_user)) {
    return(emulate_user)
  }

  fallback
}

#' Retrieve App-Specific User Roles
#'
#' @param user_id Integer scalar. Primary key identifier.
#' @return Character vector of active roles.
#' @export
utils_get_user_roles <- function(user_id) {
  log_event("Starting utils_get_user_roles")
  on.exit(log_event("Finished utils_get_user_roles"), add = TRUE)

  if (is.null(user_id) || identical(as.integer(user_id), 1L)) {
    return(character(0))
  }

  app_id <- utils_get_app_id()
  db_get_user_roles_by_app(user_id = user_id, app_id = app_id)
}

#' Check Session Authorization Permission Flag
#'
#' @param user_id Integer scalar. Resolved user profile identifier.
#' @param permission_name Character scalar. Feature flag string.
#' @return Logical scalar. TRUE if authorized, FALSE otherwise.
#' @export
utils_user_has_permission <- function(user_id, permission_name) {
  log_event("Starting utils_user_has_permission check")
  on.exit(log_event("Finished utils_user_has_permission check"), add = TRUE)

  if (is.null(user_id) || identical(as.integer(user_id), 1L)) {
    return(FALSE)
  }

  app_id <- utils_get_app_id()
  permissions <- db_get_user_permissions(user_id = user_id, app_id = app_id)

  permission_name %in% permissions
}

#' Record a User Login Event (Universal)
#'
#' Orchestrates resolving the user_id and recording the initial page load event.
#'
#' @param user Character scalar. Raw identity token from session headers.
#' @return Integer scalar. Canonical primary key [user_id].
#' @export
utils_record_login <- function(user = "Guest") {
  log_event("Starting utils_record_login orchestrator")
  on.exit(log_event("Finished utils_record_login orchestrator"), add = TRUE)

  target_user_id <- utils_resolve_user(user)

  db_insert_app_analytics(
    user_id = target_user_id,
    app_id = utils_get_app_id(),
    env_id = utils_resolve_env_id(),
    page_name = "Home page",
    action_type = "Load",
    action_sub_type = "Initial Load"
  )

  target_user_id
}
