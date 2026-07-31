#' Old User Identity Getter (Deprecated)
#'
#' @description This function is deprecated. Please shift your application workflow
#' to track the strict integer \code{user_id} using \code{db_user_create()}.
#'
#' @export
get_user_id <- function(login_token) {
  .Deprecated(
    new = "db_user_create",
    package = "dauPortalTools",
    msg = "get_user_id() is deprecated and utilizes the old 01_AIDT structure. Switch to db_user_create()."
  )
  # Redirect into the new underlying pipeline logic safely
  db_user_create(login_token)
}

#' Old Legacy User Role Lookup (Deprecated)
#'
#' @description Re-routes old token queries using character names into the
#' new normalized integer architecture.
#'
#' @export
get_user_role_legacy <- function(login_token) {
  .Deprecated(
    new = "get_user_role",
    package = "dauPortalTools",
    msg = "get_user_role_legacy() is deprecated. Update your server setup to fetch get_user_role(user_id)."
  )

  # Resolve the string into an INT first, then query roles config
  uid <- db_user_create(login_token)
  get_user_role(uid)
}
