#' User Role Edit Modal Dialog
#'
#' Generates a Shiny modal dialog that allows an administrator to update
#' the role assigned to a selected user.
#'
#' @param ns Namespace function created via `NS()` or `session$ns()` for
#'   proper scoping of input IDs within a Shiny module.
#' @param selected A single-row [`data.frame`] containing user details.
#'   Must include at least:
#'   \itemize{
#'     \item `user_id`
#'     \item `username`
#'     \item `role_id`
#'   }
#'
#' @details
#' Available roles are retrieved dynamically using [db_get_roles()] and
#' presented as a selectable dropdown.
#'
#' The selected user's ID is stored in a hidden input (`selected_user_id`)
#' so it can be accessed by server logic when applying updates.
#'
#' The modal includes:
#' \itemize{
#'   \item A title showing the selected user
#'   \item A dropdown of available roles
#'   \item An action button (`apply_role_change`) to submit the update
#' }
#'
#' This component is intended to be used with [showModal()] and expects
#' corresponding server-side observers to handle the role update logic.
#'
#' @section Side Effects:
#' \itemize{
#'   \item Calls [db_get_roles()] to retrieve role data
#'   \item Generates Shiny UI elements
#' }
#'
#' @return A `shiny::modalDialog` UI object.
#'
#' @seealso [db_get_roles()], [shiny::modalDialog()], [shiny::selectInput()]
#'
#' @export

ui_role_edit_modal <- function(ns, selected) {
  req(nrow(selected) == 1)

  roles <- db_get_roles()

  shiny::modalDialog(
    title = paste0("Change Role for ", selected$username),
    easyClose = TRUE,
    footer = NULL,
    shiny::tagList(
      shiny::tags$input(
        id = ns("selected_user_id"),
        type = "hidden",
        value = selected$user_id
      ),

      shiny::selectInput(
        ns("selected_role_id"),
        "Role:",
        choices = setNames(roles$role_id, roles$role_name),
        selected = selected$role_id
      ),

      shiny::actionButton(
        ns("apply_role_change"),
        "Apply",
        class = "btn-primary"
      )
    )
  )
}
