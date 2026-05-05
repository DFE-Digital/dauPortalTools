#' User role edit modal dialog
#'
#' Creates a Shiny modal dialog allowing an administrator to change the role
#' assigned to a selected user. The modal displays the user's name, provides
#' a dropdown of available roles, and includes an action button to apply
#' the selected role change.
#'
#' @param ns A Shiny namespace function, typically created using
#'   \code{session$ns} within a module server function.
#' @param selected A single-row data frame containing the selected user's
#'   details. Must include at least \code{user_id}, \code{username},
#'   and \code{role_id}.
#' @param conn A DBI database connection used to retrieve the list of
#'   available roles.
#'
#' @return A Shiny \code{modalDialog} UI object.
#'
#' @details
#' The user's ID is stored in a hidden text input to allow it to be accessed
#' by the server logic when applying role changes. Available roles are
#' retrieved dynamically from the database and displayed as a named vector
#' of role IDs and role names.
#'
#' This modal is intended to be shown using \code{showModal()} and assumes
#' corresponding server-side observers exist to handle the
#' \code{apply_role_change} action.
#'
#' @seealso \code{\link{modalDialog}}, \code{\link{selectInput}},
#'   \code{\link{actionButton}}
#'
#' @export

ui_role_edit_modal <- function(ns, selected, conn) {
  roles <- db_get_roles(conn)

  modalDialog(
    title = paste0("Change Role for ", selected$username),
    easyClose = TRUE,
    footer = NULL,
    tagList(
      div(
        style = "display:none;",
        textInput(
          ns("selected_user_id"),
          label = NULL,
          value = selected$user_id
        )
      ),

      selectInput(
        ns("selected_role_id"),
        "Role:",
        choices = setNames(roles$role_id, roles$role_name),
        selected = selected$role_id
      ),

      actionButton(ns("apply_role_change"), "Apply", class = "btn-primary")
    )
  )
}
