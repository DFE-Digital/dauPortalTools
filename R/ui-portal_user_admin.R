#' User Administration Module UI
#'
#' Defines the user interface for the user and role administration module.
#' This UI displays a table of application users and supports role management
#' actions via server-side logic.
#'
#' @param id Character scalar. Shiny module namespace identifier.
#'
#' @details
#' The UI consists of:
#' \itemize{
#'   \item A section heading
#'   \item A `DT` table displaying application users
#'   \item A placeholder UI element for rendering the role-edit modal
#' }
#'
#' The table is rendered using [DT::DTOutput()], and role updates are handled
#' via modal dialogs triggered by the corresponding server module.
#'
#' This UI is intended to be used with [server_portal_user_admin()].
#'
#' @section Side Effects:
#' \itemize{
#'   \item Generates Shiny UI components
#'   \item Provides output slots used by server-side logic
#' }
#'
#' @return A `shiny::tagList` containing UI elements for the module.
#'
#' @seealso [server_portal_user_admin()], [DT::DTOutput()]
#'
#' @export

ui_portal_user_admin <- function(id) {
  ns <- NS(id)

  tagList(
    h3("User Access Management"),
    DTOutput(ns("user_table")),
    uiOutput(ns("role_modal_ui"))
  )
}
