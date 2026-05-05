#' User access management UI
#'
#' Defines the user interface for the user and role administration module.
#' This UI presents a table of application users and supports role management
#' actions via server-side logic. The main components include a heading and
#' a DataTable output for displaying users.
#'
#' @param id Character string; Shiny module namespace identifier.
#'
#' @return A Shiny \code{tagList} containing the UI elements for the
#'   user administration module.
#'
#' @details
#' This UI is intended to be paired with the corresponding server module
#' \code{server_portal_user_admin()}. The user table is rendered using
#' \code{DTOutput()}, and role changes are handled via a modal dialog
#' triggered by server-side observers.
#'
#' @seealso \code{\link{NS}}, \code{\link[DT]{DTOutput}}
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
