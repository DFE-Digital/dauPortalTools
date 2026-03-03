#' User/Role Management UI
#' @param id Module ID
#' @export
ui_portal_user_admin <- function(id) {
  ns <- NS(id)

  tagList(
    h3("User Access Management"),
    DTOutput(ns("user_table")),
    uiOutput(ns("role_modal_ui"))
  )
}
