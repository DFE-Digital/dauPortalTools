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
