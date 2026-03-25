#' User/Role Management Server
#'
#' @param id Module ID
#' @export
server_portal_user_admin <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    conf <- get_config()
    app_id <- conf$app_details$app_id

    username <- get_user(session = session, fallback = "guest")

    conn <- sql_manager("dit")

    users_data <- reactiveVal(db_get_app_users(conn, app_id))

    output$user_table <- renderDT({
      datatable(
        users_data(),
        selection = "single",
        rownames = FALSE,
        options = list(pageLength = 20)
      )
    })

    observeEvent(input$user_table_cell_clicked, {
      click <- input$user_table_cell_clicked
      req(click$row)

      df <- users_data()
      selected <- df[click$row, ]
      showModal(ui_role_edit_modal(ns, selected, conn))
    })

    observeEvent(input$apply_role_change, {
      req(input$selected_user_id, input$selected_role_id)

      db_update_user_role(
        conn = conn,
        user_id = input$selected_user_id,
        role_id = input$selected_role_id,
        app_id = app_id,
        assigned_by = username()
      )

      removeModal()
      users_data(db_get_app_users(conn, app_id))
    })
  })
}
