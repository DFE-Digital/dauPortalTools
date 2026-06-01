#' Server Logic for User Administration Module
#'
#' Provides server-side logic for a Shiny module that manages application users.
#' The module displays users associated with the current application and allows
#' administrators to update user roles via a modal interaction.
#'
#' @param id Character scalar. Shiny module namespace identifier.
#'
#' @details
#' The module performs the following operations:
#' \itemize{
#'   \item Retrieves application configuration and the current user
#'   \item Loads user and role data via [db_get_app_users()]
#'   \item Renders a selectable user table using `DT`
#'   \item Opens a role-edit modal upon row interaction
#'   \item Persists role updates using [db_update_user_role()]
#'   \item Refreshes the user dataset after updates
#' }
#'
#' @section Dependencies:
#' This module expects the following functions to be available:
#' \itemize{
#'   \item [get_config()]
#'   \item [get_user()]
#'   \item [get_user_id()]
#'   \item [sql_manager()]
#'   \item [db_get_app_users()]
#'   \item [db_update_user_role()]
#'   \item [ui_role_edit_modal()]
#' }
#'
#' @return Invisibly returns `NULL`, called for its side effects.
#'
#' @seealso [moduleServer()], [DT::renderDT()]
#' @export
server_portal_user_admin <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    conf <- get_config()
    app_id <- conf$app_details$app_id

    username <- get_user(session = session, fallback = "guest")
    a_user_id <- get_user_id(username)

    users_data <- reactiveVal(db_get_app_users())

    active_selected_user <- reactiveVal(NULL)

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

      active_selected_user(selected)

      showModal(ui_role_edit_modal(ns, selected))
    })

    observeEvent(input$apply_role_change, {
      req(active_selected_user(), input$selected_role_id)

      user_info <- active_selected_user()

      db_update_user_role(
        user_id = user_info$user_id,
        role_id = input$selected_role_id,
        app_id = app_id,
        assigned_by = a_user_id
      )

      removeModal()

      active_selected_user(NULL)
      users_data(db_get_app_users())
    })
  })
}
