#' Server-side logic for the user administration module
#'
#' This Shiny module provides server logic for administering application users.
#' It displays a table of users associated with the current application,
#' allows an administrator to select a user, and presents a modal dialog
#' to update that user's role. Changes are persisted to the database and
#' the user table is refreshed after each update.
#'
#' The module:
#' \itemize{
#'   \item Retrieves application configuration and current user details
#'   \item Fetches application users from the database
#'   \item Renders a selectable DataTable of users
#'   \item Opens a role-edit modal on row selection
#'   \item Applies role changes and reloads user data
#' }
#'
#' @param id Character string; Shiny module namespace identifier.
#'
#' @return None. This function is called for its side effects
#'   (registering observers and outputs within a Shiny application).
#'
#' @details
#' This module relies on several helper functions and database utilities
#' being available in the calling environment, including:
#' \itemize{
#'   \item \code{get_config()}
#'   \item \code{get_user()}
#'   \item \code{sql_manager()}
#'   \item \code{get_user_id()}
#'   \item \code{db_get_app_users()}
#'   \item \code{db_update_user_role()}
#'   \item \code{ui_role_edit_modal()}
#' }
#'
#' It also assumes that modal UI inputs such as \code{apply_role_change},
#' \code{selected_user_id}, and \code{selected_role_id} are correctly
#' namespaced within the corresponding UI module.
#'
#' @seealso \code{\link{moduleServer}}, \code{\link[DT]{renderDT}}
#'
#' @export

server_portal_user_admin <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    conf <- get_config()
    app_id <- conf$app_details$app_id

    username <- get_user(session = session, fallback = "guest")
    conn <- sql_manager("dit")

    a_user_id <- get_user_id(conn, username)

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
        assigned_by = a_user_id
      )

      removeModal()
      users_data(db_get_app_users(conn, app_id))
    })
  })
}
