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
#'   \item Retrieves application configuration and the current administrator's identity
#'   \item Resolves the administrator's integer [user_id] via [utils_resolve_user()]
#'   \item Loads user and role data via [db_get_app_users()]
#'   \item Renders a selectable user table using `DT`
#'   \item Opens a role-edit modal upon row interaction
#'   \item Persists role updates using [db_update_user_role()]
#'   \item Refreshes the user dataset after updates
#' }
#'
#' @return Invisibly returns `NULL`, called for its side effects.
#' @seealso [shiny::moduleServer()], [DT::renderDT()]
#' @export
server_portal_user_admin <- function(id) {
  log_event(sprintf(
    "Initializing server_portal_user_admin module with id '%s'",
    id
  ))

  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    app_id <- utils_get_app_id()

    # 1. Resolve administrator's canonical user_id
    admin_token <- utils_get_user(session = session, fallback = "Guest")
    admin_user_id <- utils_resolve_user(admin_token)

    log_event(sprintf(
      "User Admin module active. Admin UID: %s for App ID: %s",
      admin_user_id,
      app_id
    ))

    # 2. Reactive user directory dataset scoped to the current app
    users_data <- shiny::reactiveVal(db_get_app_users(app_id = app_id))

    active_selected_user <- shiny::reactiveVal(NULL)

    # 3. Interactive DataTables Directory
    output$user_table <- DT::renderDT({
      df <- users_data()
      shiny::req(df)

      DT::datatable(
        df,
        selection = "single",
        rownames = FALSE,
        options = list(
          pageLength = 20,
          dom = "frtip",
          autoWidth = TRUE
        )
      )
    })

    # 4. Open Role Assignment Modal on Row Selection
    shiny::observeEvent(input$user_table_cell_clicked, {
      click <- input$user_table_cell_clicked
      shiny::req(click$row)

      df <- users_data()
      selected <- df[click$row, , drop = FALSE]

      active_selected_user(selected)

      shiny::showModal(ui_role_edit_modal(ns, selected))
    })

    # 5. Persist Role Modification
    shiny::observeEvent(input$apply_role_change, {
      shiny::req(active_selected_user(), input$selected_role_id)

      user_info <- active_selected_user()
      target_uid <- as.integer(user_info$user_id[1])
      new_role_id <- as.integer(input$selected_role_id)

      log_event(sprintf(
        "Admin UID %s updating role for target UID %s to role_id %s (app_id: %s)",
        admin_user_id,
        target_uid,
        new_role_id,
        app_id
      ))

      db_update_user_role(
        user_id = target_uid,
        role_id = new_role_id,
        app_id = app_id,
        assigned_by = admin_user_id
      )

      shiny::removeModal()
      active_selected_user(NULL)

      # Refresh table
      users_data(db_get_app_users(app_id = app_id))
    })
  })
}
