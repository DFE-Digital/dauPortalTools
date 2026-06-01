#' Quality Table Module Server (Dashboard View)
#'
#' Orchestrates fetching and filtering the full suite of live quality issues
#' across the application dashboard.
#'
#' @param id Character scalar. Shiny module ID.
#' @param app_id Character scalar. The application identifier.
#' @export
server_quality_wrapper <- function(id, app_id) {
  shiny::moduleServer(id, function(input, output, session) {
    # Hold data separately from UI
    df <- shiny::reactiveVal(NULL)

    # Load data AFTER session initialisation
    shiny::observeEvent(
      TRUE,
      {
        raw_ui <- dauPortalTools::quality_render_live(app_id = app_id)
        df(attr(raw_ui, "data"))
      },
      once = TRUE
    )

    filtered <- shiny::reactive({
      shiny::req(df())
      x <- df()

      if (
        !is.null(input$region_filter) &&
          input$region_filter != "All" &&
          "Region" %in% names(x)
      ) {
        x <- x[x$Region == input$region_filter, , drop = FALSE]
      }

      if (
        !is.null(input$with_rcs_filter) &&
          input$with_rcs_filter != "All" &&
          "With RCS?" %in% names(x)
      ) {
        x <- x[
          x$`With RCS?` == as.numeric(input$with_rcs_filter),
          ,
          drop = FALSE
        ]
      }

      x
    })

    output$filtered_table <- DT::renderDT({
      shiny::req(filtered())
      DT::datatable(
        filtered(),
        escape = FALSE,
        rownames = FALSE,
        options = list(pageLength = 15)
      )
    })
  })
}

#' Quality Issues Table Server for a Specific Record
#'
#' Module server that automatically handles app contexts, fetches quality issues
#' for a single specific record ID, and renders a clean datatable.
#'
#' @param id Character scalar. Shiny module ID.
#' @param record_id Reactive expression or numeric scalar. The active record ID to filter by.
#' @export
server_quality_record_table <- function(id, record_id) {
  shiny::moduleServer(id, function(input, output, session) {
    # Dynamic reactive dataset tied specifically to the record passed in
    record_issues <- shiny::reactive({
      r_id <- if (shiny::is.reactive(record_id)) record_id() else record_id
      shiny::req(r_id)

      # Calls your centralized DB helper function
      dauPortalTools::quality_get_data(record = r_id)
    })

    output$quality_table <- DT::renderDT({
      shiny::req(record_issues())

      DT::datatable(
        record_issues(),
        escape = FALSE,
        rownames = FALSE,
        options = list(
          pageLength = 5,
          dom = 't', # ONLY show the raw table block without empty search fields
          autoWidth = TRUE
        )
      )
    })
  })
}
