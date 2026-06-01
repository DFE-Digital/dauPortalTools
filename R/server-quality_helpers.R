#' Quality Table Module Server
#'
#' @param id Character scalar. Shiny module ID.
#' @param app_id Character scalar. The application identifier.
#' @export

server_quality_wrapper <- function(id, app_id) {
  moduleServer(id, function(input, output, session) {
    # Hold data separately from UI
    df <- reactiveVal(NULL)

    # Load data AFTER session initialisation
    observeEvent(
      TRUE,
      {
        raw_ui <- quality_render_live(app_id = app_id)

        df(attr(raw_ui, "data"))
      },
      once = TRUE
    )

    filtered <- reactive({
      req(df())
      x <- df()

      if (input$region_filter != "All") {
        x <- x[x$Region == input$region_filter, ]
      }

      if (input$with_rcs_filter != "All") {
        x <- x[x$`With RCS?` == as.numeric(input$with_rcs_filter), ]
      }

      x
    })

    output$filtered_table <- DT::renderDT({
      req(filtered())
      datatable(
        filtered(),
        escape = FALSE,
        rownames = FALSE,
        options = list(pageLength = 15)
      )
    })
  })
}
