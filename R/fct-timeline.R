#' Render Entity Timeline Card Wrapper UI
#'
#' @param id Shiny namespace identifier.
#' @return A bslib card containing the header, CSV download button, and items list.
#' @export
render_timeline_card <- function(id) {
  ns <- shiny::NS(id)

  bslib::card(
    bslib::card_header(
      class = "d-flex justify-content-between align-items-center",
      shiny::tags$h3(
        class = "govuk-heading-m mb-0",
        "Entity Activity & Inspection Timeline"
      ),
      shiny::downloadButton(
        outputId = ns("download_csv"),
        label = "Download CSV",
        class = "govuk-button govuk-button--secondary mb-0"
      )
    ),
    shiny::uiOutput(ns("timeline_items"))
  )
}

#' Generate Timeline Item HTML Elements
#'
#' @param cards Data frame returned by `build_timeline_cards()`.
#' @return Shiny HTML tags representing the ordered timeline entries.
#' @export
render_timeline_items <- function(cards) {
  if (is.null(cards) || nrow(cards) == 0) {
    return(shiny::tags$p(
      class = "govuk-body",
      "No timeline activity records found for this entity."
    ))
  }

  shiny::tags$ul(
    class = "govuk-list",
    lapply(seq_len(nrow(cards)), function(i) {
      row <- cards[i, ]
      shiny::tags$li(
        style = "border-left: 4px solid #b1b4b6; padding-left: 15px; margin-bottom: 20px;",
        shiny::tags$div(
          shiny::tags$strong(format(as.Date(row$record_date), "%d %B %Y")),
          shiny::tags$span(
            class = paste0("govuk-tag govuk-tag--", row$badge_colour),
            style = "margin-left: 10px;",
            row$type
          )
        ),
        shiny::tags$div(
          class = "govuk-body-s",
          style = "margin-top: 5px;",
          row$display_title
        ),
        if (!is.na(row$uri) && nzchar(row$uri)) {
          shiny::tags$a(
            class = "govuk-link",
            href = row$uri,
            target = "_blank",
            rel = "noopener noreferrer",
            "View Document"
          )
        }
      )
    })
  )
}

#' Self-Contained Timeline Server Logic
#'
#' @param id Shiny namespace identifier.
#' @param target Reactive expression returning a list(type = ..., id = ...).
#' @export
server_timeline_module <- function(id, target) {
  shiny::moduleServer(id, function(input, output, session) {
    r_raw <- shiny::reactive({
      t <- target()
      shiny::req(t$type, t$id)
      get_timeline_data(type = t$type, id = t$id)
    })

    output$timeline_items <- shiny::renderUI({
      cards <- build_timeline_cards(r_raw())
      render_timeline_items(cards)
    })

    output$download_csv <- shiny::downloadHandler(
      filename = function() {
        t <- target()
        clean_id <- gsub("[^A-Za-z0-9_-]", "_", as.character(t$id))
        glue::glue(
          "{tolower(t$type)}_{clean_id}_timeline_{format(Sys.Date(), '%Y%m%d')}.csv"
        )
      },
      content = function(file) {
        export_timeline_data(r_raw(), file = file)
      }
    )
  })
}
