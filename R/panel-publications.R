# ==============================================================================
# File: R/panel-publications.R
# Purpose: Reusable Shiny module UI and Server for the DAU Publications
#   Directory, featuring filtering by underlying data groups/types.
#
# Functions:
#   - ui_publication_panel()     : Nav panel UI with data type filter bar
#   - server_publication_panel() : Module server handling filtering & grid
#
# Dependencies: shiny, bslib, shinyGovstyle, internal (db_pub_*, render_publication_cards)
# ==============================================================================

#' DAU Publications Directory Panel UI
#'
#' Creates a `bslib::nav_panel` containing the DAU Statistical Publications
#' directory header, a multi-select data group/type filter bar, and the
#' responsive card grid output. Designed to sit inside any `bslib::navset_*`
#' container (such as a user profile menu or a top-level navbar).
#'
#' @param id Character scalar. Module namespace identifier. Defaults to
#'   `"publication_panel"`.
#' @param title Character scalar. Tab label shown in the parent navigation
#'   container. Defaults to `"DAU Publications"`.
#' @param value Character scalar. Tab value identifier for `bslib::nav_select()`.
#'   Defaults to `"publication_panel"`.
#'
#' @examples
#' \dontrun{
#' # 1. Inside a parent bslib::navset_tab() or navset_pill_list() UI:
#' dauPortalTools::ui_publication_panel(
#'   id = "pubs_panel",
#'   title = "DAU Publications",
#'   value = "user_sub_publications"
#' )
#'
#' # 2. Corresponding call in the parent server function:
#' dauPortalTools::server_publication_panel(id = "pubs_panel")
#' }
#'
#' @return A [`bslib::nav_panel`] UI element.
#' @export
ui_publication_panel <- function(
  id = "publication_panel",
  title = "DAU Publications",
  value = "publication_panel"
) {
  ns <- shiny::NS(id)

  bslib::nav_panel(
    title = title,
    value = value,
    shinyGovstyle::heading_text("DAU Publications Directory", size = "l"),
    shiny::p(
      "Browse official statistics, management information releases, and analytical publications. Use the filter below to narrow by underlying data types:"
    ),
    shiny::br(),

    # Filter Bar
    shiny::div(
      class = "p-3 bg-light rounded-3 border mb-4",
      bslib::layout_column_wrap(
        width = 1 / 2,
        shiny::selectizeInput(
          ns("filter_data_types"),
          "Filter by Data Groups / Types:",
          choices = NULL,
          multiple = TRUE,
          options = list(
            placeholder = "Show all data types...",
            plugins = list("remove_button")
          )
        ),
        shiny::div(
          style = "display: flex; align-items: flex-end; padding-bottom: 15px;",
          shiny::actionLink(
            ns("btn_reset_filters"),
            "Clear Filter",
            style = "font-size: 14px; text-decoration: underline;"
          )
        )
      )
    ),

    shiny::uiOutput(ns("pubs_grid_ui"))
  )
}


#' DAU Publications Directory Panel Server
#'
#' Server logic for [ui_publication_panel()]. Populates available data
#' groups from `db_pub_get_data_types()`, filters active publications from
#' `db_pub_get_directory_index()`, and renders the resulting cards via
#' [render_publication_cards()].
#'
#' @param id Character scalar. Module namespace identifier matching
#'   [ui_publication_panel()].
#' @param refresh_trigger Optional reactive or `reactiveVal` that, when
#'   invalidated, triggers a fresh query of the publications directory (e.g.,
#'   after a new publication is created in an admin panel).
#'
#' @examples
#' \dontrun{
#' # Basic usage:
#' dauPortalTools::server_publication_panel(id = "pubs_panel")
#'
#' # With a reactiveVal trigger shared with an Add Publication form:
#' pub_refresh <- shiny::reactiveVal(0)
#' dauPortalTools::server_publication_panel(
#'   id = "pubs_panel",
#'   refresh_trigger = pub_refresh
#' )
#' }
#'
#' @return A `shiny::moduleServer` instance.
#' @export
server_publication_panel <- function(
  id = "publication_panel",
  refresh_trigger = NULL
) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    available_types <- shiny::reactive({
      db_pub_get_data_types()
    })

    shiny::observe({
      types_df <- available_types()
      if (!is.null(types_df) && nrow(types_df) > 0) {
        choices <- stats::setNames(
          as.character(types_df$type_id),
          types_df$type_name
        )
        shiny::updateSelectizeInput(
          session,
          "filter_data_types",
          choices = choices,
          server = TRUE
        )
      }
    })

    shiny::observeEvent(input$btn_reset_filters, {
      shiny::updateSelectizeInput(
        session,
        "filter_data_types",
        selected = character(0)
      )
    })

    pub_directory_index <- shiny::reactive({
      if (!is.null(refresh_trigger)) {
        refresh_trigger()
      }
      db_pub_get_directory_index()
    })

    output$pubs_grid_ui <- shiny::renderUI({
      index_df <- pub_directory_index()

      if (is.null(index_df) || nrow(index_df) == 0) {
        return(shiny::tags$div(
          class = "govuk-inset-text",
          "No publications are currently available."
        ))
      }

      # Filter by selected data types
      selected_types <- input$filter_data_types
      if (length(selected_types) > 0) {
        matches_filter <- vapply(
          index_df$type_ids,
          function(id_str) {
            if (is.null(id_str) || is.na(id_str) || !nzchar(id_str)) {
              return(FALSE)
            }
            any(selected_types %in% strsplit(id_str, "\\|")[[1]])
          },
          logical(1)
        )
        index_df <- index_df[matches_filter, , drop = FALSE]
      }

      if (nrow(index_df) == 0) {
        return(shiny::tags$div(
          class = "govuk-inset-text",
          "No publications match the selected data group filters."
        ))
      }

      render_publication_cards(
        pubs = index_df,
        session = session
      )
    })
  })
}
