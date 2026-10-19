# ==============================================================================
# R/fct-timeline.R
# Timeline business logic, orchestrator, badge styling, export, and UI modules
# ==============================================================================

#' Orchestrate Entity Timeline Retrieval
#'
#' @description
#' Evaluates the entity type, queries the corresponding database function,
#' and enforces sorting from latest to oldest (top to bottom).
#'
#' @param type Character scalar. 'School', 'Trust', 'Diocese', or 'LA'. Case-insensitive.
#' @param id Identifier for the entity (URN, trust_ref, diocese name, or LA name).
#'
#' @return A data frame containing ordered timeline records (latest to oldest).
#' @export
#'
#' @examples
#' \dontrun{
#' get_timeline_data(type = "School", id = 100384)
#' get_timeline_data(type = "Trust", id = "TR01234")
#' }
get_timeline_data <- function(type, id) {
  if (
    missing(type) || is.null(type) || !is.character(type) || length(type) != 1
  ) {
    stop(
      "`type` must be a single string: 'School', 'Trust', 'Diocese', or 'LA'."
    )
  }
  if (missing(id) || is.null(id) || length(id) != 1 || is.na(id)) {
    stop("`id` must be provided as a non-null scalar value.")
  }

  normalized_type <- tolower(trimws(type))

  df <- switch(
    normalized_type,
    "school" = db_get_school_timeline(urn = id),
    "trust" = db_get_trust_timeline(trust_ref = as.character(id)),
    "diocese" = db_get_diocese_timeline(diocese = as.character(id)),
    "la" = db_get_la_timeline(la = as.character(id)),
    stop(glue::glue(
      "Unrecognized type: '{type}'. Expected 'School', 'Trust', 'Diocese', or 'LA'."
    ))
  )

  if (nrow(df) == 0) {
    return(df)
  }

  df |>
    dplyr::arrange(dplyr::desc(record_date))
}

#' Format Timeline Card Content with Distinct Type Colours
#'
#' @description
#' Formats raw event and inspection fields into display-ready items,
#' styling badges by `type` and ordering chronologically descending (latest first).
#'
#' @param df Data frame returned by \code{get_timeline_data}.
#'
#' @return Formatted data frame ready for UI rendering.
#' @export
build_timeline_cards <- function(df) {
  if (is.null(df) || nrow(df) == 0) {
    return(data.frame())
  }

  # Defensively check whether school_name was linked or present
  has_school_name <- "school_name" %in% names(df)

  df |>
    dplyr::mutate(
      display_title = if (has_school_name) {
        dplyr::case_when(
          !is.na(.data$school_name) & .data$school_name != "" ~ glue::glue(
            "{.data$school_name} - {.data$category}"
          ),
          TRUE ~ as.character(.data$category)
        )
      } else {
        as.character(.data$category)
      },
      badge_colour = dplyr::case_when(
        tolower(trimws(.data$type)) == "rise universal hubs" ~ "teal",
        tolower(trimws(.data$type)) == "significant change" ~ "blue",
        tolower(trimws(.data$type)) == "dao" ~ "purple",
        tolower(trimws(.data$type)) == "warning notice" ~ "red",
        tolower(trimws(.data$type)) == "inspection" ~ "orange",
        tolower(trimws(.data$type)) == "rise universal event" ~ "green",
        TRUE ~ "grey"
      )
    ) |>
    dplyr::arrange(dplyr::desc(.data$record_date))
}

#' Export Timeline Data to CSV
#'
#' @description
#' Cleans, sorts, and writes timeline records directly to a CSV file.
#' Drops UI-internal rendering fields and sorts chronologically from latest to oldest.
#'
#' @param df Data frame returned by \code{get_timeline_data} or \code{build_timeline_cards}.
#' @param file Target path string where the CSV should be saved.
#'
#' @return The file path invisibly.
#' @export
export_timeline_data <- function(df, file) {
  if (is.null(df) || nrow(df) == 0) {
    stop("No timeline records available to download.")
  }

  export_df <- df |>
    dplyr::select(-dplyr::any_of(c("badge_colour", "display_title", "rn")))

  if ("record_date" %in% names(export_df)) {
    export_df <- export_df |>
      dplyr::arrange(dplyr::desc(record_date))
  }

  utils::write.csv(export_df, file = file, row.names = FALSE, na = "")

  invisible(file)
}

#' Render Entity Timeline Card Wrapper UI
#'
#' @param id Shiny namespace identifier.
#'
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
#' @param cards Data frame returned by \code{build_timeline_cards}.
#'
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
      is_inspection <- tolower(trimws(row$type)) == "inspection"

      shiny::tags$li(
        style = "border-left: 4px solid #1d70b8; padding-left: 15px; margin-bottom: 25px;",

        # Header Row: Date | Type Badge | Status Badge
        shiny::tags$div(
          style = "display: flex; align-items: center; flex-wrap: wrap; gap: 8px;",
          shiny::tags$strong(
            style = "font-size: 16px; margin-right: 4px;",
            format(as.Date(row$record_date), "%d %B %Y")
          ),
          shiny::tags$span(
            class = paste0("govuk-tag govuk-tag--", row$badge_colour),
            row$type
          ),
          if (!is.na(row$status) && nzchar(trimws(row$status))) {
            shiny::tags$span(
              class = "govuk-tag govuk-tag--grey",
              row$status
            )
          }
        ),

        # Title / Category
        shiny::tags$div(
          class = "govuk-body",
          style = "margin: 6px 0 2px 0; font-weight: 600;",
          row$display_title
        ),

        # Lead Meta Information
        if (!is.na(row$lead) && nzchar(trimws(row$lead))) {
          shiny::tags$div(
            class = "govuk-hint",
            style = "margin-bottom: 4px; font-size: 14px;",
            glue::glue("Lead: {row$lead}")
          )
        },

        # Reinstated Ofsted Details (if Inspection)
        if (is_inspection && !is.na(row$urn)) {
          render_ofsted_inspection_card(
            urn = row$urn,
            publication_date = row$record_date
          )
        },

        # Link to Document / External URI
        if (!is.na(row$uri) && nzchar(row$uri)) {
          shiny::tags$div(
            style = "margin-top: 6px;",
            shiny::tags$a(
              class = "govuk-link",
              href = row$uri,
              target = "_blank",
              rel = "noopener noreferrer",
              "View Document"
            )
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
#'
#' @return Shiny module server instance.
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
