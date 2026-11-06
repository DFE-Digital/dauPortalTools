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

  has_school_name <- "school_name" %in% names(df)

  # Accommodate title fallback if legacy queries still supply category
  if (!"title" %in% names(df) && "category" %in% names(df)) {
    df$title <- df$category
  }

  df |>
    dplyr::mutate(
      display_title = if (has_school_name) {
        dplyr::case_when(
          !is.na(.data$school_name) & .data$school_name != "" ~ glue::glue(
            "{.data$school_name} - {dplyr::coalesce(.data$title, .data$type)}"
          ),
          TRUE ~ dplyr::coalesce(
            as.character(.data$title),
            as.character(.data$type)
          )
        )
      } else {
        dplyr::coalesce(as.character(.data$title), as.character(.data$type))
      },
      badge_colour = dplyr::case_when(
        tolower(trimws(.data$type)) %in%
          c("rise universal hubs", "universal rise hubs") ~ "teal",
        tolower(trimws(.data$type)) == "significant change" ~ "blue",
        tolower(trimws(.data$type)) == "dao" ~ "purple",
        tolower(trimws(.data$type)) == "warning notice" ~ "red",
        tolower(trimws(.data$type)) %in%
          c(
            "inspection",
            "ofstedinspection",
            "ofsted shj",
            "ofsted sj"
          ) ~ "orange",
        tolower(trimws(.data$type)) %in%
          c("rise universal event", "universal rise event") ~ "green",
        tolower(trimws(.data$type)) %in%
          c("targeted rise", "rise targeted") ~ "plum",
        TRUE ~ "grey"
      )
    ) |>
    dplyr::arrange(dplyr::desc(.data$record_date))
}

#' Export Timeline Data to CSV
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

  get_tag_style <- function(colour_name) {
    switch(
      tolower(colour_name),
      "teal" = "background-color: #28a197; color: #ffffff;",
      "blue" = "background-color: #1d70b8; color: #ffffff;",
      "purple" = "background-color: #4c2c92; color: #ffffff;",
      "red" = "background-color: #d4351c; color: #ffffff;",
      "orange" = "background-color: #f47738; color: #ffffff;",
      "green" = "background-color: #00703c; color: #ffffff;",
      "plum" = "background-color: #6f42c1; color: #ffffff;",
      "background-color: #f3f2f1; color: #0b0c0c;"
    )
  }

  get_border_colour <- function(colour_name) {
    switch(
      tolower(colour_name),
      "teal" = "#28a197",
      "blue" = "#1d70b8",
      "purple" = "#4c2c92",
      "red" = "#d4351c",
      "orange" = "#f47738",
      "green" = "#00703c",
      "plum" = "#6f42c1",
      "#b1b4b6"
    )
  }

  shiny::tags$div(
    style = "display: flex; flex-direction: column; gap: 24px;",
    lapply(seq_len(nrow(cards)), function(i) {
      row <- cards[i, ]
      has_urn <- !is.na(row$urn) && nzchar(trimws(as.character(row$urn)))
      norm_type <- tolower(trimws(as.character(row$type)))

      badge_col <- if (
        "badge_colour" %in% names(row) && !is.na(row$badge_colour)
      ) {
        row$badge_colour
      } else {
        "grey"
      }
      tag_css <- get_tag_style(badge_col)
      border_c <- get_border_colour(badge_col)

      # Determine URL (prioritize uri, fallback to Ofsted portal if inspection-related)
      report_url <- if (
        !is.na(row$uri) && nzchar(trimws(as.character(row$uri)))
      ) {
        trimws(as.character(row$uri))
      } else if (
        norm_type %in%
          c("inspection", "ofstedinspection", "ofsted shj", "ofsted sj") &&
          has_urn
      ) {
        glue::glue("https://reports.ofsted.gov.uk/provider/21/{row$urn}")
      } else {
        NULL
      }

      # Safe title fallback
      item_title <- if (
        "title" %in%
          names(row) &&
          !is.na(row$title) &&
          nzchar(as.character(row$title))
      ) {
        as.character(row$title)
      } else if (
        "category" %in%
          names(row) &&
          !is.na(row$category) &&
          nzchar(as.character(row$category))
      ) {
        as.character(row$category)
      } else {
        row$type
      }

      shiny::tags$div(
        class = "timeline-event-card",

        # 1. Top Meta Header
        shiny::tags$div(
          style = "display: flex; align-items: center; flex-wrap: wrap; gap: 10px; margin-bottom: 8px;",
          shiny::tags$span(
            style = "font-size: 16px; font-weight: 500; color: #0b0c0c;",
            format(as.Date(row$record_date), "%d %B %Y")
          ),

          shiny::tags$span(
            style = paste0(
              "display: inline-block; padding: 4px 10px; border-radius: 3px; ",
              "font-size: 14px; font-weight: 700; text-transform: uppercase; letter-spacing: 0.5px; ",
              tag_css
            ),
            row$type
          ),

          if (has_urn) {
            shiny::tags$span(
              style = paste(
                "border: 1px solid #1d70b8; background: #f0f4f9; color: #1d70b8;",
                "padding: 2px 8px; font-size: 13px; font-weight: 600; border-radius: 2px;"
              ),
              glue::glue("URN: {row$urn}")
            )
          },

          if (!is.na(row$lead) && nzchar(trimws(as.character(row$lead)))) {
            lead_label <- if (
              "lead_name" %in%
                names(row) &&
                !is.na(row$lead_name) &&
                nzchar(row$lead_name)
            ) {
              row$lead_name
            } else {
              glue::glue("Lead #{row$lead}")
            }
            shiny::tags$span(
              style = "border: 1px solid #b1b4b6; background: #ffffff; color: #0b0c0c; padding: 2px 8px; font-size: 13px; border-radius: 2px;",
              lead_label
            )
          },

          if (!is.na(row$status) && nzchar(trimws(as.character(row$status)))) {
            shiny::tags$span(
              style = "border: 1px solid #b1b4b6; background: #ffffff; color: #0b0c0c; padding: 2px 8px; font-size: 13px; border-radius: 2px;",
              row$status
            )
          }
        ),

        # 2. Outer Container Box with colored left-accent border
        shiny::tags$div(
          style = paste0(
            "border: 1px solid #b1b4b6; border-left: 6px solid ",
            border_c,
            "; ",
            "border-radius: 4px; padding: 16px; background-color: #ffffff;"
          ),

          # Item Title / Headline
          shiny::tags$div(
            style = "font-size: 16px; font-weight: 600; margin-bottom: 12px;",
            item_title
          ),

          # View Document Link
          shiny::tags$div(
            style = "margin-bottom: 12px;",
            if (!is.null(report_url)) {
              shiny::tags$a(
                class = "govuk-link",
                href = report_url,
                target = "_blank",
                rel = "noopener noreferrer",
                style = "font-size: 14px; text-decoration: underline;",
                "View document / external reference"
              )
            } else {
              shiny::tags$span(
                style = "color: #505a5f; font-size: 14px;",
                "No external document reference"
              )
            }
          ),

          # 3. Scorecard Box (Modern Inspection / Ofsted SHJ / Ofsted SJ)
          if (has_urn) {
            if (norm_type %in% c("inspection", "ofstedinspection")) {
              render_ofsted_inspection_card(
                urn = row$urn,
                publication_date = row$record_date
              )
            } else if (norm_type == "ofsted shj") {
              render_ofsted_shj_card(
                urn = row$urn,
                publication_date = row$record_date
              )
            } else if (norm_type == "ofsted sj") {
              render_ofsted_sj_card(
                urn = row$urn,
                publication_date = row$record_date
              )
            }
          }
        )
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
