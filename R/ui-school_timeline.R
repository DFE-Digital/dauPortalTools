#' Timeline Stylesheet & Visual Assets
#'
#' @description
#' Provides full CSS styling for the GOV.UK vertical timeline,
#' new/legacy Ofsted grade pills, category of concern/safeguarding alert banners,
#' and the responsive sub-judgement card grid.
#'
#' @keywords internal
.timeline_assets <- function() {
  htmltools::tags$style(shiny::HTML(
    "
    /* ==========================================================================
       GOV.UK Vertical Timeline
       ========================================================================== */
    .govuk-timeline {
      position: relative;
      margin: 20px 0;
      padding-left: 24px;
      list-style: none;
      border-left: 3px solid #b1b4b6;
    }
    .govuk-timeline-item {
      position: relative;
      margin-bottom: 28px;
    }
    .govuk-timeline-item::before {
      content: '';
      position: absolute;
      left: -30px;
      top: 4px;
      width: 10px;
      height: 10px;
      border-radius: 50%;
      background: #1d70b8;
      border: 3px solid #ffffff;
      box-shadow: 0 0 0 2px #1d70b8;
    }
    .govuk-timeline-item.lead-ofsted::before {
      background: #00703c;
      box-shadow: 0 0 0 2px #00703c;
    }
    .govuk-timeline-header {
      display: flex;
      flex-wrap: wrap;
      align-items: baseline;
      gap: 8px 14px;
      margin-bottom: 6px;
    }
    .govuk-timeline-date {
      font-size: 0.95rem;
      font-weight: 700;
      color: #505a5f;
    }
    .govuk-timeline-title {
      font-size: 1.15rem;
      font-weight: 700;
      color: #0b0c0c;
      margin: 0;
    }
    .govuk-timeline-tag {
      display: inline-block;
      padding: 2px 8px;
      font-size: 0.85rem;
      font-weight: 600;
      border-radius: 2px;
      background: #f3f2f1;
      color: #0b0c0c;
      border: 1px solid #b1b4b6;
    }
    .govuk-timeline-body {
      background: #ffffff;
      border: 1px solid #b1b4b6;
      border-radius: 3px;
      padding: 14px 16px;
      margin-top: 6px;
    }

    /* ==========================================================================
       Ofsted Slim Card & Top Alert Banners
       ========================================================================== */
    .ofsted-slim-container {
      background: #ffffff;
      border: 1px solid #b1b4b6;
      border-radius: 4px;
      overflow: hidden;
      margin-top: 12px;
    }
    .ofsted-alert-banner {
      background-color: #d4351c;
      color: #ffffff;
      padding: 8px 14px;
      font-size: 0.95rem;
      line-height: 1.4;
      border-bottom: 1px solid #ffffff;
    }
    .ofsted-alert-banner strong {
      font-weight: 700;
      text-transform: uppercase;
      letter-spacing: 0.5px;
    }
    .ofsted-slim-header {
      display: flex;
      justify-content: space-between;
      align-items: center;
      padding: 10px 14px;
      background-color: #f3f2f1;
      border-bottom: 1px solid #b1b4b6;
      font-size: 0.95rem;
    }
    .ofsted-slim-date-label {
      color: #505a5f;
    }
    .ofsted-safeguarding-met-tag {
      background: #00703c;
      color: #ffffff;
      font-weight: 700;
      font-size: 0.8rem;
      padding: 2px 8px;
      border-radius: 2px;
    }

    /* Sub-judgements Box Grid */
    .ofsted-judgements-grid {
      display: grid;
      grid-template-columns: repeat(auto-fit, minmax(210px, 1fr));
      gap: 10px;
      padding: 12px;
      background: #ffffff;
    }
    .ofsted-subjudgement-card {
      background: #f8f8f8;
      border: 1px solid #d8d8d8;
      border-radius: 3px;
      padding: 8px 10px;
      display: flex;
      flex-direction: column;
      justify-content: space-between;
      min-height: 68px;
    }
    .ofsted-card-label {
      font-size: 0.82rem;
      font-weight: 600;
      color: #505a5f;
      margin-bottom: 6px;
      line-height: 1.2;
    }
    .ofsted-card-value {
      margin-top: auto;
    }

    /* ==========================================================================
       Ofsted Grade Pills
       ========================================================================== */
    .ofsted-pill {
      display: inline-block;
      padding: 2px 10px;
      border-radius: 999px;
      font-weight: 700;
      font-size: 0.85rem;
      line-height: 1.6;
      border: 1px solid transparent;
      text-align: center;
    }
    .ofsted-exceptional     { background: #00703c; color: #fff; }
    .ofsted-strong          { background: #28a197; color: #fff; }
    .ofsted-expected        { background: #1d70b8; color: #fff; }
    .ofsted-needs-attention { background: #ffdd00; color: #0b0c0c; }
    .ofsted-urgent          { background: #d4351c; color: #fff; }
    .ofsted-unknown         { background: #f3f2f1; color: #0b0c0c; border-color: #b1b4b6; }
    .ofsted-note            { color: #505a5f; margin: 6px 0 0 0; font-size: 0.9rem; }
  "
  ))
}

#' Internal sub-renderer for slim Ofsted outcome card in timeline
#' @keywords internal
.render_slim_ofsted_panel <- function(row) {
  `%||%` <- function(a, b) {
    if (is.null(a) || length(a) == 0 || is.na(a)) b else a
  }

  # Pill badge helper
  grade_pill <- function(value) {
    val <- trimws(as.character(value %||% "Unknown"))
    cls <- switch(
      tolower(val),
      "exceptional" = "ofsted-exceptional",
      "strong standard" = "ofsted-strong",
      "expected standard" = "ofsted-expected",
      "needs attention" = "ofsted-needs-attention",
      "urgent improvement" = "ofsted-urgent",
      "ofsted-unknown"
    )
    htmltools::tags$span(
      class = paste("ofsted-pill", cls),
      role = "text",
      `aria-label` = paste0("Grade: ", val),
      val
    )
  }

  # Safe check on inspection_date
  val <- row[["inspection_date"]]
  if (is.null(val) || length(val) == 0 || is.na(val[1])) {
    return(htmltools::tags$div(
      class = "ofsted-slim-container",
      htmltools::tags$div(
        style = "padding: 10px 14px;",
        htmltools::tags$p(
          class = "ofsted-note",
          "No inspection details matched for this publication date."
        )
      )
    ))
  }

  # Evaluate Safeguarding flag
  safeguarding_raw <- tolower(trimws(as.character(
    row$safeguarding_standards %||% ""
  )))
  is_sg_not_met <- safeguarding_raw %in%
    c("not met", "no", "not effective", "false", "0")

  # Evaluate Category of Concern
  coc_raw <- row$coc %||% NA
  has_coc <- !is.na(coc_raw) &&
    nzchar(trimws(as.character(coc_raw))) &&
    tolower(trimws(as.character(coc_raw))) != "none"
  coc_text <- trimws(as.character(coc_raw))

  insp_date_str <- tryCatch(
    format(as.Date(val[1]), "%d %B %Y"),
    error = function(e) as.character(val[1])
  )

  # Conditional Warning Banners
  banners <- list()
  if (is_sg_not_met) {
    banners <- append(
      banners,
      list(
        htmltools::tags$div(
          class = "ofsted-alert-banner",
          htmltools::tags$strong("Safeguarding: Not Met"),
          htmltools::tags$span(
            " — Safeguarding requirements do not meet the expected standard."
          )
        )
      )
    )
  }

  if (has_coc) {
    banners <- append(
      banners,
      list(
        htmltools::tags$div(
          class = "ofsted-alert-banner",
          htmltools::tags$strong("Category of Concern: "),
          htmltools::tags$span(coc_text)
        )
      )
    )
  }

  # Sub-judgement cards
  judgements <- list(
    "Inclusion" = row$inclusion,
    "Curriculum & Teaching" = row$curriculum_and_teaching,
    "Achievement" = row$achievement,
    "Attendance & Behaviour" = row$attendance_and_behaviour,
    "Personal Development & Well-being" = row$personal_development_and_wellbeing,
    "Leadership & Governance" = row$leadership_and_governance
  )

  cards <- lapply(names(judgements), function(area_name) {
    htmltools::tags$div(
      class = "ofsted-subjudgement-card",
      htmltools::tags$div(class = "ofsted-card-label", area_name),
      htmltools::tags$div(
        class = "ofsted-card-value",
        grade_pill(judgements[[area_name]])
      )
    )
  })

  htmltools::tags$div(
    class = "ofsted-slim-container",
    if (length(banners) > 0) htmltools::tagList(banners),
    htmltools::tags$div(
      class = "ofsted-slim-header",
      htmltools::tags$div(
        htmltools::tags$span(
          class = "ofsted-slim-date-label",
          "Inspection Date: "
        ),
        htmltools::tags$strong(insp_date_str)
      ),
      if (!is_sg_not_met) {
        htmltools::tags$div(
          class = "ofsted-safeguarding-met-tag",
          "Safeguarding: Met"
        )
      }
    ),
    htmltools::tags$div(
      class = "ofsted-judgements-grid",
      cards
    )
  )
}

#' School Timeline Module UI
#'
#' @param id Character scalar. Shiny namespace identifier.
#' @return A Shiny UI element.
#' @export
ui_school_timeline_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    .timeline_assets(),
    shiny::uiOutput(ns("timeline_container"))
  )
}

#' School Timeline Module Server
#'
#' @param id Character scalar. Shiny namespace identifier.
#' @param urn Reactive expression or value yielding the school's numeric/character URN.
#' @return Module server instance.
#' @export
server_school_timeline <- function(id, urn) {
  shiny::moduleServer(id, function(input, output, session) {
    `%||%` <- function(a, b) {
      if (is.null(a) || length(a) == 0 || is.na(a)) b else a
    }

    timeline_df <- shiny::reactive({
      current_urn <- if (shiny::is.reactive(urn)) urn() else urn
      shiny::req(current_urn)
      db_get_school_timeline(current_urn)
    })

    output$timeline_container <- shiny::renderUI({
      data <- timeline_df()

      if (is.null(data) || nrow(data) == 0) {
        return(shinyGovstyle::gov_layout(
          htmltools::tags$p(
            class = "govuk-body",
            "No timeline records found for this school."
          )
        ))
      }

      nodes <- lapply(seq_len(nrow(data)), function(i) {
        row <- data[i, ]
        is_ofsted <- !is.na(row$lead) && tolower(trimws(row$lead)) == "ofsted"
        item_class <- if (is_ofsted) {
          "govuk-timeline-item lead-ofsted"
        } else {
          "govuk-timeline-item"
        }

        date_val <- row[["record_date"]]
        date_label <- if (!is.null(date_val) && !is.na(date_val)) {
          tryCatch(format(as.Date(date_val), "%d %B %Y"), error = function(e) {
            as.character(date_val)
          })
        } else {
          "Undated"
        }

        htmltools::tags$li(
          class = item_class,
          htmltools::tags$div(
            class = "govuk-timeline-header",
            htmltools::tags$span(class = "govuk-timeline-date", date_label),
            htmltools::tags$h3(
              class = "govuk-timeline-title",
              row$type %||% "Record Event"
            ),
            htmltools::tags$span(
              class = "govuk-timeline-tag",
              row$lead %||% "General"
            ),
            if (!is.na(row$status) && row$status != "") {
              htmltools::tags$span(class = "govuk-timeline-tag", row$status)
            }
          ),
          htmltools::tags$div(
            class = "govuk-timeline-body",
            htmltools::tags$p(
              htmltools::tags$strong("Category: "),
              row$category %||% "N/A"
            ),
            if (!is.null(row$uri) && !is.na(row$uri) && row$uri != "") {
              htmltools::tags$p(
                htmltools::tags$a(
                  href = row$uri,
                  target = "_blank",
                  rel = "noopener noreferrer",
                  class = "govuk-link",
                  "View document / external reference"
                )
              )
            },
            if (is_ofsted) .render_slim_ofsted_panel(row)
          )
        )
      })

      shinyGovstyle::gov_layout(
        htmltools::tags$ul(class = "govuk-timeline", nodes)
      )
    })
  })
}
