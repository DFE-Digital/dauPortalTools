#' Timeline Stylesheet (GOV.UK aligned)
#' @keywords internal
.timeline_assets <- function() {
  htmltools::tags$style(shiny::HTML(
    "
    .govuk-timeline {
      position: relative;
      margin: 20px 0;
      padding-left: 24px;
      list-style: none;
      border-left: 3px solid #b1b4b6;
    }
    .govuk-timeline-item {
      position: relative;
      margin-bottom: 30px;
    }
    .govuk-timeline-item::before {
      content: '';
      position: absolute;
      left: -30px;
      top: 3px;
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
    .timeline-ofsted-wrapper {
      margin-top: 14px;
      border-top: 2px solid #b1b4b6;
      padding-top: 12px;
    }
  "
  ))
}

#' Internal sub-renderer for Ofsted outcome card in timeline
#' @keywords internal
.render_timeline_ofsted_panel <- function(row) {
  `%||%` <- function(a, b) {
    if (is.null(a) || length(a) == 0 || is.na(a)) b else a
  }

  # Pill formatter consistent with ui_school_ofsted_history
  pill <- function(value) {
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

  # Safeguarding indicator
  safeguarding <- function(flag) {
    raw <- tolower(trimws(as.character(flag %||% "Not known")))
    state <- if (raw %in% c("met", "yes", "effective", "true", "1")) {
      "met"
    } else if (raw %in% c("not met", "no", "not effective", "false", "0")) {
      "not-met"
    } else {
      "unknown"
    }

    txt <- if (state == "unknown") "Not known" else tools::toTitleCase(state)

    htmltools::tags$div(
      class = paste("ofsted-safeguarding-panel", state),
      role = "group",
      `aria-label` = paste0("Safeguarding standards: ", txt),
      htmltools::tags$div(
        class = "ofsted-safeguarding-title",
        "Safeguarding standards"
      ),
      htmltools::tags$div(
        class = "ofsted-safeguarding-value",
        htmltools::tags$strong(txt)
      )
    )
  }

  # If no joined ofsted record found matching the publication date
  val <- row[["inspection_date"]]
  if (is.null(val) || length(val) == 0 || is.na(val[1])) {
    return(htmltools::tags$div(
      class = "timeline-ofsted-wrapper",
      htmltools::tags$p(
        class = "ofsted-note",
        "No inspection details matched for this publication date."
      )
    ))
  }

  insp_date_str <- tryCatch(
    format(as.Date(row$inspection_date), "%d %B %Y"),
    error = function(e) as.character(row$inspection_date)
  )

  areas <- c(
    "Inclusion" = row$inclusion,
    "Curriculum & teaching" = row$curriculum_and_teaching,
    "Achievement" = row$achievement,
    "Attendance & behaviour" = row$attendance_and_behaviour,
    "Personal development & well-being" = row$personal_development_and_wellbeing,
    "Leadership & governance" = row$leadership_and_governance
  )

  grade_rows <- Map(
    function(k, v) {
      htmltools::tags$tr(
        htmltools::tags$th(scope = "row", k),
        htmltools::tags$td(pill(v))
      )
    },
    names(areas),
    unname(areas)
  )

  htmltools::tags$div(
    class = "timeline-ofsted-wrapper",
    htmltools::tags$div(
      class = "ofsted-top-row",
      htmltools::tags$div(
        class = "ofsted-inspection-date",
        htmltools::tags$div(class = "ofsted-label", "Inspection date"),
        htmltools::tags$div(class = "ofsted-date", insp_date_str)
      ),
      safeguarding(row$safeguarding_standards)
    ),
    htmltools::tags$div(
      class = "ofsted-section",
      htmltools::tags$h4(class = "ofsted-h3", "Inspection grades"),
      htmltools::tags$table(
        class = "ofsted-grades-table",
        htmltools::tags$thead(
          htmltools::tags$tr(
            htmltools::tags$th(scope = "col", "Area"),
            htmltools::tags$th(scope = "col", "Grade")
          )
        ),
        htmltools::tags$tbody(htmltools::tagList(grade_rows))
      )
    )
  )
}

#' School Timeline Module UI
#'
#' @param id Shiny namespace identifier
#' @return A Shiny UI element
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
#' @param id Shiny namespace identifier
#' @param urn Reactive expression or value yielding the school's numeric/character URN.
#' @return Module server instance
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

        date_label <- if (!is.na(row$record_date)) {
          format(as.Date(row$record_date), "%d %B %Y")
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
            if (is_ofsted) .render_timeline_ofsted_panel(row)
          )
        )
      })

      shinyGovstyle::gov_layout(
        htmltools::tags$ul(class = "govuk-timeline", nodes)
      )
    })
  })
}
