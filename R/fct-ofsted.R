#' Render Standard Ofsted Inspection Card by URN
#'
#' @description
#' Independent UI component that queries and renders the standard GOV.UK-styled
#' Ofsted inspection judgement panel for any school by URN.
#'
#' @param urn Numeric or character scalar. Unique Reference Number of the school.
#' @param publication_date Optional Date or character. When provided, finds the
#'   inspection report closest to this date; otherwise returns the latest recorded.
#'
#' @return Shiny HTML tag element, or NULL if no inspection judgements exist.
#' @export
#'
#' @examples
#' \dontrun{
#' # Fetch latest inspection:
#' render_ofsted_inspection_card(urn = 100384)
#'
#' # Match an event's publication date:
#' render_ofsted_inspection_card(urn = 100384, publication_date = "2024-03-12")
#' }
render_ofsted_inspection_card <- function(urn, publication_date = NULL) {
  if (missing(urn) || is.null(urn) || is.na(urn)) {
    return(NULL)
  }

  df_ofsted <- db_get_latest_ofsted_inspection(
    urn = urn,
    publication_date = publication_date
  )
  if (is.null(df_ofsted) || nrow(df_ofsted) == 0) {
    return(NULL)
  }

  row <- df_ofsted[1, ]

  shiny::tags$div(
    style = "background-color: #f3f2f1; padding: 12px 16px; margin: 10px 0; border-radius: 4px; border-left: 4px solid #f47738;",
    shiny::tags$div(
      class = "govuk-body-xs",
      style = "margin-bottom: 6px;",
      if (!is.na(row$inspection_date)) {
        shiny::tagList(
          shiny::tags$strong("Inspection Date: "),
          format(as.Date(row$inspection_date), "%d %B %Y")
        )
      },
      if (!is.na(row$publication_date)) {
        shiny::tags$span(
          style = "margin-left: 15px;",
          shiny::tags$strong("Published: "),
          format(as.Date(row$publication_date), "%d %B %Y")
        )
      }
    ),
    shiny::tags$ul(
      class = "govuk-list govuk-list--bullet",
      style = "margin-top: 5px; margin-bottom: 0; font-size: 14px;",
      if (
        "curriculum_and_teaching" %in%
          names(row) &&
          !is.na(row$curriculum_and_teaching)
      ) {
        shiny::tags$li(
          shiny::tags$strong("Quality of Education / Teaching: "),
          row$curriculum_and_teaching
        )
      },
      if (
        "attendance_and_behaviour" %in%
          names(row) &&
          !is.na(row$attendance_and_behaviour)
      ) {
        shiny::tags$li(
          shiny::tags$strong("Behaviour & Attitudes: "),
          row$attendance_and_behaviour
        )
      },
      if (
        "personal_development_and_wellbeing" %in%
          names(row) &&
          !is.na(row$personal_development_and_wellbeing)
      ) {
        shiny::tags$li(
          shiny::tags$strong("Personal Development: "),
          row$personal_development_and_wellbeing
        )
      },
      if (
        "leadership_and_governance" %in%
          names(row) &&
          !is.na(row$leadership_and_governance)
      ) {
        shiny::tags$li(
          shiny::tags$strong("Leadership & Governance: "),
          row$leadership_and_governance
        )
      },
      if (
        "safeguarding_standards" %in%
          names(row) &&
          !is.na(row$safeguarding_standards)
      ) {
        shiny::tags$li(
          shiny::tags$strong("Safeguarding: "),
          row$safeguarding_standards
        )
      }
    )
  )
}
