# ==============================================================================
# R/fct-ofsted.R
# Reusable Ofsted card component matching the original layout
# ==============================================================================

#' Render Standard Ofsted Inspection Card by URN
#'
#' @description
#' Renders the rich GOV.UK-styled Ofsted inspection details block including
#' inspection/publication dates, core judgements, and safeguarding.
#'
#' @param urn Numeric or character scalar. Unique Reference Number of the school.
#' @param publication_date Optional Date or character scalar.
#'
#' @return Shiny HTML tag element, or NULL if no judgements exist.
#' @export
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
    class = "govuk-inset-text",
    style = "margin: 10px 0 15px 0; padding: 12px 16px; background-color: #f8f8f8; border-left: 6px solid #f47738;",

    # Header: Dates
    shiny::tags$div(
      class = "govuk-body-s",
      style = "margin-bottom: 8px; color: #505a5f;",
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

    # Judgement Grid
    shiny::tags$dl(
      class = "govuk-summary-list govuk-summary-list--no-border",
      style = "margin-bottom: 0;",

      if (!is.na(row$curriculum_and_teaching)) {
        shiny::tagList(
          shiny::tags$div(
            class = "govuk-summary-list__row",
            shiny::tags$dt(
              class = "govuk-summary-list__key",
              style = "width: 45%; font-size: 14px;",
              "Quality of Education / Teaching"
            ),
            shiny::tags$dd(
              class = "govuk-summary-list__value",
              style = "font-size: 14px;",
              row$curriculum_and_teaching
            )
          )
        )
      },
      if (!is.na(row$attendance_and_behaviour)) {
        shiny::tagList(
          shiny::tags$div(
            class = "govuk-summary-list__row",
            shiny::tags$dt(
              class = "govuk-summary-list__key",
              style = "width: 45%; font-size: 14px;",
              "Behaviour & Attitudes"
            ),
            shiny::tags$dd(
              class = "govuk-summary-list__value",
              style = "font-size: 14px;",
              row$attendance_and_behaviour
            )
          )
        )
      },
      if (!is.na(row$personal_development_and_wellbeing)) {
        shiny::tagList(
          shiny::tags$div(
            class = "govuk-summary-list__row",
            shiny::tags$dt(
              class = "govuk-summary-list__key",
              style = "width: 45%; font-size: 14px;",
              "Personal Development"
            ),
            shiny::tags$dd(
              class = "govuk-summary-list__value",
              style = "font-size: 14px;",
              row$personal_development_and_wellbeing
            )
          )
        )
      },
      if (!is.na(row$leadership_and_governance)) {
        shiny::tagList(
          shiny::tags$div(
            class = "govuk-summary-list__row",
            shiny::tags$dt(
              class = "govuk-summary-list__key",
              style = "width: 45%; font-size: 14px;",
              "Leadership & Governance"
            ),
            shiny::tags$dd(
              class = "govuk-summary-list__value",
              style = "font-size: 14px;",
              row$leadership_and_governance
            )
          )
        )
      },
      if (!is.na(row$safeguarding_standards)) {
        shiny::tagList(
          shiny::tags$div(
            class = "govuk-summary-list__row",
            shiny::tags$dt(
              class = "govuk-summary-list__key",
              style = "width: 45%; font-size: 14px;",
              "Safeguarding"
            ),
            shiny::tags$dd(
              class = "govuk-summary-list__value",
              style = "font-size: 14px;",
              row$safeguarding_standards
            )
          )
        )
      }
    )
  )
}
