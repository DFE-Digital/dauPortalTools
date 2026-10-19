# ==============================================================================
# R/fct-ofsted.R
# Styled Ofsted inspection box with conditional Safeguarding and CoC alerts
# ==============================================================================

#' Internal Helper: Style Judgment Badges
#' @noRd
.ofsted_badge <- function(val) {
  if (is.na(val) || !nzchar(trimws(val))) {
    return(NULL)
  }

  val_clean <- trimws(val)
  val_lower <- tolower(val_clean)

  # Exact colour mapping to match UI image
  style <- if (grepl("expected", val_lower)) {
    "background-color: #1d70b8; color: #ffffff;" # Blue
  } else if (grepl("needs attention", val_lower)) {
    "background-color: #ffdd00; color: #0b0c0c; font-weight: bold;" # Yellow
  } else if (grepl("met", val_lower)) {
    "background-color: #00703c; color: #ffffff; font-weight: bold;" # Dark Green
  } else if (grepl("not met|concern|urgent", val_lower)) {
    "background-color: #d4351c; color: #ffffff; font-weight: bold;" # Red
  } else {
    "background-color: #f3f2f1; color: #0b0c0c;" # Neutral Grey
  }

  shiny::tags$span(
    style = paste0(
      "display: inline-block; padding: 4px 14px; border-radius: 16px; ",
      "font-size: 13px; line-height: 1.2; ",
      style
    ),
    val_clean
  )
}

#' Internal Helper: Render Individual Scorecard Tile
#' @noRd
.ofsted_metric_tile <- function(label, value) {
  if (is.na(value) || !nzchar(trimws(value))) {
    return(NULL)
  }

  shiny::tags$div(
    style = paste(
      "background-color: #f8f8f8;",
      "border: 1px solid #e2e2e2;",
      "border-radius: 4px;",
      "padding: 10px 14px;",
      "min-height: 68px;",
      "display: flex;",
      "flex-direction: column;",
      "justify-content: space-between;"
    ),
    shiny::tags$span(
      style = "font-size: 13px; color: #505a5f; margin-bottom: 6px;",
      label
    ),
    shiny::tags$div(
      .ofsted_badge(value)
    )
  )
}

#' Render Standard Ofsted Inspection Card by URN
#'
#' @param urn Numeric or character scalar. School URN.
#' @param publication_date Optional Date or character scalar.
#'
#' @return Shiny HTML tag element, or NULL if no inspection judgements exist.
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

  # Evaluate Safeguarding status
  has_safeguarding <- "safeguarding_standards" %in%
    names(row) &&
    !is.na(row$safeguarding_standards) &&
    nzchar(trimws(row$safeguarding_standards))
  sg_val <- if (has_safeguarding) trimws(row$safeguarding_standards) else ""
  sg_is_not_met <- grepl("not met", tolower(sg_val))

  sg_bg <- if (sg_is_not_met) "#d4351c" else "#005a30" # Red if Not met, Green if Met

  # Evaluate Category of Concern (coc)
  has_coc <- "coc" %in%
    names(row) &&
    !is.na(row$coc) &&
    nzchar(trimws(row$coc)) &&
    !grepl("^none$|^no category", tolower(trimws(row$coc)))

  shiny::tags$div(
    style = paste(
      "border: 1px solid #b1b4b6;",
      "border-radius: 4px;",
      "background-color: #ffffff;",
      "margin-top: 12px;",
      "overflow: hidden;"
    ),

    # --- Header Bar: Date, CoC Alert, and Safeguarding Badge ---
    shiny::tags$div(
      style = paste(
        "display: flex;",
        "justify-content: space-between;",
        "align-items: center;",
        "flex-wrap: wrap;",
        "gap: 8px;",
        "padding: 10px 16px;",
        "background-color: #f3f2f1;",
        "border-bottom: 1px solid #dcdcdc;"
      ),
      shiny::tags$div(
        style = "font-size: 14px; color: #505a5f;",
        if (!is.na(row$inspection_date)) {
          shiny::tagList(
            "Inspection Date: ",
            shiny::tags$strong(
              style = "color: #0b0c0c;",
              format(as.Date(row$inspection_date), "%d %B %Y")
            )
          )
        }
      ),

      shiny::tags$div(
        style = "display: flex; align-items: center; gap: 10px;",
        # Red Category of Concern Callout
        if (has_coc) {
          shiny::tags$span(
            style = paste(
              "background-color: #fdf2f2;",
              "color: #d4351c;",
              "border: 1px solid #d4351c;",
              "padding: 3px 10px;",
              "border-radius: 3px;",
              "font-size: 12px;",
              "font-weight: 700;",
              "letter-spacing: 0.3px;"
            ),
            glue::glue("Category of Concern: {row$coc}")
          )
        },
        # Safeguarding Badge (Red for Not Met, Green for Met)
        if (has_safeguarding) {
          shiny::tags$span(
            style = paste0(
              "background-color: ",
              sg_bg,
              "; color: #ffffff;",
              "padding: 4px 12px; border-radius: 3px;",
              "font-size: 12px; font-weight: bold; letter-spacing: 0.3px;"
            ),
            glue::glue("Safeguarding: {sg_val}")
          )
        }
      )
    ),

    # --- 2-Column Evaluation Grid ---
    shiny::tags$div(
      style = "display: grid; grid-template-columns: 1fr 1fr; gap: 12px; padding: 14px;",
      .ofsted_metric_tile("Inclusion", row$inclusion),
      .ofsted_metric_tile("Curriculum & Teaching", row$curriculum_and_teaching),
      .ofsted_metric_tile("Achievement", row$achievement),
      .ofsted_metric_tile(
        "Attendance & Behaviour",
        row$attendance_and_behaviour
      ),
      .ofsted_metric_tile(
        "Personal Development & Well-being",
        row$personal_development_and_wellbeing
      ),
      .ofsted_metric_tile(
        "Leadership & Governance",
        row$leadership_and_governance
      )
    )
  )
}
