# ==============================================================================
# R/fct-ofsted.R
# UI card components for Ofsted inspections across modern, SHJ, and SJ eras
# ==============================================================================

#' Internal Helper: Style Judgment Badges (Modern Framework)
#' @noRd
.ofsted_badge <- function(val) {
  if (is.na(val) || !nzchar(trimws(val))) {
    return(NULL)
  }

  val_clean <- trimws(val)
  val_lower <- tolower(val_clean)

  style <- if (grepl("exceptional", val_lower)) {
    "background-color: #4c2c92; color: #ffffff; font-weight: 700;"
  } else if (grepl("strong standard|exemplary", val_lower)) {
    "background-color: #003078; color: #ffffff; font-weight: 600;"
  } else if (grepl("expected standard|expected", val_lower)) {
    "background-color: #1d70b8; color: #ffffff; font-weight: 500;"[cite:1]
  } else if (grepl("needs attention", val_lower)) {
    "background-color: #ffdd00; color: #0b0c0c; font-weight: 700;"[cite:1]
  } else if (grepl("urgent improvement|urgent", val_lower)) {
    "background-color: #d4351c; color: #ffffff; font-weight: 700;"
  } else if (grepl("^not met$", val_lower)) {
    "background-color: #d4351c; color: #ffffff; font-weight: 700;"
  } else if (grepl("^met$", val_lower)) {
    "background-color: #00703c; color: #ffffff; font-weight: 700;"[cite:1]
  } else {
    "background-color: #f3f2f1; color: #0b0c0c; border: 1px solid #b1b4b6;"
  }

  shiny::tags$span(
    style = paste0(
      "display: inline-block; padding: 4px 14px; border-radius: 16px; ",
      "font-size: 13px; line-height: 1.2; letter-spacing: 0.2px; ",
      style
    ),
    val_clean
  )
}

#' Internal Helper: Style 4-Point Scale Pills (SHJ and SJ eras)
#' @noRd
.ofsted_4point_badge <- function(val) {
  if (is.na(val) || !nzchar(trimws(val))) {
    return(NULL)
  }

  val_clean <- trimws(val)
  val_lower <- tolower(val_clean)

  style <- if (grepl("outstanding", val_lower)) {
    "background-color: #00703c; color: #ffffff; font-weight: bold;"
  } else if (grepl("good", val_lower)) {
    "background-color: #1d70b8; color: #ffffff; font-weight: bold;"
  } else if (grepl("requires improvement", val_lower)) {
    "background-color: #ffdd00; color: #0b0c0c; font-weight: bold;"
  } else if (grepl("inadequate", val_lower)) {
    "background-color: #d4351c; color: #ffffff; font-weight: bold;"
  } else {
    "background-color: #f3f2f1; color: #0b0c0c; border: 1px solid #b1b4b6;"
  }

  shiny::tags$span(
    style = paste0(
      "display: inline-block; padding: 4px 14px; border-radius: 16px; ",
      "font-size: 13px; line-height: 1.2; letter-spacing: 0.2px; ",
      style
    ),
    val_clean
  )
}

#' Internal Helper: Render Individual Scorecard Tile
#' @noRd
.ofsted_metric_tile <- function(label, value, is_four_point = FALSE) {
  if (is.na(value) || !nzchar(trimws(value))) {
    return(NULL)
  }

  badge_el <- if (is_four_point) {
    .ofsted_4point_badge(value)
  } else {
    .ofsted_badge(value)
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
    shiny::tags$div(badge_el)
  )
}

#' Render Standard Ofsted Inspection Card by URN (Modern Framework)
#'
#' @description
#' Independent UI component querying and displaying the 2-column scorecard
#' format with date header, Safeguarding pill, and Category of Concern alerts.
#'
#' @param urn Numeric or character scalar. Unique Reference Number of the school.
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

  has_safeguarding <- "safeguarding_standards" %in%
    names(row) &&
    !is.na(row$safeguarding_standards) &&
    nzchar(trimws(row$safeguarding_standards))
  sg_val <- if (has_safeguarding) trimws(row$safeguarding_standards) else ""
  sg_is_not_met <- grepl("not met", tolower(sg_val))
  sg_bg <- if (sg_is_not_met) "#d4351c" else "#005a30"

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

    # Header Bar: Inspection Date, CoC Alert, Safeguarding Badge
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

    # 2-Column Scorecard Grid
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

#' Render Ofsted Single Headline Judgement (SHJ) Card
#'
#' @description
#' Independent UI component displaying an overall effectiveness score using
#' the legacy 4-point pill scale with inspection date and safeguarding headers.
#'
#' @param urn Numeric or character scalar. Unique Reference Number of the school.
#' @param publication_date Optional Date or character scalar.
#'
#' @return Shiny HTML tag element, or NULL if no record found.
#' @export
render_ofsted_shj_card <- function(urn, publication_date = NULL) {
  if (missing(urn) || is.null(urn) || is.na(urn)) {
    return(NULL)
  }

  df <- db_get_ofsted_shj_inspection(
    urn = urn,
    publication_date = publication_date
  )
  if (is.null(df) || nrow(df) == 0) {
    return(NULL)
  }
  row <- df[1, ]

  has_sg <- "safeguarding_effective" %in%
    names(row) &&
    !is.na(row$safeguarding_effective) &&
    nzchar(trimws(row$safeguarding_effective))
  sg_val <- if (has_sg) trimws(row$safeguarding_effective) else ""
  sg_is_not_met <- grepl("no|not", tolower(sg_val))
  sg_bg <- if (sg_is_not_met) "#d4351c" else "#005a30"

  has_coc <- "category_of_concern" %in%
    names(row) &&
    !is.na(row$category_of_concern) &&
    nzchar(trimws(row$category_of_concern)) &&
    !grepl("^none$|^no category", tolower(trimws(row$category_of_concern)))

  shiny::tags$div(
    style = paste(
      "border: 1px solid #b1b4b6;",
      "border-radius: 4px;",
      "background-color: #ffffff;",
      "margin-top: 12px;",
      "overflow: hidden;"
    ),

    # Header Bar
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
        if (has_coc) {
          shiny::tags$span(
            style = paste(
              "background-color: #fdf2f2;",
              "color: #d4351c;",
              "border: 1px solid #d4351c;",
              "padding: 3px 10px;",
              "border-radius: 3px;",
              "font-size: 12px;",
              "font-weight: 700;"
            ),
            glue::glue("Category of Concern: {row$category_of_concern}")
          )
        },
        if (has_sg) {
          shiny::tags$span(
            style = paste0(
              "background-color: ",
              sg_bg,
              "; color: #ffffff;",
              "padding: 4px 12px; border-radius: 3px;",
              "font-size: 12px; font-weight: bold;"
            ),
            glue::glue("Safeguarding: {sg_val}")
          )
        }
      )
    ),

    # Overall Effectiveness Judgement Row
    shiny::tags$div(
      style = paste(
        "padding: 16px;",
        "display: flex;",
        "align-items: center;",
        "justify-content: space-between;",
        "background-color: #fafafa;"
      ),
      shiny::tags$span(
        style = "font-size: 15px; font-weight: 600; color: #0b0c0c;",
        "Overall Effectiveness"
      ),
      .ofsted_4point_badge(row$overall_effectiveness)
    )
  )
}

#' Render Ofsted Sub-Judgements (SJ) Card (Pre-09/25 Interim Format)
#'
#' @description
#' Independent UI component displaying the 4 legacy sub-judgements in a 2x2 grid
#' using standard 4-point scale pills without a single headline judgement.
#'
#' @param urn Numeric or character scalar. Unique Reference Number of the school.
#' @param publication_date Optional Date or character scalar.
#'
#' @return Shiny HTML tag element, or NULL if no record found.
#' @export
render_ofsted_sj_card <- function(urn, publication_date = NULL) {
  if (missing(urn) || is.null(urn) || is.na(urn)) {
    return(NULL)
  }

  df <- db_get_ofsted_sj_inspection(
    urn = urn,
    publication_date = publication_date
  )
  if (is.null(df) || nrow(df) == 0) {
    return(NULL)
  }
  row <- df[1, ]

  has_sg <- "safeguarding_effective" %in%
    names(row) &&
    !is.na(row$safeguarding_effective) &&
    nzchar(trimws(row$safeguarding_effective))
  sg_val <- if (has_sg) trimws(row$safeguarding_effective) else ""
  sg_is_not_met <- grepl("no|not", tolower(sg_val))
  sg_bg <- if (sg_is_not_met) "#d4351c" else "#005a30"

  has_coc <- "category_of_concern" %in%
    names(row) &&
    !is.na(row$category_of_concern) &&
    nzchar(trimws(row$category_of_concern)) &&
    !grepl("^none$|^no category", tolower(trimws(row$category_of_concern)))

  shiny::tags$div(
    style = paste(
      "border: 1px solid #b1b4b6;",
      "border-radius: 4px;",
      "background-color: #ffffff;",
      "margin-top: 12px;",
      "overflow: hidden;"
    ),

    # Header Bar
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
        if (has_coc) {
          shiny::tags$span(
            style = paste(
              "background-color: #fdf2f2;",
              "color: #d4351c;",
              "border: 1px solid #d4351c;",
              "padding: 3px 10px;",
              "border-radius: 3px;",
              "font-size: 12px;",
              "font-weight: 700;"
            ),
            glue::glue("Category of Concern: {row$category_of_concern}")
          )
        },
        if (has_sg) {
          shiny::tags$span(
            style = paste0(
              "background-color: ",
              sg_bg,
              "; color: #ffffff;",
              "padding: 4px 12px; border-radius: 3px;",
              "font-size: 12px; font-weight: bold;"
            ),
            glue::glue("Safeguarding: {sg_val}")
          )
        }
      )
    ),

    # 4 Sub-judgements 2x2 Grid
    shiny::tags$div(
      style = "display: grid; grid-template-columns: 1fr 1fr; gap: 12px; padding: 14px;",
      .ofsted_metric_tile(
        "Quality of Education",
        row$quality_of_education,
        is_four_point = TRUE
      ),
      .ofsted_metric_tile(
        "Behaviour & Attitudes",
        row$behaviour_and_attitudes,
        is_four_point = TRUE
      ),
      .ofsted_metric_tile(
        "Personal Development",
        row$personal_development,
        is_four_point = TRUE
      ),
      .ofsted_metric_tile(
        "Leadership & Management",
        row$effectiveness_of_leadership_and_management,
        is_four_point = TRUE
      )
    )
  )
}
