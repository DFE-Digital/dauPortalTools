# ==============================================================================
# R/fct-ofsted.R
# UI card components and data fetchers for Ofsted inspections across:
#   1. Modern Report Card Framework (ofstedinspection)
#   2. Pre-09/25 Graded Sub-Judgements (ofsted shj)
#   3. Single Headline Published Judgements (ofsted sj)
# ==============================================================================

# ==============================================================================
# 1. Internal Style & Tile Helpers
# ==============================================================================

#' Internal Helper: Style Judgment Badges (Modern Framework)
#' @noRd
.ofsted_badge <- function(val) {
  if (
    is.null(val) ||
      length(val) == 0 ||
      is.na(val[1]) ||
      !nzchar(trimws(as.character(val[1])))
  ) {
    return(NULL)
  }

  val_clean <- trimws(as.character(val[1]))
  val_lower <- tolower(val_clean)

  style <- if (grepl("exceptional", val_lower)) {
    "background-color: #4c2c92; color: #ffffff; font-weight: 700;"
  } else if (grepl("strong standard|exemplary", val_lower)) {
    "background-color: #003078; color: #ffffff; font-weight: 600;"
  } else if (grepl("expected standard|expected", val_lower)) {
    "background-color: #1d70b8; color: #ffffff; font-weight: 500;"
  } else if (grepl("needs attention", val_lower)) {
    "background-color: #ffdd00; color: #0b0c0c; font-weight: 700;"
  } else if (grepl("urgent improvement|urgent", val_lower)) {
    "background-color: #d4351c; color: #ffffff; font-weight: 700;"
  } else if (grepl("^not met$", val_lower)) {
    "background-color: #d4351c; color: #ffffff; font-weight: 700;"
  } else if (grepl("^met$", val_lower)) {
    "background-color: #00703c; color: #ffffff; font-weight: 700;"
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
  if (
    is.null(val) ||
      length(val) == 0 ||
      is.na(val[1]) ||
      !nzchar(trimws(as.character(val[1])))
  ) {
    return(NULL)
  }

  val_clean <- trimws(as.character(val[1]))
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
  if (
    is.null(value) ||
      length(value) == 0 ||
      is.na(value[1]) ||
      !nzchar(trimws(as.character(value[1])))
  ) {
    return(NULL)
  }

  badge_el <- if (is_four_point) {
    .ofsted_4point_badge(value[1])
  } else {
    .ofsted_badge(value[1])
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

#' Helper to standardise dates for SQL parameterized filtering
#' @noRd
.clean_ofsted_date_str <- function(d) {
  if (is.null(d) || length(d) == 0 || is.na(d[1])) {
    return(NULL)
  }
  tryCatch(
    {
      format(as.Date(d[1]), "%Y-%m-%d")
    },
    error = function(e) NULL
  )
}


# ==============================================================================
# 2. Database Fetcher Helpers (Fallback & Direct Queries)
# ==============================================================================

if (!exists("db_get_latest_ofsted_inspection", mode = "function")) {
  #' Fetch Modern Ofsted Inspection Row
  #' @noRd
  db_get_latest_ofsted_inspection <- function(urn, publication_date = NULL) {
    conn <- sql_manager("dit")
    on.exit(
      tryCatch(DBI::dbDisconnect(conn), error = function(e) NULL),
      add = TRUE
    )

    clean_date <- .clean_ofsted_date_str(publication_date)

    date_clause <- if (!is.null(clean_date)) {
      glue::glue_sql(
        "AND (CAST([publication_date] AS DATE) = CAST({clean_date} AS DATE) OR CAST([inspection_date] AS DATE) = CAST({clean_date} AS DATE))",
        .con = conn
      )
    } else {
      DBI::SQL("")
    }

    sql <- glue::glue_sql(
      "
      SELECT TOP 1 *
      FROM [Data_Insight_Team].[01_SSSR].[ofsted_weekly_graded]
      WHERE [urn] = {as.integer(urn)}
        {date_clause}
      ORDER BY [publication_date] DESC
      ",
      .con = conn
    )
    DBI::dbGetQuery(conn, sql)
  }
}

if (!exists("db_get_ofsted_shj_inspection", mode = "function")) {
  #' Fetch Pre-0925 Graded Ofsted Inspection Row
  #' @noRd
  db_get_ofsted_shj_inspection <- function(urn, published_date = NULL) {
    conn <- sql_manager("dit")
    on.exit(
      tryCatch(DBI::dbDisconnect(conn), error = function(e) NULL),
      add = TRUE
    )

    clean_date <- .clean_ofsted_date_str(published_date)

    date_clause <- if (!is.null(clean_date)) {
      glue::glue_sql(
        "AND (CAST([published_date] AS DATE) = CAST({clean_date} AS DATE) OR CAST([inspection_date] AS DATE) = CAST({clean_date} AS DATE))",
        .con = conn
      )
    } else {
      DBI::SQL("")
    }

    sql <- glue::glue_sql(
      "
      SELECT TOP 1 *
      FROM [Data_Insight_Team].[01_AIDT].[ofsted_weekly_graded_pre0925]
      WHERE [urn] = {as.integer(urn)}
        {date_clause}
      ORDER BY [published_date] DESC
      ",
      .con = conn
    )
    DBI::dbGetQuery(conn, sql)
  }
}

if (!exists("db_get_ofsted_sj_inspection", mode = "function")) {
  #' Fetch Single Headline Published Ofsted Inspection Row
  #' @noRd
  db_get_ofsted_sj_inspection <- function(urn, published_date = NULL) {
    conn <- sql_manager("dit")
    on.exit(
      tryCatch(DBI::dbDisconnect(conn), error = function(e) NULL),
      add = TRUE
    )

    clean_date <- .clean_ofsted_date_str(published_date)

    date_clause <- if (!is.null(clean_date)) {
      glue::glue_sql(
        "AND (CAST([published_date] AS DATE) = CAST({clean_date} AS DATE) OR CAST([inspection_date] AS DATE) = CAST({clean_date} AS DATE))",
        .con = conn
      )
    } else {
      DBI::SQL("")
    }

    sql <- glue::glue_sql(
      "
      SELECT TOP 1 *
      FROM [Data_Insight_Team].[01_AIDT].[ofsted_weekly_published]
      WHERE [urn] = {as.integer(urn)}
        {date_clause}
      ORDER BY [published_date] DESC
      ",
      .con = conn
    )
    DBI::dbGetQuery(conn, sql)
  }
}


# ==============================================================================
# 3. Scorecard UI Card Components
# ==============================================================================

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

  # Safeguarding evaluation
  sg_field <- if ("safeguarding_standards" %in% names(row)) {
    row$safeguarding_standards
  } else if ("safeguarding_effective" %in% names(row)) {
    row$safeguarding_effective
  } else {
    NA_character_
  }

  has_safeguarding <- !is.na(sg_field) && nzchar(trimws(as.character(sg_field)))
  sg_val <- if (has_safeguarding) trimws(as.character(sg_field)) else ""
  sg_is_not_met <- grepl("not met|no", tolower(sg_val))
  sg_bg <- if (sg_is_not_met) "#d4351c" else "#00703c"

  # Category of Concern evaluation
  coc_field <- if ("coc" %in% names(row)) {
    row$coc
  } else if ("category_of_concern" %in% names(row)) {
    row$category_of_concern
  } else {
    NA_character_
  }

  has_coc <- !is.na(coc_field) &&
    nzchar(trimws(as.character(coc_field))) &&
    !grepl("^none$|^no category", tolower(trimws(as.character(coc_field))))

  # Inspection Date display resolution
  insp_date <- if (
    "inspection_date" %in% names(row) && !is.na(row$inspection_date)
  ) {
    row$inspection_date
  } else if (
    "publication_date" %in% names(row) && !is.na(row$publication_date)
  ) {
    row$publication_date
  } else {
    NA
  }

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
        if (!is.na(insp_date)) {
          shiny::tagList(
            "Inspection Date: ",
            shiny::tags$strong(
              style = "color: #0b0c0c;",
              format(as.Date(insp_date), "%d %B %Y")
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
            glue::glue("Category of Concern: {coc_field}")
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

    # 2-Column Scorecard Grid (Modern 6 Judgement Areas)
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

#' Render Ofsted Single Headline Judgement (SHJ) Card (Pre-09/25 Graded Format)
#'
#' @param urn Numeric or character scalar. Unique Reference Number.
#' @param publication_date Optional Date or character scalar.
#'
#' @return Shiny HTML tag element.
#' @export
render_ofsted_shj_card <- function(urn, publication_date = NULL) {
  if (missing(urn) || is.null(urn) || is.na(urn)) {
    return(NULL)
  }

  df <- db_get_ofsted_shj_inspection(
    urn = urn,
    published_date = publication_date
  )
  if (is.null(df) || nrow(df) == 0) {
    return(NULL)
  }
  row <- df[1, ]

  # Safeguarding evaluation
  has_sg <- "safeguarding_effective" %in%
    names(row) &&
    !is.null(row$safeguarding_effective) &&
    !is.na(row$safeguarding_effective) &&
    nzchar(trimws(as.character(row$safeguarding_effective)))

  sg_val <- if (has_sg) trimws(as.character(row$safeguarding_effective)) else ""
  sg_is_not_met <- grepl("no|not", tolower(sg_val))
  sg_bg <- if (sg_is_not_met) "#d4351c" else "#00703c"

  # Category of Concern evaluation
  has_coc <- "category_of_concern" %in%
    names(row) &&
    !is.null(row$category_of_concern) &&
    !is.na(row$category_of_concern) &&
    nzchar(trimws(as.character(row$category_of_concern))) &&
    !grepl(
      "^none$|^no category",
      tolower(trimws(as.character(row$category_of_concern)))
    )

  # Inspection Date
  insp_date <- if (
    "inspection_date" %in% names(row) && !is.na(row$inspection_date)
  ) {
    row$inspection_date
  } else if ("published_date" %in% names(row) && !is.na(row$published_date)) {
    row$published_date
  } else {
    NA
  }

  shiny::tags$div(
    style = paste(
      "border: 1px solid #b1b4b6;",
      "border-radius: 4px;",
      "background-color: #ffffff;",
      "margin-top: 12px;",
      "overflow: hidden;"
    ),

    # --- Header Bar: Date, CoC Alert, Safeguarding Badge ---
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
        if (!is.na(insp_date)) {
          shiny::tagList(
            "Inspection Date: ",
            shiny::tags$strong(
              style = "color: #0b0c0c;",
              format(as.Date(insp_date), "%d %B %Y")
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
              "font-size: 12px; font-weight: bold; letter-spacing: 0.3px;"
            ),
            glue::glue("Safeguarding: {sg_val}")
          )
        }
      )
    ),

    # --- Body: Single Headline Pill Tile ---
    shiny::tags$div(
      style = "padding: 16px; background-color: #ffffff;",
      shiny::tags$div(
        style = paste(
          "background-color: #f8f8f8;",
          "border: 1px solid #e2e2e2;",
          "border-radius: 4px;",
          "padding: 14px 18px;",
          "display: flex;",
          "align-items: center;",
          "justify-content: space-between;"
        ),
        shiny::tags$span(
          style = "font-size: 15px; font-weight: 600; color: #0b0c0c;",
          "Overall Effectiveness"
        ),
        shiny::tags$div(.ofsted_4point_badge(row$overall_effectiveness))
      )
    )
  )
}

#' Render Ofsted Sub-Judgements (SJ) Card (Pre-09/25 Interim Format)
#'
#' @param urn Numeric or character scalar. Unique Reference Number.
#' @param publication_date Optional Date or character scalar.
#'
#' @return Shiny HTML tag element.
#' @export
render_ofsted_sj_card <- function(urn, publication_date = NULL) {
  if (missing(urn) || is.null(urn) || is.na(urn)) {
    return(NULL)
  }

  df <- db_get_ofsted_sj_inspection(
    urn = urn,
    published_date = publication_date
  )
  if (is.null(df) || nrow(df) == 0) {
    return(NULL)
  }
  row <- df[1, ]

  # Safeguarding evaluation
  has_sg <- "safeguarding_effective" %in%
    names(row) &&
    !is.null(row$safeguarding_effective) &&
    !is.na(row$safeguarding_effective) &&
    nzchar(trimws(as.character(row$safeguarding_effective)))

  sg_val <- if (has_sg) trimws(as.character(row$safeguarding_effective)) else ""
  sg_is_not_met <- grepl("no|not", tolower(sg_val))
  sg_bg <- if (sg_is_not_met) "#d4351c" else "#00703c"

  # Category of Concern evaluation
  has_coc <- "category_of_concern" %in%
    names(row) &&
    !is.null(row$category_of_concern) &&
    !is.na(row$category_of_concern) &&
    nzchar(trimws(as.character(row$category_of_concern))) &&
    !grepl(
      "^none$|^no category",
      tolower(trimws(as.character(row$category_of_concern)))
    )

  # Inspection Date
  insp_date <- if (
    "inspection_date" %in% names(row) && !is.na(row$inspection_date)
  ) {
    row$inspection_date
  } else if ("published_date" %in% names(row) && !is.na(row$published_date)) {
    row$published_date
  } else {
    NA
  }

  # Resolving Leadership & Management field name differences
  leadership_val <- if (
    "effectiveness_of_leadership_and_management" %in% names(row)
  ) {
    row$effectiveness_of_leadership_and_management
  } else if ("leadership_and_management" %in% names(row)) {
    row$leadership_and_management
  } else {
    NA_character_
  }

  shiny::tags$div(
    style = paste(
      "border: 1px solid #b1b4b6;",
      "border-radius: 4px;",
      "background-color: #ffffff;",
      "margin-top: 12px;",
      "overflow: hidden;"
    ),

    # --- Header Bar: Date, CoC Alert, Safeguarding Badge ---
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
        if (!is.na(insp_date)) {
          shiny::tagList(
            "Inspection Date: ",
            shiny::tags$strong(
              style = "color: #0b0c0c;",
              format(as.Date(insp_date), "%d %B %Y")
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
              "font-size: 12px; font-weight: bold; letter-spacing: 0.3px;"
            ),
            glue::glue("Safeguarding: {sg_val}")
          )
        }
      )
    ),

    # --- 2x2 Sub-Judgements Grid ---
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
        leadership_val,
        is_four_point = TRUE
      )
    )
  )
}
