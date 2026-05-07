#' Render Ofsted Inspection Panel
#'
#' Generates a GOV.UK-styled Ofsted inspection panel for a given URN.
#' The panel includes summary information for the latest inspection and
#' a tabbed view of historical inspection data.
#'
#' @param urn Character or numeric scalar. Unique Reference Number (URN)
#'   identifying the school.
#'
#' @details
#' The component renders two tabs:
#'
#' \strong{Latest Inspection}
#' \itemize{
#'   \item Attempts to load the most recent inspection under the
#'         post–September 2025 framework.
#'   \item Falls back to the latest pre–September 2025 inspection if no
#'         new-framework data exists.
#'   \item Displays inspection date, graded judgements, and safeguarding
#'         status.
#' }
#'
#' \strong{Inspection History}
#' \itemize{
#'   \item Displays all inspection records for the URN under both
#'         frameworks.
#'   \item Uses `gt::gt()` tables for presentation.
#' }
#'
#' @section Data Sources:
#' \itemize{
#'   \item `ofsted_weekly_graded` (post–Sep 2025 framework)
#'   \item `ofsted_weekly_graded_pre0925` (legacy framework)
#' }
#'
#' @section Presentation:
#' \itemize{
#'   \item New-framework grades use a five-level model (Exceptional → Urgent improvement).
#'   \item Legacy grades use the four-level model (Outstanding → Inadequate).
#'   \item Safeguarding status is shown in a dedicated panel.
#'   \item Tab navigation is implemented using custom HTML, CSS, and JavaScript.
#' }
#'
#' @section Accessibility:
#' \itemize{
#'   \item Grade indicators include ARIA labels for screen readers.
#'   \item Safeguarding panel uses semantic grouping.
#'   \item Table headers include appropriate scope attributes.
#' }
#'
#' @section Side Effects:
#' \itemize{
#'   \item Opens database connections via [sql_manager()].
#'   \item Performs database queries using [DBI::dbGetQuery()].
#'   \item Writes log entries via [log_event()].
#' }
#'
#' @return A Shiny UI object. If no inspection data is available, a message is returned instead.
#'
#' @examples
#' \dontrun{
#' ui_school_ofsted_history(urn = 123456)
#' }
#'
#' @seealso [school_render_overview()]
#' @export

ui_school_ofsted_history <- function(urn) {
  start_time <- Sys.time()
  log_event(glue::glue("Starting ui_school_ofsted_history with urn: {urn}"))

  conn <- sql_manager("dit")

  sql_new <- glue::glue_sql(
    "
      SELECT [urn],
             [school_name],
             [inspection_date],
             [inclusion],
             [curriculum_and_teaching],
             [achievement],
             [attendance_and_behaviour],
             [personal_development_and_well_being],
             [leadership_and_governance],
             [safeguarding_standards],
             [ay],
             [extract_date],
             [date_created]
      FROM [Data_Insight_Team].[01_AIDT].[ofsted_weekly_graded]
      WHERE urn = {urn}
    ",
    .con = conn
  )

  newfw <- tryCatch(DBI::dbGetQuery(conn, sql_new), error = function(e) {
    log_event(glue::glue("New framework query error: {e$message}"))
    data.frame()
  })

  sql_old <- glue::glue_sql(
    "
      SELECT [urn],
             [inspection_date],
             [quality_of_education],
             [behaviour_and_attitudes],
             [personal_development],
             [effectiveness_of_leadership_and_management],
             [category_of_concern],
             [safeguarding_effective],
             [published_date]
      FROM [Data_Insight_Team].[01_AIDT].[ofsted_weekly_graded_pre0925]
      WHERE urn = {urn}
    ",
    .con = conn
  )

  oldfw <- tryCatch(DBI::dbGetQuery(conn, sql_old), error = function(e) {
    log_event(glue::glue("Old framework query error: {e$message}"))
    data.frame()
  })

  to_date_safe <- function(x) {
    if (inherits(x, "Date")) {
      return(x)
    }
    suppressWarnings(as.Date(x))
  }
  `%||%` <- function(a, b) {
    if (is.null(a) || length(a) == 0 || is.na(a)) b else a
  }

  grade_pill <- function(value) {
    val <- trimws(as.character(value %||% "Unknown"))
    key <- tolower(val)

    cls <- switch(
      key,
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

  legacy_pill <- function(value) {
    val <- trimws(as.character(value %||% "Unknown"))
    key <- tolower(val)

    cls <- switch(
      key,
      "outstanding" = "ofsted-legacy-outstanding",
      "good" = "ofsted-legacy-good",
      "requires improvement" = "ofsted-legacy-ri",
      "inadequate" = "ofsted-legacy-inadequate",
      "ofsted-unknown"
    )

    htmltools::tags$span(
      class = paste("ofsted-pill", cls),
      role = "text",
      `aria-label` = paste0("Legacy grade: ", val),
      val
    )
  }

  safeguarding_panel <- function(flag) {
    raw <- tolower(trimws(as.character(flag %||% "Not known")))

    met_values <- c("met", "yes", "effective", "true", "1")
    not_met_values <- c("not met", "no", "not effective", "false", "0")

    state <- if (raw %in% met_values) {
      "met"
    } else if (raw %in% not_met_values) {
      "not met"
    } else {
      "unknown"
    }
    cls <- switch(state, "met" = "met", "not met" = "not-met", "unknown")
    txt <- if (state == "unknown") "Not known" else tools::toTitleCase(state)

    htmltools::tags$div(
      class = paste("ofsted-safeguarding-panel", cls),
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

  grades_table_new <- function(row) {
    areas <- c(
      "Inclusion" = row$inclusion,
      "Curriculum & teaching" = row$curriculum_and_teaching,
      "Achievement" = row$achievement,
      "Attendance & behaviour" = row$attendance_and_behaviour,
      "Personal development & well-being" = row$personal_development_and_well_being,
      "Leadership & governance" = row$leadership_and_governance
    )

    rows <- Map(
      function(k, v) {
        htmltools::tags$tr(
          htmltools::tags$th(scope = "row", k),
          htmltools::tags$td(grade_pill(v))
        )
      },
      names(areas),
      unname(areas)
    )

    htmltools::tags$table(
      class = "ofsted-grades-table",
      htmltools::tags$thead(
        htmltools::tags$tr(
          htmltools::tags$th(scope = "col", "Area"),
          htmltools::tags$th(scope = "col", "Grade")
        )
      ),
      htmltools::tags$tbody(
        htmltools::tagList(rows)
      )
    )
  }

  grades_table_old <- function(row) {
    areas <- c(
      "Quality of education" = row$quality_of_education,
      "Behaviour & attitudes" = row$behaviour_and_attitudes,
      "Personal development" = row$personal_development,
      "Leadership & management" = row$effectiveness_of_leadership_and_management
    )

    rows <- Map(
      function(k, v) {
        htmltools::tags$tr(
          htmltools::tags$th(scope = "row", k),
          htmltools::tags$td(legacy_pill(v))
        )
      },
      names(areas),
      unname(areas)
    )

    htmltools::tags$table(
      class = "ofsted-grades-table",
      htmltools::tags$thead(
        htmltools::tags$tr(
          htmltools::tags$th(scope = "col", "Area"),
          htmltools::tags$th(scope = "col", "Grade")
        )
      ),
      htmltools::tags$tbody(
        htmltools::tagList(rows)
      )
    )
  }

  grades_explained_details <- htmltools::tags$details(
    class = "ofsted-details",
    open = NA,
    htmltools::tags$summary("Our grades explained"),
    htmltools::tags$div(
      class = "ofsted-details-body",
      htmltools::tags$ul(
        htmltools::tags$li(
          htmltools::tags$strong("Exceptional"),
          " – practice among the very best nationally, going beyond strong standards."
        ),
        htmltools::tags$li(
          htmltools::tags$strong("Strong standard"),
          " – all expected standards are securely and consistently met, plus additional strengths."
        ),
        htmltools::tags$li(
          htmltools::tags$strong("Expected standard"),
          " – all required standards are met (legal + professional expectations)."
        ),
        htmltools::tags$li(
          htmltools::tags$strong("Needs attention"),
          " – not yet at the expected standard; aspects are inconsistent or incomplete."
        ),
        htmltools::tags$li(
          htmltools::tags$strong("Urgent improvement"),
          " – urgent action required to provide a suitable standard of education and/or care."
        ),
        htmltools::tags$li(htmltools::tags$em(
          "Safeguarding is evaluated separately as ",
          htmltools::tags$strong("met / not met"),
          "."
        ))
      ),
      htmltools::tags$p(
        class = "ofsted-source-hint",
        "Aligned to Ofsted’s report-card guidance (Dec 2025; updated Feb 2026)."
      )
    )
  )
  # (Source: GOV.UK ‘Understanding Ofsted report cards and grades’) # [1](https://www.gov.uk/guidance/understanding-ofsted-report-cards-and-grades)

  newfw$inspection_date <- to_date_safe(newfw$inspection_date)
  oldfw$inspection_date <- to_date_safe(oldfw$inspection_date)

  if (nrow(newfw) > 0) {
    latest_new <- newfw[
      order(newfw$inspection_date, decreasing = TRUE),
      ,
      drop = FALSE
    ][1, ]

    latest_panel <- htmltools::tagList(
      htmltools::tags$div(
        class = "ofsted-top-row",
        htmltools::tags$div(
          class = "ofsted-inspection-date",
          htmltools::tags$div(class = "ofsted-label", "Inspection date"),
          htmltools::tags$div(
            class = "ofsted-date",
            format(latest_new$inspection_date, "%d %B %Y")
          )
        ),
        safeguarding_panel(latest_new$safeguarding_standards)
      ),
      htmltools::tags$div(
        class = "ofsted-section",
        htmltools::tags$h3(class = "ofsted-h3", "Inspection grades"),
        grades_table_new(latest_new)
      ),
      htmltools::tags$div(class = "ofsted-section", grades_explained_details)
    )
  } else if (nrow(oldfw) > 0) {
    latest_old <- oldfw[
      order(oldfw$inspection_date, decreasing = TRUE),
      ,
      drop = FALSE
    ][1, ]

    latest_panel <- htmltools::tagList(
      htmltools::tags$div(
        class = "ofsted-top-row",
        htmltools::tags$div(
          class = "ofsted-inspection-date",
          htmltools::tags$div(class = "ofsted-label", "Inspection date"),
          htmltools::tags$div(
            class = "ofsted-date",
            format(latest_old$inspection_date, "%d %B %Y")
          )
        ),
        safeguarding_panel(latest_old$safeguarding_effective)
      ),
      htmltools::tags$div(
        class = "ofsted-section",
        htmltools::tags$h3(
          class = "ofsted-h3",
          "Inspection judgements (legacy framework)"
        ),
        grades_table_old(latest_old)
      ),
      htmltools::tags$div(
        class = "ofsted-section",
        htmltools::tags$p(
          class = "ofsted-note",
          "This inspection used the previous framework (4 judgements)."
        )
      )
    )
  } else {
    latest_panel <- htmltools::tags$p("No Ofsted data found for this URN.")
  }

  # NEW framework history (no tryCatch)
  new_hist <- DBI::dbGetQuery(
    conn,
    glue::glue_sql(
      "
  SELECT
    inspection_date,
    inclusion,
    curriculum_and_teaching,
    achievement,
    attendance_and_behaviour,
    personal_development_and_well_being,
    leadership_and_governance,
    safeguarding_standards,
    'coming soon' AS category_of_concern
  FROM [Data_Insight_Team].[01_AIDT].[ofsted_weekly_graded]
  WHERE urn = {urn}
  ORDER BY inspection_date DESC
",
      .con = conn
    )
  )

  # OLD framework history (no tryCatch)
  old_hist <- DBI::dbGetQuery(
    conn,
    glue::glue_sql(
      "
  SELECT
    inspection_date,
    quality_of_education,
    behaviour_and_attitudes,
    personal_development,
    effectiveness_of_leadership_and_management,
    category_of_concern,
    safeguarding_effective
  FROM [Data_Insight_Team].[01_AIDT].[ofsted_weekly_graded_pre0925]
  WHERE urn = {urn}
  ORDER BY inspection_date DESC
",
      .con = conn
    )
  )

  history_panel <- htmltools::tagList(
    htmltools::tags$div(
      class = "ofsted-section",
      htmltools::tags$h3(
        class = "ofsted-h3",
        "All inspections – new framework"
      ),
      if (nrow(new_hist) > 0) {
        new_hist |>
          gt::gt()
      } else {
        htmltools::tags$p(
          "No inspections under the new framework found for this URN."
        )
      }
    ),
    htmltools::tags$div(
      class = "ofsted-section",
      htmltools::tags$h3(
        class = "ofsted-h3",
        "All inspections – previous framework"
      ),
      if (nrow(old_hist) > 0) {
        old_hist |>
          gt::gt()
      } else {
        htmltools::tags$p(
          "No inspections under the previous framework found for this URN."
        )
      }
    )
  )

  tabs <- list(
    "Latest Inspection" = latest_panel,
    "Inspection History" = history_panel
  )

  tab_buttons <- htmltools::tags$div(
    class = "ofsted-tabs-buttons",
    lapply(seq_along(tabs), function(i) {
      name <- names(tabs)[i]
      htmltools::tags$button(
        class = if (i == 1) "ofsted-tab-btn active" else "ofsted-tab-btn",
        `data-tab` = name,
        name
      )
    })
  )

  tab_contents <- htmltools::tags$div(
    class = "ofsted-tabs-contents",
    lapply(seq_along(tabs), function(i) {
      name <- names(tabs)[i]
      htmltools::tags$div(
        class = if (i == 1) {
          "ofsted-tab-content active"
        } else {
          "ofsted-tab-content"
        },
        `data-tab` = name,
        tabs[[i]]
      )
    })
  )

  tab_js <- shiny::tags$script(shiny::HTML(
    "
    (function() {
      function attachOfstedTabHandlers() {
        const buttons = document.querySelectorAll('.ofsted-tab-btn');
        if (!buttons.length) { setTimeout(attachOfstedTabHandlers, 50); return; }
        buttons.forEach(btn => {
          btn.addEventListener('click', function() {
            const target = this.dataset.tab;
            document.querySelectorAll('.ofsted-tab-btn').forEach(b => b.classList.remove('active'));
            document.querySelectorAll('.ofsted-tab-content').forEach(c => c.classList.remove('active'));
            this.classList.add('active');
            const el = document.querySelector(`.ofsted-tab-content[data-tab='${target}']`);
            if (el) el.classList.add('active');
          });
        });
      }
      attachOfstedTabHandlers();
    })();
    "
  ))

  # CSS created by AI from Ofsted website to align styling
  tab_css <- shiny::tags$style(shiny::HTML(
    "
    /* Tab buttons */
    .ofsted-tabs-buttons { display:flex; flex-wrap:wrap; gap:4px; margin: 12px 0 8px; }
    .ofsted-tab-btn {
      background:#f3f2f1; border:1px solid #b1b4b6; padding:6px 12px; cursor:pointer;
      font-weight:600; border-radius:3px;
    }
    .ofsted-tab-btn.active { background:#005ea5; color:#fff; border-color:#005ea5; }
    .ofsted-tab-btn:hover:not(.active) { background:#e1e1e1; }
    .ofsted-tab-content { display:none; padding:10px 0; }
    .ofsted-tab-content.active { display:block; }

    /* Top row */
    .ofsted-top-row { display:flex; flex-wrap:wrap; gap:16px; align-items:stretch; margin-bottom:12px; }
    .ofsted-inspection-date { background:#f3f2f1; border:1px solid #b1b4b6; padding:12px; border-radius:3px; min-width:240px; }
    .ofsted-label { color:#505a5f; font-weight:600; margin-bottom:4px; }
    .ofsted-date { font-size:1.125rem; font-weight:700; }

    /* Safeguarding panel */
    .ofsted-safeguarding-panel {
      flex:1 1 280px; background:#f8f8f8; border:1px solid #b1b4b6; border-left-width:6px; border-radius:3px; padding:12px;
    }
    .ofsted-safeguarding-panel.met { border-left-color:#00703c; }
    .ofsted-safeguarding-panel.not-met { border-left-color:#d4351c; }
    .ofsted-safeguarding-title { font-weight:700; margin-bottom:4px; }
    .ofsted-safeguarding-value { font-size:1.05rem; }

    /* Grade pills (new framework) */
    .ofsted-pill { display:inline-block; padding:2px 10px; border-radius:999px; font-weight:700; line-height:1.8; border:1px solid transparent; }
    .ofsted-exceptional     { background:#00703c; color:#fff; }
    .ofsted-strong          { background:#28a197; color:#fff; }
    .ofsted-expected        { background:#1d70b8; color:#fff; }
    .ofsted-needs-attention { background:#ffdd00; color:#0b0c0c; }
    .ofsted-urgent          { background:#d4351c; color:#fff; }
    .ofsted-unknown         { background:#f3f2f1; color:#0b0c0c; border-color:#b1b4b6; }

    /* Grade pills (legacy framework) */
    .ofsted-legacy-outstanding { background:#00703c; color:#fff; }
    .ofsted-legacy-good        { background:#1d70b8; color:#fff; }
    .ofsted-legacy-ri          { background:#ffdd00; color:#0b0c0c; }
    .ofsted-legacy-inadequate  { background:#d4351c; color:#fff; }

    /* Grades table */
    .ofsted-grades-table { width:100%; border-collapse:separate; border-spacing:0 6px; }
    .ofsted-grades-table th { text-align:left; padding:6px 10px; width:45%; color:#0b0c0c; font-weight:600; }
    .ofsted-grades-table td { padding:6px 10px; }

    /* Section headings + notes */
    .ofsted-h3 { margin-top:12px; margin-bottom:4px; font-size:1.1875rem; }
    .ofsted-note { color:#505a5f; margin-top:4px; }

    /* Details (accordion) */
    details.ofsted-details { border:1px solid #b1b4b6; border-radius:3px; background:#fff; }
    details.ofsted-details > summary { padding:10px 12px; cursor:pointer; list-style:none; font-weight:700; position:relative; }
    details.ofsted-details > summary::-webkit-details-marker { display:none; }
    details.ofsted-details[open] > summary { border-bottom:1px solid #b1b4b6; }
    .ofsted-details-body { padding:10px 12px; }
    .ofsted-details-body ul { margin:0 0 6px 20px; }
    .ofsted-source-hint { color:#505a5f; margin:4px 0 0; font-size:0.9rem; }

    
    .ofsted-root {
    margin-top: -32px;
    }
    "
  ))

  ui <- shinyGovstyle::gov_layout(
    tab_css,
    htmltools::tags$div(
      class = "ofsted-root",
      tab_buttons,
      tab_contents,
      tab_js
    )
  )

  log_event(glue::glue(
    "Finished ui_school_ofsted_history in {round(difftime(Sys.time(), start_time, units = 'secs'), 2)}s"
  ))
  ui
}
