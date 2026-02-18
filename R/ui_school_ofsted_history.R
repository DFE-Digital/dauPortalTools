#' Render Ofsted Inspection Panel
#'
#' Generates a standalone GOV.UK-styled Ofsted inspection panel for a given URN.
#' The panel contains its own tabset and mirrors the structure of the new Ofsted
#' report‑card model, including coloured grade pills, an accessible safeguarding
#' panel, and an expandable “Our grades explained” section.
#'
#' This function produces **two tabs**:
#'
#' \strong{1. Latest Inspection}
#' \itemize{
#'   \item Attempts to load the most recent post–September 2025 inspection from
#'     \code{[ofsted_weekly_graded]}.
#'   \item If no new‑framework inspection exists, the function falls back to the
#'     most recent record in
#'     \code{[ofsted_weekly_graded_pre0925]}.
#'   \item Displays:
#'     \itemize{
#'       \item Inspection date
#'       \item New‑framework 5‑point grades (or legacy 4‑grade set, if falling back)
#'       \item Coloured, ARIA‑labelled grade pills
#'       \item Safeguarding status presented in an ARIA‑labelled panel
#'       \item An expandable “Our grades explained” section aligned to the
#'             new report‑card model
#'     }
#' }
#'
#' \strong{2. Inspection History}
#' \itemize{
#'   \item Includes two tables:
#'     \enumerate{
#'       \item All post–Sep 2025 inspections for the URN from
#'             \code{ofsted_weekly_graded}, with a placeholder
#'             \code{category_of_concern = "coming soon"}.
#'       \item All pre–Sep 2025 inspections for the URN from
#'             \code{ofsted_weekly_graded_pre0925}, including the legacy grade
#'             judgements, category of concern, and safeguarding effectiveness.
#'     }
#'   \item Tables are rendered using \code{DT::datatable()}.
#' }
#'
#' @param urn A length‑1 numeric or character Unique Reference Number (URN) for
#'   the school whose Ofsted inspection data should be displayed.
#'
#' @return A Shiny UI object containing the complete GOV.UK-styled Ofsted panel.
#'   If no inspection data is found for the given URN, a user‑friendly message is
#'   returned instead.
#'
#' @details
#' \strong{Data sources}
#' \itemize{
#'   \item \emph{New framework (post–September 2025)}:
#'         \code{[ofsted_weekly_graded]}
#'   \item \emph{Previous framework (pre–September 2025)}:
#'         \code{[ofsted_weekly_graded_pre0925]}
#' }
#'
#' \strong{Logic}
#' \enumerate{
#'   \item Attempt to select the latest inspection under the new framework
#'         (inspection_date ≥ \code{2025‑09‑01}).
#'   \item If not present, fall back to the most recent previous‑framework record.
#'   \item Construct the history tab from:
#'         \itemize{
#'           \item All new‑framework rows for the URN (plus placeholder category).
#'           \item All previous‑framework rows for the URN.
#'         }
#' }
#'
#' \strong{Presentation}
#' \itemize{
#'   \item New‑framework grades are mapped to the five-category model
#'         (\emph{Exceptional}, \emph{Strong standard},
#'         \emph{Expected standard}, \emph{Needs attention},
#'         \emph{Urgent improvement}).
#'   \item Previous‑framework grades (Outstanding, Good, Requires Improvement,
#'         Inadequate) are displayed with a separate pill‑colour scheme.
#'   \item Safeguarding status is displayed as “Met” or “Not met” in a dedicated
#'         panel.
#' }
#'
#' @section Accessibility:
#' This function includes some accessibility enhancements:
#' \itemize{
#'   \item Grade pills include \code{role="text"} and an \code{aria-label}
#'         describing the grade to screen readers.
#'   \item The safeguarding panel uses \code{role="group"} and an
#'         \code{aria-label} announcing the safeguarding status.
#'   \item All table headers use appropriate \code{scope="row"} and
#'         \code{scope="col"} attributes for improved screen‑reader navigation.
#' }
#'
#'
#' @section Dependencies:
#' Requires an active SQL Server connection via \code{sql_manager("dit")}.
#' Uses \code{shinyGovstyle} for layout and \code{DT} for data tables.
#'
#' @seealso
#' \code{\link{school_render_overview}} for the main school summary panel that
#' accompanies this Ofsted view.
#'
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   ui <- fluidPage(
#'     uiOutput("ofsted_panel")
#'   )
#'
#'   server <- function(input, output, session) {
#'     output$ofsted_panel <- renderUI({
#'       ui_school_ofsted_history(urn = 123456)  # Replace with a valid URN
#'     })
#'   }
#'
#'   shinyApp(ui, server)
#' }
#' }
#'
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
    "
  ))

  ui <- shinyGovstyle::gov_layout(
    tab_css,
    tab_buttons,
    tab_contents,
    tab_js
  )

  log_event(glue::glue(
    "Finished ui_school_ofsted_history in {round(difftime(Sys.time(), start_time, units = 'secs'), 2)}s"
  ))
  ui
}
