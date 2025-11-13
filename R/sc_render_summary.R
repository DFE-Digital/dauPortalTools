#' Render Significant Change Summary Panel
#'
#' Creates a GOV.UK-styled summary panel displaying three key metrics:
#' - Total live SC records
#' - Records updated in the last 30 days
#' - Open quality issues
#'
#' This UI fragment can be inserted anywhere in a Shiny app (e.g., inside
#' `uiOutput()` / `renderUI()`).
#'
#' @param region Character scalar or `NULL`. If provided, metrics are filtered
#'   to this region. If `NULL`, metrics are unfiltered. Region values must match
#'   the database column values used in the SQL.
#'
#' @param rsc bool (default = NULL). If provided, metrics are filtered
#'   by case_to_rcs. If `NULL`, metrics are unfiltered.
#'
#' @return A `shiny.tag` object representing a GOV.UK-styled table suitable for
#'   use in `renderUI()`.
#'
#' @details
#' The function queries database using `DBI::dbGetQuery()` and a
#' connection from `sql_manager()`. It uses `glue_sql()` for
#' safe SQL interpolation. If the query fails, the UI will display `NA` values.
#'
#' Styling is provided by `shinyGovstyle` components (`gov_layout()`,
#' `heading_text()`, `govTable()`).
#'
#' @examples
#' \dontrun{
#' # --- Minimal Shiny example (no region filter) -------------------
#' if (interactive()) {
#'   library(shiny)
#'   library(shinyGovstyle)
#'
#'   ui <- fluidPage(
#'     uiOutput("sc_summary_metrics")
#'   )
#'
#'   server <- function(input, output, session) {
#'     output$sc_summary_metrics <- renderUI({
#'       sc_render_summary()  # No region/rcs filter
#'     })
#'   }
#'
#'   shinyApp(ui, server)
#' }
#'
#' # --- Example with region filter and RCS enabled-------------------
#' if (interactive()) {
#'   library(shiny)
#'   library(shinyGovstyle)
#'
#'   ui <- fluidPage(
#'     shinyGovstyle::label_hint("region_hint", "Choose a region (leave blank for all regions)"),
#'     shinyGovstyle::select_Input(
#'       inputId      = "region",
#'       label        = "Select Region",
#'       select_text  = c("", "North West", "North East", "London"),
#'       select_value = c("", "North West", "North East", "London")
#'     ),
#'     uiOutput("sc_summary_metrics")
#'   )
#'
#'   server <- function(input, output, session) {
#'     output$sc_summary_metrics <- renderUI({
#'       selected <- if (is.null(input$region) || input$region == "") NULL else input$region
#'       sc_render_summary(region = selected, rcs=TRUE)
#'     })
#'   }
#'
#'   shinyApp(ui, server)
#' }
#' }
#'
#' @seealso \code{\link[shinyGovstyle]{govTable}}, \code{\link[shinyGovstyle]{select_Input}}
#' @export
#'

sc_render_summary <- function(region = NULL, rcs = NULL) {
  start_time <- Sys.time()
  dauPortalTools::log_event(glue::glue(
    "Starting sc_render_summary with region: {region} and rcs: {rcs}"
  ))

  conn <- sql_manager("dit")

  school_region <- if (!is.null(region)) {
    glue::glue_sql(" AND e.[GOR (name)] = {region}", .con = conn)
  } else {
    DBI::SQL("")
  }

  rsc_check <- if (is.null(rcs)) {
    DBI::SQL("")
  } else if (rcs == TRUE) {
    glue::glue_sql(" AND t.case_to_rcs IS NOT NULL", .con = conn)
  } else if (rcs == FALSE) {
    glue::glue_sql(" AND t.case_to_rcs IS NULL", .con = conn)
  } else {
    DBI::SQL("")
  }

  schema_00c <- DBI::SQL(conf$schemas$db_schema_00c)
  schema_01a <- DBI::SQL(conf$schemas$db_schema_01a)
  schema_01s <- DBI::SQL(conf$schemas$db_schema_01s)

  sql_command <- glue::glue_sql(
    "
WITH LatestDate AS (
    SELECT MAX(DateStamp) AS MaxDate FROM {schema_00c}.[Edubase]
)
SELECT
    (SELECT COUNT(t.sig_change_id)
     FROM {schema_01s}.[tracker] t
     LEFT JOIN {schema_00c}.[Edubase] e ON t.URN = e.URN AND e.DateStamp = (SELECT MaxDate FROM LatestDate)
     WHERE t.all_actions_completed <> 1 {school_region} {rsc_check}) AS total_live_records,
    (SELECT COUNT(t.sig_change_id)
     FROM {schema_01s}.[tracker] t
     LEFT JOIN {schema_00c}.[Edubase] e ON t.URN = e.URN AND e.DateStamp = (SELECT MaxDate FROM LatestDate)
     WHERE t.change_edit_date >= DATEADD(DAY, -30, GETDATE()) {school_region} {rsc_check}) AS updated_records,
    (SELECT COUNT(l.quality_id)
     FROM {schema_01a}.[quality_list] l
     LEFT JOIN {schema_01s}.[tracker] t ON t.sig_change_id = l.record_id AND l.app_id = 3
     LEFT JOIN {schema_00c}.[Edubase] e ON t.URN = e.URN AND e.DateStamp = (SELECT MaxDate FROM LatestDate)
     WHERE l.quality_status = 0 {school_region} {rsc_check}) AS quality_issues
",
    .con = conn
  )

  summary_data <- tryCatch(
    DBI::dbGetQuery(conn, sql_command),
    error = function(e) {
      dauPortalTools::log_event(glue::glue(
        "Error fetching summary: {e$message}"
      ))
      data.frame(
        total_live_records = NA_integer_,
        updated_records = NA_integer_,
        quality_issues = NA_integer_
      )
    }
  )

  total_live <- suppressWarnings(as.integer(summary_data$total_live_records[1]))
  updated_30d <- suppressWarnings(as.integer(summary_data$updated_records[1]))
  qual_issues <- suppressWarnings(as.integer(summary_data$quality_issues[1]))

  fmt <- function(x) {
    ifelse(is.na(x), "—", prettyNum(x, big.mark = ",", preserve.width = "none"))
  }

  ui <- shinyGovstyle::gov_layout(
    bslib::layout_column_wrap(
      width = 1 / 3,
      bslib::card(
        bslib::card_header("Total Live Records"),
        tags$h2(fmt(total_live), class = "govuk-heading-m")
      ),
      bslib::card(
        bslib::card_header("Updates This Month"),
        tags$h2(fmt(updated_30d), class = "govuk-heading-m")
      ),
      bslib::card(
        bslib::card_header("Quality Issues"),
        tags$h2(fmt(qual_issues), class = "govuk-heading-m")
      )
    )
  )

  end_time <- Sys.time()
  dauPortalTools::log_event(glue::glue(
    "Finished sc_render_summary in {round(difftime(end_time, start_time, units = 'secs'), 2)} seconds"
  ))

  ui
}
