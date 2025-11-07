#' Render Warning Notice Summary Panel
#'
#' Creates a GOV.UK-styled summary panel displaying three key metrics:
#' - Total live TWN records
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
#'     uiOutput("summary_metrics")
#'   )
#'
#'   server <- function(input, output, session) {
#'     output$summary_metrics <- renderUI({
#'       wn_render_summary()  # No region filter
#'     })
#'   }
#'
#'   shinyApp(ui, server)
#' }
#'
#' # --- Example with region filter -------------------------------
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
#'     uiOutput("summary_metrics")
#'   )
#'
#'   server <- function(input, output, session) {
#'     output$summary_metrics <- renderUI({
#'       selected <- if (is.null(input$region) || input$region == "") NULL else input$region
#'       wn_render_summary(region = selected)
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

wn_render_summary <- function(region = NULL) {
  start_time <- Sys.time()
  dauPortalTools::log_event(glue::glue(
    "Starting wn_render_summary with region: {region}"
  ))

  conn <- sql_manager("dit")

  school_region <- if (!is.null(region)) {
    glue::glue_sql(" AND school_region = {region}", .con = conn)
  } else {
    DBI::SQL("")
  }
  region_filter <- if (!is.null(region)) {
    glue::glue_sql(" AND region = {region}", .con = conn)
  } else {
    DBI::SQL("")
  }

  sql_command <- glue::glue_sql(
    "
    SELECT
      (SELECT COUNT(twn_id)
       FROM {config$database}.{config$schema$db_schema_00a}.[twn_all_notices]
       WHERE [twn_status_id] <> 7{school_region}) AS total_live_records,
      (SELECT COUNT(t.twn_date_id)
       FROM {config$database}.{config$schema$db_schema_00a}.[twn_date_tracking] t
       LEFT JOIN {config$database}.{config$schema$db_schema_00a}.[twn_all_notices] a ON t.twn_id = a.twn_id
       WHERE t.updated_on >= DATEADD(DAY, -30, GETDATE()){school_region}) AS updated_records,
      (SELECT COUNT(quality_id)
       FROM {config$database}.{config$schema$db_schema_00a}.[quality_list] l
       WHERE l.app_id > 0 AND l.app_id < 3 AND quality_status = 0{region_filter}) AS quality_issues
  ",
    .con = conn
  )

  summary_data <- tryCatch(
    {
      DBI::dbGetQuery(conn, sql_command)
    },
    error = function(e) {
      dauPortalTools::log_event(glue::glue(
        "Error fetching summary: {e$message}"
      ))
      return(data.frame(
        total_live_records = NA,
        updated_records = NA,
        quality_issues = NA
      ))
    }
  )

  df <- data.frame(
    Metric = c(
      "Total Live Records",
      "Records Updated in Last 30 Days",
      "Quality Issues"
    ),
    Value = c(
      format(summary_data$total_live_records, big.mark = ","),
      format(summary_data$updated_records, big.mark = ","),
      format(summary_data$quality_issues, big.mark = ",")
    )
  )

  heading <- if (!is.null(region)) {
    glue::glue("Summary for {region}")
  } else {
    "Summary"
  }

  ui <- shinyGovstyle::gov_layout(
    size = "two-thirds",
    shinyGovstyle::heading_text(heading, size = "l"),
    shinyGovstyle::label_hint(
      "summary_label",
      "Key metrics for Warning Notices"
    ),
    shinyGovstyle::govTable(
      inputId = "summary_table",
      df = df,
      caption = "Key Metrics",
      caption_size = "l",
      num_col = c(2)
    )
  )

  end_time <- Sys.time()
  dauPortalTools::log_event(glue::glue(
    "Finished wn_render_summary in {round(difftime(end_time, start_time, units = 'secs'), 2)} seconds"
  ))

  return(ui)
}
