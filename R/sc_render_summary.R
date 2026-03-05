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

sc_render_summary <- function() {
  start_time <- Sys.time()
  log_event(glue::glue(
    "Starting sc_render_summary"
  ))

  app_id <- conf$app_details$app_id
  db_schema_01a <- conf$schemas$db_schema_01a
  db_schema_01s <- conf$schemas$db_schema_01s

  conn <- sql_manager("dit")

  sql_command <- glue::glue_sql(
    "
    SELECT
      (SELECT COUNT(sig_change_id)
       FROM {db_schema_01s}.[tracker]
       WHERE [all_actions_completed] <> 1) AS total_live_records,
      (SELECT COUNT(sig_change_id)
       FROM {db_schema_01s}.[tracker]
       WHERE change_edit_date >= DATEADD(DAY, -30, GETDATE())) AS updated_records,
      (SELECT COUNT(quality_id)
       FROM {db_schema_01a}.[quality_list] l
       WHERE l.app_id = {app_id} AND quality_status = 0) AS quality_issues
  ",
    .con = conn
  )

  summary_data <- tryCatch(
    {
      DBI::dbGetQuery(conn, sql_command)
    },
    error = function(e) {
      log_event(glue::glue(
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

  ui <- shinyGovstyle::gov_layout(
    size = "two-thirds",
    shinyGovstyle::heading_text("Sig Change Summary", size = "l"),
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
  log_event(glue::glue(
    "Finished sc_render_summary in {round(difftime(end_time, start_time, units = 'secs'), 2)} seconds"
  ))

  return(ui)
}
