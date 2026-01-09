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

applist_render_table <- function() {
  start_time <- Sys.time()
  dauPortalTools::log_event(glue::glue(
    "Starting applist_render_table"
  ))

  conn <- sql_manager("dit")

  sql_command <- glue::glue_sql(
    "SELECT app_id,
    COUNT(*) AS total,
    SUM(CASE WHEN date_created < DATEADD(DAY, -30, GETDATE()) THEN 1 ELSE 0 END) AS total_30days
	FROM [data_insight_team].[01_AIDT].[quality_list]
WHERE quality_status = 0
GROUP BY app_id",
    .con = conn
  )

  quality_summary <- DBI::dbGetQuery(conn, sql_command)

  data <- dauPortalTools::applist_get_data() |>
    dplyr::left_join(quality_summary, by = app_id) |>
    dplyr::select(app_name, app_description, app_url, total, total_30days) |>
    dplyr::rename(
      "App Name" = app_name,
      "Description" = app_description,
      "URL" = app_url,
      "Total Live Quality Issues" = total,
      "Total Quality Issues Identified over 30 days ago" = total_30days,
    )

  data$app_url <- vapply(
    data$app_url,
    function(u) {
      dauPortalTools::make_shiny_link(u, text = "Open")
    },
    FUN.VALUE = character(1)
  )

  ui <- DT::renderDataTable(
    {
      data
    }
  )

  end_time <- Sys.time()
  dauPortalTools::log_event(glue::glue(
    "Finished applist_render_table in {round(difftime(end_time, start_time, units = 'secs'), 2)} seconds"
  ))

  ui
}
