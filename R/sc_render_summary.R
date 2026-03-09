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

sc_render_summary <- function(user = NULL) {
  start_time <- Sys.time()
  log_event(glue::glue(
    "Starting sc_render_summary"
  ))

  app_id <- conf$app_details$app_id
  db_schema_01a <- DBI::SQL(conf$schemas$db_schema_01a)
  db_schema_01s <- DBI::SQL(conf$schemas$db_schema_01s)

  conn <- sql_manager("dit")

  user_query <- if (!is.null(user)) {
    glue::glue_sql(
      " AND ([delivery_lead] = {user} OR [RSCContact] = {user})",
      .con = conn
    )
  } else {
    DBI::SQL("")
  }

  sql_command <- glue::glue_sql(
    "
    SELECT
      (SELECT COUNT(t.sig_change_id)
       FROM {db_schema_01s}.[tracker] t
       WHERE [all_actions_completed] <> 1 {user_query}) AS total_live_records,
      (SELECT COUNT(t.sig_change_id)
       FROM {db_schema_01s}.[tracker] t
       WHERE change_edit_date >= DATEADD(DAY, -30, GETDATE()){user_query}) AS updated_records,
      (SELECT COUNT(quality_id)
       FROM {db_schema_01a}.[quality_list] l
       RIGHT JOIN {db_schema_01s}.[tracker] t on t.record_id = l.sig_change_id
       WHERE l.app_id = {app_id} AND t.quality_status = 0{user_query}) AS quality_issues
  ",
    .con = conn
  )

  summary_data <- tryCatch(
    DBI::dbGetQuery(conn, sql_command),
    error = function(e) {
      log_event(glue::glue(
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
    layout_column_wrap(
      width = 1 / 3,
      card(
        card_header("Total Live Records"),
        tags$h2(fmt(total_live), class = "govuk-heading-m")
      ),
      card(
        card_header("Updates This Month"),
        tags$h2(fmt(updated_30d), class = "govuk-heading-m")
      ),
      card(
        card_header("Quality Issues"),
        tags$h2(fmt(qual_issues), class = "govuk-heading-m")
      )
    )
  )

  end_time <- Sys.time()
  log_event(glue::glue(
    "Finished sc_render_summary in {round(difftime(end_time, start_time, units = 'secs'), 2)} seconds"
  ))

  return(ui)
}
