#' Render RISE Universal Summary Metrics Panel
#'
#' Generates an upgraded full-width GOV.UK-styled overview panel displaying programmatic tracking
#' footprint metrics across the RISE Universal infrastructure layers.
#'
#' @param db_get_query Function used to execute the query (default: `utils_db_get_query`).
#' @export
ru_render_summary <- function(db_get_query = utils_db_get_query) {
  start_time <- Sys.time()
  log_event("Starting ru_render_summary dashboard calculation pipeline")

  conn <- sql_manager("dit")
  on.exit(
    {
      try(DBI::dbDisconnect(conn), silent = TRUE)
    },
    add = TRUE
  )

  schema <- utils_resolve_schema("db_schema_01r")

  sql_command <- glue::glue_sql(
    "
    SELECT
      -- 1. All-Time Footprint: Combined rows from both core tracking ledgers
      (
        SELECT (SELECT COUNT(*) FROM {schema}.[ru_events]) + 
               (SELECT COUNT(*) FROM {schema}.[ruh_support_records])
      ) AS all_time_footprint,
      
      -- 2. Current Active Context: Combined active provisions and open/non-completed events
      (
        SELECT (SELECT COUNT(*) FROM {schema}.[ruh_support_records] WHERE [ruhsr_active] = 1) + 
               (SELECT COUNT(*) FROM {schema}.[ru_events] WHERE [ruev_completed] <> 1)
      ) AS active_live_footprint,
      
      -- 3. Rolling Window: Every single creation/edit transaction across all editable tables over the last month
      (
        SELECT ISNULL(SUM(cnt), 0)
        FROM (
          -- Core Events Logging Table
          SELECT COUNT(*) AS cnt FROM {schema}.[ru_events] 
          WHERE [date_created] >= DATEADD(DAY, -30, GETDATE()) 
             OR [date_edited] >= DATEADD(DAY, -30, GETDATE())
          
          UNION ALL
          
          -- Hubs Support Provisions Records Table
          SELECT COUNT(*) AS cnt FROM {schema}.[ruh_support_records] 
          WHERE [date_created] >= DATEADD(DAY, -30, GETDATE()) 
             OR [date_edited] >= DATEADD(DAY, -30, GETDATE())
             
          UNION ALL
          
          -- Dynamic Fields Action Responses Ledger
          SELECT COUNT(*) AS cnt FROM {schema}.[ru_event_action_responses] 
          WHERE [date_created] >= DATEADD(DAY, -30, GETDATE()) 
             OR [date_edited] >= DATEADD(DAY, -30, GETDATE())
             
          UNION ALL
          
          -- Hubs Management Records Table (If it tracks timeline changes)
          SELECT COUNT(*) AS cnt FROM {schema}.[ruh_lead_schools]
          WHERE [date_created] >= DATEADD(DAY, -30, GETDATE())
             OR [date_edited] >= DATEADD(DAY, -30, GETDATE())
             
          UNION ALL
          
          -- Sub-Varieties & Lookup Configuration Tables
          SELECT COUNT(*) AS cnt FROM {schema}.[ru_event_sub_varieties] 
          WHERE [date_created] >= DATEADD(DAY, -30, GETDATE())
        ) transaction_union
      ) AS updates_this_month
    ",
    .con = conn
  )

  summary_data <- tryCatch(
    DBI::dbGetQuery(conn, sql_command),
    error = function(e) {
      log_event(glue::glue("Error fetching RISE summary metrics: {e$message}"))
      data.frame(
        all_time_footprint = NA_integer_,
        active_live_footprint = NA_integer_,
        updates_this_month = NA_integer_
      )
    }
  )

  all_time <- suppressWarnings(as.integer(summary_data$all_time_footprint[1]))
  active_live <- suppressWarnings(as.integer(summary_data$active_live_footprint[
    1
  ]))
  updated_30d <- suppressWarnings(as.integer(summary_data$updates_this_month[
    1
  ]))

  fmt <- function(x) {
    if (is.na(x)) "—" else prettyNum(x, big.mark = ",", preserve.width = "none")
  }

  ui <- shinyGovstyle::gov_layout(
    bslib::layout_column_wrap(
      width = 1 / 3,
      bslib::card(
        style = "border-top: 4px solid #1d70b8; min-height: 110px;",
        bslib::card_header(
          style = "font-weight: bold; background: none; border: none; padding-bottom: 0;",
          "All-Time Records (Hubs & Events)"
        ),
        tags$h2(
          fmt(all_time),
          class = "govuk-heading-l",
          style = "margin-top: 5px; padding-left: 15px; color: #0b0c0c;"
        )
      ),
      bslib::card(
        style = "border-top: 4px solid #00703c; min-height: 110px;",
        bslib::card_header(
          style = "font-weight: bold; background: none; border: none; padding-bottom: 0;",
          "Total Active Provisions"
        ),
        tags$h2(
          fmt(active_live),
          class = "govuk-heading-l",
          style = "margin-top: 5px; padding-left: 15px; color: #00703c;"
        )
      ),
      bslib::card(
        style = "border-top: 4px solid #f47738; min-height: 110px;",
        bslib::card_header(
          style = "font-weight: bold; background: none; border: none; padding-bottom: 0;",
          "Updates This Month"
        ),
        tags$h2(
          fmt(updated_30d),
          class = "govuk-heading-l",
          style = "margin-top: 5px; padding-left: 15px; color: #0b0c0c;"
        )
      )
    )
  )

  log_event(glue::glue(
    "Finished ru_render_summary in {round(difftime(Sys.time(), start_time, units = 'secs'), 2)} seconds"
  ))
  return(ui)
}
