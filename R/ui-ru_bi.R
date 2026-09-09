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

  # Resolve schema and wrap explicitly as raw SQL so glue_sql does not quote it as a string
  raw_schema <- utils_resolve_schema("db_schema_01r")
  # Ensure bracket formatting is clean: [01_r]
  clean_schema <- gsub("[\\[\\]]", "", raw_schema)
  schema_prefix <- DBI::SQL(paste0("[", clean_schema, "]"))

  sql_command <- glue::glue_sql(
    "
    SELECT
      -- 1. All-Time Footprint: Combined rows from both core tracking ledgers
      (
        SELECT (SELECT COUNT(*) FROM {schema_prefix}.[ru_events]) + 
               (SELECT COUNT(*) FROM {schema_prefix}.[ruh_support_records])
      ) AS all_time_footprint,
      
      -- 2. Current Active Context: Combined active provisions and open/non-completed events
      (
        SELECT (SELECT COUNT(*) FROM {schema_prefix}.[ruh_support_records] WHERE ISNULL([ruhsr_active], 0) = 1) + 
               (SELECT COUNT(*) FROM {schema_prefix}.[ru_events] WHERE ISNULL([ruev_completed], 0) <> 1)
      ) AS active_live_footprint,
      
      -- 3. Rolling Window: Every single creation/edit transaction across all editable tables over the last month
      (
        SELECT ISNULL(SUM(cnt), 0)
        FROM (
          -- Core Events Logging Table
          SELECT COUNT(*) AS cnt FROM {schema_prefix}.[ru_events] 
          WHERE [created_date] >= DATEADD(DAY, -30, GETDATE()) 
             OR [modified_date] >= DATEADD(DAY, -30, GETDATE())
          
          UNION ALL
          
          -- Hubs Support Provisions Records Table
          SELECT COUNT(*) AS cnt FROM {schema_prefix}.[ruh_support_records] 
          WHERE [created_date] >= DATEADD(DAY, -30, GETDATE()) 
             OR [modified_date] >= DATEADD(DAY, -30, GETDATE())
             
          UNION ALL
          
          -- Hubs Management Records Table
          SELECT COUNT(*) AS cnt FROM {schema_prefix}.[ruh_lead_schools]
          WHERE [created_date] >= DATEADD(DAY, -30, GETDATE())
             OR [modified_date] >= DATEADD(DAY, -30, GETDATE())
             
          UNION ALL
          
          -- Sub-Varieties & Lookup Configuration Tables
          SELECT COUNT(*) AS cnt FROM {schema_prefix}.[ru_event_sub_varieties] 
          WHERE [created_date] >= DATEADD(DAY, -30, GETDATE())
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
        shiny::tags$h2(
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
        shiny::tags$h2(
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
        shiny::tags$h2(
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

#' RISE Universal Hubs Portal Health UI
#'
#' @param id Character module namespace ID.
#' @return A Shiny tagList of bslib value boxes.
#' @export
ru_portal_health_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    bslib::layout_column_wrap(
      width = "250px",
      fill = FALSE,
      class = "mb-3",

      # 1. Designated Hubs & Entity Reach
      bslib::value_box(
        title = "Designated Hubs",
        value = shiny::textOutput(ns("n_hubs")),
        showcase = shiny::icon("building-columns"),
        theme = "primary",
        p(
          class = "text-muted mb-0 small",
          shiny::textOutput(ns("n_hub_entities"))
        )
      ),

      # 2. Lead Schools & Direct School Interventions
      bslib::value_box(
        title = "Lead Schools",
        value = shiny::textOutput(ns("n_leads")),
        showcase = shiny::icon("school"),
        theme = "info",
        p(
          class = "text-muted mb-0 small",
          shiny::textOutput(ns("n_supported_schools"))
        )
      ),

      # 3. Point-in-Time Events Logged
      bslib::value_box(
        title = "Events Logged",
        value = shiny::textOutput(ns("n_events")),
        showcase = shiny::icon("calendar-check"),
        theme = "teal",
        p(
          class = "text-muted mb-0 small",
          shiny::textOutput(ns("n_event_entities"))
        )
      ),

      # 4. Pipeline Velocity & Attention Metric
      bslib::value_box(
        title = "Updates This Month",
        value = shiny::textOutput(ns("n_month_updates")),
        showcase = shiny::icon("arrow-trend-up"),
        theme = "secondary",
        p(
          class = "text-muted mb-0 small",
          shiny::textOutput(ns("n_unassigned_leads"))
        )
      )
    )
  )
}

#' RISE Universal Hubs Portal Health Server
#'
#' @param id Character module namespace ID.
#' @param con DBI SQL Server connection or reactive returning a connection.
#' @param schema Character target RISE schema (e.g., "[01_RISE]" or "[01_RISE_b]").
#' @export
ru_portal_health_server <- function(
  id,
  con,
  schema = utils_resolve_schema("db_schema_01r")
) {
  shiny::moduleServer(id, function(input, output, session) {
    get_con <- function() {
      if (shiny::is.reactive(con)) con() else con
    }

    health_metrics <- shiny::reactive({
      conn <- get_con()
      start_of_month <- as.character(lubridate::floor_date(Sys.Date(), "month"))

      query <- glue::glue(
        "
        SELECT 
          -- Hubs (excluding fallback hub 0)
          (SELECT COUNT(DISTINCT ruhb_id) FROM {schema}.[ruh_hubs] WHERE ruhb_id > 0) AS n_hubs,
          
          -- Distinct entities attached to hubs
          (SELECT COUNT(DISTINCT CONCAT(ruhsr_entity_type, '_', CAST(ruhsr_entity_id AS NVARCHAR(30)))) 
           FROM {schema}.[ruh_support_records] WHERE ruhsr_active = 1) AS n_hub_entities,
          
          -- Active lead schools
          (SELECT COUNT(DISTINCT ruhl_id) FROM {schema}.[ruh_lead_schools] WHERE ruhl_active = 1) AS n_leads,
          
          -- Active schools supported through records
          (SELECT COUNT(DISTINCT ruhsr_entity_id) FROM {schema}.[ruh_support_records] 
           WHERE ruhsr_entity_type = 'School' AND ruhsr_active = 1) AS n_supported_schools,
          
          -- Point-in-time events & participant entities
          (SELECT COUNT(DISTINCT ruev_id) FROM {schema}.[ru_events]) AS n_events,
          (SELECT COUNT(DISTINCT CONCAT(ruev_entity_type, '_', CAST(ruev_entity_id AS NVARCHAR(30)))) 
           FROM {schema}.[ru_events]) AS n_event_entities,
          
          -- Updates logged this month across matrices
          ((SELECT COUNT(*) FROM {schema}.[ruh_support_records] 
            WHERE date_created >= '{start_of_month}' OR date_edited >= '{start_of_month}') +
           (SELECT COUNT(*) FROM {schema}.[ru_events] 
            WHERE date_created >= '{start_of_month}' OR date_edited >= '{start_of_month}')
          ) AS n_month_updates,
          
          -- Attention: Active support records missing an allocated lead school
          (SELECT COUNT(*) FROM {schema}.[ruh_support_records] 
           WHERE ruhsr_active = 1 AND ruhl_id IS NULL) AS n_unassigned_leads
      "
      )

      utils_db_get_query(conn, query)
    })

    output$n_hubs <- shiny::renderText({
      format(health_metrics()$n_hubs %||% 0, big.mark = ",")
    })

    output$n_hub_entities <- shiny::renderText({
      paste(
        format(health_metrics()$n_hub_entities %||% 0, big.mark = ","),
        "entities attached"
      )
    })

    output$n_leads <- shiny::renderText({
      format(health_metrics()$n_leads %||% 0, big.mark = ",")
    })

    output$n_supported_schools <- shiny::renderText({
      paste(
        format(health_metrics()$n_supported_schools %||% 0, big.mark = ","),
        "schools receiving support"
      )
    })

    output$n_events <- shiny::renderText({
      format(health_metrics()$n_events %||% 0, big.mark = ",")
    })

    output$n_event_entities <- shiny::renderText({
      paste(
        format(health_metrics()$n_event_entities %||% 0, big.mark = ","),
        "entities involved"
      )
    })

    output$n_month_updates <- shiny::renderText({
      format(health_metrics()$n_month_updates %||% 0, big.mark = ",")
    })

    output$n_unassigned_leads <- shiny::renderText({
      unassigned <- health_metrics()$n_unassigned_leads %||% 0
      paste(unassigned, "records without lead school")
    })
  })
}

#' Render England GOR Heatmap for RISE Universal Hubs
#'
#' @param data Data frame with columns: gor_name, n_hubs, n_supported_entities,
#'   n_lead_entities, n_events, n_event_entities.
#' @param fill_metric Metric column name (character) driving the fill gradient.
#'   Options: "n_supported_entities", "n_lead_entities", "n_hubs", "n_events", "n_event_entities".
#' @param height Canvas height in pixels. Default is 480.
#' @return A plotly htmlwidget object.
#' @export
ui_ru_gor_heatmap <- function(
  data,
  fill_metric = "n_supported_entities",
  height = 480
) {
  # 1. Filter out pseudo/non-English rows
  clean_data <- data %>%
    dplyr::filter(!gor_name %in% c("Not Applicable", "Wales (pseudo)", "", NA))

  # 2. GeoJSON boundary resolution: package asset with ONS ArcGIS fallback
  geojson_file <- system.file(
    "extdata",
    "england_regions.geojson",
    package = "dauPortalTools"
  )

  if (nzchar(geojson_file) && file.exists(geojson_file)) {
    gor_geojson <- jsonlite::fromJSON(geojson_file, simplifyVector = FALSE)
  } else {
    gor_geojson <- jsonlite::fromJSON(
      "https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Regions_December_2023_Boundaries_EN_BFC/FeatureServer/0/query?where=1%3D1&outFields=*&f=geojson",
      simplifyVector = FALSE
    )
  }

  # 3. Clean GOV.UK/DfE styled tooltip
  clean_data <- clean_data %>%
    dplyr::mutate(
      hover_text = glue::glue(
        "<b>{gor_name}</b><br>",
        "<span style='color:#b1b4b6;'>━━━━━━━━━━━━━━━━━━━━</span><br>",
        "<b>Active Hubs Serving:</b> {format(n_hubs, big.mark = ',')}<br>",
        "<b>Supported Entities:</b> {format(n_supported_entities, big.mark = ',')}<br>",
        "<b>Lead Entities:</b> {format(n_lead_entities, big.mark = ',')}<br>",
        "<b>Events (Past Year/Live):</b> {format(n_events, big.mark = ',')}<br>",
        "<b>Participating Entities:</b> {format(n_event_entities, big.mark = ',')}<extra></extra>"
      )
    )

  metric_titles <- c(
    "n_supported_entities" = "Supported Entities",
    "n_lead_entities" = "Lead Entities",
    "n_hubs" = "Active Hubs",
    "n_events" = "Active Events",
    "n_event_entities" = "Participating Entities"
  )

  legend_label <- metric_titles[[fill_metric]] %||% fill_metric

  # 4. Plotly Choropleth
  plotly::plot_ly(height = height) %>%
    plotly::add_trace(
      type = "choropleth",
      geojson = gor_geojson,
      locations = clean_data$gor_name,
      z = clean_data[[fill_metric]],
      text = clean_data$hover_text,
      hovertemplate = "%{text}",
      featureidkey = "properties.RGN23NM",
      colorscale = list(
        list(0, "#f3f2f1"), # GOV.UK light grey
        list(0.25, "#bdd7ee"),
        list(0.65, "#2b8cc4"),
        list(1, "#003078") # DfE deep navy
      ),
      marker = list(
        line = list(width = 1.2, color = "#0b0c0c")
      )
    ) %>%
    plotly::colorbar(
      title = list(text = legend_label, font = list(size = 11)),
      len = 0.75,
      thickness = 15
    ) %>%
    plotly::layout(
      geo = list(
        scope = "europe",
        fitbounds = "locations",
        visible = FALSE,
        showland = FALSE
      ),
      margin = list(l = 0, r = 0, t = 5, b = 0)
    ) %>%
    plotly::config(displayModeBar = FALSE)
}
