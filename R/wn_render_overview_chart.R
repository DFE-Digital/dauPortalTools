#' Render Warning Notice Status & Type Charts
#'
#' This function generates a GOV.UK-styled UI component for displaying
#' warning notice data by status and type. It retrieves data from SQL Server,
#' processes it, and renders interactive Plotly bar charts, download buttons,
#' and a table of underlying records.
#'
#' @param region Character string or `NULL`. Optional filter for school region.
#'   If `NULL`, charts are stacked by region; if provided, charts are filtered
#'   for that region only.
#'
#' @return A Shiny UI element containing:
#'   - Heading and descriptive hint
#'   - Download buttons for records and summaries
#'   - Two Plotly bar charts (status and type)
#'   - A table of underlying records (DT or GOV.UK table)
#'
#' @details
#' - Connects to SQL Server using `sql_manager("dit")`.
#' - Queries `[twn_all_notices]` joined with status and type config tables.
#' - Excludes records where `twn_status_id = 7`.
#' - Handles missing values by replacing with "(Unknown)".
#'
#' @examples
#' \dontrun{
#' # Render charts for all regions
#'
#'ui <- shiny::fluidPage(
#'  shiny::uiOutput("school_overview")
#')
#'
#'server <- function(input, output, session) {
#'  output$school_overview <- shiny::renderUI({
#'    wn_render_status_type_charts()
#'  })
#'}
#'
#'shiny::shinyApp(ui, server)
#'
#'
#' # Render charts for a specific region
#' wn_render_status_type_charts(region = "East Midlands")
#' }
#'
#' @import plotly forcats
#'
#' @export
#'

wn_render_status_type_charts_ <- function(region = NULL) {
  start_time <- Sys.time()
  dauPortalTools::log_event(glue::glue(
    "Starting wn_render_status_type_charts with region: {region}"
  ))

  have_DT <- requireNamespace("DT", quietly = TRUE)

  conn <- sql_manager("dit")

  region_where <- if (!is.null(region)) {
    glue::glue_sql(" AND a.school_region = {region}", .con = conn)
  } else {
    DBI::SQL("")
  }

  schema_01a <- DBI::SQL(conf$schemas$db_schema_01a)

  sql_cmd <- glue::glue_sql(
    "
    SELECT
      a.twn_id,
      a.school_name,
      a.school_region,
      a.created_on,
      a.updated_on,
      s.twn_status_id,
      s.twn_status_name,
      t.twn_type_id,
      t.twn_type_name
    FROM {schema_01a}.[twn_all_notices] a
    LEFT JOIN {schema_01a}.[twn_type_config] t ON a.type_of_notice_id = t.twn_type_id
    LEFT JOIN {schema_01a}.[twn_status_config] s ON a.twn_status_id = s.twn_status_id
    WHERE a.twn_status_id <> 7
      {region_where}
  ",
    .con = conn
  )

  df <- tryCatch(DBI::dbGetQuery(conn, sql_cmd), error = function(e) {
    dauPortalTools::log_event(glue::glue(
      "Error fetching status/type data: {e$message}"
    ))
    NULL
  })

  if (is.null(df) || !nrow(df)) {
    heading <- if (is.null(region)) {
      "Warning Notice status & type"
    } else {
      glue::glue("Warning Notice status & type — {region}")
    }
    ui_empty <- shinyGovstyle::gov_layout(
      size = "two-thirds",
      shinyGovstyle::heading_text(heading, size = "l"),
      shinyGovstyle::label_hint("wn_st_hint", "No data available to plot.")
    )
    end_time <- Sys.time()
    dauPortalTools::log_event(glue::glue(
      "Finished wn_render_status_type_charts in {round(difftime(end_time, start_time, units = 'secs'), 2)} seconds"
    ))
    return(ui_empty)
  }

  # Prepare data
  df <- df |>
    dplyr::mutate(
      school_region = dplyr::coalesce(school_region, "(Unknown)"),
      twn_status_name = dplyr::coalesce(twn_status_name, "(Unknown)"),
      twn_type_name = dplyr::coalesce(twn_type_name, "(Unknown)")
    )

  records_df <- df |>
    dplyr::transmute(
      twn_id,
      school_name,
      school_region,
      status = twn_status_name,
      type = twn_type_name,
      created_on,
      updated_on,
      link = paste0(
        "https://rsconnect/rsc/warning-notice-portal/?wnid=",
        twn_id
      )
    ) |>
    dplyr::arrange(dplyr::desc(updated_on), dplyr::desc(created_on))

  records_df <- records_df |>
    dplyr::mutate(
      link = vapply(
        link,
        function(x) as.character(dauPortalTools::make_shiny_link(x, "Open")),
        character(1)
      )
    )

  # Counts
  if (is.null(region)) {
    status_counts <- df |>
      dplyr::count(twn_status_name, school_region, name = "count")
    type_counts <- df |>
      dplyr::count(twn_type_name, school_region, name = "count")
  } else {
    status_counts <- df |>
      dplyr::count(twn_status_name, name = "count") |>
      dplyr::arrange(dplyr::desc(count))
    type_counts <- df |>
      dplyr::count(twn_type_name, name = "count") |>
      dplyr::arrange(dplyr::desc(count))
  }

  status_order <- status_counts |>
    dplyr::group_by(twn_status_name) |>
    dplyr::summarise(total = sum(count), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(total)) |>
    dplyr::pull(twn_status_name)
  type_order <- type_counts |>
    dplyr::group_by(twn_type_name) |>
    dplyr::summarise(total = sum(count), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(total)) |>
    dplyr::pull(twn_type_name)

  govuk_palette <- c(
    "#1d70b8",
    "#d4351c",
    "#00703c",
    "#ffdd00",
    "#6f72af",
    "#d53880",
    "#f47738",
    "#b58840",
    "#5694ca",
    "#28a197"
  )

  # Charts
  if (is.null(region)) {
    p_status <- plotly::plot_ly(
      status_counts,
      x = ~twn_status_name,
      y = ~count,
      color = ~school_region,
      colors = govuk_palette,
      type = "bar",
      hovertemplate = paste(
        "<b>Status:</b> %{x}<br><b>Region:</b> %{fullData.name}<br><b>Count:</b> %{y:,}<extra></extra>"
      )
    ) |>
      plotly::layout(
        barmode = "stack",
        xaxis = list(
          title = "Status",
          categoryorder = "array",
          categoryarray = status_order
        ),
        yaxis = list(title = "Count"),
        legend = list(title = list(text = "School region"))
      )

    p_type <- plotly::plot_ly(
      type_counts,
      x = ~twn_type_name,
      y = ~count,
      color = ~school_region,
      colors = govuk_palette,
      type = "bar",
      hovertemplate = paste(
        "<b>Type:</b> %{x}<br><b>Region:</b> %{fullData.name}<br><b>Count:</b> %{y:,}<extra></extra>"
      )
    ) |>
      plotly::layout(
        barmode = "stack",
        xaxis = list(
          title = "Type",
          categoryorder = "array",
          categoryarray = type_order
        ),
        yaxis = list(title = "Count"),
        legend = list(title = list(text = "School region"))
      )
  } else {
    primary_colour <- "#1d70b8"
    p_status <- plotly::plot_ly(
      status_counts,
      x = ~twn_status_name,
      y = ~count,
      type = "bar",
      marker = list(color = primary_colour),
      hovertemplate = paste(
        "<b>Status:</b> %{x}<br><b>Region:</b> ",
        htmltools::htmlEscape(region),
        "<br><b>Count:</b> %{y:,}<extra></extra>"
      )
    ) |>
      plotly::layout(
        xaxis = list(
          title = "Status",
          categoryorder = "array",
          categoryarray = status_order
        ),
        yaxis = list(title = "Count"),
        showlegend = FALSE
      )

    p_type <- plotly::plot_ly(
      type_counts,
      x = ~twn_type_name,
      y = ~count,
      type = "bar",
      marker = list(color = primary_colour),
      hovertemplate = paste(
        "<b>Type:</b> %{x}<br><b>Region:</b> ",
        htmltools::htmlEscape(region),
        "<br><b>Count:</b> %{y:,}<extra></extra>"
      )
    ) |>
      plotly::layout(
        xaxis = list(
          title = "Type",
          categoryorder = "array",
          categoryarray = type_order
        ),
        yaxis = list(title = "Count"),
        showlegend = FALSE
      )
  }

  heading <- if (is.null(region)) {
    "Warning Notice status & type"
  } else {
    glue::glue("Warning Notice status & type — {region}")
  }
  rand_id <- paste0(sample(c(letters, 0:9), 6, TRUE), collapse = "")
  dl_records_id <- paste0("wn_dl_records_csv_", rand_id)

  # Download
  session <- shiny::getDefaultReactiveDomain()
  if (!is.null(session)) {
    session$output[[dl_records_id]] <- shiny::downloadHandler(
      filename = function() {
        suffix <- if (is.null(region)) {
          "all_regions"
        } else {
          gsub("\\s+", "_", region)
        }
        paste0("wn_records_", suffix, "_", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        utils::write.csv(records_df, file, row.names = FALSE, na = "")
      }
    )
  }

  # Summary tables
  status_summary_table <- DT::datatable(
    status_counts,
    rownames = FALSE,
    options = list(pageLength = 10)
  )
  type_summary_table <- DT::datatable(
    type_counts,
    rownames = FALSE,
    options = list(pageLength = 10)
  )

  # Build table UI
  table_heading <- htmltools::div(
    style = "margin: 1rem 0 0.25rem 0; font-weight:600;",
    "Underlying records"
  )
  table_widget <- DT::datatable(
    records_df,
    escape = FALSE,
    rownames = FALSE,
    options = list(
      pageLength = 10,
      autoWidth = TRUE,
      order = list(list(6, "desc"))
    )
  )

  # --- Compose UI
  ui <- shinyGovstyle::gov_layout(
    size = "full",
    shinyGovstyle::heading_text(heading, size = "l"),
    shinyGovstyle::label_hint(
      "wn_st_hint",
      if (is.null(region)) {
        "Stacked by school region."
      } else {
        "Filtered by school region."
      }
    ),
    shiny::tabsetPanel(
      id = paste0("wn_tabs_", rand_id),
      type = "tabs",
      tabPanel(
        "Status Chart",
        shiny::fluidRow(
          width = 12,
          htmltools::div(
            style = "margin-bottom: 0.5rem; font-weight:600;",
            "By status"
          ),
          p_status
        ),
        shiny::fluidRow(
          width = 12,
          htmltools::div(
            style = "margin-top: 1rem; font-weight:600;",
            "Status Summary"
          ),
          status_summary_table
        )
      ),
      tabPanel(
        "Type Chart",
        shiny::fluidRow(
          width = 12,
          htmltools::div(
            style = "margin-bottom: 0.5rem; font-weight:600;",
            "By type"
          ),
          p_type
        ),
        shiny::fluidRow(
          width = 12,
          htmltools::div(
            style = "margin-top: 1rem; font-weight:600;",
            "Type Summary"
          ),
          type_summary_table
        )
      ),
      tabPanel(
        "Underlying Data",
        shiny::fluidRow(
          width = 12,
          htmltools::div(
            style = "margin: 0.5rem 0;",
            htmltools::tags$strong("Download all records: "),
            shiny::downloadButton(dl_records_id, "Records CSV")
          ),
          table_heading,
          table_widget
        )
      )
    )
  )

  end_time <- Sys.time()
  dauPortalTools::log_event(glue::glue(
    "Finished wn_render_status_type_charts in {round(difftime(end_time, start_time, units = 'secs'), 2)} seconds"
  ))
  return(ui)
}


wn_render_status_type_charts <- function(region = NULL) {
  start_time <- Sys.time()
  dauPortalTools::log_event(glue::glue(
    "Starting wn_render_status_type_charts with region: {region}"
  ))

  have_DT <- requireNamespace("DT", quietly = TRUE)

  conn <- sql_manager("dit")

  region_where <- if (!is.null(region)) {
    glue::glue_sql(" AND a.school_region = {region}", .con = conn)
  } else {
    DBI::SQL("")
  }

  schema_01a <- DBI::SQL(conf$schemas$db_schema_01a)

  rand_id <- paste0(sample(c(letters, 0:9), 6, TRUE), collapse = "")
  dl_records_id <- paste0("wn_dl_records_csv_", rand_id)

  # Download
  session <- shiny::getDefaultReactiveDomain()
  if (!is.null(session)) {
    session$output[[dl_records_id]] <- shiny::downloadHandler(
      filename = function() {
        suffix <- if (is.null(region)) {
          "all_regions"
        } else {
          gsub("\\s+", "_", region)
        }
        paste0("wn_records_", suffix, "_", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        utils::write.csv(records_df, file, row.names = FALSE, na = "")
      }
    )
  }
  sql_cmd <- glue::glue_sql(
    "
    SELECT
      a.twn_id,
      a.school_name,
      a.school_region,
      a.created_on,
      a.updated_on,
      s.twn_status_id,
      s.twn_status_name,
      t.twn_type_id,
      t.twn_type_name
    FROM {schema_01a}.[twn_all_notices] a
    LEFT JOIN {schema_01a}.[twn_type_config] t ON a.type_of_notice_id = t.twn_type_id
    LEFT JOIN {schema_01a}.[twn_status_config] s ON a.twn_status_id = s.twn_status_id
    WHERE a.twn_status_id <> 7
      {region_where}
  ",
    .con = conn
  )

  df <- tryCatch(DBI::dbGetQuery(conn, sql_cmd), error = function(e) {
    dauPortalTools::log_event(glue::glue(
      "Error fetching status/type data: {e$message}"
    ))
    NULL
  })

  if (is.null(df) || !nrow(df)) {
    heading <- if (is.null(region)) {
      "Warning Notice status & type"
    } else {
      glue::glue("Warning Notice status & type — {region}")
    }
    ui_empty <- shinyGovstyle::gov_layout(
      size = "two-thirds",
      shinyGovstyle::heading_text(heading, size = "l"),
      shinyGovstyle::label_hint("wn_st_hint", "No data available to plot.")
    )
    end_time <- Sys.time()
    dauPortalTools::log_event(glue::glue(
      "Finished wn_render_status_type_charts in {round(difftime(end_time, start_time, units = 'secs'), 2)} seconds"
    ))
    return(ui_empty)
  }

  # Prepare data
  df <- df |>
    dplyr::mutate(
      school_region = dplyr::coalesce(school_region, "(Unknown)"),
      twn_status_name = dplyr::coalesce(twn_status_name, "(Unknown)"),
      twn_type_name = dplyr::coalesce(twn_type_name, "(Unknown)")
    )

  records_df <- df |>
    dplyr::transmute(
      twn_id,
      school_name,
      school_region,
      status = twn_status_name,
      type = twn_type_name,
      created_on,
      updated_on,
      link = paste0(
        "https://rsconnect/rsc/warning-notice-portal/?wnid=",
        twn_id
      )
    ) |>
    dplyr::arrange(dplyr::desc(updated_on), dplyr::desc(created_on))

  records_df <- records_df |>
    dplyr::mutate(
      link = vapply(
        link,
        function(x) as.character(dauPortalTools::make_shiny_link(x, "Open")),
        character(1)
      )
    )

  # Counts
  if (is.null(region)) {
    status_counts <- df |>
      dplyr::count(twn_status_name, school_region, name = "count")
    type_counts <- df |>
      dplyr::count(twn_type_name, school_region, name = "count")
  } else {
    status_counts <- df |>
      dplyr::count(twn_status_name, name = "count") |>
      dplyr::arrange(dplyr::desc(count))
    type_counts <- df |>
      dplyr::count(twn_type_name, name = "count") |>
      dplyr::arrange(dplyr::desc(count))
  }

  # Summary tables
  status_summary_table <- DT::datatable(
    status_counts,
    rownames = FALSE,
    options = list(pageLength = 10)
  )
  type_summary_table <- DT::datatable(
    type_counts,
    rownames = FALSE,
    options = list(pageLength = 10)
  )

  # Build table UI
  table_heading <- htmltools::div(
    style = "margin: 1rem 0 0.25rem 0; font-weight:600;",
    "Underlying records"
  )
  table_widget <- DT::datatable(
    records_df,
    escape = FALSE,
    rownames = FALSE,
    options = list(
      pageLength = 10,
      autoWidth = TRUE,
      order = list(list(6, "desc"))
    )
  )

  status_per_region <- df |>
    summarise(count = dplyr::n(), .by = c(twn_status_name, school_region)) |>
    mutate(status_total = sum(count), .by = twn_status_name)

  type_per_region <- df |>
    summarise(count = dplyr::n(), .by = c(twn_type_name, school_region)) |>
    mutate(type_total = sum(count), .by = twn_type_name)

  govuk_region_names <- c(
    "East Midlands",
    "East of England",
    "London",
    "North East",
    "North West",
    "South East",
    "South West",
    "West Midlands",
    "Yorkshire and the Humber"
  )

  govuk_palette <- c(
    "#F0506B",
    "#F7921E",
    "#D446A1",
    "#00AAB9",
    "#73369B",
    "#0C9144",
    "#00468C",
    "#BF1E2E",
    "#8DC53E"
  )

  p_status <- ggplot2::ggplot(
    status_per_region,
    #filter(status_per_region, school_region %in% c("West Midlands", "East of England")),
    ggplot2::aes(
      forcats::fct_reorder(twn_status_name, status_total, .desc = TRUE),
      y = count,
      fill = school_region,
      text = glue::glue("Region: {school_region}\nSchools: {count}")
    )
  ) +
    ggplot2::geom_col() +
    ggplot2::scale_fill_manual(
      breaks = govuk_region_names,
      values = govuk_palette
    ) +
    ggplot2::labs(
      x = NULL,
      y = NULL,
      fill = NULL
    ) +
    ggplot2::scale_x_discrete(expand = c(0.15, 0), labels = function(x) {
      stringr::str_wrap(x, width = 10)
    }) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::theme_minimal()

  p_status <- plotly::ggplotly(p_status, tooltip = c("text")) |>
    plotly::layout(
      yaxis = list(fixedrange = TRUE),
      xaxis = list(fixedrange = TRUE)
    ) |>
    plotly::config(displayModeBar = FALSE) |>
    plotly::layout(
      legend = list(
        x = 0.65,
        y = 0.95,
        itemclick = FALSE,
        itemdoubleclick = FALSE
      )
    ) # puts legend inside chart to save space. fine if last two columns remain small!!

  p_type <- ggplot2::ggplot(
    type_per_region,
    #filter(status_per_region, school_region %in% c("West Midlands", "East of England")),
    ggplot2::aes(
      forcats::fct_reorder(twn_type_name, type_total),
      y = count,
      fill = school_region,
      text = glue::glue("Region: {school_region}\nSchools: {count}")
    )
  ) +
    ggplot2::geom_col() +
    ggplot2::scale_fill_manual(
      breaks = govuk_region_names,
      values = govuk_palette
    ) +
    ggplot2::labs(
      x = NULL,
      y = NULL,
      fill = NULL
    ) +
    ggplot2::scale_x_discrete(expand = c(0, 0), labels = function(x) {
      stringr::str_wrap(x, width = 30)
    }) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::theme_minimal() +
    ggplot2::coord_flip()

  p_type <- plotly::ggplotly(p_type, tooltip = c("text")) |>
    plotly::layout(
      yaxis = list(fixedrange = TRUE),
      xaxis = list(fixedrange = TRUE)
    ) |>
    plotly::config(displayModeBar = FALSE) |>
    plotly::layout(
      legend = list(
        x = 0.5,
        y = 0.85,
        itemclick = FALSE,
        itemdoubleclick = FALSE
      )
    )

  # --- Compose UI
  ui <- shinyGovstyle::gov_layout(
    size = "full",
    shinyGovstyle::label_hint(
      "wn_st_hint",
      if (is.null(region)) {
        "Stacked by school region."
      } else {
        "Filtered by school region."
      }
    ),
    shiny::tabsetPanel(
      id = paste0("wn_tabs_", rand_id),
      type = "tabs",
      tabPanel(
        "Status Chart",
        shiny::fluidRow(
          width = 12,
          htmltools::div(
            style = "margin-bottom: 0.5rem; font-weight:600;",
            "By status"
          ),
          p_status
        ),
        shiny::fluidRow(
          width = 12,
          htmltools::div(
            style = "margin-top: 1rem; font-weight:600;",
            "Status Summary"
          ),
          status_summary_table
        )
      ),
      tabPanel(
        "Type Chart",
        shiny::fluidRow(
          width = 12,
          htmltools::div(
            style = "margin-bottom: 0.5rem; font-weight:600;",
            "By type"
          ),
          p_type
        ),
        shiny::fluidRow(
          width = 12,
          htmltools::div(
            style = "margin-top: 1rem; font-weight:600;",
            "Type Summary"
          ),
          type_summary_table
        )
      ),
      tabPanel(
        "Underlying Data",
        shiny::fluidRow(
          width = 12,
          htmltools::div(
            style = "margin: 0.5rem 0;",
            htmltools::tags$strong("Download all records: "),
            shiny::downloadButton(dl_records_id, "Records CSV")
          ),
          table_widget
        )
      )
    )
  )

  end_time <- Sys.time()
  dauPortalTools::log_event(glue::glue(
    "Finished wn_render_status_type_charts in {round(difftime(end_time, start_time, units = 'secs'), 2)} seconds"
  ))
  return(ui)
}
