#' Render Parent Entity Overview Panel
#'
#' Generates a GOV.UK-styled summary layout tracking aggregated operational
#' metrics across individual grouping tiers (Trusts, Local Authorities, or Dioceses).
#'
#' @param entity_type Character scalar. Must be one of: \code{"trust"}, \code{"la"}, or \code{"diocese"}.
#' @param entity_id Character or numeric scalar. The unique identifier code matching the chosen entity type.
#' @param id Character scalar. UI container ID namespace prefix.
#' @export
entity_render_overview <- function(
  entity_type = c("trust", "la", "diocese"),
  entity_id,
  id = paste0("entity_overview_", entity_type, "_", entity_id)
) {
  start_time <- Sys.time()
  entity_type <- match.arg(entity_type)
  log_event(glue::glue(
    "Starting entity_render_overview for {entity_type} with ID: {entity_id}"
  ))

  conn <- sql_manager("dit")
  on.exit(
    {
      try(DBI::dbDisconnect(conn), silent = TRUE)
    },
    add = TRUE
  )

  schema <- utils_resolve_schema("db_schema_00c")

  # ------------------------------------------------------------------
  # 1. Pipeline Routing: Build Custom Polymorphic SQL Aggregates
  # ------------------------------------------------------------------
  sql_command <- if (entity_type == "trust") {
    glue::glue_sql(
      "
      SELECT 
        MAX([Trusts (name)]) AS entity_name,
        COUNT(CASE WHEN [CloseDate] IS NULL THEN 1 END) AS open_schools_count
      FROM {schema}.[Edubase]
      WHERE [Trusts (code)] = {entity_id}
        AND [DateStamp] = (SELECT MAX(DateStamp) FROM {schema}.[Edubase])
      ",
      .con = conn
    )
  } else if (entity_type == "la") {
    glue::glue_sql(
      "
      SELECT 
        MAX([LA (name)]) AS entity_name,
        COUNT(CASE WHEN [CloseDate] IS NULL THEN 1 END) AS open_schools_count,
        COUNT(DISTINCT NULLIF([Trusts (code)], '0')) AS associated_trusts_count
      FROM {schema}.[Edubase]
      WHERE [LA (code)] = {entity_id}
        AND [DateStamp] = (SELECT MAX(DateStamp) FROM {schema}.[Edubase])
      ",
      .con = conn
    )
  } else if (entity_type == "diocese") {
    glue::glue_sql(
      "
      SELECT 
        MAX([Diocese (name)]) AS entity_name,
        COUNT(CASE WHEN [CloseDate] IS NULL THEN 1 END) AS open_schools_count,
        (
          SELECT STRING_AGG(CAST(t.t_name AS VARCHAR(MAX)), ', ') WITHIN GROUP (ORDER BY t.t_name)
          FROM (
            SELECT DISTINCT [Trusts (name)] AS t_name
            FROM {schema}.[Edubase]
            WHERE [Diocese (code)] = {entity_id} 
              AND NULLIF([Trusts (name)], '') IS NOT NULL
              AND [DateStamp] = (SELECT MAX(DateStamp) FROM {schema}.[Edubase])
          ) t
        ) AS associated_trusts_list
      FROM {schema}.[Edubase]
      WHERE [Diocese (code)] = {entity_id}
        AND [DateStamp] = (SELECT MAX(DateStamp) FROM {schema}.[Edubase])
      ",
      .con = conn
    )
  }

  entity_data <- tryCatch(
    DBI::dbGetQuery(conn, sql_command),
    error = function(e) {
      log_event(glue::glue("Error fetching entity overview: {e$message}"))
      NULL
    }
  )

  if (
    is.null(entity_data) ||
      nrow(entity_data) == 0 ||
      is.na(entity_data$entity_name[1])
  ) {
    return(
      shiny::div(
        id = id,
        shinyGovstyle::heading_text(
          paste(tools::toTitleCase(entity_type), "Overview"),
          size = "l"
        ),
        shiny::HTML(
          "<p class='govuk-body'>No operational directory data found for this identifier context.</p>"
        )
      )
    )
  }

  # ------------------------------------------------------------------
  # 2. Map Layout Parameters and Information Metrics Cards
  # ------------------------------------------------------------------
  summary_headers <- c("Total Active Open Schools")
  summary_info <- c(prettyNum(
    entity_data$open_schools_count[1],
    big.mark = ","
  ))

  if (entity_type == "la") {
    summary_headers <- c(summary_headers, "Unique Operating MAT Trusts")
    summary_info <- c(
      summary_info,
      prettyNum(entity_data$associated_trusts_count[1], big.mark = ",")
    )
  }

  summary_list_ui <- shinyGovstyle::gov_summary(
    inputId = paste0(id, "_metrics_list"),
    headers = summary_headers,
    info = summary_info,
    border = TRUE
  )

  supplementary_ui <- NULL
  if (entity_type == "diocese") {
    trust_string <- entity_data$associated_trusts_list[1]

    supplementary_ui <- tags$div(
      class = "govuk-!-margin-top-4",
      style = "border-top: 2px solid #b1b4b6; padding-top: 15px;",
      tags$h3(
        class = "govuk-heading-m",
        "Associated Trust Footprint Directory"
      ),
      if (!is.na(trust_string) && nzchar(trust_string)) {
        tags$ul(
          class = "govuk-list govuk-list--bullet",
          lapply(strsplit(trust_string, ", ")[[1]], function(trust) {
            tags$li(class = "govuk-body", trust)
          })
        )
      } else {
        p(em(
          class = "text-muted",
          "No corporate multi-academy trusts currently registered or logged under this diocese education code profile."
        ))
      }
    )
  }

  ui_layout <- shinyGovstyle::gov_layout(
    shinyGovstyle::heading_text(
      glue::glue("{entity_data$entity_name} ({entity_id})"),
      size = "l"
    ),
    shiny::tags$span(
      class = "govuk-caption-m govuk-!-margin-bottom-4",
      paste("System Tier Record Context:", tools::toTitleCase(entity_type))
    ),
    shiny::div(
      class = "govuk-!-margin-top-3",
      summary_list_ui,
      supplementary_ui
    )
  )

  log_event(glue::glue(
    "Finished entity_render_overview for {entity_type} in {round(difftime(Sys.time(), start_time, units = 'secs'), 2)} seconds"
  ))

  shiny::div(
    id = id,
    class = "entity-overview-wrapper govuk-!-margin-top-4",
    ui_layout
  )
}
