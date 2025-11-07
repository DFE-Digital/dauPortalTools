#' Render School Overview Panel
#'
#' Creates a GOV.UK-styled tabbed panel displaying key school information grouped into sections:
#' - **School Details**: Basic identifiers and status (URN, name, LA, region, type, open/close dates).
#' - **Trust Details**: Trust reference and name (only shown if the school is part of a trust).
#' - **More School Details**: Phase, religious character, diocese, gender, pupil numbers, FSM percentage.
#' - **Important Links**: School website, Ofsted inspection link, and GIAS links for school and trust.
#'
#' This UI fragment can be inserted anywhere in a Shiny app (e.g., inside `uiOutput()` / `renderUI()`).
#'
#' @param urn Character scalar. The unique URN of the school to display.
#'
#' @return A `shiny.tag` object representing a GOV.UK-styled tabbed layout suitable for use in `renderUI()`.
#'
#' @details
#' The function:
#' - Queries the Edubase table using `DBI::dbGetQuery()` and a connection from `sql_manager("dit")`.
#' - Uses `glue_sql()` for safe SQL interpolation.
#' - Dynamically builds a long-format data frame grouped by tab names.
#' - Converts this data into `shinyGovstyle::govTabs()` panels, with clickable external links opening in a new tab.
#'
#' If the query fails or returns no data, the UI will display a fallback heading and empty tabs.
#'
#' Styling is provided by `shinyGovstyle` components (`gov_layout()`, `heading_text()`, `govTabs()`).
#'
#' @examples
#' \dontrun{
#' # --- Minimal Shiny example -------------------------------------
#' if (interactive()) {
#'   library(shiny)
#'   library(shinyGovstyle)
#'
#'   ui <- fluidPage(
#'     uiOutput("school_overview")
#'   )
#'
#'   server <- function(input, output, session) {
#'     output$school_overview <- renderUI({
#'       school_render_overview(urn = "123456")  # Replace with a valid URN
#'     })
#'   }
#'
#'   shinyApp(ui, server)
#' }
#' }
#'
#' @seealso \code{\link[shinyGovstyle]{govTabs}}, \code{\link[shinyGovstyle]{gov_layout}}
#' @export
#'

school_render_overview <- function(urn) {
  start_time <- Sys.time()
  dauPortalTools::log_event(glue::glue(
    "Starting school_render_overview with urn: {urn}"
  ))

  # --- Fetch Data ---
  conn <- sql_manager("dit")

  sql_command <- glue::glue_sql(
    "
    SELECT URN AS urn
    ,[EstablishmentName] AS school_name
    ,[LA (name)] AS la
    ,[GOR (name)] AS region
    ,[TypeOfEstablishment (name)] AS school_type
    ,[EstablishmentTypeGroup (name)] AS school_type_group
    ,[OpenDate] AS open_date
    ,[CloseDate] AS close_date
    ,[ReasonEstablishmentClosed (name)] AS reason_closed
    ,[PhaseOfEducation (name)] AS phase
    ,[ReligiousCharacter (name)] AS religious_character
    ,[Diocese (name)] AS diocese_name
    ,[Gender (name)] AS gender
    ,[NumberOfPupils] AS pupil_number
    ,[PercentageFSM] AS perc_fsm
    ,[Trusts (code)] AS trust_ref
    ,[Trusts (name)] AS trust_name
    ,[SchoolWebsite] AS school_website
    FROM {config$database}.{config$db_schema_00c}.[Edubase]
    WHERE URN = {urn}
    AND [DateStamp] = (SELECT MAX(DateStamp) FROM {config$database}.{config$schema$db_schema_00c}.[Edubase])
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
        urn = NA,
        school_name = NA
      ))
    }
  )

  # Build Long Format Data
  urn_val <- summary_data$urn[1]
  trust_ref_val <- summary_data$trust_ref[1]

  rows <- list()

  # School Details
  school_details <- c(
    urn = summary_data$urn,
    school_name = summary_data$school_name,
    la = summary_data$la,
    region = summary_data$region,
    school_type = summary_data$school_type,
    school_type_group = summary_data$school_type_group,
    open_date = summary_data$open_date
  )
  if (!is.na(summary_data$close_date)) {
    school_details <- c(
      school_details,
      close_date = summary_data$close_date,
      reason_closed = summary_data$reason_closed
    )
  }
  rows[["School Details"]] <- school_details

  # Ofsted Tab
  rows[["Ofsted"]] <- c(status = "Coming Soon")

  # Trust Details
  if (!is.na(trust_ref_val)) {
    trust_details <- c(
      trust_ref = summary_data$trust_ref,
      trust_name = summary_data$trust_name
    )
    rows[["Trust Details"]] <- trust_details
  }

  # More School Details
  more_details <- c(
    phase = summary_data$phase,
    religious_character = summary_data$religious_character,
    diocese_name = summary_data$diocese_name,
    gender = summary_data$gender,
    pupil_number = summary_data$pupil_number,
    perc_fsm = summary_data$perc_fsm
  )
  rows[["More School Details"]] <- more_details

  # Important Links
  important_links <- c(
    school_website = summary_data$school_website,
    ofsted_link = paste0(
      "http://www.ofsted.gov.uk/inspection-reports/find-inspection-report/provider/ELS/",
      urn_val
    ),
    gis_school_link = paste0(
      "https://get-information-schools.service.gov.uk/Establishments/Establishment/Details/",
      urn_val
    )
  )
  if (!is.na(trust_ref_val)) {
    important_links <- c(
      important_links,
      gis_trust_link = paste0(
        "https://get-information-schools.service.gov.uk/Groups/Group/Details/",
        trust_ref_val
      )
    )
  }
  rows[["Important Links"]] <- important_links

  final_df <- do.call(
    rbind,
    lapply(names(rows), function(tab) {
      data.frame(
        tabs = tab,
        field = names(rows[[tab]]),
        value = unname(rows[[tab]]),
        stringsAsFactors = FALSE
      )
    })
  )

  # Prepare UI
  heading <- if (!is.null(summary_data$school_name)) {
    glue::glue("{summary_data$school_name} ({urn_val})")
  } else {
    "Summary"
  }

  # Split by tab for govTabs
  tab_list <- split(final_df, final_df$tabs)

  gov_tabs_ui <- shinyGovstyle::govTabs(
    inputId = "summary_table",
    tabs = lapply(names(tab_list), function(tab_name) {
      data <- tab_list[[tab_name]]

      # Make links clickable for Important Links
      if (tab_name == "Important Links") {
        data$value <- ifelse(
          grepl("^http", data$value),
          paste0(
            "<a href='",
            data$value,
            "' target='_blank'>",
            data$field,
            "</a>"
          ),
          data$value
        )
      }

      shiny::tabPanel(
        title = tab_name,
        shiny::HTML(paste0(
          "<ul>",
          paste0(
            "<li><strong>",
            data$field,
            ":</strong> ",
            data$value,
            "</li>",
            collapse = ""
          ),
          "</ul>"
        ))
      )
    })
  )

  ui <- shinyGovstyle::gov_layout(
    size = "two-thirds",
    shinyGovstyle::heading_text(heading, size = "l"),
    shinyGovstyle::label_hint(
      "summary_label",
      "Key metrics for Warning Notices"
    ),
    gov_tabs_ui
  )

  end_time <- Sys.time()
  dauPortalTools::log_event(glue::glue(
    "Finished school_render_overview in {round(difftime(end_time, start_time, units = 'secs'), 2)} seconds"
  ))

  return(ui)
}
