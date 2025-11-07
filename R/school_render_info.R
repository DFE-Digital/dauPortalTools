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

  conn <- sql_manager("dit")
  db_schema_00c <- DBI::SQL(conf$schemas$db_schema_00c)

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
    FROM {db_schema_00c}.[Edubase]
    WHERE URN = {urn}
      AND [DateStamp] = (SELECT MAX(DateStamp) FROM {db_schema_00c}.[Edubase])
  ",
    .con = conn
  )

  summary_data <- tryCatch(
    DBI::dbGetQuery(conn, sql_command),
    error = function(e) {
      dauPortalTools::log_event(glue::glue(
        "Error fetching summary: {e$message}"
      ))
      return(data.frame(urn = NA, school_name = NA))
    }
  )

  if (nrow(summary_data) == 0 || is.na(summary_data$urn[1])) {
    return(shinyGovstyle::gov_layout(
      size = "two-thirds",
      shinyGovstyle::heading_text("School Overview", size = "l"),
      shiny::HTML("<p>No data available for this URN.</p>")
    ))
  }

  urn_val <- summary_data$urn[1]
  trust_ref_val <- summary_data$trust_ref[1]

  rows <- list()

  rows[["School Details"]] <- c(
    "URN" = urn_val,
    "Name" = summary_data$school_name,
    "LA" = summary_data$la,
    "Region" = summary_data$region,
    "Type" = summary_data$school_type,
    "Group" = summary_data$school_type_group,
    "Opened" = format(
      as.Date(summary_data$open_date, origin = "1970-01-01"),
      "%d-%m-%Y"
    )
  )
  if (!is.na(summary_data$close_date)) {
    rows[["School Details"]] <- c(
      rows[["School Details"]],
      "Closed" = format(
        as.Date(summary_data$close_date, origin = "1970-01-01"),
        "%d-%m-%Y"
      ),
      "Reason" = summary_data$reason_closed
    )
  }

  rows[["Ofsted"]] <- c("Status" = "Coming Soon")

  if (!is.na(trust_ref_val)) {
    rows[["Trust Details"]] <- c(
      "Trust Ref" = trust_ref_val,
      "Trust Name" = summary_data$trust_name
    )
  }

  rows[["More Details"]] <- c(
    "Phase" = summary_data$phase,
    "Religious" = summary_data$religious_character,
    "Diocese" = summary_data$diocese_name,
    "Gender" = summary_data$gender,
    "Pupils" = prettyNum(summary_data$pupil_number, big.mark = ","),
    "% FSM" = summary_data$perc_fsm
  )

  links <- c(
    "Website" = summary_data$school_website,
    "Ofsted" = paste0(
      "http://www.ofsted.gov.uk/inspection-reports/find-inspection-report/provider/ELS/",
      urn_val
    ),
    "GIAS School" = paste0(
      "https://get-information-schools.service.gov.uk/Establishments/Establishment/Details/",
      urn_val
    )
  )
  if (!is.na(trust_ref_val)) {
    links <- c(
      links,
      "GIAS Trust" = paste0(
        "https://get-information-schools.service.gov.uk/Groups/Group/Details/",
        trust_ref_val
      )
    )
  }
  rows[["Important Links"]] <- links

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

  final_df$value <- ifelse(
    grepl("^http", final_df$value),
    paste0(
      "<a href='",
      final_df$value,
      "' target='_blank'>",
      final_df$field,
      "</a>"
    ),
    final_df$value
  )

  ui <- shinyGovstyle::gov_layout(
    size = "two-thirds",
    shinyGovstyle::heading_text(
      glue::glue("{summary_data$school_name} ({urn_val})"),
      size = "l"
    ),
    shinyGovstyle::label_hint("school_overview_hint", "Key school information"),
    shinyGovstyle::govTabs(
      inputId = "school_tabs",
      df = final_df,
      group_col = "tabs"
    )
  )

  dauPortalTools::log_event(glue::glue(
    "Finished school_render_overview in {round(difftime(Sys.time(), start_time, units = 'secs'), 2)} seconds"
  ))
  return(ui)
}
