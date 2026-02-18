#' Render School Overview UI
#'
#' Generates a GOV.UK-styled tabbed UI layout displaying school summary information
#' for a given URN, including school details, trust details, additional metadata,
#' and useful external links (e.g., Ofsted, GIAS).
#'
#' @param urn A character string representing the Unique Reference Number (URN) of a school.
#'
#' @return A Shiny UI object containing the school overview layout. If no data is found,
#' returns a message indicating that no data is available.
#'
#' @details
#' This function:
#' - Connects to the `dit` SQL Server database.
#' - Queries the latest Edubase record for the given URN.
#' - Constructs a tabbed UI layout using `shinyGovstyle::gov_summary`.
#'
#' @examples
#' \dontrun{
#' # Run interactively in a Shiny app
#' if (interactive()) {
#'   library(shiny)
#'   library(shinyGovstyle)
#'   library(dfeshiny)
#'   library(bslib)
#'
#'   ui <- fluidPage(
#'     uiOutput("school_overview")
#'   )
#'
#'   server <- function(input, output, session) {
#'     output$school_overview <- renderUI({
#'       school_render_overview(urn = "123456") # Replace with a valid URN
#'     })
#'   }
#'
#'   shinyApp(ui, server)
#' }
#' }
#' @export
#'

school_render_overview <- function(urn) {
  start_time <- Sys.time()
  log_event(glue::glue(
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
      log_event(glue::glue(
        "Error fetching summary: {e$message}"
      ))
      return(data.frame(urn = NA, school_name = NA))
    }
  )

  if (nrow(summary_data) == 0 || is.na(summary_data$urn[1])) {
    return(shinyGovstyle::gov_layout(
      shinyGovstyle::heading_text("School Overview", size = "l"),
      shiny::HTML("<p>No data available for this URN.</p>")
    ))
  }

  # Prepare tab data
  tabs <- list()

  # School Details
  tabs[["School Details"]] <- list(
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
    tabs[["School Details"]] <- c(
      tabs[["School Details"]],
      "Closed" = format(
        as.Date(summary_data$close_date, origin = "1970-01-01"),
        "%d-%m-%Y"
      ),
      "Reason" = summary_data$reason_closed
    )
  }

  # Trust Details
  if (!is.na(summary_data$trust_ref)) {
    tabs[["Trust Details"]] <- list(
      "Trust Ref" = summary_data$trust_ref,
      "Trust Name" = summary_data$trust_name
    )
  } else {
    tabs[["Trust Details"]] <- NULL
  }

  # More Details
  tabs[["More Details"]] <- list(
    "Phase" = summary_data$phase,
    "Religious" = summary_data$religious_character,
    "Diocese" = summary_data$diocese_name,
    "Gender" = summary_data$gender,
    "Pupils" = prettyNum(summary_data$pupil_number, big.mark = ","),
    "% FSM" = summary_data$perc_fsm
  )

  links <- c(
    slic_urn_url(summary_data$urn),
    summary_data$school_website,
    ofsted_url(summary_data$urn),
    gias_school_url(summary_data$urn)
  )

  link_names <- c(
    paste("SLIC"),
    paste("Website"),
    paste("Ofsted Reports"),
    paste("GIAS")
  )

  # Add Trust link if available
  if (
    !is.na(summary_data$trust_ref) &&
      summary_data$trust_ref != "" &&
      !is.na(summary_data$trust_name) &&
      summary_data$trust_name != ""
  ) {
    links <- c(
      links,
      gias_trust_url(summary_data$trust_ref)
    )
    link_names <- c(link_names, paste(summary_data$trust_name, "GIAS Trust"))
  }

  # Name the links vector
  names(links) <- link_names

  # Convert to shiny links
  tabs[["Important Links"]] <- make_shiny_links(links)

  # Remove empty tabs
  tabs <- tabs[!sapply(tabs, is.null)]

  # Create tab buttons
  tab_buttons <- shiny::tags$div(
    class = "custom-tabs-buttons",
    lapply(names(tabs), function(tab) {
      shiny::tags$button(
        class = if (tab == names(tabs)[1]) {
          "custom-tab-btn active"
        } else {
          "custom-tab-btn"
        },
        `data-tab` = tab,
        tab
      )
    })
  )

  # Create tab contents with gov_summary
  tab_contents <- shiny::tags$div(
    class = "custom-tabs-contents",
    lapply(names(tabs), function(tab) {
      shiny::tags$div(
        class = if (tab == names(tabs)[1]) {
          "custom-tab-content active"
        } else {
          "custom-tab-content"
        },
        `data-tab` = tab,
        shinyGovstyle::gov_summary(
          inputId = paste0("sum_", gsub(" ", "_", tolower(tab))),
          headers = names(tabs[[tab]]),
          info = unname(tabs[[tab]]),
          border = TRUE
        )
      )
    })
  )

  # Vanilla JS for tabs
  tab_js <- shiny::tags$script(shiny::HTML(
    "
  (function() {
    function attachTabHandlers() {
      const buttons = document.querySelectorAll('.custom-tab-btn');
      if (!buttons.length) { setTimeout(attachTabHandlers, 50); return; }
      buttons.forEach(btn => {
        btn.addEventListener('click', function() {
          const target = this.dataset.tab;
          document.querySelectorAll('.custom-tab-btn').forEach(b => b.classList.remove('active'));
          document.querySelectorAll('.custom-tab-content').forEach(c => c.classList.remove('active'));
          this.classList.add('active');
          document.querySelector(`.custom-tab-content[data-tab='${target}']`).classList.add('active');
        });
      });
    }
    attachTabHandlers();
  })();
  "
  ))

  # CSS for UKGov-ish look
  tab_css <- shiny::tags$style(shiny::HTML(
    "
.custom-tabs-buttons ~ .custom-tabs-contents .govuk-heading-l,
.govuk-heading-l {
  margin-bottom: 14px !important;
}

.custom-tab-content {
  padding-top: 6px !important;
  padding-bottom: 6px !important; 
}

.custom-tab-content .govuk-summary-list {
  margin-bottom: 8px !important; 
}

.custom-tab-content .govuk-summary-list__row {
  margin: 0 !important;
  padding-top: 2px !important;
  padding-bottom: 2px !important;
}

.custom-tab-content .govuk-summary-list__row:nth-child(even) {
  background-color: #f3f2f1;
}

.custom-tab-content.active > :last-child {
  margin-bottom: 0 !important;
  padding-bottom: 0 !important;
}

.custom-tab-content.active .govuk-summary-list:last-child {
  margin-bottom: 0 !important;
}

.custom-tabs-buttons {
  margin-top: 8px !important;
  margin-bottom: 8px !important;
}

.custom-tabs-contents {
  margin-bottom: 0 !important;
}

"
  ))

  ui <- shinyGovstyle::gov_layout(
    shinyGovstyle::heading_text(
      glue::glue("{summary_data$school_name} ({summary_data$urn})"),
      size = "l"
    ),
    tab_css,
    tab_buttons,
    tab_contents,
    tab_js
  )

  log_event(glue::glue(
    "Finished school_render_overview in {round(difftime(Sys.time(), start_time, units = 'secs'), 2)} seconds"
  ))

  ui
}
