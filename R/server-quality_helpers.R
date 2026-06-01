#' Quality Table Module Server (Dashboard View)
#'
#' Orchestrates fetching and filtering the full suite of live quality issues
#' across the application dashboard.
#'
#' @param id Character scalar. Shiny module ID.
#' @param app_id Character scalar. The application identifier.
#' @export
server_quality_wrapper <- function(id, app_id) {
  shiny::moduleServer(id, function(input, output, session) {
    df <- shiny::reactiveVal(NULL)

    shiny::observeEvent(
      TRUE,
      {
        raw_ui <- quality_render_live(app_id = app_id)
        df(attr(raw_ui, "data"))
      },
      once = TRUE
    )

    filtered <- shiny::reactive({
      shiny::req(df())
      x <- df()

      if (
        !is.null(input$region_filter) &&
          input$region_filter != "All" &&
          "Region" %in% names(x)
      ) {
        x <- x[x$Region == input$region_filter, , drop = FALSE]
      }

      if (
        !is.null(input$with_rcs_filter) &&
          input$with_rcs_filter != "All" &&
          "With RCS?" %in% names(x)
      ) {
        x <- x[
          x$`With RCS?` == as.numeric(input$with_rcs_filter),
          ,
          drop = FALSE
        ]
      }

      if ("Description" %in% names(x)) {
        names(x)[names(x) == "Description"] <- "Steps to Fix"
      }

      for (col in c("Date Identified", "Last Reviewed")) {
        if (col %in% names(x)) {
          parsed <- tryCatch(as.Date(x[[col]]), error = function(e) NULL)
          if (!is.null(parsed)) {
            formatted <- format(parsed, "%d/%m/%Y")
            formatted[is.na(parsed)] <- ""
            x[[col]] <- formatted
          }
        }
      }

      x
    })

    output$filtered_table <- DT::renderDT({
      shiny::req(filtered())
      DT::datatable(
        filtered(),
        escape = FALSE,
        rownames = FALSE,
        options = list(pageLength = 15)
      )
    })
  })
}

#' Quality Issues Table Server for a Specific Record
#'
#' Module server that automatically handles app contexts, fetches quality issues
#' for a single specific record ID, and renders a clean datatable.
#'
#' @param id Character scalar. Shiny module ID.
#' @param record_id Reactive expression or numeric scalar. The active record ID to filter by.
#' @export
server_quality_record_table <- function(id, record_id) {
  shiny::moduleServer(id, function(input, output, session) {
    record_issues <- shiny::reactive({
      r_id <- if (shiny::is.reactive(record_id)) record_id() else record_id
      shiny::req(r_id)

      x <- quality_get_data(record = r_id)
      shiny::req(x)

      if ("Description" %in% names(x)) {
        names(x)[names(x) == "Description"] <- "Steps to Fix"
      }

      for (col in c("Date Identified", "Last Reviewed")) {
        if (col %in% names(x)) {
          parsed <- tryCatch(as.Date(x[[col]]), error = function(e) NULL)
          if (!is.null(parsed)) {
            formatted <- format(parsed, "%d/%m/%Y")
            formatted[is.na(parsed)] <- ""
            x[[col]] <- formatted
          }
        }
      }

      x
    })

    output$quality_table <- DT::renderDT({
      shiny::req(record_issues())

      DT::datatable(
        record_issues(),
        escape = FALSE,
        rownames = FALSE,
        options = list(
          pageLength = 5,
          dom = 't',
          autoWidth = TRUE
        )
      )
    })
  })
}

#' Quality Table Module Server (Dashboard View)
#'
#' Orchestrates fetching, filtering, column pruning, and deep-linking layout configuration
#' for the active quality issues dashboard view.
#'
#' @param id Character scalar. Shiny module ID.
#' @param app_id Character scalar. The application identifier.
#' @export
server_quality_wrapper <- function(id, app_id) {
  shiny::moduleServer(id, function(input, output, session) {
    df <- shiny::reactiveVal(NULL)

    shiny::observeEvent(
      TRUE,
      {
        raw_ui <- quality_render_live(app_id = app_id)
        df(attr(raw_ui, "data"))
      },
      once = TRUE
    )

    filtered <- shiny::reactive({
      shiny::req(df())
      x <- df()

      if (
        !is.null(input$region_filter) &&
          input$region_filter != "All" &&
          "Region" %in% names(x)
      ) {
        x <- x[x$Region == input$region_filter, , drop = FALSE]
      }

      if (
        !is.null(input$with_rcs_filter) &&
          input$with_rcs_filter != "All" &&
          "With RCS?" %in% names(x)
      ) {
        x <- x[
          x$`With RCS?` == as.numeric(input$with_rcs_filter),
          ,
          drop = FALSE
        ]
      }

      shiny::req(nrow(x) > 0)

      id_col <- intersect(
        names(x),
        c("sig_change_id", "Significant Change ID", "Record ID", "id")
      )[1]
      urn_col <- intersect(names(x), c("URN", "urn"))[1]
      issue_col <- intersect(
        names(x),
        c("Issue", "Quality Issue", "Description", "Steps to Fix", "issue")
      )[1]

      if (is.na(id_col)) {
        x$sig_change_id <- "N/A"
        id_col <- "sig_change_id"
      }
      if (is.na(urn_col)) {
        x$URN <- "N/A"
        urn_col <- "URN"
      }
      if (is.na(issue_col)) {
        x$Issue <- "N/A"
        issue_col <- "Issue"
      }

      links <- sapply(x[[id_col]], function(id) {
        if (is.na(id) || id == "N/A" || !nzchar(id)) {
          return("#")
        }
        scp_sc_url(id)
      })

      x$Actions <- sprintf(
        "<div class='govuk-button-group' style='margin:0; display:flex; gap:12px; align-items:center;'>
           <a href='%s' class='govuk-link' style='font-weight:700; white-space:nowrap;'>View Record</a>
           <button type='button' class='govuk-button govuk-button--secondary dt-copy-btn' data-copy-text='%s' style='margin:0; padding:4px 8px; font-size:13px; white-space:nowrap;'>Copy Link</button>
         </div>",
        links,
        links
      )

      clean_df <- data.frame(
        `URN` = x[[urn_col]],
        `Significant Change ID` = x[[id_col]],
        `Quality Issue` = x[[issue_col]],
        `Actions / Sharing` = x$Actions,
        check.names = FALSE,
        stringsAsFactors = FALSE
      )

      clean_df
    })

    output$filtered_table <- DT::renderDT({
      shiny::req(filtered())

      DT::datatable(
        filtered(),
        escape = FALSE,
        rownames = FALSE,
        options = list(
          pageLength = 15,
          autoWidth = TRUE,
          columnDefs = list(
            list(className = 'dt-left', targets = "_all"),
            list(width = '180px', targets = 3)
          )
        ),
        callback = DT::JS(
          "table.on('click', '.dt-copy-btn', function(e) {
             e.preventDefault();
             e.stopPropagation();
             var text = $(this).attr('data-copy-text');
             var $btn = $(this);
             
             if (text && text !== '#') {
               navigator.clipboard.writeText(text).then(function() {
                 var originalText = $btn.text();
                 $btn.text('Copied!').css('background-color', '#00703c').css('color', '#ffffff');
                 setTimeout(function() {
                   $btn.text(originalText).css('background-color', '').css('color', '');
                 }, 1500);
               });
             }
           });"
        )
      )
    })
  })
}
