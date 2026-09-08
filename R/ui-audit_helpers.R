#' Render Audit Record Status Metadata Banner
#'
#' Takes creation and modification audit values and formats them into a
#' standard GDS-styled inset metadata panel.
#'
#' @param cuid Integer/Character. Created user ID or email.
#' @param cdate Date/POSIXct/Character. Creation timestamp.
#' @param euid Integer/Character. Edited/Modified user ID or email.
#' @param edate Date/POSIXct/Character. Edited/Modified timestamp.
#' @param session Shiny session object (optional).
#' @return A shiny.tag HTML element.
#' @export
ui_render_record_metadata_banner <- function(
  cuid = NULL,
  cdate = NULL,
  euid = NULL,
  edate = NULL,
  session = NULL
) {
  format_dt <- function(dt) {
    if (is.null(dt) || is.na(dt) || identical(trimws(as.character(dt)), "")) {
      return("N/A")
    }
    parsed <- suppressWarnings(as.POSIXct(dt))
    if (is.na(parsed)) {
      return(as.character(dt))
    }
    format(parsed, "%d %b %Y, %H:%M")
  }

  htmltools::tags$div(
    class = "govuk-inset-text audit-metadata-banner",
    style = paste(
      "margin-top: 10px;",
      "margin-bottom: 15px;",
      "padding: 10px 15px;",
      "border-left: 5px solid #1d70b8;",
      "background-color: #f8f9fa;",
      "font-size: 0.95rem;",
      "color: #0b0c0c;"
    ),
    htmltools::tags$table(
      style = "width: 100%; border-collapse: collapse;",
      htmltools::tags$tr(
        htmltools::tags$td(
          style = "width: 50%; vertical-align: top; padding-right: 10px;",
          htmltools::tags$strong("Created by: "),
          ui_audit_user_badge(cuid, session = session),
          htmltools::tags$br(),
          htmltools::tags$span(
            style = "color: #505a5f; font-size: 0.85rem;",
            htmltools::tags$strong("Created on: "),
            format_dt(cdate)
          )
        ),
        htmltools::tags$td(
          style = "width: 50%; vertical-align: top; padding-left: 10px; border-left: 1px solid #bfc1c3;",
          htmltools::tags$strong("Last edited by: "),
          ui_audit_user_badge(euid, session = session),
          htmltools::tags$br(),
          htmltools::tags$span(
            style = "color: #505a5f; font-size: 0.85rem;",
            htmltools::tags$strong("Last edited on: "),
            format_dt(edate)
          )
        )
      )
    )
  )
}

#' Resolve Audit User Display Element
#'
#' Resolves a user ID to a presentation tag, prioritizing email over username.
#' Returns a shiny.tag element with metadata attributes so it can later be converted
#' into a clickable profile link without changing calling code.
#'
#' @param user_id Integer/Character. Primary key of user or raw identifier.
#' @param session Shiny session object (optional, for future actionLink bindings).
#' @return A shiny.tag HTML element (currently `tags$span`).
#' @export
ui_audit_user_badge <- function(user_id, session = NULL) {
  if (
    is.null(user_id) ||
      is.na(user_id) ||
      identical(trimws(as.character(user_id)), "")
  ) {
    return(htmltools::tags$span(class = "audit-user-empty", "N/A"))
  }

  # If an email/raw string is passed directly instead of an ID
  if (is.character(user_id) && grepl("@", user_id)) {
    display_name <- trimws(user_id)
    uid <- NA_integer_
  } else {
    uid <- suppressWarnings(as.integer(user_id))
    display_name <- paste0("User #", user_id)

    if (!is.na(uid) && uid > 0) {
      res <- tryCatch(
        db_get_user_by_id(uid),
        error = function(e) NULL
      )

      if (!is.null(res) && nrow(res) > 0) {
        # Priority: Email first, then Username, then fallback
        if (!is.null(res$email) && nzchar(trimws(res$email[1]))) {
          display_name <- trimws(res$email[1])
        } else if (!is.null(res$username) && nzchar(trimws(res$username[1]))) {
          display_name <- trimws(res$username[1])
        }
      }
    }
  }

  # Currently a styled span. Later you can swap this inner return to:
  # shiny::actionLink(inputId = paste0("audit_user_", uid), label = display_name, ...)
  # or htmltools::tags$a(href = paste0("#profile?uid=", uid), display_name)
  htmltools::tags$span(
    class = "audit-user-badge",
    `data-user-id` = if (!is.na(uid)) uid else NULL,
    style = "font-weight: 500; color: inherit;",
    display_name
  )
}
