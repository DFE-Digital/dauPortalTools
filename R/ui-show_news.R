#' Render Portal News and Alerts Feed
#'
#' Displays a vertical feed of portal news and alerts within a scrollable sidebar card.
#' Messages are prioritized by severity and visually highlighted using DfE-consistent
#' styling.
#'
#' @details
#' This function generates a Zero-Dependency UI component using standard HTML5
#' tags (`<details>` and `<summary>`). It is designed to fit into narrow layout
#' columns (e.g., a width-4 sidebar) and includes:
#' \itemize{
#'   \item \strong{Priority Sorting:} Critical messages (Priority 1) appear at the top.
#'   \item \strong{Visual Cues:} Priority-based background colors (Red/Amber/White).
#'   \item \strong{Temporal Badges:} A "NEW" badge for messages posted within the last 48 hours.
#'   \item \strong{Smart Truncation:} Summary text is truncated for the sidebar view,
#'         with full content available on expansion.
#'   \item \strong{Contained Layout:} Uses `bslib::card` with `height = "fit-content"`
#'         to prevent vertical layout gaps.
#' }
#'
#' @section Side Effects:
#' \itemize{
#'   \item Executes database queries via [db_get_portal_messages()].
#'   \item Gracefully handles database connection errors by returning a fallback message.
#' }
#'
#' @return A `bslib::card` object containing a list of news items.
#'         If no data is found, returns a `shiny::tags$p` informative message.
#'
#' @examples
#' \dontrun{
#' # In UI:
#' uiOutput("portal_news")
#'
#' # In Server:
#' output$portal_news <- renderUI({
#'   ui_show_news()
#' })
#' }
#'
#' @seealso [db_get_portal_messages()]
#'
#' @export

ui_show_news <- function() {
  df <- tryCatch(
    {
      db_get_portal_messages()
    },
    error = function(e) {
      NULL
    }
  )

  if (is.null(df) || nrow(df) == 0) {
    return(shiny::tags$p(
      "No news or alerts available.",
      style = "color: #505a5f;"
    ))
  }

  now_utc <- Sys.time()
  df$message_date <- as.POSIXct(df$message_date, tz = "UTC")
  df <- df[order(df$priority, df$message_date, decreasing = FALSE), ]

  news_items <- lapply(seq_len(nrow(df)), function(i) {
    row <- df[i, ]
    is_new <- difftime(now_utc, row$message_date, units = "hours") <= 48

    bg_color <- switch(
      as.character(row$priority),
      "1" = "#fdecea", # Red-ish
      "2" = "#fff4e5", # Amber-ish
      "#ffffff" # Default white
    )

    shiny::tags$details(
      style = paste0(
        "background-color:",
        bg_color,
        "; border: 1px solid #b1b4b6; padding: 10px; margin-bottom: 5px; border-radius: 4px;"
      ),
      shiny::tags$summary(
        style = "cursor: pointer; font-weight: bold; outline: none; display: flex; align-items: center;",
        if (is_new) {
          shiny::tags$span(
            "NEW",
            style = "background:#005ea5; color:white; padding:2px 6px; border-radius:3px; font-size:0.75em; margin-right:10px; flex-shrink: 0;"
          )
        },
        shiny::tags$span(
          paste0(
            substring(gsub("<[^>]+>", "", row$message_text), 1, 40),
            "..."
          ),
          style = "white-space: nowrap; overflow: hidden; text-overflow: ellipsis;"
        )
      ),
      shiny::tags$div(
        style = "padding: 15px 5px 5px 5px; border-top: 1px solid #dcdcdc; margin-top: 10px;",
        shiny::HTML(row$message_text),
        shiny::tags$p(
          style = "font-size: 0.8em; color: #505a5f; margin-top: 10px;",
          paste(
            "Posted by:",
            row$ad_username,
            "|",
            format(row$message_date, "%d %b %H:%M")
          )
        )
      )
    )
  })

  bslib::card(
    full_screen = TRUE,
    height = "fit-content",
    style = "max-height: 600px; display: flex; flex-direction: column;",
    bslib::card_header("System News & Alerts"),
    bslib::card_body(
      style = "overflow-y: auto; padding: 10px;",
      shiny::tagList(news_items)
    )
  )
}
