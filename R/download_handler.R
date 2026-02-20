#' Unified download button helper for Shiny apps
#'
#' Creates a consistent GOV.UK-styled download button with safe IDs,
#' safe filenames, and support for both reactive and static data. Also
#' records download analytics via `record_download()` including auto-
#' inferred page name.
#'
#' @param df Data frame or reactive expression returning a data frame.
#' @param file_label1 Main filename label (required).
#' @param file_label2 Optional secondary filename label.
#'
#' @return A Shiny downloadButton with server-side downloadHandler.
#'
#' @export
#'

download_handler <- function(
  df,
  file_label1,
  file_label2 = NULL
) {
  safe_label <- function(x) {
    if (is.null(x)) {
      return("")
    }
    x <- gsub("[^A-Za-z0-9]+", "_", x)
    x <- gsub("_+", "_", x)
    x <- tolower(x)
    trimws(x, which = "both", whitespace = "_")
  }

  infer_page_name <- function(session) {
    if (!is.null(session$input$`router$page`)) {
      return(session$input$`router$page`)
    }

    if (!is.null(session$input$tabs)) {
      return(session$input$tabs)
    }

    if (!is.null(session$input$tab)) {
      return(session$input$tab)
    }

    if (!is.null(session$userData$page_name)) {
      return(session$userData$page_name)
    }

    return("Unknown Page")
  }

  rand_id <- paste0(sample(c(letters, 0:9), 6, TRUE), collapse = "")
  hash_id <- digest::digest(paste(file_label1, file_label2, rand_id))
  dl_id <- paste0("dl_", hash_id)

  session <- shiny::getDefaultReactiveDomain()

  if (!is.null(session)) {
    session$output[[dl_id]] <- shiny::downloadHandler(
      filename = function() {
        paste0(
          safe_label(file_label1),
          if (!is.null(file_label2)) paste0("_", safe_label(file_label2)),
          "_",
          format(Sys.Date(), "%Y%m%d"),
          ".csv"
        )
      },

      content = function(file) {
        data_to_write <- shiny::isolate(
          if (shiny::is.reactive(df) || is.function(df)) df() else df
        )

        data_to_write <-
          filename_final <- paste0(
            safe_label(file_label1),
            if (!is.null(file_label2)) paste0("_", safe_label(file_label2)),
            "_",
            format(Sys.Date(), "%Y%m%d"),
            ".csv"
          )

        page_name_auto <- try(
          {
            infer_page_name(session)
          },
          silent = TRUE
        )

        if (inherits(page_name_auto, "try-error") || is.null(page_name_auto)) {
          page_name_auto <- "Unknown Page"
        }
        shiny::withProgress(message = "Preparing your download...", value = 0, {
          utils::write.csv(
            data_to_write,
            file,
            row.names = FALSE,
            na = ""
          )

          incProgress(1)
        })

        record_download(
          user = session$user %||% "Guest",
          page_name = page_name_auto,
          file_name = filename_final
        )
      },

      contentType = "text/csv"
    )
  }

  shiny::downloadButton(
    dl_id,
    "Download CSV",
    class = "govuk-button govuk-button--secondary"
  )
}
