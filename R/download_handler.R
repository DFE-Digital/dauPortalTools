download_handler <- function(
  df,
  file_label1,
  file_label2 = NULL,
  button_label = "Download CSV"
) {
  # Generate random ID
  rand_id <- paste0(sample(c(letters, 0:9), 6, TRUE), collapse = "")
  dl_id <- paste0(file_label1, file_label2, rand_id)

  # Get current Shiny session
  session <- shiny::getDefaultReactiveDomain()

  # Generate download
  if (!is.null(session)) {
    session$output[[dl_id]] <- shiny::downloadHandler(
      filename = function() {
        file_label2 <- if (is.null(file_label2)) {
          ""
        } else {
          gsub("\\s+", "_", file_label2)
        }
        paste0(
          file_label1,
          file_label2,
          "_",
          format(Sys.Date(), "%Y%m%d"),
          ".csv"
        )
      },
      content = function(file) {
        utils::write.csv(df, file, row.names = FALSE, na = "")
      }
    )
  }

  # Return the UI download button
  shiny::downloadButton(dl_id, label)
}
