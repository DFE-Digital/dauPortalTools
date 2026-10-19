# ==============================================================================
# R/fct-timeline.R
# Timeline business logic, orchestrator, badge styling, and download helpers
# ==============================================================================

#' Orchestrate Entity Timeline Retrieval
#'
#' @description
#' Evaluates the entity type, queries the corresponding database function,
#' and enforces sorting from latest to oldest (top to bottom).
#'
#' @param type Character scalar. 'School', 'Trust', 'Diocese', or 'LA'. Case-insensitive.
#' @param id Identifier for the entity (URN, trust_ref, diocese name, or LA name).
#'
#' @return A data frame containing ordered timeline records (latest to oldest).
#' @export
#'
#' @examples
#' \dontrun{
#' get_timeline_data(type = "School", id = 100384)
#' get_timeline_data(type = "Trust", id = "TR01234")
#' }
get_timeline_data <- function(type, id) {
  if (
    missing(type) || is.null(type) || !is.character(type) || length(type) != 1
  ) {
    stop(
      "`type` must be a single string: 'School', 'Trust', 'Diocese', or 'LA'."
    )
  }
  if (missing(id) || is.null(id) || length(id) != 1 || is.na(id)) {
    stop("`id` must be provided as a non-null scalar value.")
  }

  normalized_type <- tolower(trimws(type))

  df <- switch(
    normalized_type,
    "school" = db_get_school_timeline(urn = id),
    "trust" = db_get_trust_timeline(trust_ref = as.character(id)),
    "diocese" = db_get_diocese_timeline(diocese = as.character(id)),
    "la" = db_get_la_timeline(la = as.character(id)),
    stop(glue::glue(
      "Unrecognized type: '{type}'. Expected 'School', 'Trust', 'Diocese', or 'LA'."
    ))
  )

  if (nrow(df) == 0) {
    return(df)
  }

  # Ensure sorting is latest to oldest (top to bottom)
  df |>
    dplyr::arrange(dplyr::desc(record_date))
}

#' Format Timeline Card Content with Distinct Type Colours
#'
#' @description
#' Formats raw event and inspection fields into display-ready items,
#' styling badges by `type` and ordering chronologically descending (latest first).
#'
#' @param df Data frame returned by \code{get_timeline_data}.
#'
#' @return Formatted data frame ready for UI rendering.
#' @export
build_timeline_cards <- function(df) {
  if (is.null(df) || nrow(df) == 0) {
    return(data.frame())
  }

  df |>
    dplyr::mutate(
      display_title = dplyr::case_when(
        !is.na(school_name) & school_name != "" ~ glue::glue(
          "{school_name} - {category}"
        ),
        TRUE ~ category
      ),
      badge_colour = dplyr::case_when(
        # Exact/case-insensitive matches for the 6 target types
        tolower(trimws(type)) == "rise universal hubs" ~ "teal",
        tolower(trimws(type)) == "significant change" ~ "blue",
        tolower(trimws(type)) == "dao" ~ "purple",
        tolower(trimws(type)) == "warning notice" ~ "red",
        tolower(trimws(type)) == "inspection" ~ "orange",
        tolower(trimws(type)) == "rise universal event" ~ "green",
        TRUE ~ "grey"
      )
    ) |>
    dplyr::arrange(dplyr::desc(record_date))
}

#' Export Timeline Data to CSV
#'
#' @description
#' Cleans, sorts, and writes timeline records directly to a CSV file.
#' Drops UI-internal rendering fields and sorts chronologically from latest to oldest.
#'
#' @param df Data frame returned by \code{get_timeline_data} or \code{build_timeline_cards}.
#' @param file Target path string where the CSV should be saved (typically passed from Shiny's \code{downloadHandler}).
#'
#' @return The file path invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' # Inside a Shiny server or module:
#' output$dl_timeline <- downloadHandler(
#'   filename = function() {
#'     glue::glue("{tolower(input$entity_type)}_{input$entity_id}_timeline_{format(Sys.Date(), '%Y%m%d')}.csv")
#'   },
#'   content = function(file) {
#'     export_timeline_data(r_timeline_data(), file = file)
#'   }
#' )
#' }
export_timeline_data <- function(df, file) {
  if (is.null(df) || nrow(df) == 0) {
    stop("No timeline records available to download.")
  }

  # Remove internal helper columns generated strictly for UI cards
  export_df <- df |>
    dplyr::select(-dplyr::any_of(c("badge_colour", "display_title", "rn")))

  # Enforce latest to oldest order
  if ("record_date" %in% names(export_df)) {
    export_df <- export_df |>
      dplyr::arrange(dplyr::desc(record_date))
  }

  # Write directly to CSV
  utils::write.csv(export_df, file = file, row.names = FALSE, na = "")

  invisible(file)
}
