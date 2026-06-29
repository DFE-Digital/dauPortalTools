#' Build a Dynamic Hub Selection Selector
#'
#' Renders a bslib or standard selectInput filled directly with active rows from ruh_hubs.
#'
#' @param id Character scalar. Shiny namespace input identification string.
#' @param label Character scalar. Display text assigned above select field.
#' @param include_all Logical. Append an 'All Hubs' baseline selection choice.
#' @export
ui_filter_hub_select <- function(
  id,
  label = "Select Hub Framework:",
  include_all = TRUE
) {
  # Queries lookups directly to fetch fresh active options on UI generation block
  hubs_df <- db_ruh_get_hubs()

  choices <- if (nrow(hubs_df) > 0) {
    setNames(hubs_df$ruhb_id, hubs_df$hub_name)
  } else {
    character(0)
  }

  if (include_all) {
    choices <- c("All Hub Regions" = "", choices)
  }

  selectInput(id, label, choices = choices, selected = choices[1])
}

#' Build a Dynamic Event Type Selection Selector
#'
#' @param id Character scalar.
#' @param label Character scalar.
#' @export
ui_filter_event_type_select <- function(
  id,
  label = "Select Event Interaction Category:"
) {
  evt_df <- db_ru_get_event_types()

  choices <- if (nrow(evt_df) > 0) {
    setNames(evt_df$ruevt_id, evt_df$ruevt_name)
  } else {
    character(0)
  }

  selectInput(id, label, choices = choices, selected = choices[1])
}
