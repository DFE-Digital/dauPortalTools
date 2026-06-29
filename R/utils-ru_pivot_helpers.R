#' Pivot and Cast Dynamic Event Responses
#'
#' Takes a raw vertical data frame of event action responses and dynamically pivots
#' them wide, casting string values into proper R types based on their rule configuration.
#'
#' @param raw_responses A data.frame returned by \code{db_ru_get_event_action_responses} or a custom query.
#' @return A pivoted, flattened \code{data.frame} where columns represent configured fields with proper types.
#' @importFrom stats reshape
#' @export
utils_ru_pivot_responses <- function(raw_responses) {
  log_event("Starting utils_ru_pivot_responses")

  if (is.null(raw_responses) || nrow(raw_responses) == 0) {
    return(data.frame())
  }

  required_cols <- c("ruev_id", "rueva_name", "ruevar_value", "rueva_rule_type")
  missing_cols <- setdiff(required_cols, names(raw_responses))
  if (length(missing_cols) > 0) {
    stop(paste(
      "Missing required reporting columns in input data frame:",
      paste(missing_cols, collapse = ", ")
    ))
  }

  type_map <- unique(raw_responses[, c("rueva_name", "rueva_rule_type")])

  wide_df <- reshape(
    raw_responses[, c("ruev_id", "rueva_name", "ruevar_value")],
    idvar = "ruev_id",
    timevar = "rueva_name",
    direction = "wide"
  )

  names(wide_df) <- gsub("^ruevar_value\\.", "", names(wide_df))

  for (i in seq_len(nrow(type_map))) {
    col_name <- type_map$rueva_name[i]
    rule_type <- type_map$rueva_rule_type[i]

    if (!col_name %in% names(wide_df)) {
      next
    }

    wide_df[[col_name]] <- switch(
      rule_type,
      "Integer" = as.integer(wide_df[[col_name]]),
      "Date" = as.Date(wide_df[[col_name]], format = "%Y-%m-%d"),
      "Boolean" = wide_df[[col_name]] == "1",
      as.character(wide_df[[col_name]])
    )
  }

  log_event("Finished utils_ru_pivot_responses successfully")
  return(wide_df)
}
