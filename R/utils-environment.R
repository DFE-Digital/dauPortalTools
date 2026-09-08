#' Resolve Environment Context Name to Integer Key
#'
#' Evaluates the application host URL strings to determine the active running environment.
#'
#' @return Integer scalar matching environments_config map (1 = dev, 2 = test, 3 = beta, 4 = live)
#' @keywords internal
#' @export
utils_resolve_env_id <- function() {
  log_event("Starting utils_resolve_env_id")

  active_env <- Sys.getenv("R_CONFIG_ACTIVE", "default")

  host_url <- tryCatch(
    shiny::getShinyOption("app_url"),
    error = function(e) {
      NULL
    }
  )

  if (is.null(host_url) || grepl("localhost|127.0.0.1", host_url)) {
    log_event("Environment resolved to Local Development (1L).")
    return(1L)
  } else if (grepl("test", active_env)) {
    log_event("Environment resolved to Test (2L).")
    return(2L)
  } else if (grepl("beta", active_env)) {
    log_event("Environment resolved to beta (3L).")
    return(3L)
  } else if (grepl("live", active_env)) {
    log_event("Environment resolved to Live (4L).")
    return(4L)
  }
}


#' Format Environment Status Badge
#'
#' Evaluates the active environment token (e.g., from R_CONFIG_ACTIVE)
#' and returns a stylized HTML span for display in footers and metadata headers.
#'
#' @param env Character scalar. Environment name (e.g., "live", "production", "test", "beta", "default").
#' @return A Shiny HTML span object.
#' @export
utils_format_env_status <- function(
  env = Sys.getenv("R_CONFIG_ACTIVE", "default")
) {
  clean_env <- tolower(trimws(env %||% "default"))

  status_config <- switch(
    clean_env,
    "live" = ,
    "production" = list(
      label = "Live",
      color = "#00703c" # GOV.UK green
    ),
    "test" = ,
    "staging" = list(
      label = "Test",
      color = "#f47738" # GOV.UK orange
    ),
    "beta" = ,
    "dev" = ,
    "development" = list(
      label = "Beta",
      color = "#d4351c" # GOV.UK red
    ),
    # Default fallback
    list(
      label = tools::toTitleCase(clean_env),
      color = "#505a5f" # GOV.UK dark grey
    )
  )

  shiny::span(
    style = paste0("color: ", status_config$color, "; font-weight: bold;"),
    status_config$label
  )
}

#' Render Environment Alert Banner
#'
#' Generates an alert banner on non-production environments (Test, Beta, Dev)
#' to make it immediately obvious to users that they are working with test data.
#' Automatically returns NULL when running in Live / Production environments.
#'
#' @param env Character scalar. Environment name. Defaults to Sys.getenv("R_CONFIG_ACTIVE", "default").
#' @return A Shiny tagList containing the banner HTML, or NULL if Live.
#' @export
ui_environment_banner <- function(
  env = Sys.getenv("R_CONFIG_ACTIVE", "default")
) {
  clean_env <- tolower(trimws(env %||% "default"))

  # Suppress entirely on Live / Production
  if (clean_env %in% c("live", "production", "default")) {
    return(NULL)
  }

  cfg <- switch(
    clean_env,
    "test" = ,
    "staging" = list(
      tag_label = "TEST ENVIRONMENT",
      color = "#f47738", # GOV.UK Orange
      bg_color = "#fff7f2",
      description = "You are currently using the Test environment. Data entered here will not affect live production records."
    ),
    "beta" = ,
    "dev" = ,
    "development" = ,
    "local" = list(
      tag_label = "BETA / DEV ENVIRONMENT",
      color = "#d4351c", # GOV.UK Red
      bg_color = "#fdf2f2",
      description = "This is an active development instance. Features and database schemas may change without notice."
    ),
    # Fallback for unexpected non-live tokens
    list(
      tag_label = toupper(clean_env),
      color = "#f47738",
      bg_color = "#fff7f2",
      description = paste0(
        "You are currently running in the '",
        clean_env,
        "' environment."
      )
    )
  )

  shiny::div(
    role = "region",
    `aria-label` = paste(cfg$tag_label, "Notice"),
    style = paste0(
      "background-color: ",
      cfg$bg_color,
      "; ",
      "border-left: 10px solid ",
      cfg$color,
      "; ",
      "padding: 12px 18px; ",
      "margin-bottom: 25px; ",
      "box-shadow: 0 1px 3px rgba(0,0,0,0.08);"
    ),
    shiny::div(
      style = "display: flex; align-items: center; gap: 15px; flex-wrap: wrap;",
      shiny::span(
        style = paste0(
          "background-color: ",
          cfg$color,
          "; ",
          "color: #ffffff; ",
          "font-weight: 700; ",
          "letter-spacing: 1px; ",
          "font-size: 0.85rem; ",
          "padding: 4px 8px; ",
          "display: inline-block; ",
          "text-transform: uppercase;"
        ),
        cfg$tag_label
      ),
      shiny::span(
        class = "govuk-body",
        style = "margin: 0; font-weight: 600; color: #0b0c0c;",
        cfg$description
      )
    )
  )
}
