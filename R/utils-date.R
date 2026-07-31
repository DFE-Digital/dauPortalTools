#' Parse a single value to Date
#'
#' Attempts to coerce a single value to `Date`, accepting `Date`, `POSIXt`, and
#' character strings in common ISO and UK formats. Returns `NA` Date on failure.
#'
#' @param x A length-1 value to parse.
#' @return A `Date` scalar (possibly `NA`).
#' @examples
#' to_date("2024-12-31"); to_date("31/12/2024 23:59:59"); to_date(Sys.time())
#' @seealso [as_Date_vec()]
#' @export
to_date <- function(x) {
  if (inherits(x, "Date")) {
    return(x)
  }

  if (inherits(x, c("POSIXct", "POSIXlt"))) {
    return(as.Date(x, tz = "UTC"))
  }

  # try common formats
  if (is.character(x)) {
    # ISO date
    d <- suppressWarnings(as.Date(x, format = "%Y-%m-%d"))
    if (!is.na(d)) {
      return(d)
    }

    # ISO datetime
    dt <- suppressWarnings(as.POSIXct(
      x,
      format = "%Y-%m-%d %H:%M:%S",
      tz = "UTC"
    ))
    if (!is.na(dt)) {
      return(as.Date(dt, tz = "UTC"))
    }

    # UK date
    d <- suppressWarnings(as.Date(x, format = "%d/%m/%Y"))
    if (!is.na(d)) {
      return(d)
    }

    # UK datetime
    dt <- suppressWarnings(as.POSIXct(
      x,
      format = "%d/%m/%Y %H:%M:%S",
      tz = "UTC"
    ))
    if (!is.na(dt)) {
      return(as.Date(dt, tz = "UTC"))
    }

    d <- suppressWarnings(as.Date(x))
    if (!is.na(d)) return(d)
  }

  as.Date(NA)
}

#' GOV.UK style date input (DD / MMM / YYYY)
#'
#' Renders three inputs for day, month (short name), and year.
#' Accepts a value and safely splits into components.
#'
#' @param field Field name (base id)
#' @param label Label text
#' @param value A Date or coercible value
#' @return Shiny UI element
#' @export
ui_date_input <- function(field, label, value = NA) {
  d <- to_date(value)

  day <- if (!is.na(d)) format(d, "%d") else ""
  month <- if (!is.na(d)) format(d, "%b") else ""
  year <- if (!is.na(d)) format(d, "%Y") else ""

  day_choices <- c(
    "DD" = "",
    setNames(sprintf("%02d", 1:31), sprintf("%02d", 1:31))
  )
  month_choices <- c("MMM" = "", setNames(month.abb, month.abb))
  year_choices <- c("YYYY" = "", setNames(as.character(2020:2040), 2020:2040))

  return(
    div(
      class = "govuk-form-group",
      style = "margin-bottom: 15px;",

      tags$label(
        class = "govuk-label",
        style = "font-weight: bold; font-size: 0.9em; margin-bottom: 2px;",
        label
      ),

      div(
        style = "display: flex; align-items: center; height: 28px;",

        div(
          style = "border: 1px solid #b1b4b6; border-right: none; border-radius: 4px 0 0 4px; overflow: hidden; height: 100%;",
          selectInput(
            paste0(field, "_day"),
            NULL,
            choices = day_choices,
            selected = day,
            width = "55px",
            selectize = FALSE
          )
        ),

        div(
          style = "border: 1px solid #b1b4b6; border-right: none; overflow: hidden; height: 100%;",
          selectInput(
            paste0(field, "_month"),
            NULL,
            choices = month_choices,
            selected = month,
            width = "70px",
            selectize = FALSE
          )
        ),

        div(
          style = "border: 1px solid #b1b4b6; border-radius: 0 4px 4px 0; overflow: hidden; height: 100%;",
          selectInput(
            paste0(field, "_year"),
            NULL,
            choices = year_choices,
            selected = year,
            width = "80px",
            selectize = FALSE
          )
        )
      ),

      tags$style(HTML(
        "
        .govuk-form-group select {
          border: none !important;
          outline: none !important;
          height: 100% !important;
          padding: 2px 4px !important; /* Reduced vertical padding */
          font-size: 0.85em !important; /* Slightly smaller text to fit height */
          background: transparent;
          appearance: none; /* Removes some browser-specific 'chunk' */
          -webkit-appearance: none;
        }
        /* Optional: adds back a tiny arrow if 'appearance: none' hides it too much */
        .govuk-form-group select {
          background-image: url('data:image/svg+xml;charset=US-ASCII,%3Csvg%20xmlns%3D%22http%3A%2F%2Fwww.w3.org%2F2000%2Fsvg%22%20width%3D%22292.4%22%20height%3D%22292.4%22%3E%3Cpath%20fill%3D%22%23000000%22%20d%3D%22M287%2069.4a17.6%2017.6%200%200%200-13-5.4H18.4c-5%200-9.3%201.8-12.9%205.4A17.6%2017.6%200%200%200%200%2082.2c0%205%201.8%209.3%205.4%2012.9l128%20127.9c3.6%203.6%207.8%205.4%2012.8%205.4s9.2-1.8%2012.8-5.4L287%2095c3.5-3.5%205.4-7.8%205.4-12.8%200-5-1.9-9.2-5.5-12.8z%22%2F%3E%3C%2Fxml%3E');
          background-repeat: no-repeat;
          background-position: right 0.4rem top 50%;
          background-size: 0.55rem auto;
          padding-right: 1.2rem !important;
        }
      "
      ))
    )
  )
}


#' Combine GOV date inputs back into Date
#'
#' Reads day/month/year inputs and returns a Date or NA
#' @param field Field base name
#' @param input Shiny input object
#' @return Date or NA
#' @export

input_to_date <- function(field, input) {
  day <- input[[paste0(field, "_day")]]
  month <- input[[paste0(field, "_month")]]
  year <- input[[paste0(field, "_year")]]

  is_empty <- function(x, placeholder) {
    is.null(x) || length(x) == 0 || x == "" || x == placeholder
  }

  if (is_empty(day, "DD") || is_empty(month, "MMM") || is_empty(year, "YYYY")) {
    return(NA)
  }

  month_num <- match(month, month.abb)
  if (is.na(month_num)) {
    return(NA)
  }

  res <- tryCatch(
    {
      d <- as.Date(sprintf(
        "%04d-%02d-%02d",
        as.integer(year),
        month_num,
        as.integer(day)
      ))
      if (is.na(d)) NA else d
    },
    error = function(e) NA
  )

  return(res)
}

#' Vectorized conversion to Date with multiple common formats
#'
#' Attempts to convert various inputs (`Date`, `POSIXt`, `character`, `factor`)
#' to `Date`. Supports ISO (`YYYY-mm-dd`), ISO datetime, UK (`dd/mm/YYYY`),
#' and UK datetime formats, with fallbacks.
#'
#' @param x A vector of dates in mixed classes or formats.
#' @return A `Date` vector of the same length as `x`, with `NA` where parsing failed.
#' @examples
#' as_Date_vec(c("2024-01-01", "01/02/2024 12:00:00"))
#' as_Date_vec(Sys.time())
#' @seealso [as.Date()], [as.POSIXct()]
#' @export
as_Date_vec <- function(x) {
  if (inherits(x, "Date")) {
    return(x)
  }

  if (is.factor(x)) {
    x <- as.character(x)
  }

  if (inherits(x, c("POSIXct", "POSIXlt"))) {
    return(as.Date(x, tz = "UTC"))
  }

  if (is.character(x)) {
    suppressWarnings({
      pc <- as.POSIXct(
        x,
        tz = "UTC",
        tryFormats = c(
          "%Y-%m-%d",
          "%Y-%m-%d %H:%M:%S",
          "%d/%m/%Y",
          "%d/%m/%Y %H:%M:%S"
        )
      )
    })
    d <- as.Date(pc, tz = "UTC")

    still_na <- is.na(d)
    if (any(still_na)) {
      d2 <- suppressWarnings(as.Date(x[still_na], format = "%Y-%m-%d"))
      d[still_na] <- d2
    }
    still_na <- is.na(d)
    if (any(still_na)) {
      d2 <- suppressWarnings(as.Date(x[still_na], format = "%d/%m/%Y"))
      d[still_na] <- d2
    }
    still_na <- is.na(d)
    if (any(still_na)) {
      d2 <- suppressWarnings(as.Date(x[still_na]))
      d[still_na] <- d2
    }

    return(d)
  }

  rep(as.Date(NA), length(x))
}
