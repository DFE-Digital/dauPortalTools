#' Create a Shiny HTML Link
#'
#' Generates an HTML anchor tag for use in Shiny UI, marking external links.
#'
#' @param url A character string representing the URL to link to.
#' @param text A character string representing the link text to display.
#' @param target A character string specifying the target attribute (e.g., "_blank" to open in a new tab). Defaults to "_blank".
#'
#' @return A Shiny HTML object containing the anchor tag, or `NULL` if the URL is invalid.
#' @examples
#' make_shiny_link("https://www.gov.uk", "Gov.uk")
#' make_shiny_link("example.com", "Example Site")
#' @export
#'

make_shiny_link <- function(url, text, target = "_blank") {
  if (is.null(url) || is.na(url) || url == "") {
    return(NULL)
  }

  if (!grepl("^http[s]?://", url)) {
    url <- paste0("http://", url)
  }

  external_note <- if (
    !grepl("\\.gov\\.uk", url) &&
      !grepl("rsconnect-pp/rsc|rsconnect/rsc/", url)
  ) {
    " (external)"
  } else {
    ""
  }

  shiny::HTML(paste0(
    "<a href='",
    url,
    "' target='",
    target,
    "'>",
    text,
    external_note,
    "</a>"
  ))
}

#' Create Multiple Shiny HTML Links
#'
#' Generates a named list of Shiny HTML anchor tags from a named vector of URLs.
#'
#' @param urls A named character vector where names are link texts and values are URLs.
#'
#' @return A named list of Shiny HTML anchor tags.
#' @examples
#' make_shiny_links(c("DfE" = "https://www.gov.uk/government/organisations/department-for-education"))
#' @export
#'

make_shiny_links <- function(urls) {
  lapply(names(urls), function(nm) make_shiny_link(urls[[nm]], text = nm)) |>
    setNames(names(urls))
}
