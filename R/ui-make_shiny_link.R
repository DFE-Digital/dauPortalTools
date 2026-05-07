#' Create a Shiny HTML Link
#'
#' Generates an HTML anchor tag for use in Shiny UI. External links are
#' automatically identified and annotated with a visual "(external)" suffix.
#'
#' @param url Character scalar. The URL to link to. If the URL is missing,
#'   empty, or `NA`, the function returns `NULL`.
#' @param text Character scalar. The text to display for the link.
#' @param target Character scalar. Value for the `target` attribute
#'   (e.g. `"_blank"` to open in a new tab). Defaults to `"_blank"`.
#'
#' @details
#' If the supplied `url` does not include a scheme (`http://` or `https://`),
#' `http://` is prepended automatically.
#'
#' Links that do not point to a `.gov.uk` domain or recognised internal
#' RStudio Connect paths are labelled as external by appending
#' `" (external)"` to the display text.
#'
#' @section Side Effects:
#' \itemize{
#'   \item Returns HTML content intended for rendering in a Shiny UI.
#'   \item Does not perform validation beyond basic string checks.
#' }
#'
#' @return A `shiny::HTML` object containing an anchor tag, or `NULL`
#' if the URL is invalid.
#'
#' @examples
#' make_shiny_link("https://www.gov.uk", "GOV.UK")
#' make_shiny_link("example.com", "Example Site")
#'
#' @export

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

#' Create Multiple Shiny HTML Links#'#' Generates a named list of Shiny HTML anchor tags from a named vector of URLs.
#'
#' @param urls Named character vector. Names represent the link text, and
#'   values represent the URLs.
#'
#' @details
#' Each element of `urls` is passed to [make_shiny_link()]. The output
#' preserves the original names.
#'
#' @return A named list of `shiny::HTML` objects.
#'
#' @examples
#' make_shiny_links(
#'   c("DfE" = "https://www.gov.uk/government/organisations/department-for-education")
#' )
#'
#' @seealso [make_shiny_link()]
#'
#' @export

make_shiny_links <- function(urls) {
  lapply(names(urls), function(nm) make_shiny_link(urls[[nm]], text = nm)) |>
    setNames(names(urls))
}
