#' Generate Ofsted Inspection Report URL
#'
#' Constructs a URL to the Ofsted inspection report page for a given school URN.
#'
#' @param urn Character scalar. Unique Reference Number (URN) identifying a school.
#'
#' @return Character scalar containing the Ofsted report URL, or `NULL` if the
#'   input is missing or invalid.
#'
#' @examples
#' ofsted_url("123456")
#' ofsted_url(NULL)
#'
#' @export

ofsted_url <- function(urn) {
  if (is.null(urn) || is.na(urn) || urn == "") {
    return(NULL)
  }
  paste0(
    "http://www.ofsted.gov.uk/inspection-reports/find-inspection-report/provider/ELS/",
    urn
  )
}

#' Generate GIAS School URL
#'
#' Generate the Get Information About Schools (GIAS) page for a given
#' school URN.
#'
#' @param urn Character scalar. Unique Reference Number (URN) identifying a school.
#'
#' @return Character scalar containing the GIAS school URL, or `NULL` if the
#'   input is missing or invalid.
#'
#' @examples
#' gias_school_url("123456")
#' gias_school_url("")
#'
#' @export

gias_school_url <- function(urn) {
  if (is.null(urn) || is.na(urn) || urn == "") {
    return(NULL)
  }
  paste0(
    "https://get-information-schools.service.gov.uk/Establishments/Establishment/Details/",
    urn
  )
}

#' Generate GIAS Trust URL
#'
#' Constructs a URL to the GIAS page for a given trust reference number.
#' The input is normalised by removing any leading "TR" prefix and leading zeros.
#'
#' @param trust_ref Character scalar. Trust reference identifier.
#'
#' @return Character scalar containing the GIAS trust URL, or `NULL` if the
#'   input is missing or invalid after cleaning.
#'
#' @examples
#' gias_trust_url("TR01234")
#' gias_trust_url("0001234")
#' gias_trust_url("")
#'
#' @export

gias_trust_url <- function(trust_ref) {
  if (is.null(trust_ref) || is.na(trust_ref) || trust_ref == "") {
    return(NULL)
  }

  trust_ref_clean <- sub("^0+", "", sub("^tr", "", tolower(trust_ref)))

  if (trust_ref_clean == "") {
    return(NULL)
  }

  paste0(
    "https://get-information-schools.service.gov.uk/Groups/Group/Details/",
    trust_ref_clean
  )
}

#' Generate Warning Notice Portal URL
#'
#' Constructs a URL to the internal warning notice portal for a given
#' warning notice ID.
#'
#' @param twn_id Character scalar. Identifier for a warning notice record.
#'
#' @return Character scalar containing the portal URL, or `NULL` if the
#'   input is missing or invalid.
#'
#' @examples
#' wnp_wn_url("1234")
#' wnp_wn_url(NULL)
#'
#' @export

wnp_wn_url <- function(twn_id) {
  if (is.null(twn_id) || is.na(twn_id) || twn_id == "") {
    return(NULL)
  }
  paste0(
    "https://rsconnect/rsc/warning-notice-portal/?wnid=",
    twn_id
  )
}

#' Generate Significant Change Portal URL
#'
#' Constructs a URL to the internal significant change portal for a given
#' significant change ID.
#'
#' @param sigchange_id Character scalar. Identifier for a significant change.
#'
#' @return Character scalar containing the portal URL, or `NULL` if the
#'   input is missing or invalid.
#'
#' @examples
#' scp_sc_url("1234")
#' scp_sc_url(NULL)
#'
#' @export

scp_sc_url <- function(sigchange_id) {
  if (is.null(sigchange_id) || is.na(sigchange_id) || sigchange_id == "") {
    return(NULL)
  }
  paste0(
    "https://rsconnect/rsc/sig-change-portal/?scid=",
    sigchange_id
  )
}

#' Generate SLIC Portal URLs
#'
#' Constructs URLs to the SLIC portal for one or more school URNs.
#' This function is vectorised and returns one URL per input value.
#'
#' @param urn Character vector (or coercible to character) of URNs.
#'
#' @return A character vector of SLIC URLs. Invalid or missing values
#'   are returned as `NA_character_`.
#'
#' @examples
#' slic_urn_url("1234")
#' slic_urn_url(c("1234", NA, ""))
#'
#' @export

slic_urn_url <- function(urn) {
  urn <- as.character(urn)
  urn <- trimws(urn)

  invalid <- is.na(urn) | urn == ""
  out <- paste0("https://rsconnect/rsc/slic/?urn=", urn)
  out[invalid] <- NA_character_
  out
}
