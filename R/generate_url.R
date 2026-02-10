#' Generate Ofsted Inspection Report URL
#'
#' Constructs a URL to the Ofsted inspection report page for a given school URN.
#'
#' @param urn A character string representing the Unique Reference Number (URN) of a school.
#'
#' @return A character string containing the full URL to the Ofsted inspection report, or `NULL` if the URN is invalid.
#' @examples
#' ofsted_url("123456")
#' ofsted_url(NULL) # returns NULL
#' @export
#'

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
#' Constructs a URL to the GIAS (Get Information About Schools) page for a given school URN.
#'
#' @param urn A character string representing the Unique Reference Number (URN) of a school.
#'
#' @return A character string containing the full URL to the GIAS school details page, or `NULL` if the URN is invalid.
#' @examples
#' gias_school_url("123456")
#' gias_school_url("") # returns NULL
#' @export
#'

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
#' Cleans the input by removing leading "tr" and zeroes.
#'
#' @param trust_ref A character string representing the trust reference number.
#'
#' @return A character string containing the full URL to the GIAS trust details page, or `NULL` if the reference is invalid.
#' @examples
#' gias_trust_url("TR01234")
#' gias_trust_url("0001234")
#' gias_trust_url("") # returns NULL
#' @export
#'

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

#' Generate Warning Notice URL for the warning notice portal
#'
#' Constructs a Warning Notice to our internal portal for a given twn_id.
#'
#' @param twn_id. A character string representing the twn_id of a warning notice.
#'
#' @return A character string containing the full URL to the wn portal, or `NULL` if the twn_id is invalid.
#' @examples
#' wnp_wn_url("1234")
#' wnp_wn_url(NULL) # returns NULL
#' @export
#'

wnp_wn_url <- function(twn_id) {
  if (is.null(twn_id) || is.na(twn_id) || twn_id == "") {
    return(NULL)
  }
  paste0(
    "https://rsconnect/rsc/warning-notice-portal/?wnid=",
    twn_id
  )
}

#' Generate Sig Change URL for the sig change portal
#'
#' Constructs a Sig Change URL to our internal portal for a given sigchange_id
#'
#' @param sigchange_id A character string representing the sigchange_id of a sig change.
#'
#' @return A character string containing the full sig change id to the sc portal, or `NULL` if the sc id is invalid.
#' @examples
#' scp_sc_url("1234")
#' scp_sc_url(NULL) # returns NULL
#' @export
#'

scp_sc_url <- function(sigchange_id) {
  if (is.null(sigchange_id) || is.na(sigchange_id) || sigchange_id == "") {
    return(NULL)
  }
  paste0(
    "https://rsconnect/rsc/sig-change-portal/?sigchangeid=",
    sigchange_id
  )
}

#' Generate SLIC URL for the SLIC portal
#'
#' Constructs a SLIC URL to our internal portal for a given urn
#'
#' @param sigchange_id A character string representing the urn of a school.
#'
#' @return A character string containing the full link to the slic portal, or `NULL` if the urn is invalid.
#' @examples
#' slic_urn_url("1234")
#' slic_urn_url(NULL) # returns NULL
#' @export
#'

slic_urn_url <- function(urn) {
  if (is.null(urn) || is.na(urn) || urn == "") {
    return(NULL)
  }
  paste0(
    "https://rsconnect/rsc/slic/?urn=",
    urn
  )
}
