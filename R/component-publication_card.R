# ==============================================================================
# File: R/component-publication_card.R
# Purpose: Reusable Shiny UI components for rendering individual DAU
#   Publication cards, batch grids, and random 2-card spotlight widgets.
#
# Functions:
#   - render_publication_card()             : Single card from pub_id
#   - render_publication_cards()            : Grid of cards from df/vector
#   - render_random_publications()          : 2 completely random pub cards
#   - render_random_publications_by_type()  : 2 random pub cards by data_type
#
# Dependencies: shiny, bslib, bsicons, internal (db_pub_*)
# ==============================================================================

#' Render a Standardized DAU Publication Card
#'
#' Queries publication metadata, linked data group badges, and maintainer leads
#' for a given `pub_id` and renders a `bslib::card` with an optional modal for
#' methodology/guidance notes.
#'
#' @param pub_id Integer scalar. Primary key of the publication in
#'   `publication_list` (required).
#' @param session Active Shiny session (used to wire the guidance popup modal).
#'   Defaults to `shiny::getDefaultReactiveDomain()`.
#'
#' @details
#' Call inside a `shiny::renderUI()` block anywhere in a portal app. If the
#' publication has non-empty `pub_guidance` text, clicking the info icon in the
#' card header automatically opens a modal dialog scoped to `session`.
#'
#' @examples
#' \dontrun{
#' # In UI:
#' shiny::uiOutput(ns("featured_publication"))
#'
#' # In Server:
#' output$featured_publication <- shiny::renderUI({
#'   dauPortalTools::render_publication_card(
#'     pub_id = 4,
#'     session = session
#'   )
#' })
#' }
#'
#' @return A [`bslib::card`] UI element.
#' @export
render_publication_card <- function(
  pub_id,
  session = shiny::getDefaultReactiveDomain()
) {
  is_valid_str <- function(x) {
    !is.null(x) &&
      length(x) > 0 &&
      !is.na(x) &&
      is.character(x) &&
      nzchar(trimws(x))
  }

  pid <- suppressWarnings(as.integer(pub_id))
  if (length(pid) == 0 || is.na(pid)) {
    return(
      bslib::card(
        class = "border-danger-subtle p-3 text-muted small",
        "Invalid publication ID provided."
      )
    )
  }

  # Pull publication data directly from the database
  row <- db_pub_get_card_details(pub_id = pid)

  if (is.null(row) || nrow(row) == 0) {
    return(
      bslib::card(
        class = "border-danger-subtle p-3 text-muted small",
        paste("Publication ID not found or inactive:", pid)
      )
    )
  }

  ns <- if (!is.null(session)) session$ns else identity
  guidance_btn_id <- paste0("btn_pub_guidance_", pid)
  has_guidance <- is_valid_str(row$pub_guidance[1])

  # Wire Guidance Modal
  if (!is.null(session) && has_guidance) {
    guidance_text <- row$pub_guidance[1]
    pub_title <- row$pub_name[1]

    shiny::observeEvent(
      session$input[[guidance_btn_id]],
      {
        shiny::showModal(shiny::modalDialog(
          title = shiny::tags$div(
            class = "d-flex align-items-center gap-2",
            bsicons::bs_icon("info-circle-fill", class = "text-info"),
            paste("Methodology & Guidance:", pub_title)
          ),
          shiny::tags$div(
            class = "p-3 bg-light rounded border text-break",
            guidance_text
          ),
          easyClose = TRUE,
          size = "m",
          footer = shiny::modalButton("Close")
        ))
      },
      ignoreInit = TRUE
    )
  }

  # Data Type Badges
  pills <- NULL
  if (is_valid_str(row$type_names[1])) {
    names_vec <- strsplit(row$type_names[1], "\\|")[[1]]
    colours_vec <- strsplit(row$type_colours[1], "\\|")[[1]]

    pills <- lapply(seq_along(names_vec), function(idx) {
      bg_col <- if (idx <= length(colours_vec) && nzchar(colours_vec[idx])) {
        colours_vec[idx]
      } else {
        "#1d70b8"
      }
      shiny::tags$span(
        class = "badge rounded-pill me-1 mb-1",
        style = sprintf(
          "background-color: %s; color: #ffffff; font-weight: 500; font-size: 11px; padding: 4px 8px;",
          bg_col
        ),
        names_vec[idx]
      )
    })
  }

  # Maintainers (Primary first)
  maintainer_ui <- NULL
  if (is_valid_str(row$maintainers_str[1])) {
    m_entries <- strsplit(row$maintainers_str[1], "; ")[[1]]
    m_tags <- lapply(m_entries, function(entry) {
      parts <- strsplit(entry, "::")[[1]]
      email <- parts[1]
      is_prim <- length(parts) > 1 && parts[2] == "1"

      shiny::tags$span(
        class = "me-2",
        shiny::tags$a(
          href = paste0("mailto:", email),
          class = "text-decoration-none",
          email
        ),
        if (is_prim) {
          shiny::tags$span(
            class = "badge bg-primary-subtle text-primary border ms-1",
            style = "font-size: 10px;",
            "Primary"
          )
        }
      )
    })

    maintainer_ui <- shiny::tags$span(
      class = "text-secondary ms-2 ps-2 border-start",
      shiny::tags$strong("Leads: "),
      m_tags
    )
  }

  # Format Published Date
  pub_date_str <- if (
    !is.null(row$published_date[1]) && !is.na(row$published_date[1])
  ) {
    format(as.Date(row$published_date[1]), "%d %b %Y")
  } else {
    NULL
  }

  bslib::card(
    class = "shadow-sm border-light-subtle h-100",
    bslib::card_header(
      class = "d-flex justify-content-between align-items-center bg-body-tertiary py-2",
      shiny::tags$span(
        class = "fw-semibold text-truncate me-2",
        title = row$pub_name[1],
        row$pub_name[1]
      ),
      shiny::tagList(
        shiny::tags$div(
          class = "d-flex align-items-center gap-1 flex-shrink-0",
          if (is_valid_str(row$publication_type[1])) {
            shiny::tags$span(
              class = "badge text-bg-secondary rounded-pill",
              row$publication_type[1]
            )
          },
          if (has_guidance) {
            shiny::actionLink(
              ns(guidance_btn_id),
              label = bsicons::bs_icon(
                "question-circle-fill",
                class = "text-primary fs-5"
              ),
              title = "View Methodology & Notes",
              style = "line-height: 1; padding: 0 2px;"
            )
          }
        )
      )
    ),
    bslib::card_body(
      class = "d-flex flex-column justify-content-between gap-3",
      shiny::tags$div(
        class = "d-flex flex-column gap-2",
        shiny::tags$div(
          class = "d-flex flex-wrap justify-content-between align-items-center gap-1",
          if (!is.null(pills)) {
            shiny::tags$div(
              class = "d-flex flex-wrap align-items-center",
              pills
            )
          } else {
            shiny::tags$div()
          },
          if (!is.null(pub_date_str)) {
            shiny::tags$span(
              class = "text-muted small",
              bsicons::bs_icon("calendar3", class = "me-1"),
              paste("Published:", pub_date_str)
            )
          }
        ),
        if (is_valid_str(row$pub_description[1])) {
          shiny::tags$p(class = "text-secondary mb-1", row$pub_description[1])
        }
      ),
      shiny::tags$div(
        class = "pt-2 border-top d-flex justify-content-between align-items-center mt-auto",
        shiny::tags$div(
          class = "d-flex align-items-center flex-wrap small",
          shiny::tags$span(
            class = "text-muted",
            shiny::tags$strong("Team: "),
            if (is_valid_str(row$team[1])) row$team[1] else "SSSR"
          ),
          maintainer_ui
        ),
        if (is_valid_str(row$pub_url[1])) {
          shiny::tags$a(
            href = row$pub_url[1],
            target = "_blank",
            rel = "noopener noreferrer",
            class = "btn btn-primary btn-sm d-inline-flex align-items-center gap-2",
            "View Release ↗"
          )
        } else {
          shiny::tags$span(class = "text-muted small", "No URL provided")
        }
      )
    )
  )
}


#' Render Multiple DAU Publication Cards in a Responsive Grid
#'
#' Iterates over a `data.frame` (containing a `pub_id` column), list, or vector
#' of publication IDs and arranges the generated cards in a responsive
#' `bslib::layout_column_wrap()` grid.
#'
#' @param pubs A `data.frame` containing a `pub_id` column, or an integer/numeric
#'   vector or list of `pub_id` values.
#' @param width Column width passed to [bslib::layout_column_wrap()]. Defaults to `1 / 2`.
#' @param session Active Shiny session. Defaults to `shiny::getDefaultReactiveDomain()`.
#'
#' @examples
#' \dontrun{
#' # In UI:
#' shiny::uiOutput(ns("selected_publications"))
#'
#' # In Server (using a vector of IDs):
#' output$selected_publications <- shiny::renderUI({
#'   dauPortalTools::render_publication_cards(
#'     pubs = c(1, 4, 7),
#'     width = 1 / 2,
#'     session = session
#'   )
#' })
#'
#' # In Server (using a filtered data.frame with a pub_id column):
#' output$selected_publications <- shiny::renderUI({
#'   dauPortalTools::render_publication_cards(
#'     pubs = filtered_pubs_df,
#'     session = session
#'   )
#' })
#' }
#'
#' @return A [`bslib::layout_column_wrap`] UI element (or inset message if empty).
#' @export
render_publication_cards <- function(
  pubs,
  width = 1 / 2,
  session = shiny::getDefaultReactiveDomain()
) {
  pub_ids <- if (is.data.frame(pubs)) {
    if ("pub_id" %in% names(pubs)) {
      pubs$pub_id
    } else {
      stop(
        "render_publication_cards: Data frame must contain a 'pub_id' column."
      )
    }
  } else if (is.list(pubs)) {
    unlist(pubs)
  } else {
    pubs
  }

  pub_ids <- suppressWarnings(as.integer(pub_ids))
  pub_ids <- unique(pub_ids[!is.na(pub_ids)])

  if (length(pub_ids) == 0) {
    return(shiny::tags$div(
      class = "govuk-inset-text",
      "No matching publications to display."
    ))
  }

  cards <- lapply(pub_ids, function(pid) {
    render_publication_card(pub_id = pid, session = session)
  })

  bslib::layout_column_wrap(
    width = width,
    !!!cards
  )
}


#' Render Two Completely Random DAU Publication Cards
#'
#' Selects up to `n` (default 2) active publications at random from the entire
#' publications catalogue and renders them side-by-side. Ideal for general
#' landing pages or "Discover Publications" spotlight sections.
#'
#' @param n Integer scalar. Number of random cards to display. Defaults to `2`.
#' @param session Active Shiny session. Defaults to `shiny::getDefaultReactiveDomain()`.
#'
#' @examples
#' \dontrun{
#' # In UI (e.g., on a Home / Landing page):
#' shiny::uiOutput(ns("random_pub_spotlight"))
#'
#' # In Server:
#' output$random_pub_spotlight <- shiny::renderUI({
#'   dauPortalTools::render_random_publications(
#'     n = 2,
#'     session = session
#'   )
#' })
#' }
#'
#' @return A [`bslib::layout_column_wrap`] containing the randomly selected publication cards.
#' @export
render_random_publications <- function(
  n = 2,
  session = shiny::getDefaultReactiveDomain()
) {
  index_df <- db_pub_get_directory_index()

  if (is.null(index_df) || nrow(index_df) == 0) {
    return(shiny::tags$div(
      class = "govuk-inset-text",
      "No publications are currently available."
    ))
  }

  all_ids <- unique(as.integer(index_df$pub_id))
  sample_size <- min(as.integer(n), length(all_ids))
  chosen_ids <- if (length(all_ids) == 1) {
    all_ids
  } else {
    sample(all_ids, size = sample_size)
  }

  render_publication_cards(
    pubs = chosen_ids,
    width = 1 / 2,
    session = session
  )
}


#' Render Two Random DAU Publication Cards Filtered by Data Type
#'
#' Filters active publications linked to a specific data category in
#' `[01_SSSR].[data_type]` (accepted as either integer `type_id` or character
#' `type_name`) and randomly selects up to `n` (default 2) cards to display.
#' Ideal for contextual "Related Publications" sections on topic-specific pages.
#'
#' @param data_type Integer `type_id` or character `type_name` from
#'   `[01_SSSR].[data_type]` (e.g., `3` or `"Academies"`).
#' @param n Integer scalar. Number of random cards to display. Defaults to `2`.
#' @param session Active Shiny session. Defaults to `shiny::getDefaultReactiveDomain()`.
#'
#' @examples
#' \dontrun{
#' # In UI (e.g., at the bottom of the Academies Delivery page):
#' shiny::uiOutput(ns("related_academies_pubs"))
#'
#' # In Server (passing data_type by name):
#' output$related_academies_pubs <- shiny::renderUI({
#'   dauPortalTools::render_random_publications_by_type(
#'     data_type = "Academies",
#'     n = 2,
#'     session = session
#'   )
#' })
#'
#' # Or passing data_type by integer type_id:
#' output$related_academies_pubs <- shiny::renderUI({
#'   dauPortalTools::render_random_publications_by_type(
#'     data_type = 2,
#'     session = session
#'   )
#' })
#' }
#'
#' @return A [`bslib::layout_column_wrap`] containing up to `n` matching publication cards.
#' @export
render_random_publications_by_type <- function(
  data_type,
  n = 2,
  session = shiny::getDefaultReactiveDomain()
) {
  if (is.null(data_type) || length(data_type) == 0 || is.na(data_type)) {
    return(render_random_publications(n = n, session = session))
  }

  # Resolve target type_id whether caller passed an integer ID or a character type_name
  target_type_id <- suppressWarnings(as.integer(data_type))

  if (is.na(target_type_id) && is.character(data_type)) {
    types_df <- db_pub_get_data_types()
    if (!is.null(types_df) && nrow(types_df) > 0) {
      match_idx <- which(
        tolower(trimws(types_df$type_name)) == tolower(trimws(data_type))
      )
      if (length(match_idx) > 0) {
        target_type_id <- as.integer(types_df$type_id[match_idx[1]])
      }
    }
  }

  if (is.na(target_type_id)) {
    return(shiny::tags$div(
      class = "govuk-inset-text",
      paste("No matching data type found for:", data_type)
    ))
  }

  index_df <- db_pub_get_directory_index()
  if (is.null(index_df) || nrow(index_df) == 0) {
    return(shiny::tags$div(
      class = "govuk-inset-text",
      "No publications are currently available."
    ))
  }

  # Filter index_df to publications linked to target_type_id
  target_str <- as.character(target_type_id)
  matches <- vapply(
    index_df$type_ids,
    function(id_str) {
      if (is.null(id_str) || is.na(id_str) || !nzchar(id_str)) {
        return(FALSE)
      }
      target_str %in% strsplit(id_str, "\\|")[[1]]
    },
    logical(1)
  )

  filtered_df <- index_df[matches, , drop = FALSE]

  if (nrow(filtered_df) == 0) {
    return(shiny::tags$div(
      class = "govuk-inset-text",
      "No publications found for the specified data type."
    ))
  }

  matching_ids <- unique(as.integer(filtered_df$pub_id))
  sample_size <- min(as.integer(n), length(matching_ids))
  chosen_ids <- if (length(matching_ids) == 1) {
    matching_ids
  } else {
    sample(matching_ids, size = sample_size)
  }

  render_publication_cards(
    pubs = chosen_ids,
    width = 1 / 2,
    session = session
  )
}
