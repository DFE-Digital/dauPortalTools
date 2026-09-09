#' @importFrom DBI dbGetQuery dbExecute dbDisconnect
#' @importFrom glue glue
#' @importFrom tibble as_tibble
#' @importFrom shiny showModal modalDialog p
NULL

# ==============================================================================
# READ / SELECT FUNCTIONS
# ==============================================================================

#' Select all lead allocation records
#'
#' @return A \code{tibble} containing all records, or NULL on error.
#' @export
select_ru_lead_allocations_all <- function() {
  conn <- sql_manager("dit")
  on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

  tryCatch(
    {
      query <- glue::glue(
        "SELECT * FROM {utils_resolve_schema('db_schema_01r')}.[ru_lead_allocations];"
      )
      res <- DBI::dbGetQuery(conn, query)
      tibble::as_tibble(res)
    },
    error = function(e) {
      shiny::showModal(shiny::modalDialog(
        title = "Database Error",
        shiny::p(paste("Failed to retrieve lead allocations:", e$message)),
        easyClose = TRUE
      ))
      NULL
    }
  )
}

#' Select a single lead allocation record by ID
#'
#' @param rula_id Integer. The primary key ID (\code{rula_id}).
#' @return A single-row \code{tibble}, empty tibble, or NULL on error.
#' @export
select_ru_lead_allocation_by_id <- function(rula_id) {
  conn <- sql_manager("dit")
  on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

  tryCatch(
    {
      query <- glue::glue(
        "SELECT * FROM {utils_resolve_schema('db_schema_01r')}.[ru_lead_allocations] WHERE [rula_id] = ?;"
      )
      res <- DBI::dbGetQuery(conn, query, params = list(as.integer(rula_id)))
      tibble::as_tibble(res)
    },
    error = function(e) {
      shiny::showModal(shiny::modalDialog(
        title = "Database Error",
        shiny::p(paste("Failed to retrieve allocation record:", e$message)),
        easyClose = TRUE
      ))
      NULL
    }
  )
}

#' Select lead allocations for a specific lead school/entity
#'
#' @param ruhl_id Integer. The lead school record ID (\code{ruhl_id}).
#' @return A \code{tibble} of matching allocation records, or NULL on error.
#' @export
select_ru_lead_allocations_by_lead <- function(ruhl_id) {
  conn <- sql_manager("dit")
  on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

  tryCatch(
    {
      query <- glue::glue(
        "SELECT * FROM {utils_resolve_schema('db_schema_01r')}.[ru_lead_allocations] WHERE [ruhl_id] = ?;"
      )
      res <- DBI::dbGetQuery(conn, query, params = list(as.integer(ruhl_id)))
      tibble::as_tibble(res)
    },
    error = function(e) {
      shiny::showModal(shiny::modalDialog(
        title = "Database Error",
        shiny::p(paste(
          "Failed to retrieve allocations by lead ID:",
          e$message
        )),
        easyClose = TRUE
      ))
      NULL
    }
  )
}

#' Select lead allocations by support source (HUB or EVENT)
#'
#' @param source_type Character. Must be either \code{'HUB'} or \code{'EVENT'}.
#' @param source_id Integer. The hub ID (\code{ruhb_id}) or event ID (\code{ruev_id}).
#' @return A \code{tibble} of matching allocation records, or NULL on error.
#' @export
select_ru_lead_allocations_by_source <- function(source_type, source_id) {
  clean_source_type <- toupper(trimws(as.character(source_type)))
  if (!clean_source_type %in% c("HUB", "EVENT")) {
    shiny::showModal(shiny::modalDialog(
      title = "Validation Warning",
      shiny::p("Support source type must be either 'HUB' or 'EVENT'."),
      easyClose = TRUE
    ))
    return(NULL)
  }

  conn <- sql_manager("dit")
  on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

  tryCatch(
    {
      query <- glue::glue(
        "SELECT * FROM {utils_resolve_schema('db_schema_01r')}.[ru_lead_allocations]
       WHERE [support_source_type] = ?
         AND [support_source_id] = ?;"
      )
      res <- DBI::dbGetQuery(
        conn,
        query,
        params = list(clean_source_type, as.integer(source_id))
      )
      tibble::as_tibble(res)
    },
    error = function(e) {
      shiny::showModal(shiny::modalDialog(
        title = "Database Error",
        shiny::p(paste(
          "Failed to retrieve allocations by support source:",
          e$message
        )),
        easyClose = TRUE
      ))
      NULL
    }
  )
}

# ==============================================================================
# CREATE / INSERT FUNCTION
# ==============================================================================

#' Insert a new lead allocation record
#'
#' @param ruhl_id Integer. Foreign key to \code{ru_lead_schools}.
#' @param support_source_type Character. Support mechanism (\code{'HUB'} or \code{'EVENT'}).
#' @param support_source_id Integer. The ID of the hub or event.
#' @param rula_dateactive Date or character (\code{'YYYY-MM-DD'}).
#' @param rula_dateended Date or character (\code{'YYYY-MM-DD'}). Optional.
#' @param rula_active Logical. Default is \code{TRUE}.
#' @param rula_comment Character. Optional notes or context.
#' @param created_by Character. Username or audit identifier.
#' @return Integer \code{rula_id} of the inserted record, or NULL on error.
#' @export
create_ru_lead_allocation <- function(
  ruhl_id,
  support_source_type,
  support_source_id,
  rula_dateactive = Sys.Date(),
  rula_dateended = NULL,
  rula_active = TRUE,
  rula_comment = NULL,
  created_by = NULL
) {
  # Maintain backwards compatibility if legacy param was passed explicitly
  if (!is.null(created_by)) {
    created_by <- created_by
  }

  clean_source_type <- toupper(trimws(as.character(support_source_type)))
  if (!clean_source_type %in% c("HUB", "EVENT")) {
    shiny::showModal(shiny::modalDialog(
      title = "Configuration Warning",
      shiny::p(
        "Support source type must be configured as either 'HUB' or 'EVENT' before creating an allocation."
      ),
      easyClose = TRUE
    ))
    return(NULL)
  }

  conn <- sql_manager("dit")
  on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

  tryCatch(
    {
      query <- glue::glue(
        "INSERT INTO {utils_resolve_schema('db_schema_01r')}.[ru_lead_allocations] (
           [ruhl_id],
           [support_source_type],
           [support_source_id],
           [rula_dateactive],
           [rula_dateended],
           [rula_active],
           [rula_comment],
           [created_date],
           [created_by]
         )
         OUTPUT INSERTED.rula_id
         VALUES (?, ?, ?, ?, ?, ?, ?, SYSUTCDATETIME(), ?);"
      )

      params <- list(
        as.integer(ruhl_id),
        clean_source_type,
        as.integer(support_source_id),
        if (!is.null(rula_dateactive)) {
          as.character(rula_dateactive)
        } else {
          NA_character_
        },
        if (!is.null(rula_dateended)) {
          as.character(rula_dateended)
        } else {
          NA_character_
        },
        as.integer(rula_active),
        if (!is.null(rula_comment)) {
          as.character(rula_comment)
        } else {
          NA_character_
        },
        as.character(created_by)
      )

      res <- DBI::dbGetQuery(conn, query, params = params)
      as.integer(res$rula_id[[1]])
    },
    error = function(e) {
      shiny::showModal(shiny::modalDialog(
        title = "Database Error",
        shiny::p(paste("Failed to create lead allocation record:", e$message)),
        easyClose = TRUE
      ))
      NULL
    }
  )
}

# ==============================================================================
# UPDATE / DEACTIVATE FUNCTIONS
# ==============================================================================

#' Update fields on an existing lead allocation record
#'
#' @param rula_id Integer. The primary key ID to update.
#' @param ruhl_id Integer. Optional updated lead school ID.
#' @param support_source_type Character. Optional (\code{'HUB'} or \code{'EVENT'}).
#' @param support_source_id Integer. Optional updated source ID.
#' @param rula_dateactive Date or character (\code{'YYYY-MM-DD'}). Optional.
#' @param rula_dateended Date or character (\code{'YYYY-MM-DD'}). Optional.
#' @param rula_active Logical. Optional.
#' @param rula_comment Character. Optional.
#' @param modified_by Character. Username or audit identifier.
#' @return Integer number of affected rows, or NULL on error.
#' @export
update_ru_lead_allocation <- function(
  rula_id,
  ruhl_id = NULL,
  support_source_type = NULL,
  support_source_id = NULL,
  rula_dateactive = NULL,
  rula_dateended = NULL,
  rula_active = NULL,
  rula_comment = NULL,
  modified_by = NULL
) {
  if (!is.null(modified_by)) {
    modified_by <- modified_by
  }

  set_clauses <- character()
  params <- list()

  if (!is.null(ruhl_id)) {
    set_clauses <- c(set_clauses, "[ruhl_id] = ?")
    params <- append(params, list(as.integer(ruhl_id)))
  }

  if (!is.null(support_source_type)) {
    clean_source_type <- toupper(trimws(as.character(support_source_type)))
    if (!clean_source_type %in% c("HUB", "EVENT")) {
      shiny::showModal(shiny::modalDialog(
        title = "Configuration Warning",
        shiny::p(
          "Support source type must be configured as either 'HUB' or 'EVENT'."
        ),
        easyClose = TRUE
      ))
      return(NULL)
    }
    set_clauses <- c(set_clauses, "[support_source_type] = ?")
    params <- append(params, list(clean_source_type))
  }

  if (!is.null(support_source_id)) {
    set_clauses <- c(set_clauses, "[support_source_id] = ?")
    params <- append(params, list(as.integer(support_source_id)))
  }

  if (!missing(rula_dateactive)) {
    set_clauses <- c(set_clauses, "[rula_dateactive] = ?")
    params <- append(
      params,
      list(
        if (!is.null(rula_dateactive)) {
          as.character(rula_dateactive)
        } else {
          NA_character_
        }
      )
    )
  }

  if (!missing(rula_dateended)) {
    set_clauses <- c(set_clauses, "[rula_dateended] = ?")
    params <- append(
      params,
      list(
        if (!is.null(rula_dateended)) {
          as.character(rula_dateended)
        } else {
          NA_character_
        }
      )
    )
  }

  if (!is.null(rula_active)) {
    set_clauses <- c(set_clauses, "[rula_active] = ?")
    params <- append(params, list(as.integer(rula_active)))
  }

  if (!missing(rula_comment)) {
    set_clauses <- c(set_clauses, "[rula_comment] = ?")
    params <- append(
      params,
      list(
        if (!is.null(rula_comment)) {
          as.character(rula_comment)
        } else {
          NA_character_
        }
      )
    )
  }

  if (length(set_clauses) == 0) {
    shiny::showModal(shiny::modalDialog(
      title = "Configuration Warning",
      shiny::p("No fields were supplied to update."),
      easyClose = TRUE
    ))
    return(0L)
  }

  set_clauses <- c(
    set_clauses,
    "[modified_date] = SYSUTCDATETIME()",
    "[modified_by] = ?"
  )
  params <- append(params, list(as.character(modified_by)))
  params <- append(params, list(as.integer(rula_id)))

  conn <- sql_manager("dit")
  on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

  tryCatch(
    {
      query <- glue::glue(
        "UPDATE {utils_resolve_schema('db_schema_01r')}.[ru_lead_allocations]
         SET {paste(set_clauses, collapse = ', ')}
         WHERE [rula_id] = ?;"
      )
      DBI::dbExecute(conn, query, params = params)
    },
    error = function(e) {
      shiny::showModal(shiny::modalDialog(
        title = "Database Error",
        shiny::p(paste("Failed to update lead allocation record:", e$message)),
        easyClose = TRUE
      ))
      NULL
    }
  )
}

#' Soft delete / deactivate a lead allocation record
#'
#' @param rula_id Integer. The primary key ID to deactivate.
#' @param modified_by Character. Username or audit identifier.
#' @return Integer number of affected rows, or NULL on error.
#' @export
deactivate_ru_lead_allocation <- function(
  rula_id,
  user_id
) {
  update_ru_lead_allocation(
    rula_id = rula_id,
    rula_active = FALSE,
    rula_dateended = Sys.Date(),
    modified_by = user_id
  )
}

# ==============================================================================
# DELETE FUNCTION
# ==============================================================================

#' Hard delete a lead allocation record
#'
#' @param rula_id Integer. The primary key ID to delete.
#' @return Integer number of affected rows, or NULL on error.
#' @export
delete_ru_lead_allocation <- function(rula_id) {
  conn <- sql_manager("dit")
  on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

  tryCatch(
    {
      query <- glue::glue(
        "DELETE FROM {utils_resolve_schema('db_schema_01r')}.[ru_lead_allocations] WHERE [rula_id] = ?;"
      )
      DBI::dbExecute(conn, query, params = list(as.integer(rula_id)))
    },
    error = function(e) {
      shiny::showModal(shiny::modalDialog(
        title = "Database Error",
        shiny::p(paste("Failed to delete lead allocation record:", e$message)),
        easyClose = TRUE
      ))
      NULL
    }
  )
}
