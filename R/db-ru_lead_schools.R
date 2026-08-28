#' @importFrom DBI dbGetQuery dbExecute dbDisconnect
#' @importFrom glue glue
#' @importFrom tibble as_tibble
#' @importFrom shiny showModal modalDialog p
NULL

# ==============================================================================
# READ / SELECT FUNCTIONS
# ==============================================================================

#' Select all lead school records
#'
#' @return A \code{tibble} containing all lead school records, or NULL on error.
#' @export
select_ru_lead_schools_all <- function() {
  conn <- sql_manager("dit")
  on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

  tryCatch(
    {
      query <- glue::glue(
        "SELECT * FROM {utils_resolve_schema('db_schema_01r')}.[ru_lead_schools];"
      )
      res <- DBI::dbGetQuery(conn, query)
      tibble::as_tibble(res)
    },
    error = function(e) {
      shiny::showModal(shiny::modalDialog(
        title = "Database Error",
        shiny::p(paste("Failed to retrieve lead schools:", e$message)),
        easyClose = TRUE
      ))
      NULL
    }
  )
}

#' Select a single lead school record by primary key ID
#'
#' @param ruhl_id Integer. The lead school record ID (\code{ruhl_id}).
#' @return A single-row \code{tibble}, empty tibble, or NULL on error.
#' @export
select_ru_lead_school_by_id <- function(ruhl_id) {
  conn <- sql_manager("dit")
  on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

  tryCatch(
    {
      query <- glue::glue(
        "SELECT * FROM {utils_resolve_schema('db_schema_01r')}.[ru_lead_schools] WHERE [ruhl_id] = ?;"
      )
      res <- DBI::dbGetQuery(conn, query, params = list(as.integer(ruhl_id)))
      tibble::as_tibble(res)
    },
    error = function(e) {
      shiny::showModal(shiny::modalDialog(
        title = "Database Error",
        shiny::p(paste("Failed to retrieve lead school record:", e$message)),
        easyClose = TRUE
      ))
      NULL
    }
  )
}

#' Select lead school records by entity type and entity ID
#'
#' @param entity_type Character. Entity category (e.g., 'URN', 'LAESTAB', 'MAT').
#' @param entity_id Character or numeric. The identifier value.
#' @return A \code{tibble} of matching records, or NULL on error.
#' @export
select_ru_lead_schools_by_entity <- function(entity_type, entity_id) {
  conn <- sql_manager("dit")
  on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

  tryCatch(
    {
      query <- glue::glue(
        "SELECT * FROM {utils_resolve_schema('db_schema_01r')}.[ru_lead_schools]
       WHERE [ruhl_entity_type] = ?
         AND [ruhl_entity_id] = ?;"
      )
      res <- DBI::dbGetQuery(
        conn,
        query,
        params = list(as.character(entity_type), as.character(entity_id))
      )
      tibble::as_tibble(res)
    },
    error = function(e) {
      shiny::showModal(shiny::modalDialog(
        title = "Database Error",
        shiny::p(paste("Failed to retrieve lead school by entity:", e$message)),
        easyClose = TRUE
      ))
      NULL
    }
  )
}

#' Select lead school records associated with a specific Hub
#'
#' @param ruhb_id Integer. The hub ID (\code{ruhb_id}).
#' @return A \code{tibble} of matching lead schools, or NULL on error.
#' @export
select_ru_lead_schools_by_hub <- function(ruhb_id) {
  conn <- sql_manager("dit")
  on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

  tryCatch(
    {
      query <- glue::glue(
        "SELECT * FROM {utils_resolve_schema('db_schema_01r')}.[ru_lead_schools] WHERE [ruhb_id] = ?;"
      )
      res <- DBI::dbGetQuery(conn, query, params = list(as.integer(ruhb_id)))
      tibble::as_tibble(res)
    },
    error = function(e) {
      shiny::showModal(shiny::modalDialog(
        title = "Database Error",
        shiny::p(paste(
          "Failed to retrieve lead schools by hub ID:",
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

#' Insert a new lead school record
#'
#' @param ruhl_entity_type Character. Type of entity (e.g. 'URN', 'MAT').
#' @param ruhl_entity_id Character or numeric. The entity identifier code.
#' @param ruhb_id Integer. Optional Hub ID association.
#' @param ruhl_dateactive Date or character (\code{'YYYY-MM-DD'}). Defaults to current date.
#' @param ruhl_dateended Date or character (\code{'YYYY-MM-DD'}). Optional.
#' @param ruhl_active Logical. Default is \code{TRUE}.
#' @param ruhl_comment Character. Optional notes/context.
#' @param user_id_created Character. Username or audit identifier.
#' @return Integer \code{ruhl_id} of the inserted record, or NULL on error.
#' @export
create_ru_lead_school <- function(
  ruhl_entity_type,
  ruhl_entity_id,
  ruhb_id = NULL,
  ruhl_dateactive = Sys.Date(),
  ruhl_dateended = NULL,
  ruhl_active = TRUE,
  ruhl_comment = NULL,
  user_id_created = Sys.getenv("USERNAME", "SYSTEM")
) {
  if (
    missing(ruhl_entity_type) ||
      is.null(ruhl_entity_type) ||
      nzchar(trimws(ruhl_entity_type)) == 0 ||
      missing(ruhl_entity_id) ||
      is.null(ruhl_entity_id) ||
      nzchar(trimws(as.character(ruhl_entity_id))) == 0
  ) {
    shiny::showModal(shiny::modalDialog(
      title = "Configuration Warning",
      shiny::p(
        "Both Entity Type and Entity ID must be specified to create a lead school record."
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
        "INSERT INTO {utils_resolve_schema('db_schema_01r')}.[ru_lead_schools] (
         [ruhb_id],
         [ruhl_entity_type],
         [ruhl_entity_id],
         [ruhl_dateactive],
         [ruhl_dateended],
         [ruhl_active],
         [ruhl_comment],
         [date_created],
         [user_id_created]
       )
       OUTPUT INSERTED.ruhl_id
       VALUES (?, ?, ?, ?, ?, ?, ?, SYSUTCDATETIME(), ?);"
      )

      params <- list(
        if (!is.null(ruhb_id)) as.integer(ruhb_id) else NA_integer_,
        as.character(trimws(ruhl_entity_type)),
        as.character(trimws(as.character(ruhl_entity_id))),
        if (!is.null(ruhl_dateactive)) {
          as.character(ruhl_dateactive)
        } else {
          NA_character_
        },
        if (!is.null(ruhl_dateended)) {
          as.character(ruhl_dateended)
        } else {
          NA_character_
        },
        as.integer(ruhl_active),
        if (!is.null(ruhl_comment)) {
          as.character(ruhl_comment)
        } else {
          NA_character_
        },
        as.character(user_id_created)
      )

      res <- DBI::dbGetQuery(conn, query, params = params)
      as.integer(res$ruhl_id[[1]])
    },
    error = function(e) {
      shiny::showModal(shiny::modalDialog(
        title = "Database Error",
        shiny::p(paste("Failed to create lead school record:", e$message)),
        easyClose = TRUE
      ))
      NULL
    }
  )
}

# ==============================================================================
# UPDATE / DEACTIVATE FUNCTIONS
# ==============================================================================

#' Update fields on an existing lead school record
#'
#' @param ruhl_id Integer. The primary key ID to update.
#' @param ruhl_entity_type Character. Optional updated entity type.
#' @param ruhl_entity_id Character. Optional updated entity ID.
#' @param ruhb_id Integer. Optional updated Hub ID.
#' @param ruhl_dateactive Date or character (\code{'YYYY-MM-DD'}). Optional.
#' @param ruhl_dateended Date or character (\code{'YYYY-MM-DD'}). Optional.
#' @param ruhl_active Logical. Optional.
#' @param ruhl_comment Character. Optional.
#' @param user_id_edited Character. Username or audit identifier.
#' @return Integer number of affected rows, or NULL on error.
#' @export
update_ru_lead_school <- function(
  ruhl_id,
  ruhl_entity_type = NULL,
  ruhl_entity_id = NULL,
  ruhb_id = NULL,
  ruhl_dateactive = NULL,
  ruhl_dateended = NULL,
  ruhl_active = NULL,
  ruhl_comment = NULL,
  user_id_edited = Sys.getenv("USERNAME", "SYSTEM")
) {
  set_clauses <- character()
  params <- list()

  if (!is.null(ruhl_entity_type)) {
    set_clauses <- c(set_clauses, "[ruhl_entity_type] = ?")
    params <- append(params, list(as.character(trimws(ruhl_entity_type))))
  }

  if (!is.null(ruhl_entity_id)) {
    set_clauses <- c(set_clauses, "[ruhl_entity_id] = ?")
    params <- append(
      params,
      list(as.character(trimws(as.character(ruhl_entity_id))))
    )
  }

  if (!missing(ruhb_id)) {
    set_clauses <- c(set_clauses, "[ruhb_id] = ?")
    params <- append(
      params,
      list(if (!is.null(ruhb_id)) as.integer(ruhb_id) else NA_integer_)
    )
  }

  if (!missing(ruhl_dateactive)) {
    set_clauses <- c(set_clauses, "[ruhl_dateactive] = ?")
    params <- append(
      params,
      list(
        if (!is.null(ruhl_dateactive)) {
          as.character(ruhl_dateactive)
        } else {
          NA_character_
        }
      )
    )
  }

  if (!missing(ruhl_dateended)) {
    set_clauses <- c(set_clauses, "[ruhl_dateended] = ?")
    params <- append(
      params,
      list(
        if (!is.null(ruhl_dateended)) {
          as.character(ruhl_dateended)
        } else {
          NA_character_
        }
      )
    )
  }

  if (!is.null(ruhl_active)) {
    set_clauses <- c(set_clauses, "[ruhl_active] = ?")
    params <- append(params, list(as.integer(ruhl_active)))
  }

  if (!missing(ruhl_comment)) {
    set_clauses <- c(set_clauses, "[ruhl_comment] = ?")
    params <- append(
      params,
      list(
        if (!is.null(ruhl_comment)) {
          as.character(ruhl_comment)
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
    "[date_edited] = SYSUTCDATETIME()",
    "[user_id_edited] = ?"
  )
  params <- append(params, list(as.character(user_id_edited)))
  params <- append(params, list(as.integer(ruhl_id)))

  conn <- sql_manager("dit")
  on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

  tryCatch(
    {
      query <- glue::glue(
        "UPDATE {utils_resolve_schema('db_schema_01r')}.[ru_lead_schools]
       SET {paste(set_clauses, collapse = ', ')}
       WHERE [ruhl_id] = ?;"
      )
      DBI::dbExecute(conn, query, params = params)
    },
    error = function(e) {
      shiny::showModal(shiny::modalDialog(
        title = "Database Error",
        shiny::p(paste("Failed to update lead school record:", e$message)),
        easyClose = TRUE
      ))
      NULL
    }
  )
}

#' Soft delete / deactivate a lead school record
#'
#' @param ruhl_id Integer. The primary key ID to deactivate.
#' @param user_id_edited Character. Username or audit identifier.
#' @return Integer number of affected rows, or NULL on error.
#' @export
deactivate_ru_lead_school <- function(
  ruhl_id,
  user_id_edited = Sys.getenv("USERNAME", "SYSTEM")
) {
  update_ru_lead_school(
    ruhl_id = ruhl_id,
    ruhl_active = FALSE,
    ruhl_dateended = Sys.Date(),
    user_id_edited = user_id_edited
  )
}

# ==============================================================================
# DELETE FUNCTION
# ==============================================================================

#' Hard delete a lead school record
#'
#' @param ruhl_id Integer. The primary key ID to delete.
#' @return Integer number of affected rows, or NULL on error.
#' @export
delete_ru_lead_school <- function(ruhl_id) {
  conn <- sql_manager("dit")
  on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

  tryCatch(
    {
      query <- glue::glue(
        "DELETE FROM {utils_resolve_schema('db_schema_01r')}.[ru_lead_schools] WHERE [ruhl_id] = ?;"
      )
      DBI::dbExecute(conn, query, params = list(as.integer(ruhl_id)))
    },
    error = function(e) {
      shiny::showModal(shiny::modalDialog(
        title = "Database Error",
        shiny::p(paste("Failed to delete lead school record:", e$message)),
        easyClose = TRUE
      ))
      NULL
    }
  )
}
