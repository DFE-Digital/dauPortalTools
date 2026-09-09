#' Select lead allocations joined with lead school metadata by support source
#'
#' @param source_type Character. 'HUB' or 'EVENT'.
#' @param source_id Integer. The support record ID (e.g. ruhsr_id) or event ID.
#' @return A \code{tibble} of matching records, or empty tibble on error.
#' @export
select_ru_lead_allocations_with_school_by_source <- function(
  source_type,
  source_id
) {
  clean_source_type <- toupper(trimws(as.character(source_type)))
  if (!clean_source_type %in% c("HUB", "EVENT")) {
    shiny::showModal(shiny::modalDialog(
      title = "Validation Warning",
      shiny::p("Support source type must be either 'HUB' or 'EVENT'."),
      easyClose = TRUE
    ))
    return(tibble::tibble())
  }

  conn <- sql_manager("dit")
  on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

  tryCatch(
    {
      query <- glue::glue(
        "SELECT 
         la.[rula_id],
         la.[ruhl_id],
         ls.[ruhl_entity_type],
         ls.[ruhl_entity_id],
         la.[support_source_type],
         la.[support_source_id],
         la.[rula_dateactive],
         la.[rula_dateended],
         la.[rula_active],
         la.[rula_comment],
         la.[created_date],
         la.[created_by],
         la.[modified_date],
         la.[modified_by]
       FROM {utils_resolve_schema('db_schema_01r')}.[ru_lead_allocations] la
       INNER JOIN {utils_resolve_schema('db_schema_01r')}.[ru_lead_schools] ls 
         ON la.[ruhl_id] = ls.[ruhl_id]
       WHERE la.[support_source_type] = ?
         AND la.[support_source_id] = ?
       ORDER BY la.[rula_active] DESC, la.[rula_dateactive] DESC;"
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
          "Failed to retrieve lead allocations with entity info:",
          e$message
        )),
        easyClose = TRUE
      ))
      tibble::tibble()
    }
  )
}

#' Select all active/historical support allocations linked to a lead school
#'
#' @param ruhl_id Integer. The lead school primary key.
#' @return A \code{tibble} of allocations joined with target school/support contract metadata.
#' @export
select_ru_lead_allocations_by_lead_with_targets <- function(ruhl_id) {
  conn <- sql_manager("dit")
  on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

  tryCatch(
    {
      query <- glue::glue(
        "SELECT 
         la.[rula_id],
         la.[ruhl_id],
         la.[support_source_type],
         la.[support_source_id],
         sr.[ruhsr_entity_type],
         sr.[ruhsr_entity_id],
         st.[ruht_name] AS [support_type_name],
         hb.[ruhb_name],
         la.[rula_dateactive],
         la.[rula_dateended],
         la.[rula_active],
         la.[rula_comment],
         la.[created_date],
         la.[created_by],
         la.[modified_date],
         la.[modified_by]
       FROM {utils_resolve_schema('db_schema_01r')}.[ru_lead_allocations] la
       LEFT JOIN {utils_resolve_schema('db_schema_01r')}.[ruh_support_records] sr 
         ON la.[support_source_id] = sr.[ruhsr_id] AND la.[support_source_type] = 'HUB'
       LEFT JOIN {utils_resolve_schema('db_schema_01r')}.[ruh_support_types] st 
         ON sr.[ruht_id] = st.[ruht_id]
       LEFT JOIN {utils_resolve_schema('db_schema_01r')}.[ruh_hubs] hb 
         ON sr.[ruhb_id] = hb.[ruhb_id]
       WHERE la.[ruhl_id] = ?
       ORDER BY la.[rula_active] DESC, la.[rula_dateactive] DESC;"
      )
      res <- DBI::dbGetQuery(conn, query, params = list(as.integer(ruhl_id)))
      tibble::as_tibble(res)
    },
    error = function(e) {
      shiny::showModal(shiny::modalDialog(
        title = "Database Error",
        shiny::p(paste(
          "Failed to retrieve allocations for lead provider:",
          e$message
        )),
        easyClose = TRUE
      ))
      tibble::tibble()
    }
  )
}

#' Select Lead Schools associated with a Hub including active caseload metrics
#'
#' @param hub_id Integer. Primary key of the Hub.
#' @return A \code{tibble} containing lead schools and their caseload counts.
#' @export
select_ru_lead_schools_by_hub_with_metrics <- function(hub_id) {
  conn <- sql_manager("dit")
  on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

  tryCatch(
    {
      query <- glue::glue(
        "SELECT 
         ls.[ruhl_id],
         ls.[ruhb_id],
         ls.[ruhl_entity_type],
         ls.[ruhl_entity_id],
         ls.[ruhl_dateactive],
         ls.[ruhl_dateended],
         ls.[ruhl_active],
         ls.[ruhl_comment],
         COUNT(CASE WHEN la.[rula_active] = 1 THEN 1 END) AS [active_caseload],
         COUNT(la.[rula_id]) AS [total_caseload]
       FROM {utils_resolve_schema('db_schema_01r')}.[ru_lead_schools] ls
       LEFT JOIN {utils_resolve_schema('db_schema_01r')}.[ru_lead_allocations] la 
         ON ls.[ruhl_id] = la.[ruhl_id]
       WHERE ls.[ruhb_id] = ?
       GROUP BY 
         ls.[ruhl_id],
         ls.[ruhb_id],
         ls.[ruhl_entity_type],
         ls.[ruhl_entity_id],
         ls.[ruhl_dateactive],
         ls.[ruhl_dateended],
         ls.[ruhl_active],
         ls.[ruhl_comment]
       ORDER BY ls.[ruhl_active] DESC, ls.[ruhl_dateactive] DESC;"
      )
      res <- DBI::dbGetQuery(conn, query, params = list(as.integer(hub_id)))
      tibble::as_tibble(res)
    },
    error = function(e) {
      shiny::showModal(shiny::modalDialog(
        title = "Database Error",
        shiny::p(paste(
          "Failed to retrieve lead providers for hub:",
          e$message
        )),
        easyClose = TRUE
      ))
      tibble::tibble()
    }
  )
}

#' Select Lead School records by entity type and ID with Hub names
#'
#' @param entity_type Character. Entity type (e.g. 'school', 'trust', 'la').
#' @param entity_id Character or numeric. The entity ID or URN.
#' @return A \code{tibble} of matching lead records.
#' @export
select_ru_lead_schools_by_entity_with_hub <- function(entity_type, entity_id) {
  conn <- sql_manager("dit")
  on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

  tryCatch(
    {
      query <- glue::glue(
        "SELECT 
         ls.[ruhl_id],
         ls.[ruhl_entity_type],
         ls.[ruhl_entity_id],
         ls.[ruhb_id],
         COALESCE(hb.[ruhb_name], 'Global / Unassigned') AS [hub_name],
         ls.[ruhl_dateactive],
         ls.[ruhl_dateended],
         ls.[ruhl_active],
         ls.[ruhl_comment]
       FROM {utils_resolve_schema('db_schema_01r')}.[ru_lead_schools] ls
       LEFT JOIN {utils_resolve_schema('db_schema_01r')}.[ruh_hubs] hb 
         ON ls.[ruhb_id] = hb.[ruhb_id]
       WHERE ls.[ruhl_entity_type] = ?
         AND ls.[ruhl_entity_id] = ?
       ORDER BY ls.[ruhl_active] DESC, ls.[ruhl_dateactive] DESC;"
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
        shiny::p(paste(
          "Failed to retrieve lead records for entity:",
          e$message
        )),
        easyClose = TRUE
      ))
      tibble::tibble()
    }
  )
}

#' Select Lead Providers allocated to a specific Event Type
#'
#' @param ruevt_id Integer. The event master / type ID.
#' @return A \code{tibble} of lead schools allocated to events under this type.
#' @export
select_ru_event_lead_providers_by_type <- function(ruevt_id) {
  conn <- sql_manager("dit")
  on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

  tryCatch(
    {
      query <- glue::glue(
        "SELECT DISTINCT
         ls.[ruhl_id],
         ls.[ruhl_entity_type],
         ls.[ruhl_entity_id],
         ls.[ruhl_dateactive],
         ls.[ruhl_active],
         COUNT(la.[rula_id]) AS [event_allocations_count]
       FROM {utils_resolve_schema('db_schema_01r')}.[ru_lead_allocations] la
       INNER JOIN {utils_resolve_schema('db_schema_01r')}.[ru_lead_schools] ls 
         ON la.[ruhl_id] = ls.[ruhl_id]
       INNER JOIN {utils_resolve_schema('db_schema_01r')}.[ru_events] e 
         ON la.[support_source_id] = e.[ruev_id] AND la.[support_source_type] = 'EVENT'
       WHERE e.[ruevt_id] = ?
       GROUP BY 
         ls.[ruhl_id],
         ls.[ruhl_entity_type],
         ls.[ruhl_entity_id],
         ls.[ruhl_dateactive],
         ls.[ruhl_active]
       ORDER BY ls.[ruhl_active] DESC;"
      )
      res <- DBI::dbGetQuery(conn, query, params = list(as.integer(ruevt_id)))
      tibble::as_tibble(res)
    },
    error = function(e) {
      shiny::showModal(shiny::modalDialog(
        title = "Database Error",
        shiny::p(paste("Failed to retrieve event lead providers:", e$message)),
        easyClose = TRUE
      ))
      tibble::tibble()
    }
  )
}
