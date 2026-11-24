# ==============================================================================
# File: R/db-publications.R
# Purpose: Database query and mutation functions for DAU Statistical
#   Publications, data table links, and maintainer assignments.
#
# Functions:
#   - db_pub_get_data_types()            : Fetch active data types for filtering
#   - db_pub_get_available_data_tables() : Fetch active data tables for linking
#   - db_pub_get_available_users()       : Fetch active users for lead dropdowns
#   - db_pub_get_directory_index()       : Fetch publication catalogue index
#   - db_pub_get_card_details()          : Fetch single publication metadata
#   - db_pub_create_publication()        : Insert publication, links & leads
#   - db_admin_get_publications()        : Fetch all publications for admin grid
#   - db_admin_save_publication()        : Upsert publication metadata
#   - db_admin_get_pub_maintainers()     : Fetch maintainers for a publication
#   - db_admin_add_pub_maintainer()      : Assign a maintainer to a publication
#   - db_admin_delete_pub_maintainer()   : Remove a maintainer from a publication
#   - db_admin_get_data_pub_links()      : Fetch publications linked to a dataset
#   - db_admin_add_data_pub_link()       : Link a publication to a dataset
#   - db_admin_delete_data_pub_link()    : Unlink a publication from a dataset
#
# Dependencies: DBI, glue, internal
# ==============================================================================

#' Retrieve Active Data Types for Publication Filtering
#'
#' Queries the shared `data_type` reference table for active data categories
#' used to filter the publications directory.
#'
#' @details
#' The database schema is resolved via [utils_resolve_schema()], and
#' the query is executed using [utils_db_get_query()].
#'
#' @return A [`data.frame`] containing `type_id` and `type_name`.
#' @export
db_pub_get_data_types <- function() {
  log_event("Starting db_pub_get_data_types", debug = TRUE)

  conn <- sql_manager("dit")
  on.exit(
    {
      try(DBI::dbDisconnect(conn), silent = TRUE)
      log_event("Finished db_pub_get_data_types")
    },
    add = TRUE
  )

  schema_01sr <- utils_resolve_schema("db_schema_01sr")

  query <- glue::glue_sql(
    "
    SELECT
      [type_id],
      [type_name]
    FROM {schema_01sr}.[data_type]
    WHERE [is_active] = 1
    ORDER BY [type_name] ASC;
    ",
    .con = conn
  )

  tryCatch(
    utils_db_get_query(conn, query),
    error = function(e) {
      log_event(
        paste("Error in db_pub_get_data_types:", conditionMessage(e)),
        level = "ERROR"
      )
      data.frame()
    }
  )
}


#' Retrieve Available Data Tables for Publication Linking
#'
#' Queries the shared `data_table` and `data_type` tables for active datasets
#' that can be linked to a publication.
#'
#' @details
#' The database schema is resolved via [utils_resolve_schema()], and
#' the query is executed using [utils_db_get_query()].
#'
#' @return A [`data.frame`] containing `data_id`, `data_name`, and `type_name`.
#' @export
db_pub_get_available_data_tables <- function() {
  log_event("Starting db_pub_get_available_data_tables", debug = TRUE)

  conn <- sql_manager("dit")
  on.exit(
    {
      try(DBI::dbDisconnect(conn), silent = TRUE)
      log_event("Finished db_pub_get_available_data_tables")
    },
    add = TRUE
  )

  schema_01sr <- utils_resolve_schema("db_schema_01sr")

  query <- glue::glue_sql(
    "
    SELECT
      d.[data_id],
      d.[data_name],
      t.[type_name]
    FROM {schema_01sr}.[data_table] d
    LEFT JOIN {schema_01sr}.[data_type] t
      ON d.[data_type_fk] = t.[type_id]
    WHERE d.[is_active] = 1
    ORDER BY t.[type_name] ASC, d.[data_name] ASC;
    ",
    .con = conn
  )

  tryCatch(
    utils_db_get_query(conn, query),
    error = function(e) {
      log_event(
        paste(
          "Error in db_pub_get_available_data_tables:",
          conditionMessage(e)
        ),
        level = "ERROR"
      )
      data.frame()
    }
  )
}


#' Retrieve Available Portal Users for Maintainer Selection
#'
#' Queries the shared `users` directory for active users with valid email
#' addresses to populate primary and secondary publication lead selectors.
#'
#' @details
#' The database schema is resolved via [utils_resolve_schema()], and
#' the query is executed using [utils_db_get_query()].
#'
#' @return A [`data.frame`] containing `user_id` and `email`.
#' @export
db_pub_get_available_users <- function() {
  log_event("Starting db_pub_get_available_users", debug = TRUE)

  conn <- sql_manager("dit")
  on.exit(
    {
      try(DBI::dbDisconnect(conn), silent = TRUE)
      log_event("Finished db_pub_get_available_users")
    },
    add = TRUE
  )

  schema_01sr <- utils_resolve_schema("db_schema_01sr")

  query <- glue::glue_sql(
    "
    SELECT
      [user_id],
      [email]
    FROM {schema_01sr}.[users]
    WHERE [email] IS NOT NULL
      AND LTRIM(RTRIM([email])) <> ''
    ORDER BY [email] ASC;
    ",
    .con = conn
  )

  tryCatch(
    utils_db_get_query(conn, query),
    error = function(e) {
      log_event(
        paste("Error in db_pub_get_available_users:", conditionMessage(e)),
        level = "ERROR"
      )
      data.frame()
    }
  )
}


#' Retrieve Publication Catalogue Index
#'
#' Queries active publications along with aggregated data type IDs for search
#' and category filtering on the publications directory page.
#'
#' @details
#' The database schema is resolved via [utils_resolve_schema()], and
#' the query is executed using [utils_db_get_query()].
#'
#' @return A [`data.frame`] containing `pub_id`, `pub_name`, `pub_description`,
#'   `publication_type`, `published_date`, `team`, and pipe-delimited `type_ids`.
#' @export
db_pub_get_directory_index <- function() {
  log_event("Starting db_pub_get_directory_index", debug = TRUE)

  conn <- sql_manager("dit")
  on.exit(
    {
      try(DBI::dbDisconnect(conn), silent = TRUE)
      log_event("Finished db_pub_get_directory_index")
    },
    add = TRUE
  )

  schema_01sr <- utils_resolve_schema("db_schema_01sr")

  query <- glue::glue_sql(
    "
    SELECT
      p.[pub_id],
      p.[pub_name],
      p.[pub_description],
      p.[publication_type],
      p.[published_date],
      p.[team],
      STRING_AGG(CAST(d.[data_type_fk] AS VARCHAR(10)), '|') AS [type_ids]
    FROM {schema_01sr}.[publication_list] p
    LEFT JOIN {schema_01sr}.[publication_data_links] l
      ON p.[pub_id] = l.[pub_id] AND l.[is_active] = 1
    LEFT JOIN {schema_01sr}.[data_table] d
      ON l.[data_id] = d.[data_id] AND d.[is_active] = 1
    WHERE p.[pub_active] = 1
    GROUP BY
      p.[pub_id],
      p.[pub_name],
      p.[pub_description],
      p.[publication_type],
      p.[published_date],
      p.[team]
    ORDER BY p.[published_date] DESC, p.[pub_name] ASC;
    ",
    .con = conn
  )

  tryCatch(
    utils_db_get_query(conn, query),
    error = function(e) {
      log_event(
        paste("Error in db_pub_get_directory_index:", conditionMessage(e)),
        level = "ERROR"
      )
      data.frame()
    }
  )
}


#' Retrieve Single Publication Details for Card Rendering
#'
#' Queries full metadata, aggregated data type badges, and ordered maintainer
#' contacts for a single publication by its primary key `pub_id`.
#'
#' @param pub_id An integer or numeric scalar identifying the publication.
#'
#' @details
#' The database schema is resolved via [utils_resolve_schema()], and
#' the query is executed using [utils_db_get_query()].
#'
#' @return A 1-row [`data.frame`] containing publication metadata,
#'   `maintainers_str`, `type_names`, and `type_colours`.
#' @export
db_pub_get_card_details <- function(pub_id) {
  log_event("Starting db_pub_get_card_details", debug = TRUE)

  pid <- suppressWarnings(as.integer(pub_id))
  if (length(pid) == 0 || is.na(pid)) {
    log_event(
      "Invalid pub_id supplied to db_pub_get_card_details",
      debug = TRUE
    )
    return(data.frame())
  }

  conn <- sql_manager("dit")
  on.exit(
    {
      try(DBI::dbDisconnect(conn), silent = TRUE)
      log_event("Finished db_pub_get_card_details")
    },
    add = TRUE
  )

  schema_01sr <- utils_resolve_schema("db_schema_01sr")

  query <- glue::glue_sql(
    "
    WITH PubTypes AS (
      SELECT
        l.[pub_id],
        t.[type_id],
        t.[type_name],
        t.[type_colour]
      FROM {schema_01sr}.[publication_data_links] l
      INNER JOIN {schema_01sr}.[data_table] d
        ON l.[data_id] = d.[data_id]
      INNER JOIN {schema_01sr}.[data_type] t
        ON d.[data_type_fk] = t.[type_id]
      WHERE l.[is_active] = 1
        AND d.[is_active] = 1
        AND t.[is_active] = 1
        AND l.[pub_id] = {pid}
      GROUP BY l.[pub_id], t.[type_id], t.[type_name], t.[type_colour]
    ),
    Maintainers AS (
      SELECT
        m.[pub_id],
        STRING_AGG(CONCAT(u.[email], '::', CAST(m.[is_primary] AS VARCHAR(1))), '; ')
          WITHIN GROUP (ORDER BY m.[is_primary] DESC, u.[email] ASC) AS [maintainers_str]
      FROM {schema_01sr}.[publication_maintainers] m
      INNER JOIN {schema_01sr}.[users] u
        ON m.[user_id] = u.[user_id]
      WHERE m.[pub_id] = {pid}
      GROUP BY m.[pub_id]
    )
    SELECT
      p.[pub_id],
      p.[pub_name],
      p.[pub_description],
      p.[pub_guidance],
      p.[pub_url],
      p.[publication_type],
      p.[published_date],
      p.[update_cycle],
      p.[team],
      m.[maintainers_str],
      STRING_AGG(pt.[type_name], '|') AS [type_names],
      STRING_AGG(pt.[type_colour], '|') AS [type_colours]
    FROM {schema_01sr}.[publication_list] p
    LEFT JOIN PubTypes pt
      ON p.[pub_id] = pt.[pub_id]
    LEFT JOIN Maintainers m
      ON p.[pub_id] = m.[pub_id]
    WHERE p.[pub_id] = {pid}
      AND p.[pub_active] = 1
    GROUP BY
      p.[pub_id],
      p.[pub_name],
      p.[pub_description],
      p.[pub_guidance],
      p.[pub_url],
      p.[publication_type],
      p.[published_date],
      p.[update_cycle],
      p.[team],
      m.[maintainers_str];
    ",
    .con = conn
  )

  tryCatch(
    utils_db_get_query(conn, query),
    error = function(e) {
      log_event(
        paste("Error in db_pub_get_card_details:", conditionMessage(e)),
        level = "ERROR"
      )
      data.frame()
    }
  )
}


#' Create a New Publication Record with Data Links and Maintainers
#'
#' Executes a transactional insert into `publication_list`, `publication_data_links`,
#' and `publication_maintainers`, recording audit metadata (`created_by`,
#' `created_on`, `updated_by`, `updated_on`).
#'
#' @param pub_name Character scalar title of the publication.
#' @param pub_description Optional character scalar summary description.
#' @param pub_guidance Optional character scalar methodology and guidance notes.
#' @param pub_url Optional character scalar external release URL.
#' @param publication_type Character scalar release classification.
#' @param published_date Date object or `YYYY-MM-DD` string of publication date.
#' @param update_cycle Character scalar release frequency (e.g., `"Annual"`).
#' @param team Character scalar owning team name.
#' @param data_ids Optional integer vector of linked `data_id` keys.
#' @param primary_maintainer_id Integer `user_id` of the primary lead.
#' @param secondary_maintainer_ids Optional integer vector of additional `user_id` leads.
#' @param user_id Optional integer or reactive returning the acting user's `user_id`.
#'
#' @details
#' The database schema is resolved via [utils_resolve_schema()], and statements
#' are executed within a `DBI` transaction.
#'
#' @return A named [`list`] with `success` (`logical`), `pub_id` (`integer` or `NULL`),
#'   and `message` (`character`).
#' @export
db_pub_create_publication <- function(
  pub_name,
  pub_description = NA_character_,
  pub_guidance = NA_character_,
  pub_url = NA_character_,
  publication_type = "Official Statistics",
  published_date = Sys.Date(),
  update_cycle = "Annual",
  team = "SSSR",
  data_ids = integer(0),
  primary_maintainer_id = NULL,
  secondary_maintainer_ids = integer(0),
  user_id = NULL
) {
  log_event("Starting db_pub_create_publication", debug = TRUE)

  conn <- sql_manager("dit")
  on.exit(
    {
      try(DBI::dbDisconnect(conn), silent = TRUE)
      log_event("Finished db_pub_create_publication")
    },
    add = TRUE
  )

  schema_01sr <- utils_resolve_schema("db_schema_01sr")

  uid_val <- if (is.function(user_id)) user_id() else user_id
  actor_id <- suppressWarnings(as.integer(uid_val))
  if (length(actor_id) == 0 || is.na(actor_id)) {
    actor_id <- NA_integer_
  }

  DBI::dbBegin(conn)

  tryCatch(
    {
      pub_date_val <- if (
        is.null(published_date) ||
          is.na(published_date) ||
          !nzchar(as.character(published_date))
      ) {
        NA_character_
      } else {
        as.character(as.Date(published_date))
      }

      # 1. Insert core publication record and capture generated pub_id
      insert_pub_sql <- glue::glue_sql(
        "
        INSERT INTO {schema_01sr}.[publication_list] (
          [pub_name],
          [pub_description],
          [pub_guidance],
          [pub_url],
          [publication_type],
          [published_date],
          [update_cycle],
          [team],
          [pub_active],
          [created_by],
          [created_on],
          [updated_by],
          [updated_on]
        )
        OUTPUT INSERTED.[pub_id]
        VALUES (
          {trimws(pub_name)},
          {pub_description},
          {pub_guidance},
          {pub_url},
          {publication_type},
          {pub_date_val},
          {update_cycle},
          {team},
          1,
          {actor_id},
          GETDATE(),
          {actor_id},
          GETDATE()
        );
        ",
        .con = conn
      )

      res <- utils_db_get_query(conn, insert_pub_sql)
      new_pub_id <- as.integer(res$pub_id[1])

      # 2. Insert linked data tables
      clean_data_ids <- unique(suppressWarnings(as.integer(data_ids)))
      clean_data_ids <- clean_data_ids[!is.na(clean_data_ids)]

      for (did in clean_data_ids) {
        link_sql <- glue::glue_sql(
          "
          INSERT INTO {schema_01sr}.[publication_data_links] (
            [pub_id],
            [data_id],
            [is_active],
            [created_by],
            [created_on],
            [updated_by],
            [updated_on]
          )
          VALUES (
            {new_pub_id},
            {did},
            1,
            {actor_id},
            GETDATE(),
            {actor_id},
            GETDATE()
          );
          ",
          .con = conn
        )
        DBI::dbExecute(conn, link_sql)
      }

      # 3. Insert primary maintainer
      prim_id <- suppressWarnings(as.integer(primary_maintainer_id))
      if (length(prim_id) > 0 && !is.na(prim_id)) {
        prim_sql <- glue::glue_sql(
          "
          INSERT INTO {schema_01sr}.[publication_maintainers] (
            [pub_id],
            [user_id],
            [is_primary],
            [created_by],
            [created_on],
            [updated_by],
            [updated_on]
          )
          VALUES (
            {new_pub_id},
            {prim_id},
            1,
            {actor_id},
            GETDATE(),
            {actor_id},
            GETDATE()
          );
          ",
          .con = conn
        )
        DBI::dbExecute(conn, prim_sql)
      }

      # 4. Insert secondary maintainers (excluding primary if duplicated)
      sec_ids <- unique(suppressWarnings(as.integer(secondary_maintainer_ids)))
      sec_ids <- sec_ids[!is.na(sec_ids) & !(sec_ids %in% prim_id)]

      for (uid in sec_ids) {
        sec_sql <- glue::glue_sql(
          "
          INSERT INTO {schema_01sr}.[publication_maintainers] (
            [pub_id],
            [user_id],
            [is_primary],
            [created_by],
            [created_on],
            [updated_by],
            [updated_on]
          )
          VALUES (
            {new_pub_id},
            {uid},
            0,
            {actor_id},
            GETDATE(),
            {actor_id},
            GETDATE()
          );
          ",
          .con = conn
        )
        DBI::dbExecute(conn, sec_sql)
      }

      DBI::dbCommit(conn)
      list(
        success = TRUE,
        pub_id = new_pub_id,
        message = "Publication added successfully."
      )
    },
    error = function(e) {
      try(DBI::dbRollback(conn), silent = TRUE)
      log_event(
        paste("Error in db_pub_create_publication:", conditionMessage(e)),
        level = "ERROR"
      )
      list(
        success = FALSE,
        pub_id = NULL,
        message = conditionMessage(e)
      )
    }
  )
}


# =============================================================================
# Publications & Maintainers Admin CRUD
# =============================================================================

#' Fetch All Registered Publications for Admin Management
#'
#' Retrieves all active and inactive records from `publication_list` along
#' with full audit trail metadata.
#'
#' @details
#' The database schema is resolved via [utils_resolve_schema()], and
#' the query is executed using [utils_db_get_query()].
#'
#' @return A [`data.frame`] of all registered publications.
#' @export
db_admin_get_publications <- function() {
  log_event("Starting db_admin_get_publications", debug = TRUE)

  conn <- sql_manager("dit")
  on.exit(
    {
      try(DBI::dbDisconnect(conn), silent = TRUE)
      log_event("Finished db_admin_get_publications")
    },
    add = TRUE
  )

  schema_01sr <- utils_resolve_schema("db_schema_01sr")

  query <- glue::glue_sql(
    "
    SELECT
      [pub_id],
      [pub_name],
      [pub_description],
      [pub_guidance],
      [pub_url],
      [publication_type],
      [published_date],
      [update_cycle],
      [team],
      [pub_active],
      [created_by],
      [created_on],
      [updated_by],
      [updated_on]
    FROM {schema_01sr}.[publication_list]
    ORDER BY [published_date] DESC, [pub_name] ASC;
    ",
    .con = conn
  )

  tryCatch(
    utils_db_get_query(conn, query),
    error = function(e) {
      log_event(
        paste("Error in db_admin_get_publications:", conditionMessage(e)),
        level = "ERROR"
      )
      data.frame()
    }
  )
}


#' Insert or Update a Publication Record
#'
#' Performs an `INSERT` when `pub_id` is `NULL` or `NA`, or an `UPDATE` when
#' `pub_id` is provided, populating standard audit trail fields.
#'
#' @param pub_id Optional integer primary key (`NULL` for new record).
#' @param pub_name Character scalar publication title.
#' @param pub_description Optional character scalar summary description.
#' @param pub_guidance Optional character scalar methodology and guidance notes.
#' @param pub_url Optional character scalar external release URL.
#' @param publication_type Character scalar release classification.
#' @param published_date Date object or `YYYY-MM-DD` string.
#' @param update_cycle Character scalar release frequency.
#' @param team Character scalar owning team name.
#' @param pub_active Integer visibility status (`1` = active, `0` = inactive).
#' @param user_id Optional integer `user_id` of the acting administrator.
#'
#' @details
#' The database schema is resolved via [utils_resolve_schema()].
#'
#' @return Integer count of affected rows.
#' @export
db_admin_save_publication <- function(
  pub_id = NULL,
  pub_name,
  pub_description = NULL,
  pub_guidance = NULL,
  pub_url = NULL,
  publication_type = "Official Statistics",
  published_date = Sys.Date(),
  update_cycle = "Annual",
  team = "SSSR",
  pub_active = 1L,
  user_id = NULL
) {
  log_event("Starting db_admin_save_publication", debug = TRUE)

  conn <- sql_manager("dit")
  on.exit(
    {
      try(DBI::dbDisconnect(conn), silent = TRUE)
      log_event("Finished db_admin_save_publication")
    },
    add = TRUE
  )

  schema_01sr <- utils_resolve_schema("db_schema_01sr")

  sql_or_null <- function(x) {
    if (is.null(x) || !nzchar(trimws(as.character(x)))) {
      DBI::SQL("NULL")
    } else {
      trimws(as.character(x))
    }
  }

  desc_val <- sql_or_null(pub_description)
  guidance_val <- sql_or_null(pub_guidance)
  url_val <- sql_or_null(pub_url)
  pub_date_val <- if (is.null(published_date) || is.na(published_date)) {
    DBI::SQL("NULL")
  } else {
    format(as.Date(published_date), "%Y-%m-%d")
  }
  actor_id <- if (is.null(user_id) || is.na(user_id)) {
    DBI::SQL("NULL")
  } else {
    as.integer(user_id)
  }

  if (is.null(pub_id) || is.na(pub_id)) {
    query <- glue::glue_sql(
      "
      INSERT INTO {schema_01sr}.[publication_list] (
        [pub_name],
        [pub_description],
        [pub_guidance],
        [pub_url],
        [publication_type],
        [published_date],
        [update_cycle],
        [team],
        [pub_active],
        [created_by],
        [created_on],
        [updated_by],
        [updated_on]
      )
      VALUES (
        {trimws(pub_name)},
        {desc_val},
        {guidance_val},
        {url_val},
        {publication_type},
        {pub_date_val},
        {update_cycle},
        {team},
        {as.integer(pub_active)},
        {actor_id},
        GETDATE(),
        {actor_id},
        GETDATE()
      );
      ",
      .con = conn
    )
  } else {
    query <- glue::glue_sql(
      "
      UPDATE {schema_01sr}.[publication_list]
      SET
        [pub_name]         = {trimws(pub_name)},
        [pub_description]  = {desc_val},
        [pub_guidance]     = {guidance_val},
        [pub_url]          = {url_val},
        [publication_type] = {publication_type},
        [published_date]   = {pub_date_val},
        [update_cycle]     = {update_cycle},
        [team]             = {team},
        [pub_active]       = {as.integer(pub_active)},
        [updated_by]       = {actor_id},
        [updated_on]       = GETDATE()
      WHERE [pub_id] = {as.integer(pub_id)};
      ",
      .con = conn
    )
  }

  DBI::dbExecute(conn, query)
}


#' Retrieve Assigned Maintainers for a Publication
#'
#' @param pub_id Integer primary key of the publication.
#'
#' @details
#' The database schema is resolved via [utils_resolve_schema()], and
#' the query is executed using [utils_db_get_query()].
#'
#' @return A [`data.frame`] containing `maintainer_id`, `user_id`, `username`,
#'   `email`, and `is_primary`.
#' @export
db_admin_get_pub_maintainers <- function(pub_id) {
  log_event("Starting db_admin_get_pub_maintainers", debug = TRUE)

  conn <- sql_manager("dit")
  on.exit(
    {
      try(DBI::dbDisconnect(conn), silent = TRUE)
      log_event("Finished db_admin_get_pub_maintainers")
    },
    add = TRUE
  )

  schema_01sr <- utils_resolve_schema("db_schema_01sr")

  query <- glue::glue_sql(
    "
    SELECT
      m.[maintainer_id],
      m.[user_id],
      u.[username],
      u.[email],
      m.[is_primary]
    FROM {schema_01sr}.[publication_maintainers] m
    INNER JOIN {schema_01sr}.[users] u
      ON m.[user_id] = u.[user_id]
    WHERE m.[pub_id] = {as.integer(pub_id)}
    ORDER BY m.[is_primary] DESC, u.[username] ASC;
    ",
    .con = conn
  )

  tryCatch(
    utils_db_get_query(conn, query),
    error = function(e) {
      log_event(
        paste("Error in db_admin_get_pub_maintainers:", conditionMessage(e)),
        level = "ERROR"
      )
      data.frame()
    }
  )
}


#' Assign a Maintainer to a Publication
#'
#' @param pub_id Integer primary key of the publication.
#' @param user_id Integer `user_id` of the user being assigned.
#' @param is_primary Integer flag (`1` = primary lead, `0` = contributor).
#' @param user_id_actor Optional integer `user_id` of the acting administrator.
#'
#' @return Integer count of affected rows.
#' @export
db_admin_add_pub_maintainer <- function(
  pub_id,
  user_id,
  is_primary = 0L,
  user_id_actor = NULL
) {
  log_event("Starting db_admin_add_pub_maintainer", debug = TRUE)

  conn <- sql_manager("dit")
  on.exit(
    {
      try(DBI::dbDisconnect(conn), silent = TRUE)
      log_event("Finished db_admin_add_pub_maintainer")
    },
    add = TRUE
  )

  schema_01sr <- utils_resolve_schema("db_schema_01sr")

  actor_id <- if (is.null(user_id_actor) || is.na(user_id_actor)) {
    DBI::SQL("NULL")
  } else {
    as.integer(user_id_actor)
  }

  query <- glue::glue_sql(
    "
    INSERT INTO {schema_01sr}.[publication_maintainers] (
      [pub_id],
      [user_id],
      [is_primary],
      [created_by],
      [created_on],
      [updated_by],
      [updated_on]
    )
    VALUES (
      {as.integer(pub_id)},
      {as.integer(user_id)},
      {as.integer(is_primary)},
      {actor_id},
      GETDATE(),
      {actor_id},
      GETDATE()
    );
    ",
    .con = conn
  )

  DBI::dbExecute(conn, query)
}


#' Remove a Maintainer from a Publication
#'
#' @param maintainer_id Integer primary key of the `publication_maintainers` row.
#'
#' @return Integer count of affected rows.
#' @export
db_admin_delete_pub_maintainer <- function(maintainer_id) {
  log_event("Starting db_admin_delete_pub_maintainer", debug = TRUE)

  conn <- sql_manager("dit")
  on.exit(
    {
      try(DBI::dbDisconnect(conn), silent = TRUE)
      log_event("Finished db_admin_delete_pub_maintainer")
    },
    add = TRUE
  )

  schema_01sr <- utils_resolve_schema("db_schema_01sr")

  query <- glue::glue_sql(
    "
    DELETE FROM {schema_01sr}.[publication_maintainers]
    WHERE [maintainer_id] = {as.integer(maintainer_id)};
    ",
    .con = conn
  )

  DBI::dbExecute(conn, query)
}


# =============================================================================
# Publication Data Links Junction Helpers
# =============================================================================

#' Retrieve Linked Publications for a Data Asset
#'
#' @param data_id Integer primary key of the data asset in `data_table`.
#'
#' @details
#' The database schema is resolved via [utils_resolve_schema()], and
#' the query is executed using [utils_db_get_query()].
#'
#' @return A [`data.frame`] containing `link_id`, `pub_id`, and `pub_name`.
#' @export
db_admin_get_data_pub_links <- function(data_id) {
  log_event("Starting db_admin_get_data_pub_links", debug = TRUE)

  conn <- sql_manager("dit")
  on.exit(
    {
      try(DBI::dbDisconnect(conn), silent = TRUE)
      log_event("Finished db_admin_get_data_pub_links")
    },
    add = TRUE
  )

  schema_01sr <- utils_resolve_schema("db_schema_01sr")

  query <- glue::glue_sql(
    "
    SELECT
      l.[link_id],
      l.[pub_id],
      p.[pub_name]
    FROM {schema_01sr}.[publication_data_links] l
    INNER JOIN {schema_01sr}.[publication_list] p
      ON l.[pub_id] = p.[pub_id]
    WHERE l.[data_id] = {as.integer(data_id)}
      AND l.[is_active] = 1
    ORDER BY p.[pub_name];
    ",
    .con = conn
  )

  tryCatch(
    utils_db_get_query(conn, query),
    error = function(e) {
      log_event(
        paste("Error in db_admin_get_data_pub_links:", conditionMessage(e)),
        level = "ERROR"
      )
      data.frame()
    }
  )
}


#' Link a Publication to a Data Asset
#'
#' @param data_id Integer primary key of the data asset.
#' @param pub_id Integer primary key of the publication.
#' @param user_id Optional integer `user_id` of the acting administrator.
#'
#' @return Integer count of affected rows.
#' @export
db_admin_add_data_pub_link <- function(data_id, pub_id, user_id = NULL) {
  log_event("Starting db_admin_add_data_pub_link", debug = TRUE)

  conn <- sql_manager("dit")
  on.exit(
    {
      try(DBI::dbDisconnect(conn), silent = TRUE)
      log_event("Finished db_admin_add_data_pub_link")
    },
    add = TRUE
  )

  schema_01sr <- utils_resolve_schema("db_schema_01sr")

  actor_id <- if (is.null(user_id) || is.na(user_id)) {
    DBI::SQL("NULL")
  } else {
    as.integer(user_id)
  }

  query <- glue::glue_sql(
    "
    INSERT INTO {schema_01sr}.[publication_data_links] (
      [data_id],
      [pub_id],
      [is_active],
      [created_by],
      [created_on],
      [updated_by],
      [updated_on]
    )
    VALUES (
      {as.integer(data_id)},
      {as.integer(pub_id)},
      1,
      {actor_id},
      GETDATE(),
      {actor_id},
      GETDATE()
    );
    ",
    .con = conn
  )

  DBI::dbExecute(conn, query)
}


#' Remove a Publication Link from a Data Asset
#'
#' @param link_id Integer primary key of the `publication_data_links` row.
#'
#' @return Integer count of affected rows.
#' @export
db_admin_delete_data_pub_link <- function(link_id) {
  log_event("Starting db_admin_delete_data_pub_link", debug = TRUE)

  conn <- sql_manager("dit")
  on.exit(
    {
      try(DBI::dbDisconnect(conn), silent = TRUE)
      log_event("Finished db_admin_delete_data_pub_link")
    },
    add = TRUE
  )

  schema_01sr <- utils_resolve_schema("db_schema_01sr")

  query <- glue::glue_sql(
    "
    DELETE FROM {schema_01sr}.[publication_data_links]
    WHERE [link_id] = {as.integer(link_id)};
    ",
    .con = conn
  )

  DBI::dbExecute(conn, query)
}
