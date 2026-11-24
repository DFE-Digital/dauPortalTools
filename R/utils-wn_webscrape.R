#' Collect All Notice Links Across Regions
#'
#' Scrapes the regional index pages defined in the config and returns a complete,
#' unique character vector of individual warning notice paths.
#'
#' @return A character vector of fully qualified GOV.UK notice URLs.
#' @export
wn_get_all_notice_links <- function() {
  log_event(
    "Starting link discovery across all configured regions"
  )
  config <- get_config("./config.yml")

  # map() allows each iteration to return a vector of strings
  all_links_list <- purrr::map(names(config$region), function(region_name) {
    region_url <- paste0(config$wn_gov_url, config$region[[region_name]])

    safe_read <- purrr::safely(rvest::read_html)(region_url)
    if (!is.null(safe_read$error)) {
      log_event(paste(
        "Failed to read region link:",
        region_url,
        "-",
        safe_read$error$message
      ))
      return(character())
    }

    links <- safe_read$result |>
      rvest::html_nodes("a") |>
      rvest::html_attr("href") |>
      purrr::keep(
        ~ stringr::str_detect(., "^/government/publications/") &&
          !stringr::str_detect(
            .,
            "collections|schools-causing-concern|list-of-letters"
          )
      )

    # Absolute path reconciliation
    ifelse(
      stringr::str_starts(links, "http"),
      links,
      paste0("https://www.gov.uk", links)
    )
  })

  # Flatten the list of vectors into a single unique character vector
  all_links <- all_links_list |>
    purrr::list_c() |>
    unique()

  log_event(paste(
    "Discovered",
    length(all_links),
    "unique notice paths."
  ))
  return(all_links)
}

#' Scrape Attributes From a Vector of Notice Pages
#'
#' Loops through a provided list of URLs, parses out standard attributes defensively,
#' and structures the data into a single dataframe.
#'
#' @param notice_urls Character vector. The list of exact GOV.UK pages to crawl.
#' @return A processed tibble/dataframe containing notice metadata fields.
#' @export
wn_scrape_all_pages <- function(notice_urls) {
  if (length(notice_urls) == 0) {
    log_event("No URLs provided to scrape.")
    return(tibble::tibble())
  }

  log_event(paste(
    "Starting text parsing extraction for",
    length(notice_urls),
    "pages..."
  ))

  # Single page parsing engine wrapped locally
  parse_single_notice <- function(url) {
    safe_page <- purrr::safely(rvest::read_html)(url)
    if (!is.null(safe_page$error)) {
      return(NULL)
    }

    text_content <- safe_page$result |> rvest::html_text()

    urn <- stringr::str_extract(text_content, "(?is)(?<=URN:\\s)\\d+") |>
      as.integer()

    school_name <- stringr::str_extract(
      text_content,
      "(?is)(?<=\\bissued to:\\s)[\\s\\S]*?(?=\\\\u003c|</p>|\\n|\\.|$)"
    ) |>
      stringr::str_remove_all("<.*?>") |>
      stringr::str_squish()

    reason_raw <- stringr::str_extract(
      text_content,
      "(?is)(?<=for issue:\\s).*?(?=DfE|\\n|$)"
    ) |>
      stringr::str_remove_all("<.*?>") |>
      stringr::str_squish()

    # Normalize Reason Logic
    reason <- if (!is.na(reason_raw)) {
      r_low <- stringr::str_to_lower(reason_raw)
      dplyr::case_when(
        stringr::str_detect(
          r_low,
          "requires significant improvement"
        ) ~ "Requires Significant Improvement Ofsted Judgement",
        stringr::str_detect(
          r_low,
          "special measures"
        ) ~ "Special Measures Ofsted Judgement",
        stringr::str_detect(
          r_low,
          "inadequate"
        ) ~ "Inadequate Ofsted Judgement",
        TRUE ~ stringr::str_to_title(reason_raw)
      )
    } else {
      NA_character_
    }

    region <- stringr::str_extract(
      text_content,
      "(?is)(?<=\\bdirector office:\\s)[\\s\\S]*?(?=\\\\u003c/p\\\\u003e|</p>|\\n|$)"
    ) |>
      stringr::str_remove_all("<.*?>") |>
      stringr::str_squish()

    la <- stringr::str_extract(
      text_content,
      "(?is)(?<=\\bLocal authority:\\s)[\\s\\S]*?(?=\\\\u003c|</p>|\\n|$)"
    ) |>
      stringr::str_remove_all("<.*?>") |>
      stringr::str_squish()

    published_date <- stringr::str_extract(
      text_content,
      "(?i)(?<=Published\\s)\\d{1,2}\\s+[A-Za-z]+\\s+\\d{4}"
    ) |>
      lubridate::dmy()

    tibble::tibble(
      school_link = url,
      twn_id = NA_integer_,
      urn = urn,
      school_name = school_name,
      reason = reason,
      region = region,
      la = la,
      published_date = published_date
    )
  }

  # Map over links with polite delay pacing
  scraped_df <- purrr::map_df(notice_urls, function(url) {
    Sys.sleep(runif(1, 0.5, 1.2))
    parse_single_notice(url)
  })

  if (nrow(scraped_df) > 0) {
    # Format times directly into clean character strings SQL Server understands
    current_timestamp_str <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

    scraped_df <- scraped_df |>
      dplyr::mutate(
        first_scraped_post = current_timestamp_str,
        last_scraped_post = current_timestamp_str,
        region_link = NA_character_
      )
  }

  return(scraped_df)
}

#' Sync Scraped Dataframe into [01_SSSR].[wn_online_notices]
#'
#' Matches existing rows by `notice_url` and updates `last_scraped_at`
#' without overwriting cleaned URNs or school names. Inserts new URLs.
#'
#' @param post_scrape Dataframe. Output from `wn_scrape_all_pages()`.
#' @param con DBI database connection to `Data_Insight_Team`.
#' @return A character message summarizing updates and inserts.
#' @export
wn_web_publish_sql <- function(post_scrape, con) {
  if (is.null(post_scrape) || nrow(post_scrape) == 0) {
    return("No incoming data found to commit to DB.")
  }

  existing_posts <- DBI::dbGetQuery(
    con,
    "SELECT wn_online_notice_id, wn_id, urn, school_name, notice_url, published_date
     FROM [Data_Insight_Team].[01_SSSR].[wn_online_notices]"
  )

  existing_links <- stringr::str_trim(existing_posts$notice_url)
  now_str <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

  new_count <- 0L
  update_count <- 0L

  for (i in seq_len(nrow(post_scrape))) {
    row <- post_scrape[i, ]
    url_clean <- stringr::str_trim(row$school_link)
    if (is.na(url_clean) || url_clean == "") {
      next
    }

    if (url_clean %in% existing_links) {
      matched <- existing_posts[existing_links == url_clean, ][1, ]

      # Preserve existing published_date if already populated
      pub_val <- if (!is.na(matched$published_date)) {
        as.character(matched$published_date)
      } else if (!is.na(row$published_date)) {
        as.character(row$published_date)
      } else {
        NA_character_
      }

      DBI::dbExecute(
        con,
        "UPDATE [Data_Insight_Team].[01_SSSR].[wn_online_notices]
         SET last_scraped_at = ?,
             published_date  = COALESCE(published_date, ?)
         WHERE wn_online_notice_id = ?",
        params = list(now_str, pub_val, matched$wn_online_notice_id)
      )
      update_count <- update_count + 1L
    } else {
      pub_val <- if (!is.na(row$published_date)) {
        as.character(row$published_date)
      } else {
        as.character(as.Date(now_str))
      }

      DBI::dbExecute(
        con,
        "INSERT INTO [Data_Insight_Team].[01_SSSR].[wn_online_notices]
         (wn_id, urn, school_name, local_authority, region, notice_reason,
          published_date, first_scraped_at, last_scraped_at, region_url, notice_url)
         VALUES (NULL, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
        params = list(
          row$urn,
          row$school_name,
          row$la,
          row$region,
          row$reason,
          pub_val,
          now_str,
          now_str,
          row$region_link,
          url_clean
        )
      )
      new_count <- new_count + 1L
    }
  }

  paste0(
    "Sync completed! Updates executed: ",
    update_count,
    " | Inserts executed: ",
    new_count
  )
}


#' Reconcile Unlinked [01_SSSR].[wn_online_notices] Against [01_SSSR].[wn_records]
#'
#' Links unallocated online notices by URN (preferring active Created/Issued/Published
#' records), logs Published Date (wn_date_id = 4), and flips Created/Issued to Published (5).
#'
#' @param con DBI database connection to `Data_Insight_Team`.
#' @export
wn_web_notice_link <- function(con) {
  log_event("Starting wn_web_notice_link matching execution against [01_SSSR]")

  unlinked <- DBI::dbGetQuery(
    con,
    "SELECT wn_online_notice_id, urn, published_date, first_scraped_at
     FROM [Data_Insight_Team].[01_SSSR].[wn_online_notices]
     WHERE wn_id IS NULL AND urn IS NOT NULL"
  )

  if (nrow(unlinked) == 0) {
    cat("No unmatched URNs discovered in [01_SSSR].[wn_online_notices].\n")
    return(invisible(NULL))
  }

  master_records <- DBI::dbGetQuery(
    con,
    "SELECT wn_id, urn, wn_status_id, first_published_date
     FROM [Data_Insight_Team].[01_SSSR].[wn_records]"
  )

  linked_count <- 0L

  for (i in seq_len(nrow(unlinked))) {
    target_urn <- unlinked$urn[i]
    online_id <- unlinked$wn_online_notice_id[i]

    candidates <- master_records |>
      dplyr::filter(urn == target_urn) |>
      dplyr::arrange(
        dplyr::if_else(wn_status_id %in% c(1L, 4L, 5L), 0L, 1L),
        dplyr::desc(first_published_date),
        dplyr::desc(wn_id)
      )

    if (nrow(candidates) > 0) {
      target_wn_id <- candidates$wn_id[1]
      target_status <- candidates$wn_status_id[1]
      pub_date <- as.character(
        dplyr::coalesce(
          unlinked$published_date[i],
          as.Date(unlinked$first_scraped_at[i])
        )
      )

      # 1. Link online notice to wn_records
      DBI::dbExecute(
        con,
        "UPDATE [Data_Insight_Team].[01_SSSR].[wn_online_notices]
         SET wn_id = ?
         WHERE wn_online_notice_id = ?",
        params = list(target_wn_id, online_id)
      )

      # 2. Insert Published Date (wn_date_id = 4) if missing
      DBI::dbExecute(
        con,
        "INSERT INTO [Data_Insight_Team].[01_SSSR].[wn_date_tracking]
         (wn_id, wn_date_id, wn_date, wn_date_notes, created_by, created_on)
         SELECT ?, 4, CAST(? AS date), N'Auto-linked by GOV.UK scraper', 2, GETDATE()
         WHERE NOT EXISTS (
           SELECT 1 FROM [Data_Insight_Team].[01_SSSR].[wn_date_tracking]
           WHERE wn_id = ? AND wn_date_id = 4
         )",
        params = list(target_wn_id, pub_date, target_wn_id)
      )

      # 3. Flip Created (1) or Issued (4) -> Published (5)
      if (target_status %in% c(1L, 4L)) {
        DBI::dbExecute(
          con,
          "UPDATE [Data_Insight_Team].[01_SSSR].[wn_records]
           SET wn_status_id = 5, updated_on = GETDATE()
           WHERE wn_id = ?",
          params = list(target_wn_id)
        )
      }

      linked_count <- linked_count + 1L
    }
  }

  print(data.frame(
    Message = paste("Successfully linked", linked_count, "notices.")
  ))
}
