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

#' Sync Scraped Dataframe into SQL Table
#'
#' Processes an extracted dataframe against existing records to trigger updates or additions.
#'
#' @param post_scrape Dataframe. Output dataset stemming from `wn_scrape_all_pages`.
#' @return A character message tracking execution operations.
#' @export
wn_web_publish_sql <- function(post_scrape) {
  if (is.null(post_scrape) || nrow(post_scrape) == 0) {
    return("No incoming data found to commit to DB.")
  }

  # Fetch data in memory to completely avoid loop queries
  existing_posts <- db_get_scraped_posts()
  existing_links <- existing_posts$school_link

  new_count <- 0
  update_count <- 0

  for (i in seq_len(nrow(post_scrape))) {
    current_row <- post_scrape[i, ]
    link_exists <- current_row$school_link %in% existing_links

    if (link_exists) {
      matched_meta <- existing_posts[
        existing_posts$school_link == current_row$school_link,
      ][1, ]

      update_payload <- list(
        urn = current_row$urn,
        twn_id = matched_meta$twn_id,
        school_name = current_row$school_name,
        la = current_row$la,
        region = current_row$region,
        reason = current_row$reason,
        last_scraped_post = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        first_scraped_post = matched_meta$first_scraped_post,
        region_link = current_row$region_link,
        school_link = current_row$school_link,
        published_date = current_row$published_date
      )

      db_update_scraped_post(
        post_twn_id = matched_meta$post_twn_id,
        update_data = update_payload
      )
      update_count <- update_count + 1
    } else {
      insert_payload <- as.list(current_row)
      db_insert_scraped_post(post_data = insert_payload)
      new_count <- new_count + 1
    }
  }

  return(paste0(
    "Sync completed! Updates executed: ",
    update_count,
    " | Inserts executed: ",
    new_count
  ))
}


#' Reconcile Unlinked Staging Notices against Master Data
#'
#' Pulls unlinked staging table items and maps them against the master notices
#' matrix using optimized vector evaluations, writing the paired IDs back.
#'
#' @export
wn_web_notice_link <- function() {
  log_event("Starting wn_web_notice_link matching execution")

  # 1. Fetch tables as complete data sets into R environment memory
  # Filter for records that need an ID pairing
  online_notices <- db_get_scraped_posts() |> dplyr::filter(is.na(twn_id))
  all_notices <- db_get_all_notices()

  if (nrow(online_notices) == 0) {
    cat("No unmatched URNs discovered in staging.\n")
    return(invisible(NULL))
  }

  # Normalize notice reason strings to match your database lookup codes
  online_notices <- online_notices |>
    dplyr::mutate(
      lookup_type = dplyr::case_when(
        reason == "Inadequate Ofsted Judgement" ~ "1",
        stringr::str_detect(stringr::str_to_lower(reason), "necessary") ~ "2",
        TRUE ~ "99"
      )
    )

  update_count <- 0

  # 2. Vectorized loop evaluation to bind keys together
  for (i in seq_len(nrow(online_notices))) {
    match <- all_notices |>
      dplyr::filter(
        urn == online_notices$urn[i],
        type_of_notice_id == online_notices$lookup_type[i]
      )

    # If a match is found in our master dataset, link it!
    if (nrow(match) > 0) {
      target_twn_id <- match$twn_id[1]
      target_post_id <- online_notices$post_twn_id[i]

      # Execute single-purpose database rewrite to inject the linked master ID
      db_link_post_to_master(
        post_twn_id = target_post_id,
        twn_id = target_twn_id
      )
      update_count <- update_count + 1
    }
  }

  print(data.frame(
    Message = paste("Successfully linked", update_count, "notices.")
  ))
}
