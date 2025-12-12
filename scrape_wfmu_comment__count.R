# get comment counts
# commenting starts in 2007, show number 21600 is first show of 2007

# Dependencies: tidyverse, rvest, httr, xml2, lubridate, stringr, purrr
library(tidyverse)
library(rvest)
library(httr)
library(xml2)
library(rlang)

base_url = "https://www.wfmu.org/playlists"
date_regex <- "\\b(?:January|February|March|April|May|June|July|August|September|October|November|December)\\s+\\d{1,2},\\s+\\d{4}\\b"

known_stream_tags <- c(
  "On WFMU's Give the Drummer Radio",
  "On WFMU's Rock'N'Soul Radio",
  "On WFMU's Sheena's Jungle Room"
)

known_streams <- c(
  "Give the Drummer Radio",
  "Rock'N'Soul Radio",
  "Sheena's Jungle Room"
)

pause = 0.5

ua <- httr::user_agent(
  "wfmu-comment-counter/1.0 (contact: aspteinmetz@yahoo.com)"
)
safe_get_html <- function(url) {
  res <- tryCatch(httr::GET(url, ua, httr::timeout(30)), error = function(e) {
    NULL
  })
  if (is.null(res) || httr::http_error(res)) {
    return(NULL)
  }
  tryCatch(read_html(res), error = function(e) NULL)
}

# helper: absolute urls
abs <- function(href, base) {
  href <- href[!is.na(href)]
  if (length(href) == 0) {
    return(character(0))
  }
  xml2::url_absolute(href, base)
}

# fetch base and find DJ archive links like /playlists/<dj>
base_doc <- safe_get_html(base_url)
if (is.null(base_doc)) {
  abort("Failed to fetch base URL")
}

all_hrefs <- base_doc %>%
  html_nodes("a") %>%
  html_attr("href") %>%
  discard(is.na) %>%
  unique()

dj_ids <- all_hrefs[str_detect(all_hrefs, "^/playlists/[^/]+$")] |>
  str_extract("(?<=/playlists/)[^/]+$") |>
  unique() |>
  # remove "JM" from list, never comments
  discard(\(x) x == "JM")

# -----------------------------------------------
# from each DJ page gather show links that look like /playlists/<dj>/<something>
get_show_links <- function(dj_id, back_playist = NULL) {
  if (is.null(back_playlist)) {
    dj_url <- paste0(base_url, "/", dj_id)
  } else {
    dj_url <- paste0(base_url, "/", back_playlist)
  }
  print(dj_url)

  #doc <- safe_get_html(dj_url)
  #if (is.null(doc)) {
  #  return(tibble(dj_id = character(0), show_url = character(0)))
  #}

  doc_html <- GET(dj_url, ua, httr::timeout(30)) |>
    content(as = "text", encoding = "UTF-8") |>
    strsplit("<br>") |>
    unlist()

  if (is.null(doc_html)) {
    return(tibble(dj_id = character(0), show_url = character(0)))
  }
  # playlist_links <- html_elements(doc, "a") |>
  #  html_attr("href")

  #  playlist_text <- html_elements(doc, "a") |>
  #   html_text()

  if (is.null(back_playlist)) {
    prior_years <-
      doc_html[str_detect(
        doc_html,
        paste0(dj_id, "\\d{4}")
      )] |>
      str_extract_all(paste0(dj_id, "\\d{4}")) |>
      unlist() |>
      unique()
  } else {
    prior_years <- character(0)
  }

  items_with_dates <- doc_html[str_detect(doc_html, date_regex)]

  playlist_links <- items_with_dates |>
    map(read_html) |>
    map(html_nodes, "a")

  texts <- playlist_links |>
    map(html_text) |>
    unlist()
  hrefs <- playlist_links |>
    map(html_attr, "href") |>
    unlist()
  dates <- str_extract(items_with_dates, date_regex) |>
    parse_date_time(orders = "BdY", quiet = TRUE) |>
    as.Date()

  playlist_rows <- tibble(
    dj = dj_id,
    text = texts,
    show_id = str_remove(hrefs, "\\/playlists\\/")
  ) |>
    filter(str_detect(text, "laylist")) |>
    select(dj, show_id) |>
    tibble(date = dates)

  if (length(prior_years) > 0) {
    back_playlist <- map(prior_years, \(back_year) {
      get_show_links(dj_id, back_year)
    }) |>
      bind_rows()
    playlist_rows <- bind_rows(playlist_rows, back_playlist)
  }
  return(playlist_rows)
}

test <- get_show_links("WA")

# if show_urls.rds exists, load it instead of re-fetching
if (file.exists("wfmu_show_urls.rds")) {
  # load show URLs
  show_urls <- readRDS("wfmu_show_urls.rds")
} else {
  show_urls <- dj_ids |>
    map_dfr(get_show_links) |>
    distinct()
  saveRDS(show_urls, "wfmu_show_urls.rds")
}


# add show number column to show_url using number at end of URL
show_urls <- show_urls %>%
  mutate(
    show_number = str_extract(show_url, "(\\d+)$") %>% as.integer()
  ) %>%
  arrange(show_number)

show_urls_sample <- show_urls %>%
  # shows since 2015
  filter(show_number >= 59000) %>%
  # sample 100 random shows for testing
  #  slice_sample(n = 100) |>
  arrange(show_number)


# function to extract fields from a single show page ==================
parse_show <- function(url) {
  # Sys.sleep(pause)
  doc <- safe_get_html(url)
  if (is.null(doc)) {
    return(tibble(
      "stream_name" = NA_character_,
      "DJ" = NA_character_,
      "Date" = as.Date(NA),
      "comment_count" = NA_integer_,
      "url" = url
    ))
  }

  text_all <- doc %>% html_text2() %>% str_squish()

  # Stream: best-effort by finding first occurring known stream in page text

  pos <- map_int(known_stream_tags, \(x) str_detect(text_all, x))
  if (all(pos == 0)) {
    stream <- "WFMU Main"
  } else {
    stream <- known_streams[which.max(pos)]
  }

  # DJ: attempt from page <h1> or from title
  dj <- NA_character_
  h1 <- doc %>% html_node("h1") %>% html_text2() %>% str_squish()
  if (!is.na(h1) && h1 != "") {
    dj <- h1
  }
  if (is.na(dj) || dj == "") {
    title <- doc %>% html_node("title") %>% html_text2() %>% str_squish()
    dj <- ifelse(is.na(title) || title == "", NA_character_, title)
  }
  # retain only the text between the two colons in dj
  if (!is.na(dj) && str_detect(dj, ":")) {
    parts <- str_split(dj, ":", n = 3)[[1]]
    if (length(parts) >= 2) {
      dj <- str_squish(parts[2])
    }
  }

  # Date: prefer <time datetime>, else try to find common date patterns in page text
  date_val <- NA_Date_
  # look for patterns like "January 2, 2020" or "2020-01-02"
  m <- str_extract(
    text_all,
    "\\b(?:[A-Z][a-z]+\\s+\\d{1,2},\\s+\\d{4}|\\d{4}-\\d{2}-\\d{2})\\b"
  )
  if (!is.na(m)) {
    p <- parse_date_time(m, orders = c("BdY", "ymd"), quiet = TRUE)
    if (!is.na(p)) date_val <- as_date(p)
  }

  # comment_count: count rows in #comments-table (tbody tr or direct tr)
  comments_table <- doc |>
    html_node("#comments-table")

  if (!is.null(comments_table)) {
    comment_count <- comments_table |>
      xml_children() |>
      length()
  } else {
    comment_count <- 0L
  }
  comment_count <- max(0L, comment_count)

  return(tibble(
    "stream_name" = ifelse(is.na(stream), NA_character_, stream),
    "DJ" = ifelse(is.na(dj), NA_character_, dj),
    "Date" = date_val,
    "comment_count" = as.integer(comment_count),
    "url" = url
  ))
}


# START OF LOOP TO PROCESS SHOW URLS =========================
failure_info <- NULL
# url_subset <- show_urls |> filter(dj_id == "WH")
url_subset <- show_urls_sample
results_list <- vector("list", length(url_subset))
# results_list <- list()
# for (i in 1:nrow(url_subset)) {
for (i in 1:nrow(url_subset)) {
  # print(i)
  url <- url_subset$show_url[i]
  dj_id <- url_subset$dj_id[i]
  res <- tryCatch(
    parse_show(url),
    error = function(e) {
      failure_info <<- list(
        index = i,
        url = url,
        message = conditionMessage(e)
      )
      NULL
    }
  )
  if (is.null(res) && !is.null(failure_info)) {
    # encountered failure — stop further processing
    break
  }
  results_list[[i]] <- res %>% mutate(dj_id = dj_id)

  # report progress every 10 shows
  if (i %% 10 == 0) {
    message(sprintf("Processed %d / %d shows", i, nrow(url_subset)))
    message(res$DJ)
  }
}
# END OF LOOP TO PROCESS SHOW URLS =========================
# remove NULL entries from results_list
results_list <- compact(results_list)

# convert date in results to Date type

comment_history <- results_list |>
  bind_rows()

if (!is.null(failure_info)) {
  attr(comment_history, "failure") <- failure_info
}

# show dj fields include " Playlist -"" followed by a date" Remove that
comment_history <- comment_history |>
  mutate(
    DJ = str_replace(DJ, "(P|p)laylist(s?).*$", "") %>% str_squish()
  ) |>
  filter(DJ != "")


# save comment_history
save(comment_history, file = "wfmu_comment_history.rdata")

# comment_history <- readRDS("wfmu_comment_history.rds")

# plot comment counts over time

ggplot(comment_history, aes(x = Date, y = comment_count)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "WFMU Show Comment Counts Over Time",
    x = "Date",
    y = "Comment Count"
  ) +
  theme_minimal()
