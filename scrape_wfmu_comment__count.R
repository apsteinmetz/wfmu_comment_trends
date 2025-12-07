# Dependencies: tidyverse, rvest, httr, xml2, lubridate, stringr, purrr
library(tidyverse)
library(rvest)
library(httr)
library(xml2)
library(lubridate)
library(stringr)
library(purrr)
library(rlang)

base_url = "https://www.wfmu.org/playlists"
known_streams = c(
  "WFMU Live",
  "Rock'N'Soul",
  "Give the Drummer Some",
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
dj_hrefs <- all_hrefs[str_detect(all_hrefs, "^/playlists/[^/]+$")]
dj_urls <- abs(dj_hrefs, base_url) %>% unique()

# from each DJ page gather show links that look like /playlists/<dj>/<something>
get_show_links <- function(dj_url) {
  doc <- safe_get_html(dj_url)
  if (is.null(doc)) {
    return(tibble(dj_id = character(0), show_url = character(0)))
  }
  hrefs <- doc %>%
    html_nodes("a") %>%
    html_attr("href") %>%
    discard(is.na) %>%
    unique()
  show_hrefs <- hrefs[str_detect(hrefs, "^/playlists/[^/]+/.+")]
  show_urls_local <- abs(show_hrefs, dj_url) %>% unique()

  # extract the end segment after /playlists/ and take the last two characters as the DJ id
  seg <- str_extract(dj_url, "(?<=/playlists/)[^/]+$")
  dj_id <- ifelse(
    is.na(seg),
    NA_character_,
    ifelse(nchar(seg) >= 2, substr(seg, nchar(seg) - 1, nchar(seg)), seg)
  )
  tibble(dj_id = dj_id, show_url = show_urls_local)
}
# this takes some time
# show_urls <- dj_urls |>
  map_dfr(get_show_links) |>
  distinct()

# save show URLs for reference
# saveRDS(show_urls, "wfmu_show_urls.rds")
# load show URLs
show_urls <- readRDS("wfmu_show_urls.rds")

# function to extract fields from a single show page
parse_show <- function(url) {
  Sys.sleep(pause)
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
  pos <- map_int(known_streams, ~ {
    loc <- str_locate(text_all, fixed(.x))
    if (is.na(loc[1])) NA_integer_ else as.integer(loc[1])
  })
  stream <- if (all(is.na(pos))) {
    NA_character_
  } else {
    known_streams[which.min(pos)]
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
  time_node <- doc %>% html_node("time")
  if (!is.null(time_node)) {
    dt_attr <- time_node %>% html_attr("datetime")
    dt_text <- ifelse(
      is.na(dt_attr),
      time_node %>% html_text2() %>% str_squish(),
      dt_attr
    )
    dt_parsed <- parse_date_time(
      dt_text,
      orders = c("Ymd", "ymd", "mdy", "BdY", "dby"),
      quiet = TRUE
    )
    if (!is.na(dt_parsed)) date_val <- as_date(dt_parsed)
  }
  if (is.na(date_val)) {
    # look for patterns like "January 2, 2020" or "2020-01-02"
    m <- str_extract(
      text_all,
      "\\b(?:[A-Z][a-z]+\\s+\\d{1,2},\\s+\\d{4}|\\d{4}-\\d{2}-\\d{2})\\b"
    )
    if (!is.na(m)) {
      p <- parse_date_time(m, orders = c("BdY", "ymd"), quiet = TRUE)
      if (!is.na(p)) date_val <- as_date(p)
    }
  }

  # comment_count: count rows in #comments-table (tbody tr or direct tr)
  comments_table <- doc %>% html_node("#comments-table")
  comment_count <- 0L
  if (!is.null(comments_table)) {
    rows <- comments_table %>% html_nodes("tbody tr")
    if (length(rows) == 0) {
      rows <- comments_table %>% html_nodes("tr")
    }
    comment_count <- length(rows)
  }

  tibble(
    "stream_name" = ifelse(is.na(stream), NA_character_, stream),
    "DJ" = ifelse(is.na(dj), NA_character_, dj),
    "Date" = ifelse(is.na(date_val), as.Date(NA), date_val),
    "comment_count" = as.integer(comment_count),
    "url" = url
  )
}

failure_info <- NULL
# url_subset <- show_urls |> filter(dj_id == "WH")
url_subset <- show_urls
results_list <- vector("list", length(url_subset))
# results_list <- list()
for (i in 1:nrow(url_subset)) {
  print(i)
  url <- url_subset$show_url[i]
  dj_id <- url_subset$dj_id[i]
  res <- tryCatch(
    parse_show(url),
    error = function(e) {
      failure_info <<- list(index = i, url = url, message = conditionMessage(e))
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
  }
}

comment_history <- results_list |>
  bind_rows() |>
  mutate(Date = as.Date(Date)) |>
  arrange(Date)

if (!is.null(failure_info)) {
  attr(comment_history, "failure") <- failure_info
}
# save comment_history
saveRDS(comment_history, "wfmu_comment_history.rds")



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
