extract_playlist_info <- function(doc) {
  date_pattern <- "\\b(?:January|February|March|April|May|June|July|August|September|October|November|December|Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Sept|Oct|Nov|Dec)\\.?\\s+\\d{1,2},\\s+\\d{4}\\b"

  anchors <- html_elements(
    doc,
    xpath = ".//a[(contains(translate(@href,'ABCDEFGHIJKLMNOPQRSTUVWXYZ','abcdefghijklmnopqrstuvwxyz'),'/playlist/')) or (contains(translate(@href,'ABCDEFGHIJKLMNOPQRSTUVWXYZ','abcdefghijklmnopqrstuvwxyz'),'/playlists/'))]"
  )
  if (length(anchors) == 0) {
    return(tibble(date = as.Date(character()), href = character()))
  }

  hrefs <- html_attr(anchors, "href")
  anchor_text <- html_text2(anchors)
  parent_text <- map_chr(xml_parent(anchors), html_text2)

  dates_from_anchor <- str_extract(anchor_text, date_pattern)
  dates_from_parent <- str_extract(parent_text, date_pattern)

  dates_chr <- ifelse(
    !is.na(dates_from_anchor),
    dates_from_anchor,
    dates_from_parent
  )

  keep <- !is.na(dates_chr)
  if (!any(keep)) {
    return(tibble(date = as.Date(character()), href = character()))
  }

  tibble(
    date = dates_chr[keep] |>
      parse_date_time(orders = "BdY", quiet = TRUE) |>
      as.Date(),
    href = hrefs[keep]
  )
}

temp <- extract_playlist_info(doc)
temp |> head()
temp |> tail()
