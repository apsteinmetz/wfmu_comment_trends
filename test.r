links_a <- playlist_links |>
  keep(\(x) str_detect(x, paste0("\\/playlists\\/", dj_id, "\\d{4}")))


links_b <- playlist_links[str_detect(
  playlist_links,
  paste0("\\/playlists\\/", dj_id, "\\d{4}")
)]
