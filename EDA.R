# EDA
library(tidyverse)
library(gt)
library(gtsummary)

# comment_history data frame from wfmu_comment_history.rdata
load("wfmu_comment_history.rdata")
comment_history <- comment_history |>
  filter(!is.na(stream_name)) |>
  #remove tail month
  filter(Date < as.Date("2025-12-1")) |>
  filter(year(Date) >= 2015)

# Stream summary statistics
stream_summary <- comment_history |>
  filter(!is.na(stream_name)) |>
  group_by(stream_name) |>
  summarise(
    total_shows = n(),
    total_comments = sum(comment_count, na.rm = TRUE),
    avg_comments = mean(comment_count, na.rm = TRUE),
    first_show = min(Date, na.rm = TRUE)
  ) |>
  arrange(desc(total_comments))

cat("Comment statistics by stream:\n")
stream_summary

# Monthly trends by stream
stream_trends <- comment_history |>
  filter(!is.na(stream_name), year(Date) >= 2015) |>
  mutate(month = floor_date(Date, "month")) |>
  group_by(stream_name, month) |>
  summarise(
    total_comments = sum(comment_count, na.rm = TRUE),
    n_shows = n(),
    avg_comments = mean(comment_count, na.rm = TRUE),
    .groups = "drop"
  )

# Plot by stream
stream_trends |>
  ggplot(aes(x = month, y = total_comments, color = stream_name)) +
  geom_line(linewidth = 1) +
  scale_y_continuous(labels = scales::comma) +
  scale_color_brewer(palette = "Set1") +
  labs(
    title = "Comment Trends by Stream Since 2015",
    subtitle = "Monthly total comments",
    x = "Date",
    y = "Total Comments per Month",
    color = "Stream"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "bottom"
  )

# Filter out December 2025 and recreate the chart
stream_trends |>
  ggplot(aes(x = month, y = total_comments, fill = stream_name)) +
  geom_area(alpha = 0.8) +
  scale_fill_brewer(palette = "Set2") +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "WFMU Comment Volume by Stream (2015-2025)",
    subtitle = "Stacked area chart showing total monthly comments (December 2025 excluded)",
    x = "Date",
    y = "Total Comments per Month",
    fill = "Stream"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  )

# use gt() to make a nice table of stream summary with aggregate stats at the bottom
# ---------------------------------------------
gt_stream_summary <- function() {
  stream_summary |>
    arrange(desc(total_comments)) |>
    gt() |>
    tab_header(
      title = "WFMU Comment Summary by Stream",
      subtitle = "Shows, total comments, and per-show averages (2015–2025)"
    ) |>
    cols_label(
      stream_name = "Stream",
      total_shows = "Shows",
      total_comments = "Total comments",
      avg_comments = "Avg comments",
      first_show = "First show"
    ) |>
    fmt_number(
      columns = c("total_shows"),
      decimals = 0,
      use_seps = TRUE
    ) |>
    fmt_number(
      columns = c("total_comments"),
      decimals = 0,
      use_seps = TRUE
    ) |>
    fmt_number(
      columns = c("avg_comments"),
      decimals = 0,
      use_seps = TRUE
    ) |>
    fmt_date(
      columns = c("first_show"),
      date_style = 3
    ) |>
    grand_summary_rows(
      columns = c("total_shows", "total_comments"),
      fns = list(
        "Sum" = ~ sum(., na.rm = TRUE)
      ),
      fmt = ~ fmt_number(., decimals = 0, use_seps = TRUE)
    ) |>
    grand_summary_rows(
      columns = c("avg_comments"),
      fns = list(
        "Mean" = ~ mean(., na.rm = TRUE)
      ),
      fmt = ~ fmt_number(., decimals = 0, use_seps = TRUE)
    ) |>
    tab_source_note(
      source_note = "Note: End Date 2025-12-01."
    )
}
# ---------------------------------------------
gt_stream_summary()

# Monthly trends by stream
stream_trends <- comment_history |>
  mutate(month = floor_date(Date, "month")) |>
  group_by(stream_name, month) |>
  summarise(
    total_comments = sum(comment_count, na.rm = TRUE),
    n_shows = n(),
    avg_comments = mean(comment_count, na.rm = TRUE),
    .groups = "drop"
  )

# Plot by stream
stream_trends |>
  ggplot(aes(x = month, y = total_comments, color = stream_name)) +
  geom_line(linewidth = 1) +
  scale_y_continuous(labels = scales::comma) +
  scale_color_brewer(palette = "Set1") +
  labs(
    title = "Comment Trends by Stream Since 2015",
    subtitle = "Monthly total comments",
    x = "Date",
    y = "Total Comments per Month",
    color = "Stream"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "bottom"
  )

# plot average comments per show by stream over time
stream_trends |>
  ggplot(aes(x = month, y = avg_comments, color = stream_name)) +
  geom_line(linewidth = 1) +
  scale_y_continuous(labels = scales::comma) +
  scale_color_brewer(palette = "Set1") +
  labs(
    title = "Average Comments per Show by Stream Since 2015",
    subtitle = "Monthly average comments per show",
    x = "Date",
    y = "Average Comments per Show",
    color = "Stream"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "bottom"
  )

# DJ summary statistics
dj_summary <- comment_history |>
  filter(!is.na(DJ)) |>
  group_by(stream_name, DJ) |>
  summarise(
    total_shows = n(),
    total_comments = sum(comment_count, na.rm = TRUE),
    avg_comments = mean(comment_count, na.rm = TRUE),
    first_show = min(Date, na.rm = TRUE)
  ) |>
  filter(total_shows > 4) |>
  arrange(desc(total_comments))

# filter by top 10 and bottom 10 DJs by average comments
dj_summary_top <- dj_summary |>
  arrange(avg_comments) |>
  slice(c(1:10, (n() - 9):n()))

# use gt() to make a nice table of DJ summary with aggregate stats at the bottom
gt_summary_top <- function() {
  dj_summary_top |>
    arrange(desc(total_comments)) |>
    gt() |>
    tab_header(
      title = "WFMU Comment Summary by DJ",
      subtitle = "Shows, total comments, and per-show averages (2015–2025)"
    ) |>
    cols_label(
      DJ = "DJ",
      total_shows = "Shows",
      total_comments = "Total comments",
      avg_comments = "Avg comments",
      first_show = "First show"
    ) |>
    fmt_number(
      columns = c("total_shows"),
      decimals = 0,
      use_seps = TRUE
    ) |>
    fmt_number(
      columns = c("total_comments"),
      decimals = 0,
      use_seps = TRUE
    ) |>
    fmt_number(
      columns = c("avg_comments"),
      decimals = 0,
      use_seps = TRUE
    ) |>
    fmt_date(
      columns = c("first_show"),
      date_style = 3
    ) |>
    grand_summary_rows(
      columns = c("total_shows", "total_comments"),
      fns = list(
        "Sum" = ~ sum(., na.rm = TRUE)
      ),
      fmt = ~ fmt_number(., decimals = 0, use_seps = TRUE)
    ) |>
    grand_summary_rows(
      columns = c("avg_comments"),
      fns = list(
        "Mean" = ~ mean(., na.rm = TRUE)
      ),
      fmt = ~ fmt_number(., decimals = 0, use_seps = TRUE)
    ) |>
    tab_source_note(
      source_note = "Note: End Date 2025-12-01."
    )
}
gt_summary_top()
