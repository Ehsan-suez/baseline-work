# ============================================================
# Figure S8 (RSV): A on top, B & C on bottom with ONE legend
# ============================================================

library(tidyverse)
library(lubridate)
library(stringr)
library(cowplot)

# ----------------------------
# 1) Build RSV backfill dataset
# ----------------------------
archive_dir <- "data/rsv/archive"

rsv_archive_raw <- list.files(
  archive_dir,
  pattern = "rsvnet_hospitalization\\.csv$",
  full.names = TRUE
) %>%
  tibble(file = .) %>%
  mutate(issue_date = ymd(str_extract(basename(file), "\\d{4}-\\d{2}-\\d{2}"))) %>%
  mutate(data = map(file, read_csv, show_col_types = FALSE)) %>%
  unnest(data)

locations <- read_csv("data/locations.csv", show_col_types = FALSE)

rsv_truth <- rsv_archive_raw %>%
  rename(target_end_date = date) %>%
  filter(
    age_group == "0-130",
    target == "inc hosp",
    target_end_date >= as.Date("2023-10-01"),
    location != "37"
  ) %>%
  left_join(locations %>% select(location, location_name), by = "location") %>%
  mutate(
    target_variable = "inc hosp rsv",
    model = "truth",
    value = round(value),
    location = location_name
  ) %>%
  select(model, target_variable, target_end_date, location, value, issue_date)

rsv_first_vs_final <- rsv_truth %>%
  arrange(location, target_end_date, issue_date) %>%
  group_by(location, target_end_date) %>%
  summarise(
    first_value = first(value),
    final_value = last(value),
    total_revision = final_value - first_value,
    abs_revision = abs(total_revision),
    .groups = "drop"
  )

rsv_first_vs_final_plotdat <- rsv_first_vs_final %>%
  mutate(
    pct_revision = if_else(first_value > 0, 100 * total_revision / first_value, NA_real_)
  )


# -----------------------------
# 0) USER SETTINGS
# -----------------------------
flu_archive_path <- "data/Influenza/archive"   # <-- change if needed

# If your RSV object already exists in memory, keep this TRUE.
# If you want to load RSV from disk here, set FALSE and add your RSV load code below.
rsv_object_already_in_memory <- TRUE

# -----------------------------
# 1) READ ALL FLU ARCHIVE SNAPSHOTS
# -----------------------------
flu_files <- list.files(
  flu_archive_path,
  pattern = "^target-hospital-admissions_\\d{4}-\\d{2}-\\d{2}\\.csv$",
  full.names = TRUE
)

stopifnot(length(flu_files) > 0)

flu_archive <- purrr::map_dfr(flu_files, function(f) {
  
  snapshot_date <- stringr::str_extract(basename(f), "\\d{4}-\\d{2}-\\d{2}") |>
    as.Date()
  
  readr::read_csv(f, show_col_types = FALSE) %>%
    transmute(
      snapshot_date,
      target_end_date = as.Date(date),
      location        = as.character(location),
      location_name   = as.character(location_name),
      value           = as.numeric(value),
      weekly_rate     = as.numeric(weekly_rate)
    )
})

# Quick sanity checks
stopifnot(all(c("snapshot_date","target_end_date","location_name","value") %in% names(flu_archive)))
message("Flu archive rows: ", nrow(flu_archive))
message("Flu snapshots: ", dplyr::n_distinct(flu_archive$snapshot_date))

# -----------------------------
# 2) FIRST VS FINAL (COUNTS)
# -----------------------------
flu_first_vs_final_plotdat <- flu_archive %>%
  group_by(location_name, target_end_date) %>%
  summarise(
    first_value    = value[which.min(snapshot_date)],
    final_value    = value[which.max(snapshot_date)],
    total_revision = final_value - first_value,
    abs_revision   = abs(total_revision),
    pct_revision   = if_else(first_value == 0, NA_real_, 100 * abs_revision / first_value),
    .groups = "drop"
  ) %>%
  rename(location = location_name)

# -----------------------------
# 3) OPTIONAL: FIRST VS FINAL (WEEKLY RATE)
# -----------------------------
flu_rate_first_vs_final_plotdat <- flu_archive %>%
  group_by(location_name, target_end_date) %>%
  summarise(
    first_value    = weekly_rate[which.min(snapshot_date)],
    final_value    = weekly_rate[which.max(snapshot_date)],
    total_revision = final_value - first_value,
    abs_revision   = abs(total_revision),
    pct_revision   = if_else(first_value == 0, NA_real_, 100 * abs_revision / first_value),
    .groups = "drop"
  ) %>%
  rename(location = location_name)

# -----------------------------
# 4) RSV OBJECT CHECK (must exist)


# Ensure RSV columns match expectation; rename if needed
# Required columns: location, target_end_date, first_value, final_value, total_revision, abs_revision, pct_revision
needed_cols <- c("location","target_end_date","first_value","final_value","total_revision","abs_revision","pct_revision")
missing_rsv <- setdiff(needed_cols, names(rsv_first_vs_final_plotdat))

if (length(missing_rsv) > 0) {
  stop("RSV object is missing required columns: ", paste(missing_rsv, collapse = ", "))
}

# -----------------------------
# 5) COMBINE RSV + FLU
# -----------------------------
revision_df <- bind_rows(
  rsv_first_vs_final_plotdat %>% mutate(disease = "RSV"),
  flu_first_vs_final_plotdat %>% mutate(disease = "Flu")
)

# -----------------------------
# 6) SUMMARY TABLE (for paper text)
# -----------------------------
revision_summary <- revision_df %>%
  group_by(disease) %>%
  summarise(
    n_pairs         = n(),
    median_pct_rev  = median(pct_revision, na.rm = TRUE),
    mean_pct_rev    = mean(pct_revision, na.rm = TRUE),
    p90_pct_rev     = quantile(pct_revision, 0.90, na.rm = TRUE),
    median_abs_rev  = median(abs_revision, na.rm = TRUE),
    p90_abs_rev     = quantile(abs_revision, 0.90, na.rm = TRUE),
    .groups = "drop"
  )

print(revision_summary)

# -----------------------------
# 7) PLOTS: RSV vs Flu revisions
# -----------------------------
library(ggplot2)
library(dplyr)

plot_df <- revision_df %>%
  filter(pct_revision > 0)

p_pub <- ggplot(plot_df,
                aes(x = disease,
                    y = pct_revision,
                    fill = disease)) +
  
  geom_violin(trim = TRUE,
              alpha = 0.35,
              linewidth = 0.6,
              color = "black") +
  
  geom_boxplot(width = 0.18,
               linewidth = 0.8,
               outlier.alpha = 0.15) +
  
  scale_y_log10(
    breaks = c(1, 10, 100, 1000),
    labels = c("1", "10", "100", "1000")
  ) +
  
  scale_fill_manual(values = c(
    Flu = "#2C6BA0",
    RSV = "#D7191C"
  )) +
  
  labs(
    x = NULL,
    y = "revision(%)"
  ) +
  
  theme_bw(base_size = 18) +
  
  theme(
    legend.position = "none",
    axis.title.y = element_text(face = "bold"),
    axis.text.x = element_text(size = 16),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

p_pub

ggsave(
  filename = "plots/paper/rsv_flu_rev_summary.png",
  plot = p_pub,
  dpi = 600,
  width = 6,
  height = 4,
  units = "in",
  bg = "white"
)
