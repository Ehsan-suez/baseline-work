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


scores_dir <- "results/rsv/scores/"

score_files <- list.files(
  path = scores_dir,
  pattern = "^rsv_baseline_scores_.*\\.csv$",
  full.names = TRUE
)

extract_issue_date <- function(filename) {
  date_str <- str_extract(filename, "\\d{4}-\\d{2}-\\d{2}")
  as.Date(date_str)
}

rsv_scores_all <- map_dfr(score_files, function(f) {
  read_csv(f, show_col_types = FALSE) %>%
    mutate(issue_date = extract_issue_date(f))
}) %>%
  arrange(issue_date)

rsv_scores_all_transformed <- rsv_scores_all %>%
  mutate(
    variation = sub("_w_?(\\d+|all)$", "", model),
    data = sub("^.*_w_?(\\d+|all)$", "\\1", model)
  ) %>%
  mutate(
    variation = recode(
      variation,
      "none_nonsym" = "Drift",
      "none_sym"    = "Flatline",
      "sqrt_nonsym" = "Drift (transformed)",
      "sqrt_sym"    = "Flatline (transformed)"
    )
  )



# ----------------------------
# 4) Figure S8B: Mean WIS vs Weeks of Data (Drift vs Flatline)
# ----------------------------
a_df_rsv <- rsv_scores_all_transformed %>%
  group_by(variation, data) %>%
  summarise(
    mean_wis   = mean(wis, na.rm = FALSE),
    mean_cov50 = mean(quantile_coverage_0.5, na.rm = FALSE),
    mean_cov95 = mean(quantile_coverage_0.95, na.rm = FALSE),
    .groups = "drop"
  ) %>%
  mutate(
    data_num = ifelse(data == "all", NA, as.numeric(data)),
    data_factor = ifelse(is.na(data_num), "all", sprintf("%02d", data_num))
  ) %>%
  mutate(
    data_factor = factor(
      data_factor,
      levels = c(sprintf("%02d", sort(unique(na.omit(data_num)))), "all")
    )
  ) %>%
  filter(variation %in% c("Drift", "Flatline"))

# keep every 5th level + "all"
x_breaks_5 <- levels(a_df_rsv$data_factor)
x_breaks_5 <- x_breaks_5[
  x_breaks_5 == "all" |
    (as.numeric(x_breaks_5) %% 5 == 0)
]

fig_s8a <- ggplot(
  a_df_rsv,
  aes(x = data_factor, y = mean_wis, color = variation, group = variation)
) +
  geom_line(size = 0.4) +
  geom_point(size = 0.8) +
  scale_x_discrete(breaks = x_breaks_5) +
  labs(x = "Weeks of Data", y = "WIS", color = "Model") +
  theme_minimal(base_size = 13) +
  theme(
    panel.border = element_rect(color = "black", fill = NA),
    legend.position = "right",
    axis.text.x = element_text(hjust = 1)
  ) +
  scale_color_manual(values = manual_colors)

fig_s8a


# ----------------------------
# 5) Figure S8C: WIS vs Forecast Date (Drift vs Flatline)
#     NOTE: uses quantile_coverage_* columns to avoid mismatch
# ----------------------------
b_df_rsv <- rsv_scores_all_transformed %>%
  group_by(variation, target_end_date) %>%
  summarise(
    mean_wis   = mean(wis, na.rm = FALSE),
    mean_cov50 = mean(quantile_coverage_0.5, na.rm = FALSE),
    mean_cov95 = mean(quantile_coverage_0.95, na.rm = FALSE),
    .groups = "drop"
  ) %>%
  filter(variation %in% c("Drift", "Flatline"))

# make sure these are Date
rsv_first_vs_final_plotdat <- rsv_first_vs_final_plotdat %>%
  mutate(target_end_date = as.Date(target_end_date))

b_df_rsv <- b_df_rsv %>%
  mutate(target_end_date = as.Date(target_end_date))

# shared date window
date_min <- max(min(rsv_first_vs_final_plotdat$target_end_date, na.rm = TRUE),
                min(b_df_rsv$target_end_date, na.rm = TRUE))
date_max <- min(max(rsv_first_vs_final_plotdat$target_end_date, na.rm = TRUE),
                max(b_df_rsv$target_end_date, na.rm = TRUE))

date_min; date_max

# US backfill in shared window
rsv_us_backfill_aligned <- rsv_first_vs_final_plotdat %>%
  filter(location == "US",
         target_end_date >= date_min,
         target_end_date <= date_max)

# WIS by forecast date in shared window (already mean'd)
b_df_rsv_aligned <- b_df_rsv %>%
  filter(target_end_date >= date_min,
         target_end_date <= date_max)

# scaling factor: map backfill range onto WIS range
wis_rng <- range(b_df_rsv_aligned$mean_wis, na.rm = TRUE)
rev_rng <- range(rsv_us_backfill_aligned$total_revision, na.rm = TRUE)

scale_factor <- diff(wis_rng) / diff(rev_rng)

fig_overlay <- ggplot() +
  # C: WIS lines (Drift vs Flatline)
  geom_line(
    data = b_df_rsv_aligned,
    aes(x = target_end_date, y = mean_wis, color = variation),
    linewidth = 0.4
  ) +
  geom_point(
    data = b_df_rsv_aligned,
    aes(x = target_end_date, y = mean_wis, color = variation),
    size = 0.4
  ) +
  
  # A: backfill (scaled into WIS axis)
  geom_line(
    data = rsv_us_backfill_aligned,
    aes(x = target_end_date, y = (total_revision - rev_rng[1]) * scale_factor + wis_rng[1]),
    color = "gray30",
    alpha = 0.35,
    linewidth = 0.8
  ) +
  geom_hline(yintercept = wis_rng[1], linetype = "dashed", color = "gray70") +
  
  scale_color_manual(values = manual_colors) +
  scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m") +
  
  # left axis = WIS, right axis = backfill revisions (original units)
  scale_y_continuous(
    name = "WIS",
    sec.axis = sec_axis(
      trans = ~ (. - wis_rng[1]) / scale_factor + rev_rng[1],
      name = "Revision"
    )
  ) +
  
  labs(x = "Dates", color = "Model") +
  theme_minimal(base_size = 13) +
  theme(
    panel.border = element_rect(color = "black", fill = NA),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    legend.position = "right"
  )

fig_overlay



library(cowplot)

# --- 1) Extract ONE legend (use overlay legend) ---
legend_shared <- cowplot::get_legend(
  fig_overlay +
    theme(
      legend.position = "right",
      legend.box.margin = margin(0, 0, 0, 0)
    )
)

# --- 2) Remove legends from both panels ---
fig_s8a_noleg <- fig_s8a + theme(legend.position = "none")
fig_overlay_noleg <- fig_overlay + theme(legend.position = "none")

# --- 3) Build the left side (A on top, B bottom) ---
left_panels <- plot_grid(
  fig_s8a_noleg,
  fig_overlay_noleg,
  ncol = 1,
  labels = c("A", "B"),
  label_size = 14,
  label_x = 0.01,
  label_y = 0.98,
  align = "v",
  axis = "lr",
  rel_heights = c(1.0, 1.35)  # give B more space (dates + dual axis)
)

# --- 4) Combine left panels + shared legend ---
fig_8_final <- plot_grid(
  left_panels,
  legend_shared,
  ncol = 2,
  rel_widths = c(1, 0.18)  # controls how much space legend gets
)

fig_8_final

ggsave(
  filename = "plots/paper/rsv_Revision_vs_wis.png",
  plot = fig_8_final,
  dpi = 600,          # High resolution
  width = 8,          # in inches
  height = 6,
  units = "in",
  bg = 'white'
)




