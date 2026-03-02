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
      "none_sym"    = "Flatline"
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
  scale_color_manual(values = manual_colors_two)

fig_s8a


# WIS by target_end_date (mean over issue_date, horizons, etc. as in your original)
b_df_rsv <- rsv_scores_all_transformed %>%
  group_by(variation, target_end_date) %>%
  summarise(
    mean_wis = mean(wis, na.rm = TRUE),
    .groups  = "drop"
  ) %>%
  filter(variation %in% c("Drift", "Flatline")) %>%
  mutate(target_end_date = as.Date(target_end_date))

# Revision by target_end_date for US
rsv_us_rev <- rsv_first_vs_final_plotdat %>%
  filter(location == "US") %>%
  transmute(
    target_end_date,
    revision_pct = abs(pct_revision)
  )

# Wide so we can compute Flatline - Drift
rwis_df <- b_df_rsv %>%
  
  select(target_end_date, variation, mean_wis) %>%
  
  tidyr::pivot_wider(
    names_from  = variation,
    values_from = mean_wis
  ) %>%
  
  mutate(
    
    rWIS = Flatline / Drift
    
  )

scatter_df <- rwis_df %>%
  
  left_join(
    rsv_first_vs_final_plotdat %>%
      filter(location == "US") %>%
      select(target_end_date, pct_revision),
    by = "target_end_date"
  ) %>%
  
  rename(
    revision_pct = pct_revision
  )

manual_colors_two <- c(
  "Drift"    = "#ff7f00",
  "Flatline" = "#5e3c99"
)

scatter_df <- scatter_df %>%
  
  mutate(
    
    variation = ifelse(
      rWIS < 1,
      "Drift",        # your requested mapping
      "Flatline"
    )
    
  )



fig_s8b <- scatter_df %>%
  
  filter(revision_pct > 0) %>%
  
  ggplot(aes(
    x = revision_pct,
    y = rWIS,
    color = variation
  )) +
  
  geom_hline(
    yintercept = 1,
    linetype = "dashed",
    linewidth = 0.8,
    color = "grey40"
  ) +
  
  geom_point(
    size = 2.3,
    alpha = 0.7
  ) +
  
  geom_smooth(
    method = "lm",
    se = TRUE,
    linewidth = 1.0,
    color = "black"
  ) +
  
  scale_color_manual(values = manual_colors_two) +
  
  scale_x_log10(
    name = "Revision (%)"
  ) +
  
  labs(
    y = "rWIS",
    color = "variation"
  ) +
  
  theme_minimal(base_size = 13) +
  
  theme(
    panel.border = element_rect(color = "black", fill = NA),
    legend.position = "right"
  )

fig_s8b
# ----------------------------
# 5) One shared legend (from Panel A)
# ----------------------------
legend_shared <- cowplot::get_legend(
  fig_s8a + theme(legend.position = "right")
)

fig_s8a_noleg <- fig_s8a + theme(legend.position = "none")
fig_s8b_noleg <- fig_s8b + theme(legend.position = "none")

left_panels <- plot_grid(
  fig_s8a_noleg,
  fig_s8b_noleg,
  ncol = 1,
  labels = c("A", "B"),
  label_size = 14,
  label_x = 0.01,
  label_y = 0.98,
  align = "v",
  axis = "lr",
  rel_heights = c(1.0, 1.05)
)

fig_8_final <- plot_grid(
  left_panels,
  legend_shared,
  ncol = 2,
  rel_widths = c(1, 0.18)
)

fig_8_final

ggsave(
  filename = "plots/paper/rsv_revision_scatter_vs_rwis.png",
  plot = fig_8_final,
  dpi = 600,
  width = 6,
  height = 4,
  units = "in",
  bg = "white"
)
