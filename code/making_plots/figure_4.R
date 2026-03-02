# =========================
flatline_sym_sqrt_10 <- read_csv("results/ili/flatline_sym_sqrt_10.csv")
seasonal_gam_forecasts_all <- read_csv("results/seasonal/forecasts/seasonal_gam_forecasts_all.csv")

flat_dates <- unique(flatline_sym_sqrt_10$forecast_date)
gam_dates  <- unique(seasonal_gam_forecasts_all$forecast_date)

# Dates that match (intersection)
matched_dates <- intersect(flat_dates, gam_dates)

length(flat_dates)
length(matched_dates)
all(flat_dates %in% gam_dates)

library(dplyr)

# 1. Get common forecast dates
common_dates <- intersect(
  unique(flatline_sym_sqrt_10$forecast_date),
  unique(seasonal_gam_forecasts_all$forecast_date)
)

# 2. Filter BOTH datasets to matching dates only
flatline_matched <- flatline_sym_sqrt_10 %>%
  filter(forecast_date %in% as.Date(common_dates))

seasonal_gam_matched <- seasonal_gam_forecasts_all %>%
  filter(forecast_date %in% as.Date(common_dates))

identical(
  sort(unique(flatline_matched$forecast_date)),
  sort(unique(seasonal_gam_matched$forecast_date))
)

# 3. Combine
combined_forecasts <- bind_rows(
  flatline_matched,
  seasonal_gam_matched
)

# 4. Save as CSV
write_csv(
  combined_forecasts,
  "results/seasonal/flatline_vs_seasonal_gam_matched_forecasts.csv"
)

truth_ili <- read_csv("data/ili/truth_ili.csv")

library(covidHubUtils)

scores_flatline_vs_seasonal <- score_forecasts(
  forecasts = combined_forecasts,
  truth = truth_ili,
  metrics = c(
    "abs_error",
    "wis",
    "wis_components",
    "interval_coverage",
    "quantile_coverage"
  ),
  return_format = "wide",
  use_median_as_point = TRUE
)

p <- scores_flatline_vs_seasonal %>%
  dplyr::group_by(model, horizon) %>%
  dplyr::summarise(wis = mean(wis)) %>%
  scoringutils::plot_heatmap(metric = "wis", x = "horizon")
p

ggsave(
  filename = "plots/paper/seasonal_vs_flatline.png",
  plot = p,
  dpi = 600,          # High resolution
  width = 9,          # in inches
  height = 6,
  units = "in",
  bg = 'white'
)


summary_score <- scores_flatline_vs_seasonal  %>%
  dplyr::group_by(forecast_date, location, model, horizon) %>%
  dplyr::summarise(wis = mean(wis, na.rm = TRUE)) %>%
  dplyr::arrange(forecast_date, location, model, horizon)

View(summary_score)

# Assuming your data is in a dataframe named score

average_wis_per_group <- scores_flatline_vs_seasonal %>%
  group_by(forecast_date, location, model) %>%
  summarise(avg_wis = mean(wis, na.rm = TRUE), .groups = "drop") %>%
  arrange(forecast_date, location, model)

improv_df <- average_wis_per_group %>%
  filter(model %in% c("seasonal", "flatline")) %>%
  group_by(forecast_date, location) %>%
  summarise(
    seasonal_wis = avg_wis[model == "seasonal"][1],
    flatline_wis = avg_wis[model == "flatline"][1],
    .groups = "drop"
  ) %>%
  mutate(
    rel_wis_flatline = flatline_wis / seasonal_wis,
    improvement_pct  = (1 - rel_wis_flatline) * 100
  )

# -------------------------
# 2) Helpers: seasons/labels
# -------------------------
season_oct_may <- function(d) {
  case_when(
    month(d) >= 10 ~ paste0(year(d), "/", year(d) + 1),
    month(d) <=  5 ~ paste0(year(d) - 1, "/", year(d)),
    TRUE ~ NA_character_
  )
}

season_start_year <- function(d) if_else(month(d) >= 10, year(d), year(d) - 1)

season_label_short <- function(start_year) {
  paste0(start_year, "–", substr(as.character(start_year + 1), 3, 4))
}

# -------------------------
# 3) In-season improvement data (Oct–May)
# -------------------------
loc_order <- c("nat", paste0("hhs", 1:10))

improv_inseason <- improv_df %>%
  filter(month(forecast_date) %in% c(10,11,12,1,2,3,4,5)) %>%
  mutate(
    season_start = season_start_year(forecast_date),
    season = factor(
      season_label_short(season_start),
      levels = sort(unique(season_label_short(season_start)))
    )
  )

# Global factor levels (shared x for B/C)
all_dates <- sort(unique(improv_inseason$forecast_date))

# Heatmap df (panel C)
heat_df2 <- improv_inseason %>%
  mutate(
    improvement_cap = pmax(pmin(improvement_pct, 100), -100),
    location = factor(location, levels = loc_order),
    forecast_date_f = factor(forecast_date, levels = all_dates)
  )

# Bar df (panel B)
bar_df2 <- improv_inseason %>%
  group_by(season, forecast_date) %>%
  summarise(mean_improvement = mean(improvement_pct, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    forecast_date_f = factor(forecast_date, levels = all_dates)
  )

# -------------------------
# 4) Panel A: NAT ribbons + truth across seasons
# -------------------------
step_k <- 5
truth_start_date <- as.Date("2014-09-01")

truth_nat <- truth_ili %>%
  filter(
    location == "nat",
    target_variable == "inc wili",
    target_end_date >= truth_start_date
  ) %>%
  mutate(season = season_oct_may(target_end_date)) %>%
  filter(!is.na(season)) %>%
  rename(truth = value) %>%
  select(season, target_end_date, truth)

fc_nat <- combined_forecasts %>%
  filter(
    location == "nat",
    target_variable == "inc wili",
    type == "quantile",
    quantile %in% c(0.05, 0.25, 0.50, 0.75, 0.95),
    model %in% c("flatline", "seasonal")
  ) %>%
  mutate(season = season_oct_may(forecast_date)) %>%
  filter(!is.na(season)) %>%
  arrange(season, forecast_date) %>%
  group_by(season) %>%
  mutate(
    fd_rank = dense_rank(forecast_date),
    keep_fd = (fd_rank == 1) | ((fd_rank - 1) %% step_k == 0)
  ) %>%
  ungroup() %>%
  filter(keep_fd)

fc_nat_wide <- fc_nat %>%
  group_by(season, forecast_date, target_end_date, model) %>%
  summarise(
    lo90 = value[quantile == 0.05][1],
    lo50 = value[quantile == 0.25][1],
    med  = value[quantile == 0.50][1],
    hi50 = value[quantile == 0.75][1],
    hi90 = value[quantile == 0.95][1],
    .groups = "drop"
  ) %>%
  mutate(
    model = recode(model, flatline = "Flatline", seasonal = "Seasonal"),
    model = factor(model, levels = c("Seasonal", "Flatline"))
  ) %>%
  mutate(season_target = season_oct_may(target_end_date)) %>%
  filter(season_target == season) %>%
  select(-season_target)

# Plot A (REMOVE x ticks and labels)
p_nat_all_seasons <- ggplot() +
  geom_ribbon(
    data = fc_nat_wide,
    aes(
      x = target_end_date, ymin = lo90, ymax = hi90,
      fill = model,
      group = interaction(model, forecast_date)
    ),
    alpha = 0.18
  ) +
  geom_ribbon(
    data = fc_nat_wide,
    aes(
      x = target_end_date, ymin = lo50, ymax = hi50,
      fill = model,
      group = interaction(model, forecast_date)
    ),
    alpha = 0.35
  ) +
  geom_line(
    data = fc_nat_wide,
    aes(
      x = target_end_date, y = med,
      color = model,
      group = interaction(model, forecast_date)
    ),
    linewidth = 0.9
  ) +
  geom_line(
    data = truth_nat,
    aes(x = target_end_date, y = truth),
    color = "black",
    linewidth = 0.5
  ) +
  scale_color_manual(values = c("Seasonal" = "red", "Flatline" = "blue")) +
  scale_fill_manual(values  = c("Seasonal" = "red", "Flatline" = "blue")) +
  facet_grid(. ~ season, scales = "free_x", space = "free_x") +
  labs(x = NULL, y = "wILI% (National)", color = NULL, fill = NULL) +
  theme_cowplot() +
  theme(
    legend.position = "right",
    strip.text = element_text(size = 11, face = "bold"),
    strip.background = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank()
  )

# -------------------------
# 5) Panel C: regional heatmap (season labels at bottom)
# -------------------------
p_heat2 <- ggplot(heat_df2, aes(x = forecast_date_f, y = location, fill = improvement_cap)) +
  geom_tile(color = "white", linewidth = 0.3) +
  scale_fill_gradient2(
    low = "red", mid = "white", high = "blue",
    midpoint = 0, limits = c(-100, 100),
    name = NULL
  ) +
  facet_grid(. ~ season, scales = "free_x", space = "free_x", switch = "x") +
  labs(x = NULL, y = NULL) +
  theme_cowplot() +
  theme(
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank(),
    
    strip.text.x = element_text(size = 10, face = "bold"),
    strip.background = element_blank(),
    strip.placement = "outside",
    
    legend.position = "right"
  )

# -------------------------
# 6) Panel B: overall bars (NO x ticks, NO x labels)
# -------------------------
p_bar2 <- ggplot(bar_df2, aes(x = forecast_date_f, y = mean_improvement, fill = mean_improvement)) +
  geom_col(width = 0.9) +
  geom_hline(yintercept = 0, linewidth = 0.7) +
  scale_fill_gradient2(
    low = "red", mid = "white", high = "blue",
    midpoint = 0,
    name = NULL
  ) +
  facet_grid(. ~ season, scales = "free_x", space = "free_x") +
  labs(x = NULL, y = "% improvement") +
  theme_cowplot() +
  theme(
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank(),
    strip.text   = element_blank(),
    legend.position = "right"
  )

# -------------------------
# 7) Assemble (B in middle; C at bottom)
# -------------------------
p_ABC <- plot_grid(
  p_nat_all_seasons,  # A
  p_bar2,             # B (overall) in the middle
  p_heat2,            # C (regional) at the bottom
  ncol = 1,
  labels = c("A", "B", "C"),
  label_fontface = "bold",
  label_size = 14,
  align = "v",
  axis = "lr",
  rel_heights = c(1.2, 0.8, 1.0)
)

p_ABC



ggsave(
  filename = "plots/paper/seasonal_vs_flatline.png",
  plot = p_ABC,
  dpi = 1000,          # High resolution
  width = 12,          # in inches
  height = 8,
  units = "in",
  bg = 'white'
)