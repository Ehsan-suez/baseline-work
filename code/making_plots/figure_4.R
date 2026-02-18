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
  summarise(avg_wis = mean(wis, na.rm = TRUE)) %>%
  arrange(forecast_date, location, model)
library(dplyr)
library(tidyr)

improv_df <- average_wis_per_group %>%
  filter(model %in% c("seasonal", "flatline")) %>%
  group_by(forecast_date, location) %>%
  summarise(
    seasonal_wis  = avg_wis[model == "seasonal"][1],
    flatline_wis  = avg_wis[model == "flatline"][1],
    .groups = "drop"
  ) %>%
  mutate(
    rel_wis_flatline = flatline_wis / seasonal_wis,
    improvement_pct  = (1 - rel_wis_flatline) * 100
  )


# Barplot: % improvement across ALL regions by forecast date
# WITH season separators
# X-axis shows season labels like "2014–15"
# =========================
library(dplyr)
library(ggplot2)
library(cowplot)
library(lubridate)

# --- 1) Aggregate improvement across locations per forecast date (NO cap)
bar_df <- improv_df %>%
  mutate(
    season = case_when(
      month(forecast_date) >= 10 ~ paste0(year(forecast_date), "/", year(forecast_date) + 1),
      month(forecast_date) <=  5 ~ paste0(year(forecast_date) - 1, "/", year(forecast_date)),
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(season)) %>%
  group_by(forecast_date, season) %>%
  summarise(
    mean_improvement = mean(improvement_pct, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(forecast_date)

# --- 2) Discrete x (NO gaps)
bar_df <- bar_df %>%
  mutate(
    forecast_date_f = factor(
      forecast_date,
      levels = unique(forecast_date)
    )
  )

# --- 3) Season separators (same as heatmap)
season_breaks_bar <- bar_df %>%
  group_by(season) %>%
  summarise(first_date = first(forecast_date), .groups = "drop") %>%
  mutate(
    x_pos = match(first_date, levels(bar_df$forecast_date_f))
  ) %>%
  arrange(x_pos)

# --- 4) Season centers for x-axis labels
season_centers <- bar_df %>%
  group_by(season) %>%
  summarise(
    x_center = mean(match(forecast_date, levels(bar_df$forecast_date_f))),
    .groups = "drop"
  ) %>%
  mutate(
    season_label = gsub("/", "–", season)  # 2014/2015 → 2014–2015
  )

# --- 5) Plot
p_bar <- ggplot(
  bar_df,
  aes(
    x = forecast_date_f,
    y = mean_improvement,
    fill = mean_improvement
  )
) +
  geom_col(width = 0.9) +
  geom_hline(yintercept = 0, linewidth = 0.7) +
  
  # season separators
  geom_vline(
    data = season_breaks_bar,
    aes(xintercept = x_pos - 0.5),
    color = "black",
    linewidth = 0.7
  ) +
  
  # red–white–blue (no capping)
  scale_fill_gradient2(
    low = "red",
    mid = "white",
    high = "blue",
    midpoint = 0,
    name = "% improv\n(over seasonal)"
  ) +
  
  # season labels only
  scale_x_discrete(
    breaks = levels(bar_df$forecast_date_f)[round(season_centers$x_center)],
    labels = season_centers$season_label
  ) +
  
  labs(
    x = NULL,
    y = "% improvement"
  ) +
  theme_cowplot() +
  theme(
    axis.text.x = element_text(size = 11, face = "bold"),
    axis.text.y = element_text(size = 10),
    legend.position = "right"
  )

p_bar


# =========================
# Heatmap with NO gaps + season separators
# X axis shows season labels like "2014–15"
# ========================
library(dplyr)
library(ggplot2)
library(cowplot)
library(lubridate)

# --- 1) Order for locations
loc_order <- c("nat", paste0("hhs", 1:10))

# --- 2) Prep heatmap df (cap improvement, set factor order) + season (Oct–May only)
heat_df <- improv_df %>%
  mutate(
    improvement_cap = pmax(pmin(improvement_pct, 100), -100),
    location = factor(location, levels = loc_order),
    season = case_when(
      month(forecast_date) >= 10 ~ paste0(year(forecast_date), "/", year(forecast_date) + 1),
      month(forecast_date) <=  5 ~ paste0(year(forecast_date) - 1, "/", year(forecast_date)),
      TRUE ~ NA_character_  # Jun–Sep off-season
    )
  ) %>%
  filter(!is.na(season)) %>%
  arrange(forecast_date)

# --- 3) Make x discrete to remove gaps (one tile per forecast date)
heat_df <- heat_df %>%
  mutate(
    forecast_date_f = factor(
      forecast_date,
      levels = sort(unique(forecast_date))
    )
  )

# --- 4) Find first forecast date of each season (for vertical separators)
season_breaks <- heat_df %>%
  arrange(forecast_date) %>%
  group_by(season) %>%
  summarise(first_date = first(forecast_date), .groups = "drop") %>%
  mutate(x_pos = match(first_date, levels(heat_df$forecast_date_f))) %>%
  filter(!is.na(x_pos)) %>%
  arrange(x_pos)

# --- 5) Season centers for x-axis labels ("2014–15")
season_centers <- heat_df %>%
  group_by(season) %>%
  summarise(
    x_center = mean(match(forecast_date, levels(heat_df$forecast_date_f))),
    .groups = "drop"
  ) %>%
  mutate(
    season_label = paste0(
      substr(sub("/.*", "", season), 1, 4),  # start year
      "–",
      substr(sub(".*/", "", season), 3, 4)   # end year last 2 digits
    )
  )

x_breaks_season <- levels(heat_df$forecast_date_f)[round(season_centers$x_center)]
x_labels_season <- season_centers$season_label

# --- 6) Plot
p_heat <- ggplot(
  heat_df,
  aes(x = forecast_date_f, y = location, fill = improvement_cap)
) +
  geom_tile(color = "white", linewidth = 0.3) +
  geom_vline(
    data = season_breaks,
    aes(xintercept = x_pos - 0.5),
    color = "black",
    linewidth = 0.7
  ) +
  scale_fill_gradient2(
    low = "red", mid = "white", high = "blue",
    midpoint = 0,
    limits = c(-100, 100),
    name = "% improv\n(over seasonal)"
  ) +
  scale_x_discrete(
    breaks = x_breaks_season,
    labels = x_labels_season
  ) +
  labs(x = NULL, y = NULL) +
  theme_cowplot() +
  theme(
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 11, face = "bold", hjust = 0.5),
    legend.position = "right"
  )

p_heat


library(dplyr)
library(ggplot2)
library(cowplot)
library(lubridate)

# =========================
# Settings
# =========================
step_k <- 5
truth_start_date <- as.Date("2014-09-01")

# =========================
# Helper: Oct–May season
# =========================
season_oct_may <- function(d) {
  case_when(
    month(d) >= 10 ~ paste0(year(d), "/", year(d) + 1),
    month(d) <=  5 ~ paste0(year(d) - 1, "/", year(d)),
    TRUE ~ NA_character_
  )
}

# =========================
# 1) NAT truth (from Sept 2014)
# =========================
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

# =========================
# 2) NAT forecasts: select forecast dates + keep quantiles
# =========================
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

# =========================
# 3) Build interval columns WITHOUT pivot_wider (robust)
# =========================
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
  )

# keep target_end_dates within same season window
fc_nat_wide <- fc_nat_wide %>%
  mutate(season_target = season_oct_may(target_end_date)) %>%
  filter(season_target == season) %>%
  select(-season_target)

# =========================
# 4) Plot: ribbons + medians + truth (all seasons, NAT)
# =========================
p_nat_all_seasons <- ggplot() +
  # 90% ribbons
  geom_ribbon(
    data = fc_nat_wide,
    aes(
      x = target_end_date,
      ymin = lo90,
      ymax = hi90,
      fill = model,
      group = interaction(model, forecast_date)
    ),
    alpha = 0.18
  ) +
  # 50% ribbons
  geom_ribbon(
    data = fc_nat_wide,
    aes(
      x = target_end_date,
      ymin = lo50,
      ymax = hi50,
      fill = model,
      group = interaction(model, forecast_date)
    ),
    alpha = 0.35
  ) +
  # median lines
  geom_line(
    data = fc_nat_wide,
    aes(
      x = target_end_date,
      y = med,
      color = model,
      group = interaction(model, forecast_date)
    ),
    linewidth = 0.9
  ) +
  # truth
  geom_line(
    data = truth_nat,
    aes(x = target_end_date, y = truth),
    color = "black",
    linewidth = 0.5
  ) +
  scale_color_manual(values = c("Seasonal" = "red", "Flatline" = "blue")) +
  scale_fill_manual(values = c("Seasonal" = "red", "Flatline" = "blue")) +
  facet_wrap(~ season, nrow = 1, scales = "free_x") +
  labs(
    x = NULL, y = "wILI% (National)", color = NULL, fill = NULL,
    title = NULL
  ) +
  theme_cowplot() +
  theme(
    legend.position = "right",
    strip.text = element_text(size = 11, face = "bold")
  )

p_nat_all_seasons <- p_nat_all_seasons +
  theme(
    strip.background = element_blank(),  # removes gray strip
    panel.background = element_blank(),  # ensures white panels
    panel.grid = element_blank()          # removes gray gridlines
  )

p_nat_all_seasons




library(dplyr)
library(ggplot2)
library(cowplot)
library(lubridate)

season_start_year <- function(d) if_else(month(d) >= 10, year(d), year(d) - 1)

season_label_short <- function(start_year) {
  paste0(start_year, "–", substr(as.character(start_year + 1), 3, 4))
}

facet_grid(. ~ season, scales = "free_x", space = "free_x")

loc_order <- c("nat", paste0("hhs", 1:10))

heat_df2 <- improv_df %>%
  mutate(
    improvement_cap = pmax(pmin(improvement_pct, 100), -100),
    location = factor(location, levels = loc_order),
    season_start = season_start_year(forecast_date),
    season = factor(
      season_label_short(season_start),
      levels = sort(unique(season_label_short(season_start)))
    )
  ) %>%
  # keep Oct–May only
  filter(month(forecast_date) %in% c(10,11,12,1,2,3,4,5)) %>%
  group_by(season) %>%
  mutate(
    forecast_date_f = factor(forecast_date, levels = sort(unique(forecast_date)))
  ) %>%
  ungroup()

p_heat2 <- ggplot(heat_df2, aes(x = forecast_date_f, y = location, fill = improvement_cap)) +
  geom_tile(color = "white", linewidth = 0.3) +
  scale_fill_gradient2(
    low = "red", mid = "white", high = "blue",
    midpoint = 0, limits = c(-100, 100),
    name = NULL
  ) +
  facet_grid(. ~ season, scales = "free_x", space = "free_x") +
  labs(x = NULL, y = NULL) +
  theme_cowplot() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    strip.text = element_blank(),
    legend.position = "right"
  )

bar_df2 <- improv_df %>%
  mutate(
    season_start = season_start_year(forecast_date),
    season = factor(
      season_label_short(season_start),
      levels = sort(unique(season_label_short(season_start)))
    )
  ) %>%
  filter(month(forecast_date) %in% c(10,11,12,1,2,3,4,5)) %>%
  group_by(season, forecast_date) %>%
  summarise(mean_improvement = mean(improvement_pct, na.rm = TRUE), .groups = "drop") %>%
  group_by(season) %>%
  mutate(forecast_date_f = factor(forecast_date, levels = sort(unique(forecast_date)))) %>%
  ungroup()

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
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    strip.text = element_blank(),   # IMPORTANT: don’t repeat season strip (B already shows it)
    legend.position = "right"
  )

p_ABC <- plot_grid(
  p_nat_all_seasons,  # but with facet_grid space="free_x"
  p_heat2,
  p_bar2,
  ncol = 1,
  labels = c("A", "B", "C"),
  label_fontface = "bold",
  label_size = 14,
  align = "v",
  axis = "lr",
  rel_heights = c(1.2, 1, 0.8)
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

###alternative
# ============================================================
# A) NAT forecast plot (all seasons, ribbons + truth)
# B) Heatmap (% improvement) with DISCRETE BIN COLORS
# C) Barplot (mean % improvement) with DISCRETE BIN COLORS
# Combine into one A/B/C figure (season-aligned; season labels ONLY in A)
# ============================================================
# 
# library(dplyr)
# library(ggplot2)
# library(cowplot)
# library(lubridate)
# 
# # =========================
# # Global settings
# # =========================
# step_k <- 5
# truth_start_date <- as.Date("2014-09-01")
# loc_order <- c("nat", paste0("hhs", 1:10))
# 
# # =========================
# # Season helpers (Oct–May)
# # =========================
# season_oct_may <- function(d) {
#   case_when(
#     month(d) >= 10 ~ paste0(year(d), "/", year(d) + 1),
#     month(d) <=  5 ~ paste0(year(d) - 1, "/", year(d)),
#     TRUE ~ NA_character_
#   )
# }
# 
# # For facet strip text: "2014–15"
# season_label_short <- function(d) {
#   start_year <- if_else(month(d) >= 10, year(d), year(d) - 1)
#   paste0(start_year, "–", substr(as.character(start_year + 1), 3, 4))
# }
# 
# # =========================
# # BIN COLORS for Panels B & C
# # =========================
# bin_colors <- c(
#   "< -100%"       = "#67000D",
#   "-100% to -50%" = "#A50F15",
#   "-50% to 0%"    = "#CB181D",
#   "0%"            = "gray85",
#   "0% to 50%"     = "#6BAED6",
#   "50% to 100%"   = "#2171B5",
#   "> 100%"        = "#08306B"
# )
# bin_levels <- names(bin_colors)
# 
# bin_improvement <- function(x) {
#   case_when(
#     x <  -100              ~ "< -100%",
#     x >= -100 & x < -50    ~ "-100% to -50%",
#     x >=  -50 & x < 0      ~ "-50% to 0%",
#     x ==  0                ~ "0%",
#     x >    0 & x <= 50     ~ "0% to 50%",
#     x >   50 & x <= 100    ~ "50% to 100%",
#     x >  100               ~ "> 100%"
#   )
# }
# 
# # ============================================================
# # PANEL A: NAT forecasts vs truth (all seasons in one figure)
# # ============================================================
# 
# # 1) NAT truth (from Sept 2014), Oct–May only
# truth_nat <- truth_ili %>%
#   filter(
#     location == "nat",
#     target_variable == "inc wili",
#     target_end_date >= truth_start_date
#   ) %>%
#   mutate(
#     season = season_oct_may(target_end_date),
#     season = season_label_short(target_end_date)
#   ) %>%
#   filter(!is.na(season_oct_may(target_end_date))) %>%
#   rename(truth = value) %>%
#   select(season, target_end_date, truth)
# 
# # 2) NAT forecasts: select forecast dates (first + every 5th), keep key quantiles
# fc_nat <- combined_forecasts %>%
#   filter(
#     location == "nat",
#     target_variable == "inc wili",
#     type == "quantile",
#     quantile %in% c(0.05, 0.25, 0.50, 0.75, 0.95),
#     model %in% c("flatline", "seasonal")
#   ) %>%
#   mutate(
#     season_raw = season_oct_may(forecast_date),
#     season = season_label_short(forecast_date)
#   ) %>%
#   filter(!is.na(season_raw)) %>%
#   arrange(season, forecast_date) %>%
#   group_by(season) %>%
#   mutate(
#     fd_rank = dense_rank(forecast_date),
#     keep_fd = (fd_rank == 1) | ((fd_rank - 1) %% step_k == 0)
#   ) %>%
#   ungroup() %>%
#   filter(keep_fd)
# 
# # 3) Interval columns WITHOUT pivot_wider (robust)
# fc_nat_wide <- fc_nat %>%
#   group_by(season, forecast_date, target_end_date, model) %>%
#   summarise(
#     lo90 = value[quantile == 0.05][1],
#     lo50 = value[quantile == 0.25][1],
#     med  = value[quantile == 0.50][1],
#     hi50 = value[quantile == 0.75][1],
#     hi90 = value[quantile == 0.95][1],
#     .groups = "drop"
#   ) %>%
#   mutate(
#     model = recode(model, flatline = "Flatline", seasonal = "Seasonal"),
#     model = factor(model, levels = c("Seasonal", "Flatline"))
#   )
# 
# # Keep target_end_dates within same season window
# fc_nat_wide <- fc_nat_wide %>%
#   mutate(season_target_raw = season_oct_may(target_end_date)) %>%
#   filter(!is.na(season_target_raw)) %>%
#   mutate(season_target = season_label_short(target_end_date)) %>%
#   filter(season_target == season) %>%
#   select(-season_target_raw, -season_target)
# 
# # Make season factor order consistent across all panels
# all_seasons <- sort(unique(fc_nat_wide$season))
# truth_nat <- truth_nat %>% mutate(season = factor(season, levels = all_seasons))
# fc_nat_wide <- fc_nat_wide %>% mutate(season = factor(season, levels = all_seasons))
# 
# p_A <- ggplot() +
#   # 90% ribbons
#   geom_ribbon(
#     data = fc_nat_wide,
#     aes(
#       x = target_end_date,
#       ymin = lo90,
#       ymax = hi90,
#       fill = model,
#       group = interaction(model, forecast_date)
#     ),
#     alpha = 0.18
#   ) +
#   # 50% ribbons
#   geom_ribbon(
#     data = fc_nat_wide,
#     aes(
#       x = target_end_date,
#       ymin = lo50,
#       ymax = hi50,
#       fill = model,
#       group = interaction(model, forecast_date)
#     ),
#     alpha = 0.35
#   ) +
#   # median lines
#   geom_line(
#     data = fc_nat_wide,
#     aes(
#       x = target_end_date,
#       y = med,
#       color = model,
#       group = interaction(model, forecast_date)
#     ),
#     linewidth = 0.9
#   ) +
#   # truth
#   geom_line(
#     data = truth_nat,
#     aes(x = target_end_date, y = truth),
#     color = "black",
#     linewidth = 0.5
#   ) +
#   scale_color_manual(values = c("Seasonal" = "red", "Flatline" = "blue")) +
#   scale_fill_manual(values = c("Seasonal" = "red", "Flatline" = "blue")) +
#   facet_grid(. ~ season, scales = "free_x", space = "free_x") +
#   labs(x = NULL, y = "wILI% (National)", color = NULL, fill = NULL, title = NULL) +
#   theme_cowplot() +
#   theme(
#     legend.position = "right",
#     strip.text = element_text(size = 11, face = "bold")
#   )
# 
# # ============================================================
# # PANEL B: Heatmap (% improvement) — binned colors
# # ============================================================
# 
# heat_df2 <- improv_df %>%
#   mutate(
#     location = factor(location, levels = loc_order),
#     season_raw = season_oct_may(forecast_date),
#     season = season_label_short(forecast_date)
#   ) %>%
#   filter(!is.na(season_raw)) %>%
#   group_by(season) %>%
#   mutate(
#     forecast_date_f = factor(forecast_date, levels = sort(unique(forecast_date))),
#     improv_bin = factor(bin_improvement(improvement_pct), levels = bin_levels)
#   ) %>%
#   ungroup() %>%
#   mutate(season = factor(season, levels = all_seasons))
# 
# p_B <- ggplot(heat_df2, aes(x = forecast_date_f, y = location, fill = improv_bin)) +
#   geom_tile(color = "white", linewidth = 0.3) +
#   scale_fill_manual(values = bin_colors, drop = FALSE, name = NULL) +
#   facet_grid(. ~ season, scales = "free_x", space = "free_x") +
#   labs(x = NULL, y = NULL) +
#   theme_cowplot() +
#   theme(
#     axis.text.x = element_blank(),
#     axis.ticks.x = element_blank(),
#     strip.text = element_blank(),     # season labels ONLY in A
#     legend.position = "right"
#   )
# 
# # ============================================================
# # PANEL C: Barplot (mean % improvement) — binned colors
# # ============================================================
# 
# bar_df2 <- improv_df %>%
#   mutate(
#     season_raw = season_oct_may(forecast_date),
#     season = season_label_short(forecast_date)
#   ) %>%
#   filter(!is.na(season_raw)) %>%
#   group_by(season, forecast_date) %>%
#   summarise(mean_improvement = mean(improvement_pct, na.rm = TRUE), .groups = "drop") %>%
#   group_by(season) %>%
#   mutate(
#     forecast_date_f = factor(forecast_date, levels = sort(unique(forecast_date))),
#     improv_bin = factor(bin_improvement(mean_improvement), levels = bin_levels)
#   ) %>%
#   ungroup() %>%
#   mutate(season = factor(season, levels = all_seasons))
# 
# p_C <- ggplot(bar_df2, aes(x = forecast_date_f, y = mean_improvement, fill = improv_bin)) +
#   geom_col(width = 0.9) +
#   geom_hline(yintercept = 0, linewidth = 0.7) +
#   scale_fill_manual(values = bin_colors, drop = FALSE, name = NULL) +
#   facet_grid(. ~ season, scales = "free_x", space = "free_x") +
#   labs(x = NULL, y = "% improvement") +
#   theme_cowplot() +
#   theme(
#     axis.text.x = element_blank(),
#     axis.ticks.x = element_blank(),
#     strip.text = element_blank(),     # season labels ONLY in A
#     legend.position = "right"
#   )
# 
# # ============================================================
# # Combine A, B, C in one figure
# # ============================================================
# p_ABC <- plot_grid(
#   p_A, p_B, p_C,
#   ncol = 1,
#   labels = c("A", "B", "C"),
#   label_fontface = "bold",
#   label_size = 14,
#   align = "v",
#   axis = "lr",
#   rel_heights = c(1.25, 1, 0.85)
# )
# 
# p_ABC
# 
# # Optional save
# # ggsave("ABC_figure.png", p_ABC, width = 16, height = 9, dpi = 400)
# # ggsave("ABC_figure.pdf", p_ABC, width = 16, height = 9)

