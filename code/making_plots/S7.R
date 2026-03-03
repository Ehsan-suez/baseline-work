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

p1 <- scores_flatline_vs_seasonal %>%
  dplyr::group_by(model, horizon) %>%
  dplyr::summarise(wis = mean(wis)) %>%
  scoringutils::plot_heatmap(metric = "wis", x = "horizon")
library(dplyr)
library(ggplot2)
library(cowplot)

# =========================
# 1) Summarise exactly like before
# =========================
avg_h <- scores_flatline_vs_seasonal %>%
  group_by(model, horizon) %>%
  summarise(
    mean_wis = mean(wis, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    horizon = as.integer(horizon),
    
    # Rename models for clean legend (EDIT if needed)
    model_label = case_when(
      model == "flatline" ~ "Flatline",
      model == "seasonal" ~ "Seasonal",
      TRUE ~ model
    )
  )

# =========================
# 2) Define manual colors
# =========================
manual_colors <- c(
  "Flatline" = "#5e3c99",
  "Seasonal"           = "#ff7f00"
)

# =========================
# 3) Plot: solid lines + points
# =========================
p_wis_horizon <- ggplot(
  avg_h,
  aes(
    x = horizon,
    y = mean_wis,
    group = model_label,
    color = model_label
  )
) +
  
  geom_line(
    linewidth = 1.2,
    linetype = "solid"
  ) +
  
  geom_point(
    size = 2.5
  ) +
  
  scale_color_manual(
    values = manual_colors,
    breaks = names(manual_colors)
  ) +
  
  scale_x_continuous(
    breaks = sort(unique(avg_h$horizon))
  ) +
  
  labs(
    x = "Forecast horizon (weeks)",
    y = "WIS",
    color = NULL
  ) +
  
  theme_cowplot() +
  
  theme(
    legend.position = "right",
    
    legend.text = element_text(
      face = "bold",
      size = 11
    ),
    
    axis.title = element_text(
      face = "bold"
    ),
    
    panel.grid = element_blank()
  )

# =========================
# 4) Show plot
# =========================
p_wis_horizon


# =========================
# 5) Save
# =========================
ggsave(
  "plots/paper/horizon_wis_line.png",
  plot = p_wis_horizon,
  height = 4.5,
  width = 7.5,
  units = "in",
  dpi = 400,
  bg = "white"
)


