library(readr)
flatline_sym_sqrt_10 <- read_csv("results/ili/flatline_sym_sqrt_10.csv")
seasonal_gam_forecasts_all <- read_csv("results/seasonal/forecasts/seasonal_gam_forecasts_all.csv")
combined_forecasts <- bind_rows(
  flatline_sym_sqrt_10,
  seasonal_gam_forecasts_all
)

truth_ili_seasonal <- read_csv("data/ili/truth_ili_seasonal.csv")

score_both <- covidHubUtils::score_forecasts(
  forecasts           = combined_forecasts,
  truth               = truth_ili_seasonal,
  return_format       = "wide",
  metrics             = c("wis", "interval_coverage"),
  use_median_as_point = TRUE
)

library(readr)
flatline_sym_sqrt_10 <- read_csv("results/ili/flatline_sym_sqrt_10.csv")
seasonal_gam_forecasts_all <- read_csv("results/seasonal/forecasts/seasonal_gam_forecasts_all.csv")
combined_forecasts <- bind_rows(
  flatline_sym_sqrt_10,
  seasonal_gam_forecasts_all
)

truth_ili_seasonal <- read_csv("data/ili/truth_ili_seasonal.csv")

score_both <- covidHubUtils::score_forecasts(
  forecasts           = combined_forecasts,
  truth               = truth_ili_seasonal,
  return_format       = "wide",
  metrics             = c("wis", "interval_coverage"),
  use_median_as_point = TRUE
)

View(score_both)

p1 <- score_both %>%
  dplyr::group_by(model, horizon) %>%
  dplyr::summarise(wis = mean(wis)) %>%
  scoringutils::plot_heatmap(metric = "wis", x = "horizon")
library(dplyr)
library(ggplot2)
library(cowplot)

# =========================
# 1) Summarise exactly like before
# =========================
avg_h <- score_both %>%
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

