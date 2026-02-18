library(readr)
library(dplyr)
library(tidyverse)
library(cowplot)
library(ggplot2)
library(tidyr)
library(grid)
library(patchwork)
library(covidHubUtils)
library(gtable)
library(lubridate)
library(scales)

# COVID
dir_path <- "results/covid/forecasts"

files <- list.files(
  path = dir_path,
  pattern = "\\.csv$",
  full.names = TRUE
)

files_every_6th <- files[seq(1, length(files), by = 6)]

combined_df_covid <- files_every_6th %>%
  lapply(function(f) {
    read_csv(f, show_col_types = FALSE) %>%
      filter(
        model %in% c("none_nonsym_w10", "none_sym_w10"),
        location == "US"
      )
  }) %>%
  bind_rows()

covid_wide <- combined_df_covid %>%
  filter(type == "quantile") %>%
  mutate(q = sprintf("%.3f", quantile)) %>%
  filter(q %in% c("0.025", "0.250", "0.500", "0.750", "0.975")) %>%
  select(location, forecast_date, target_end_date, horizon, model, q, value) %>%
  pivot_wider(names_from = q, values_from = value) %>%
  select(
    location, forecast_date, target_end_date, horizon, model,
    `0.025`, `0.250`, `0.500`, `0.750`, `0.975`
  )

US_forecast_combined_covid <- covid_wide %>%
  mutate(
    model = recode(
      model,
      "none_nonsym_w10" = "Drift",
      "none_sym_w10"    = "Flatline"
    )
  )

all_target_dates_covid <- sort(unique(combined_df_covid$target_end_date))

truth_data_covid <- load_truth(
  truth_source = "HealthData",
  target_variable = "inc hosp",
  locations = "US"
)


# Shared plot function
plot_forecast_location <- function(location_forecast, location_truth,
                                   y_label = "Admits",
                                   scale_type = "comma") {
  
  retro_fill <- c(
    "Drift"    = "#f4a261",
    "Flatline" = "#9e9ac8"
  )
  retro_color <- c(
    "Drift"    = "#ff7f00",
    "Flatline" = "#5e3c99"
  )
  retro_levels <- names(retro_color)
  
  y_scale <- switch(
    scale_type,
    "percent" = scale_y_continuous(labels = function(x) paste0(x, "%")),
    "comma"   = scale_y_continuous(labels = scales::comma),
    scale_y_continuous()
  )
  
  ggplot() +
    geom_ribbon(
      data = location_forecast,
      aes(
        x = target_end_date,
        ymin = `0.025`,
        ymax = `0.975`,
        fill = model,
        group = interaction(model, forecast_date)
      ),
      alpha = 0.3
    ) +
    geom_line(
      data = location_forecast,
      aes(
        x = target_end_date,
        y = `0.500`,
        color = model,
        group = interaction(model, forecast_date)
      ),
      linewidth = 0.6
    ) +
    geom_line(
      data = location_truth,
      aes(x = target_end_date, y = value),
      color = "black", linewidth = 0.5
    ) +
    labs(x = NULL, y = y_label, title = NULL) +
    scale_fill_manual(values = retro_fill, limits = retro_levels, drop = FALSE) +
    scale_color_manual(values = retro_color, limits = retro_levels, drop = FALSE) +
    y_scale +
    coord_cartesian(clip = "off") +
    cowplot::theme_cowplot() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      
      axis.line = element_line(color = "black", linewidth = 0.4),
      
      legend.position  = "none",
      plot.margin      = margin(10, 20, 10, 10)
    )
  
}

# COVID plot

retro_covid_plot <- NULL

for (curr_location_name in unique(US_forecast_combined_covid$location)) {
  
  location_forecast <- US_forecast_combined_covid %>%
    filter(location == curr_location_name) %>%
    complete(
      target_end_date = all_target_dates_covid,
      nesting(model),
      fill = list(
        `0.025` = NA_real_,
        `0.250` = NA_real_,
        `0.500` = NA_real_,
        `0.750` = NA_real_,
        `0.975` = NA_real_
      )
    )
  
  location_truth <- truth_data_covid %>%
    filter(
      location == curr_location_name,
      (year(target_end_date) > 2020 |
         (year(target_end_date) == 2020 & month(target_end_date) >= 11)),
      (year(target_end_date) < 2023 |
         (year(target_end_date) == 2023 & month(target_end_date) <= 10))
    ) %>%
    arrange(target_end_date) %>%
    slice(seq(1, n(), by = 7))
  
  if (nrow(location_forecast) > 0 && nrow(location_truth) > 0) {
    retro_covid_plot <- plot_forecast_location(
      location_forecast, location_truth,
      y_label = "COVID-19 admits",
      scale_type = "comma"
    ) +
      scale_x_date(date_breaks = "6 months", date_labels = "%b %Y")
  }
}

# FLU

dir_path <- "results/flu/forecasts"
prefix   <- "flu_baseline_forecasts_"

dates_selected <- as.Date(c(
  "2023-10-28",
  "2023-12-02",
  "2024-01-06",
  "2024-02-10",
  "2024-03-16",
  "2024-04-20",
  "2024-11-16",
  "2024-12-21",
  "2025-02-01",
  "2025-03-08",
  "2025-04-12",
  "2025-05-17"
))

files_selected <- file.path(
  dir_path,
  paste0(prefix, dates_selected, ".csv")
)

files_selected <- files_selected[file.exists(files_selected)]

combined_df_flu <- files_selected %>%
  lapply(function(f) {
    read_csv(f, show_col_types = FALSE) %>%
      filter(
        model %in% c("none_nonsym_w10", "none_sym_w10"),
        location == "US"
      )
  }) %>%
  bind_rows()

flu_wide <- combined_df_flu %>%
  filter(type == "quantile") %>%
  mutate(q = sprintf("%.3f", quantile)) %>%
  filter(q %in% c("0.025", "0.250", "0.500", "0.750", "0.975")) %>%
  select(location, forecast_date, target_end_date, horizon, model, q, value) %>%
  pivot_wider(names_from = q, values_from = value) %>%
  select(
    location, forecast_date, target_end_date, horizon, model,
    `0.025`, `0.250`, `0.500`, `0.750`, `0.975`
  )

US_forecast_combined_flu <- flu_wide %>%
  mutate(
    model = recode(
      model,
      "none_nonsym_w10" = "Drift",
      "none_sym_w10"    = "Flatline"
    )
  )

all_target_dates_flu <- sort(unique(combined_df_flu$target_end_date))

truth_data_flu <- read_csv("data/Influenza/archive/truth_flu.csv", show_col_types = FALSE) %>%
  rename(target_end_date = date)

retro_flu_plot <- NULL

for (curr_location_name in unique(US_forecast_combined_flu$location)) {
  
  location_forecast <- US_forecast_combined_flu %>%
    filter(location == curr_location_name) %>%
    complete(
      target_end_date = all_target_dates_flu,
      nesting(model),
      fill = list(
        `0.025` = NA_real_,
        `0.250` = NA_real_,
        `0.500` = NA_real_,
        `0.750` = NA_real_,
        `0.975` = NA_real_
      )
    )
  
  location_truth <- truth_data_flu %>%
    filter(
      location == curr_location_name,
      target_end_date >= min(all_target_dates_flu, na.rm = TRUE),
      target_end_date <= max(all_target_dates_flu, na.rm = TRUE),
      !(target_end_date >= as.Date("2024-07-01") &
          target_end_date <= as.Date("2024-10-31"))
    )
  
  if (nrow(location_forecast) > 0 && nrow(location_truth) > 0) {
    retro_flu_plot <- plot_forecast_location(
      location_forecast, location_truth,
      y_label = "Influenza admits",
      scale_type = "comma"
    ) +
      scale_x_date(date_breaks = "4 months", date_labels = "%b %Y")
  }
}

# RSV
rsv_path <- "results/rsv/retro_full_final_data_redo.csv"
rsv_df <- read_csv(rsv_path, show_col_types = FALSE)

forecast_dates_rsv <- rsv_df %>%
  distinct(forecast_date) %>%
  arrange(forecast_date) %>%
  pull(forecast_date)

forecast_dates_keep_rsv <- forecast_dates_rsv[seq(1, length(forecast_dates_rsv), by = 5)]

combined_df_rsv <- rsv_df %>%
  filter(forecast_date %in% forecast_dates_keep_rsv) %>%
  filter(
    model %in% c("none_nonsym_w10", "none_sym_w10"),
    location == "US"
  )

rsv_wide <- combined_df_rsv %>%
  filter(type == "quantile") %>%
  mutate(q = sprintf("%.3f", quantile)) %>%
  filter(q %in% c("0.025", "0.250", "0.500", "0.750", "0.975")) %>%
  select(location, forecast_date, target_end_date, horizon, model, q, value) %>%
  pivot_wider(names_from = q, values_from = value) %>%
  select(
    location, forecast_date, target_end_date, horizon, model,
    `0.025`, `0.250`, `0.500`, `0.750`, `0.975`
  )

US_forecast_combined_rsv <- rsv_wide %>%
  mutate(
    model = recode(
      model,
      "none_nonsym_w10" = "Drift",
      "none_sym_w10"    = "Flatline"
    )
  )

all_target_dates_rsv <- sort(unique(combined_df_rsv$target_end_date))

truth_data_rsv <- read_csv("data/rsv/truth_rsv.csv", show_col_types = FALSE) %>%
  rename(target_end_date = date) %>%
  filter(age_group == "0-130", target == "inc hosp", location == "US")

retro_rsv_plot <- NULL

for (curr_location_name in unique(US_forecast_combined_rsv$location)) {
  
  location_forecast <- US_forecast_combined_rsv %>%
    filter(location == curr_location_name) %>%
    complete(
      target_end_date = all_target_dates_rsv,
      nesting(model),
      fill = list(
        `0.025` = NA_real_,
        `0.250` = NA_real_,
        `0.500` = NA_real_,
        `0.750` = NA_real_,
        `0.975` = NA_real_
      )
    )
  
  location_truth <- truth_data_rsv %>%
    filter(
      location == curr_location_name,
      target_end_date >= min(all_target_dates_rsv, na.rm = TRUE),
      target_end_date <= max(all_target_dates_rsv, na.rm = TRUE)
    )
  
  if (nrow(location_forecast) > 0 && nrow(location_truth) > 0) {
    retro_rsv_plot <- plot_forecast_location(
      location_forecast, location_truth,
      y_label = "RSV admits",
      scale_type = "comma"
    ) +
      scale_x_date(date_breaks = "6 months", date_labels = "%b %Y")
  }
}

# ILI (wILI) — MODIFIED: show only last ~5 years and MATCH truth + forecast window

ili_path <- "results/ili/retro_full.csv"
ili_df <- read_csv(ili_path, show_col_types = FALSE)

forecast_dates_ili <- ili_df %>%
  distinct(forecast_date) %>%
  arrange(forecast_date) %>%
  pull(forecast_date)

forecast_dates_keep_ili <- forecast_dates_ili[seq(1, length(forecast_dates_ili), by = 10)]

combined_df_ili <- ili_df %>%
  filter(forecast_date %in% forecast_dates_keep_ili) %>%
  filter(
    model %in% c("none_nonsym_w10", "none_sym_w10"),
    location == "nat"
  )

ili_wide <- combined_df_ili %>%
  filter(type == "quantile") %>%
  mutate(q = sprintf("%.3f", quantile)) %>%
  filter(q %in% c("0.025", "0.250", "0.500", "0.750", "0.975")) %>%
  select(location, forecast_date, target_end_date, horizon, model, q, value) %>%
  pivot_wider(names_from = q, values_from = value) %>%
  select(
    location, forecast_date, target_end_date, horizon, model,
    `0.025`, `0.250`, `0.500`, `0.750`, `0.975`
  )

US_forecast_combined_ili <- ili_wide %>%
  mutate(
    model = recode(
      model,
      "none_nonsym_w10" = "Drift",
      "none_sym_w10"    = "Flatline"
    )
  )

all_target_dates_ili <- sort(unique(combined_df_ili$target_end_date))

truth_data_ili <- read_csv("data/ili/truth_ili.csv", show_col_types = FALSE)

retro_ili_plot <- NULL

for (curr_location_name in unique(US_forecast_combined_ili$location)) {
  
  # --- define shared 5-year window based on truth ---
  end_date_ili <- truth_data_ili %>%
    filter(location == curr_location_name) %>%
    summarise(end = max(target_end_date, na.rm = TRUE)) %>%
    pull(end)
  
  start_date_ili <- end_date_ili %m-% years(5)
  
  # --- forecast: complete then trim to the same window ---
  location_forecast <- US_forecast_combined_ili %>%
    filter(location == curr_location_name) %>%
    complete(
      target_end_date = all_target_dates_ili,
      nesting(model),
      fill = list(
        `0.025` = NA_real_,
        `0.250` = NA_real_,
        `0.500` = NA_real_,
        `0.750` = NA_real_,
        `0.975` = NA_real_
      )
    ) %>%
    filter(
      target_end_date >= start_date_ili,
      target_end_date <= end_date_ili
    )
  
  # --- truth: trim to the same window ---
  location_truth <- truth_data_ili %>%
    filter(
      location == curr_location_name,
      target_end_date >= start_date_ili,
      target_end_date <= end_date_ili
    )
  
  if (nrow(location_forecast) > 0 && nrow(location_truth) > 0) {
    retro_ili_plot <- plot_forecast_location(
      location_forecast, location_truth,
      y_label = "wILI (%)",
      scale_type = "percent"
    ) +
      scale_x_date(date_breaks = "12 months", date_labels = "%b %Y")
  }
}


# 4-panel figure with ABCD labels

four_panel <- plot_grid(
  retro_covid_plot, retro_flu_plot,
  retro_ili_plot,   retro_rsv_plot,
  rel_widths = c(0.6, 0.4),
  ncol = 2,
  labels = c("A", "B", "C", "D"),
  label_size = 16,
  label_fontface = "bold",
  align = "hv",
  axis = "tblr"
)

# ONE legend (RIGHT side)

manual_colors <- c(
  "Drift"    = "#ff7f00",
  "Flatline" = "#5e3c99"
)

legend_df <- tibble(
  variation = factor(
    rep(c("Drift", "Flatline"), each = 2),
    levels = c("Drift", "Flatline")
  ),
  x = rep(c(1, 2), times = 2),
  y = rep(c(1, 2), each = 2)
)

legend_plot <- ggplot(
  legend_df,
  aes(x = x, y = y, color = variation, group = variation)
) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(values = manual_colors) +
  guides(color = guide_legend(direction = "vertical")) +
  theme_void() +
  theme(
    legend.title    = element_blank(),
    legend.text     = element_text(size = 12, face = "bold"),
    legend.position = "right"
  )

legend_grob <- gtable::gtable_filter(ggplotGrob(legend_plot), "guide-box")

final <- plot_grid(
  four_panel,
  legend_grob,
  ncol = 2,
  rel_widths = c(1, 0.075)
)

# Print final
final

# Save
ggsave(
  filename = "plots/paper/main_summary_2.png",
  plot = final,
  dpi = 600,
  width = 18,
  height = 7,
  units = "in",
  bg = "white"
)
