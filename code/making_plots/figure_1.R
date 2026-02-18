
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(cowplot)
library(lubridate)

# Load forecasts (single file)

covid_baseline_forecasts <- read_csv(
  "results/covid/forecasts/covid_baseline_forecasts_2022-01-10.csv",
  show_col_types = FALSE
)

models_keep <- c(
  "none_sym_w10",
  "sqrt_sym_w10",
  "none_nonsym_w10",
  "sqrt_nonsym_w10",
  "none_sym_w_all",
  "sqrt_sym_w_all",
  "none_nonsym_w_all",
  "sqrt_nonsym_w_all"
)

covid_fc_us <- covid_baseline_forecasts %>%
  filter(model %in% models_keep, location == "US") %>%
  mutate(
    target_end_date = as.Date(target_end_date),
    forecast_date   = as.Date(forecast_date)
  )


# Load truth (daily) and keep US

truth_covid_daily <- read_csv("data/covid/archive/truth_covid_daily.csv", show_col_types = FALSE)

truth_covid_daily_us <- truth_covid_daily %>%
  rename(target_end_date = time_value) %>%
  filter(location == "US") %>%
  mutate(target_end_date = as.Date(target_end_date)) %>%
  filter(!is.na(value)) %>%
  arrange(target_end_date)


# Choose which forecast_date to plot

plot_forecast_date <- as.Date("2022-01-09")


# Align truth to weekly (Saturday anchor) up to anchor_date

anchor_date <- as.Date("2022-01-08")

truth_covid_us_weekly <- truth_covid_daily_us %>%
  filter(
    target_end_date <= anchor_date,
    (as.integer(anchor_date - target_end_date) %% 7) == 0
  )

truth_plot <- truth_covid_us_weekly %>%
  filter(target_end_date < plot_forecast_date) %>%
  arrange(target_end_date)

truth_fit_w10 <- truth_plot %>% slice_tail(n = 10)
truth_fit_all <- truth_plot

min_plot_date <- min(truth_plot$target_end_date, na.rm = TRUE)

y_min <- min(
  truth_plot$value,
  covid_fc_us %>% filter(forecast_date == plot_forecast_date) %>% pull(value),
  na.rm = TRUE
)

y_max <- max(
  truth_plot$value,
  covid_fc_us %>% filter(forecast_date == plot_forecast_date) %>% pull(value),
  na.rm = TRUE
)

# optional: make the top a nice round number
y_max <- ceiling(y_max / 1000) * 1000

# consistent breaks in every panel
y_breaks <- pretty(c(y_min, y_max), n = 6)


model_colors <- c("Flatline" = "#5e3c99", "Drift" = "#b35806")
model_fills  <- c("Flatline" = "#b2abd2", "Drift" = "#f1a340")

base_theme <- cowplot::theme_cowplot() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    legend.position = "right",
    legend.title    = element_blank(),
    legend.text     = element_text(face = "bold", size = 10),
    
    legend.background     = element_blank(),
    legend.key            = element_blank(),
    legend.box.background = element_blank(),
    
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 11),
    axis.text.x  = element_text(size = 9),
    axis.text.y  = element_text(size = 9),
    plot.title   = element_text(hjust = 0.5, face = "bold", size = 12),
    plot.tag     = element_text(face = "bold", size = 12)
  )


make_panel <- function(forecasts, truth, fit_truth, title_text, show_y = TRUE) {
  
  df_wide <- forecasts %>%
    filter(type == "quantile", quantile %in% c(0.025, 0.5, 0.975)) %>%
    mutate(q = paste0("q", quantile)) %>%
    select(target_end_date, model, q, value) %>%
    pivot_wider(names_from = q, values_from = value)
  
  p <- ggplot(df_wide, aes(x = target_end_date, y = q0.5, color = model, fill = model)) +
    
    geom_ribbon(
      aes(ymin = q0.025, ymax = q0.975),
      alpha = 0.45,
      color = NA,
      show.legend = FALSE
    ) +
    
    geom_line(linewidth = 0.8) +
    
    geom_line(
      data = truth,
      aes(x = target_end_date, y = value, group = 1),
      inherit.aes = FALSE,
      color = "black",
      linewidth = 0.35
    ) +
    
    geom_line(
      data = fit_truth,
      aes(x = target_end_date, y = value, group = 1),
      inherit.aes = FALSE,
      color = "blue",
      linewidth = 0.5
    ) +
    
    scale_color_manual(values = model_colors, breaks = c("Flatline", "Drift")) +
    scale_fill_manual(values  = model_fills,  guide = "none") +
    
    # SHARED y-axis
    scale_y_continuous(
      limits = c(y_min, y_max),
      breaks = y_breaks,
      expand = expansion(mult = c(0, 0.05))
    ) +
    
    labs(
      title = title_text,
      y = if (show_y) "Hospital admits" else NULL
    ) +
    base_theme
  
  if (!show_y) p <- p + theme(axis.title.y = element_blank())
  p
}


# Helper: subset forecasts for a panel + map to Drift/Flatline

prep_panel_fc <- function(df, keep_models) {
  df %>%
    filter(
      forecast_date == plot_forecast_date,
      model %in% keep_models,
      target_end_date >= min_plot_date
    ) %>%
    mutate(model = ifelse(grepl("nonsym", model), "Drift", "Flatline"))
}

# Build panels

pA <- prep_panel_fc(covid_fc_us, c("none_sym_w10", "none_nonsym_w10")) %>%
  make_panel(truth_plot, truth_fit_w10,
             title_text = expression(italic(w) == 10),
             show_y = TRUE)

pB <- prep_panel_fc(covid_fc_us, c("sqrt_sym_w10", "sqrt_nonsym_w10")) %>%
  make_panel(truth_plot, truth_fit_w10,
             title_text = expression(italic(w) == 10 ~ "(Transformed)"),
             show_y = TRUE)

pC <- prep_panel_fc(covid_fc_us, c("none_sym_w_all", "none_nonsym_w_all")) %>%
  make_panel(truth_plot, truth_fit_all,
             title_text = expression(italic(w) == "All"),
             show_y = FALSE)

pD <- prep_panel_fc(covid_fc_us, c("sqrt_sym_w_all", "sqrt_nonsym_w_all")) %>%
  make_panel(truth_plot, truth_fit_all,
             title_text = expression(italic(w) == "All" ~ "(Transformed)"),
             show_y = FALSE)


# Assemble with ONE legend on the right (cowplot, stable)

legend_grob <- cowplot::get_legend(pA + theme(legend.position = "right"))

pA_noleg <- pA + theme(legend.position = "none")
pB_noleg <- pB + theme(legend.position = "none")
pC_noleg <- pC + theme(legend.position = "none")
pD_noleg <- pD + theme(legend.position = "none")

four_panel <- cowplot::plot_grid(
  pA_noleg, pB_noleg,
  pC_noleg, pD_noleg,
  ncol = 2,
  labels = c("A", "B", "C", "D"),
  label_size = 12,
  label_fontface = "bold",
  align = "hv",
  axis = "tblr"
)

final_plot <- cowplot::plot_grid(
  four_panel,
  legend_grob,
  nrow = 1,
  rel_widths = c(1, 0.18)
)

final_plot


# Save

ggsave(
  "plots/paper/conceptual.png",
  plot   = final_plot,
  height = 6,
  width  = 9,
  units  = "in",
  dpi    = 400,
  bg     = "white"
)
