

suppressPackageStartupMessages({
  library(tidyverse)
  library(MMWRweek)
  library(cowplot)
})


avg_all_fcast <- bind_rows(
  b_df_ili   %>% mutate(dataset = "wILI"),
  b_df_covid %>% mutate(dataset = "COVID-19"),
  b_df_flu   %>% mutate(dataset = "Influenza"),
  b_df_rsv   %>% mutate(dataset = "RSV")
)

df_epi <- avg_all_fcast %>%
  filter(variation %in% c("Flatline", "Drift")) %>%
  mutate(
    forecast_date = as.Date(forecast_date),
    epiweek = MMWRweek(forecast_date)$MMWRweek
  )


df_rw <- df_epi %>%
  select(dataset, forecast_date, epiweek, variation, mean_wis) %>%
  pivot_wider(names_from = variation, values_from = mean_wis) %>%
  mutate(
    rWIS = Flatline / Drift
  ) %>%
  filter(is.finite(rWIS), rWIS >= 0)


epi_levels_custom <- c(40:53, 1:20)

df_covid <- df_rw %>%
  filter(dataset == "COVID-19") %>%
  mutate(epiweek_f = factor(epiweek, levels = sort(unique(epiweek))))

df_other <- df_rw %>%
  filter(
    dataset %in% c("Influenza", "RSV", "wILI"),
    epiweek %in% epi_levels_custom
  ) %>%
  mutate(epiweek_f = factor(epiweek, levels = epi_levels_custom))


every_2nd_level <- function(f) {
  lv <- levels(f)
  lv[seq(1, length(lv), by = 2)]
}


make_box_panel_rw <- function(df, title) {
  
  ggplot(df, aes(x = epiweek_f, y = rWIS)) +
    
    geom_boxplot(
      outlier.shape = NA,
      alpha = 0.35,
      linewidth = 0.5,
      width = 0.7
    ) +
    
    geom_hline(
      yintercept = 1,
      linetype = "dashed",
      linewidth = 0.4
    ) +
    
    scale_x_discrete(
      breaks = every_2nd_level(df$epiweek_f)
    ) +
    
    # Linear scale with 0,1,2
    scale_y_continuous(
      breaks = c(0, 1, 2),
      limits = c(0, 2)
    ) +
    
    labs(
      x = NULL,
      y = NULL,
      title = title
    ) +
    
    theme_cowplot() +
    theme(
      plot.title  = element_text(face = "bold", hjust = 0.5),
      axis.text.x = element_text(size = 8),
      panel.grid.major.y = element_line(color = "grey85", linewidth = 0.3),
      panel.grid.minor = element_blank()
    )
}


p_covid <- make_box_panel_rw(df_covid, "COVID-19")

p_flu <- make_box_panel_rw(
  df_other %>% filter(dataset == "Influenza"),
  "Influenza"
)

p_rsv <- make_box_panel_rw(
  df_other %>% filter(dataset == "RSV"),
  "RSV"
)

p_wili <- make_box_panel_rw(
  df_other %>% filter(dataset == "wILI"),
  "wILI"
)


stack <- plot_grid(
  p_covid,
  p_flu,
  p_rsv,
  p_wili,
  ncol = 1,
  align = "v",
  axis = "lr"
)


final_plot <- ggdraw() +
  
  draw_plot(
    stack,
    x = 0.08,
    y = 0.06,
    width = 0.90,
    height = 0.90
  ) +
  
  draw_label(
    "rWIS",
    x = 0.02,
    y = 0.5,
    angle = 90,
    fontface = "bold"
  ) +
  
  draw_label(
    "epiweek",
    x = 0.52,
    y = 0.02,
    fontface = "bold"
  )

final_plot


ggsave(
  "plots/paper/fcast_wis.png",
  plot = final_plot,
  height = 11,
  width = 8.5,
  dpi = 800,
  bg = "white"
)
