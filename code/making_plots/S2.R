b_df_covid
b_df_flu
b_df_ili
b_df_rsv

avg_all_fcast <- bind_rows(
  b_df_ili %>% mutate(dataset = "wILI"),
  b_df_covid %>% mutate(dataset = "COVID-19"),
  b_df_flu %>% mutate(dataset = "Influenza"),
  b_df_rsv %>% mutate(dataset = "RSV")
) 






####
avg_all_fcast_flat_drift <- avg_all_fcast%>%
  filter(variation %in% c("Flatline", "Drift"))

avg_all_fcast_flat_drift %>%
  group_by(dataset) %>%
  summarise(
    n_forecast_dates = n_distinct(forecast_date))
avg_all_fcast_flat_drift_epi <- avg_all_fcast_flat_drift %>%
  mutate(
    epiweek = MMWRweek(forecast_date)$MMWRweek)


library(dplyr)
library(MMWRweek)

library(dplyr)
library(ggplot2)
library(cowplot)
library(MMWRweek)

# -----------------------------
# Prep data: epiweek + filtering
# -----------------------------
df_epi <- avg_all_fcast_flat_drift %>%
  mutate(epiweek = MMWRweek(forecast_date)$MMWRweek)

epi_levels_custom <- c(40:53, 1:20)

# COVID keeps all weeks; others restricted
df_covid <- df_epi %>%
  filter(dataset == "COVID-19") %>%
  mutate(epiweek_f = factor(epiweek, levels = sort(unique(epiweek))))

df_other <- df_epi %>%
  filter(dataset %in% c("Influenza", "RSV", "wILI"),
         epiweek %in% epi_levels_custom) %>%
  mutate(epiweek_f = factor(epiweek, levels = epi_levels_custom))

# -----------------------------
# Panel function (boxplot)
# -----------------------------
library(ggplot2)
library(cowplot)

make_box_panel <- function(df, title, show_legend = FALSE) {
  ggplot(df, aes(x = epiweek_f, y = mean_wis, color = variation)) +
    geom_boxplot(
      outlier.shape = NA,
      alpha = 0.25,
      position = position_dodge(width = 0.8)
    ) +
    scale_color_manual(values = manual_colors) +
    labs(x = NULL, y = NULL, title = title) +   # ← remove axis titles
    theme_cowplot() +
    theme(
      legend.position = if (show_legend) "right" else "none",
      plot.title = element_text(face = "bold", hjust = 0.5),
      axis.text.x = element_text(size = 7)
    )
}



# -----------------------------
# Build 4 panels
# -----------------------------
p_covid <- make_box_panel(df_covid, "COVID-19", show_legend = TRUE)
p_flu   <- make_box_panel(df_other %>% filter(dataset == "Influenza"), "Influenza")
p_rsv   <- make_box_panel(df_other %>% filter(dataset == "RSV"), "RSV")
p_wili  <- make_box_panel(df_other %>% filter(dataset == "wILI"), "wILI")

leg <- get_legend(p_covid)
p_covid_noleg <- p_covid + theme(legend.position = "none")

stack <- plot_grid(
  p_covid_noleg,
  p_flu,
  p_rsv,
  p_wili,
  ncol = 1,
  align = "v",
  axis = "lr",
  rel_heights = c(1.2, 1, 1, 1)  # COVID slightly taller (optional)
)

final_plot <- ggdraw() +
  draw_plot(stack, x = 0.06, y = 0.05, width = 0.78, height = 0.9) +
  draw_plot(leg,   x = 0.88, y = 0.25, width = 0.12, height = 0.5) +
  draw_label(
    "WIS",
    x = 0.02, y = 0.5,
    angle = 90,
    fontface = "bold"
  ) +
  draw_label(
    "epiweek",
    x = 0.47, y = 0.02,
    fontface = "bold"
  )

final_plot

# Save
ggsave("plots/paper/fcast_wis.png", 
       plot = final_plot, height = 8, width = 10,
       units = "in", dpi = 400, bg = "white")
