c_df_covid
c_df_flu
c_df_ili
c_df_rsv

library(dplyr)

c_df_covid <- c_df_covid %>%
  mutate(
    horizon = recode(
      horizon,
      `7`  = 1,
      `14` = 2,
      `21` = 3,
      `28` = 4
    )
  )

avg_all_horizon <- bind_rows(
  c_df_ili %>% mutate(dataset = "wILI"),
  c_df_covid %>% mutate(dataset = "COVID-19"),
  c_df_flu %>% mutate(dataset = "Influenza"),
  c_df_rsv %>% mutate(dataset = "RSV")
) 

avg_all_horizon <- avg_all_horizon %>%
  mutate(horizon = factor(horizon))

manual_colors <- c(
  "Drift (transformed)"    = "#ff7f00",
  "Drift"                  = "#ff7f00",
  "Flatline"               = "#5e3c99",
  "Flatline (transformed)" = "#5e3c99"
)

manual_linetypes <- c(
  "Drift (transformed)"    = "solid",
  "Flatline"               = "dotted",
  "Drift"                  = "dotted",
  "Flatline (transformed)" = "solid"
)


p_wis_horizon <- ggplot(
  avg_all_horizon,
  aes(
    x = horizon,
    y = mean_wis,
    color = variation,
    linetype = variation,   # ← ADD THIS
    group = variation
  )
) +
  geom_line(linewidth = 0.6) +
  geom_point(size = 1.8) +
  facet_wrap(~ dataset, ncol = 2, scales = "free_y") +
  scale_color_manual(values = manual_colors) +
  scale_linetype_manual(values = manual_linetypes) +   # ← ADD THIS
  labs(x = "Horizon", y = "WIS") +
  theme_cowplot() +
  theme(
    legend.position = "right",
    strip.text = element_text(face = "bold"),
    panel.grid = element_blank()
  )

p_wis_horizon

# Save
ggsave(
  "plots/paper/horizon_wis.png",
  plot = p_wis_horizon,
  height = 6,
  width = 9,
  units = "in",
  dpi = 400,
  bg = "white"
)
