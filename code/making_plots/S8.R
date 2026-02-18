library(dplyr)
library(ggplot2)
library(cowplot)
library(lubridate)
library(grid)   # <-- IMPORTANT for unit()

# -------------------------
# Settings
# -------------------------
loc_order <- c("nat", paste0("hhs", 1:10))
truth_start_date <- as.Date("2014-09-01")

pretty_loc <- function(x) {
  ifelse(x == "nat", "National", paste0("Region", gsub("hhs", "", x)))
}

# Oct–May only
season_oct_may <- function(d) {
  case_when(
    month(d) >= 10 ~ TRUE,
    month(d) <=  5 ~ TRUE,
    TRUE ~ FALSE
  )
}

# -------------------------
# 1) Bars: improvement by forecast_date (Oct–May only) + CAP at ±100
# -------------------------
bar_df <- improv_df %>%
  mutate(
    improvement_cap = pmax(pmin(improvement_pct, 100), -100),
    location = factor(location, levels = loc_order),
    loc_label = factor(pretty_loc(as.character(location)), levels = pretty_loc(loc_order))
  ) %>%
  filter(season_oct_may(forecast_date)) %>%
  arrange(location, forecast_date)

# -------------------------
# 2) Truth: wILI by target_end_date (Oct–May only, start Sept 2014)
# -------------------------
truth_df <- truth_ili %>%
  filter(
    target_variable == "inc wili",
    target_end_date >= truth_start_date
  ) %>%
  mutate(
    location = factor(location, levels = loc_order),
    loc_label = factor(pretty_loc(as.character(location)), levels = pretty_loc(loc_order))
  ) %>%
  filter(season_oct_may(target_end_date)) %>%
  select(loc_label, date = target_end_date, truth = value)

# -------------------------
# 3) Scale truth per location (robust 5–95% scaling to [0, 100])
# -------------------------
truth_scaled <- truth_df %>%
  group_by(loc_label) %>%
  mutate(
    lo = quantile(truth, 0.05, na.rm = TRUE),
    hi = quantile(truth, 0.95, na.rm = TRUE),
    truth_scaled = 100 * (truth - lo) / (hi - lo)
  ) %>%
  ungroup()

# -------------------------
# 4) Plot: capped bars + truth overlay + gradient legend (NO TEXT)
# -------------------------
p <- ggplot() +
  geom_col(
    data = bar_df,
    aes(x = forecast_date, y = improvement_cap, fill = improvement_cap),
    width = 6
  ) +
  geom_hline(yintercept = 0, linewidth = 0.9, color = "black") +
  geom_line(
    data = truth_scaled,
    aes(x = date, y = truth_scaled, group = loc_label),
    color = "black",
    linewidth = 0.25
  ) +
  facet_wrap(~ loc_label, ncol = 3) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(-110, 110)) +
  scale_fill_gradient2(
    low = "red", mid = "white", high = "blue",
    midpoint = 0, limits = c(-100, 100),
    name = NULL,
    guide = "colourbar"   # <-- force colorbar guide
  ) +
  guides(
    fill = guide_colorbar(
      title = NULL,
      barheight = unit(45, "mm"),  # <-- force visible size
      barwidth  = unit(5,  "mm"),
      frame.colour = "black"
    )
  ) +
  labs(x = NULL, y = "% improvement") +
  theme_cowplot() +
  theme(
    strip.text = element_text(face = "bold", size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = 9),
    
    # legend ON, but no text/ticks
    legend.position = "right",
    legend.title = element_blank(),
    legend.text  = element_blank(),
    legend.ticks = element_blank()
  )

p

ggsave(
  filename = "plots/paper/seasonal_vs_flatline_supp.png",
  plot = p,
  dpi = 600,
  width = 10,
  height = 8,
  units = "in",
  bg = "white"
)
