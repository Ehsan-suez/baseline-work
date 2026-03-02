library(ggplot2)
library(dplyr)
library(cowplot)
library(scales)

# =========================
# Example data (unchanged)
# =========================
df <- data.frame(
  rel_improve = rep(c(0, 10, 20, 30), 3),
  pct_models  = c(
    18/27, 14/27, 12/27, 3/27,     # COVIDhub
    8/22,  1/22,  1/22, 0.0001,    # flusight 21-22
    11/18, 11/18,  3/18, 1/18      # flusight 22-23
  ),
  hub = rep(c("COVIDhub", "FluSight 2021-2022", "FluSight 2022-2023"), each = 4)
)

custom_colors <- c(
  "COVIDhub"           = "#d8b365",
  "FluSight 2022-2023" = "#8c510a",
  "FluSight 2021-2022" = "#543005"
)

# helper for x tick labels with %
x_pct_labels <- function(x) paste0(x, "%")

# =========================
# Panel A
# =========================
plot_hub_compares <- ggplot(df, aes(x = rel_improve, y = pct_models, fill = hub)) +
  geom_col(position = "dodge", width = 7.5) +
  scale_y_continuous(labels = percent, limits = c(0, 0.75), breaks = seq(0, 0.75, 0.1)) +
  scale_x_continuous(
    breaks = c(0, 10, 20, 30),
    labels = x_pct_labels
  ) +
  scale_fill_manual(values = custom_colors, name = "Hub") +
  labs(
    x = "Baseline Improvement",     # <- no % in label text
    y = "Models Beating Baseline"
  ) +
  theme_minimal(base_size = 14) +
  theme_cowplot() +
  theme(
    axis.text    = element_text(face = "plain"),
    axis.title.x = element_text(size = 12, face = "plain"),
    axis.title.y = element_text(size = 12, face = "plain"),
    legend.title = element_blank(),
    legend.text  = element_text(size = 12, face = "bold"),
    legend.position = "right"
  )

# =========================
# Panel B
# =========================
rank_df <- data.frame(
  rel_improve = rep(c(0, 10, 20, 30), 3),
  hub = rep(c("COVIDhub", "FluSight 2021-2022", "FluSight 2022-2023"), each = 4),
  rank = c(
    19, 15, 13, 4,
    9,  2,  2,  1,
    12, 12, 4,  2
  ),
  total_models = c(
    rep(28, 4),
    rep(23, 4),
    rep(19, 4)
  )
)

plot_rank_shift <- ggplot(rank_df, aes(x = rel_improve, y = rank, color = hub, group = hub)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_y_reverse(breaks = seq(1, 28, 2)) +
  scale_x_continuous(
    breaks = c(0, 10, 20, 30),
    labels = x_pct_labels
  ) +
  scale_color_manual(values = custom_colors) +
  labs(
    x = "Baseline Improvement",     # <- no % in label text
    y = "Rank among Models"
  ) +
  theme_minimal(base_size = 14) +
  theme_cowplot() +
  theme(
    axis.text    = element_text(face = "plain"),
    axis.title.x = element_text(size = 12, face = "plain"),
    axis.title.y = element_text(size = 12, face = "plain"),
    legend.position = "none"
  )

# =========================
# Shared legend on RIGHT
# =========================
shared_legend <- get_legend(
  plot_hub_compares +
    theme(
      legend.position = "right",
      legend.direction = "vertical",
      legend.justification = "center",
      legend.text  = element_text(size = 12, face = "plain"),
      legend.title = element_blank()
    )
)

plot_hub_compares_nolegend <- plot_hub_compares + theme(legend.position = "none")
plot_rank_shift_nolegend   <- plot_rank_shift   + theme(legend.position = "none")

panel_plots <- plot_grid(
  plot_hub_compares_nolegend + labs(tag = "A"),
  plot_rank_shift_nolegend   + labs(tag = "B"),
  nrow = 1,
  rel_widths = c(1.5, 1.1)
)

# Put legend to the RIGHT of both panels
final_plot <- plot_grid(
  panel_plots,
  shared_legend,
  nrow = 1,
  rel_widths = c(1, 0.22)  # adjust legend width
)

final_plot

ggsave("plots/paper/ranking.png", 
       plot =  final_plot, 
       height = 4, 
       width = 10, 
       units = "in", 
       bg = "white", 
       dpi = 600)
