# ============================================================
# Full script: 2x2 PIC plots with ONE legend on right-middle,
# plus global axis labels and A–D panel tags
# ============================================================

a_df_covid
a_df_flu
a_df_ili
a_df_rsv

library(tidyverse)
library(cowplot)
library(ggplot2)
library(dplyr)
library(tidyr)
library(grid)
library(patchwork)
library(gtable)

# =========================
# Shared theme bundle
# =========================
theme_text_aes <- list(
  theme_minimal(base_size = 14),
  theme_cowplot(),
  theme(
    text         = element_text(face = "plain"),
    axis.text    = element_text(face = "plain"),
    axis.title.x = element_text(face = "plain"),
    axis.title.y = element_text(
      face   = "plain",
      margin = margin(r = 15)   # equal spacing for all y-axis titles
    ),
    legend.title = element_blank()
  )
)

# =========================
# Build combined summary data
# =========================
avg_all <- bind_rows(
  a_df_ili   %>% mutate(dataset = "wILI"),
  a_df_covid %>% mutate(dataset = "COVID-19"),
  a_df_flu   %>% mutate(dataset = "Influenza"),
  a_df_rsv   %>% mutate(dataset = "RSV")
)

# =========================
# Manual color mapping
# =========================
manual_colors <- c(
  "Drift (transformed)"    = "#ff7f00",
  "Drift"                  = "#ff7f00",
  "Flatline"               = "#5e3c99",
  "Flatline (transformed)" = "#5e3c99"
)

manual_linetypes <- c(
  "Drift (transformed)"    = "solid",
  "Drift"                  = "dotted",
  "Flatline"               = "dotted",
  "Flatline (transformed)" = "solid"
)


avg_all_plot <- avg_all %>%
  mutate(variation = factor(variation, levels = names(manual_colors)))

# =========================
# Plot function
# =========================
make_summary_plot <- function(df, dataset_name, show_title = FALSE) {
  
  # safety check: variations must match color map
  missing <- setdiff(unique(as.character(df$variation)), names(manual_colors))
  if (length(missing)) stop("Unknown variation(s): ", paste(missing, collapse = ", "))
  
  # detect "all" and its plotted x-position
  has_all <- any(tolower(trimws(as.character(df$data))) == "all")
  x_all   <- if (has_all) max(df$data_num, na.rm = TRUE) else NA_real_
  x_max   <- max(df$data_num, na.rm = TRUE)
  
  # custom breaks for COVID; simpler breaks for others
  x_breaks <- if (dataset_name == "COVID-19") {
    brks <- c(
      seq(10, 100, by = 30),
      seq(130, x_max, by = 50),
      x_all
    )
    sort(unique(brks[!is.na(brks) & brks <= x_max]))
  } else {
    brks <- c(0, 5, 10, 20, 30, 40, 50, x_all)
    sort(unique(brks[!is.na(brks) & brks <= x_max]))
  }
  
  ggplot(df, aes(
    x = data_num, y = mean_cov50,
    group = variation,
    color = variation,
    linetype = variation
  )) +
    geom_line(linewidth = 0.5) +
    geom_vline(xintercept = 10, linetype = "dashed", color = "black") +
    geom_hline(yintercept = 0.5, linetype = "dotted", color = "grey40") +
    scale_color_manual(values = manual_colors) +
    scale_linetype_manual(values = manual_linetypes) +
    
    labs(
      x = NULL,
      y = NULL,
      title = if (show_title) dataset_name else NULL
    ) +
    scale_y_continuous(
      breaks = 0.5,
      labels = "0.5",
      limits = c(0, 1)
    ) +
    scale_x_continuous(
      breaks = x_breaks,
      labels = function(x) {
        if (has_all) ifelse(x == x_all, "all", as.character(x)) else as.character(x)
      }
    ) +
    coord_cartesian(clip = "off") +
    theme_text_aes +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.title       = element_text(hjust = 0.5, face = "bold", size = 14),
      legend.position  = "none",                    # IMPORTANT: remove per-panel legends
      plot.margin      = margin(15, 20, 15, 15)
    )
}

# =========================
# Generate summary plots (NO legends inside)
# =========================
plot_covid <- avg_all_plot %>% filter(dataset == "COVID-19")  %>% make_summary_plot("COVID-19",  show_title = TRUE)
plot_flu   <- avg_all_plot %>% filter(dataset == "Influenza") %>% make_summary_plot("Influenza", show_title = TRUE)
plot_wili  <- avg_all_plot %>% filter(dataset == "wILI")      %>% make_summary_plot("wILI",      show_title = TRUE)
plot_rsv   <- avg_all_plot %>% filter(dataset == "RSV")       %>% make_summary_plot("RSV",       show_title = TRUE)

# =========================
# 2x2 layout (cowplot)
# =========================
two_by_two <- plot_grid(
  plot_covid, plot_flu,
  plot_wili,  plot_rsv,
  ncol = 2,
  align = "hv",
  axis = "tblr"
)

# =========================
# Custom legend (as a grob), vertical, RIGHT
# =========================
legend_df <- tibble(
  variation = factor(
    rep(names(manual_colors), each = 2),
    levels = names(manual_colors)
  ),
  x = rep(c(1, 2), times = length(manual_colors)),
  y = rep(seq_along(manual_colors), each = 2)
)

legend_plot <- ggplot(
  legend_df,
  aes(
    x = x, y = y,
    color = variation,
    linetype = variation,
    group = variation
  )
) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(values = manual_colors) +
  scale_linetype_manual(values = manual_linetypes) +
  guides(
    # keep ONLY ONE legend (color), but force it to display the correct linetypes
    color = guide_legend(
      ncol = 1,
      override.aes = list(
        linetype  = unname(manual_linetypes[names(manual_colors)]),
        linewidth = 1.2
      )
    ),
    linetype = "none"
  ) +
  theme_void() +
  theme(
    legend.title    = element_blank(),
    legend.text     = element_text(size = 12, face = "bold"),
    legend.position = "right"
  )

legend_grob <- gtable::gtable_filter(ggplotGrob(legend_plot), "guide-box")


# =========================
# Add A–D tags to ONLY the 2x2 panel (so legend column doesn't affect coordinates)
# If you want A C / B D ordering, set label = c("A","C","B","D")
# =========================
two_by_two_tagged <- ggdraw(two_by_two) +
  draw_plot_label(
    label = c("A", "B", "C", "D"),
    x = c(0.02, 0.52, 0.02, 0.52),
    y = c(0.98, 0.98, 0.49, 0.49),
    hjust = 0, vjust = 1,
    fontface = "bold", size = 12
  )

# =========================
# Combine 2x2 + legend (RIGHT-MIDDLE)
# =========================
final_with_legend <- plot_grid(
  two_by_two_tagged,
  plot_grid(
    NULL,
    legend_grob,
    NULL,
    ncol = 1,
    rel_heights = c(0.35, 0.30, 0.35)   # centers legend vertically
  ),
  ncol = 2,
  rel_widths = c(1, 0.22),
  align = "hv"
)

# =========================
# Add ONE global axis labels
final <- ggdraw(final_with_legend) +
  # Global Y label
  draw_label(
    "PIC",
    x = 0.012, y = 0.55,
    angle = 90,
    fontface = "bold", size = 14
  ) +
  # Global X label (centered under the 2x2 panel, not legend)
  draw_label(
    "Data Points",
    x = 0.42,    # <- centers under plots, avoids legend squeeze
    y = 0.015,
    fontface = "bold", size = 14
  )

final




# Save
ggsave("plots/paper/supp_summary_50_cov.png", 
       plot = final, height = 6, width = 12,
       units = "in", dpi = 400, bg = "white")


make_summary_plot <- function(df, dataset_name, show_title = FALSE) {
  
  # safety check: variations must match color map
  missing <- setdiff(unique(as.character(df$variation)), names(manual_colors))
  if (length(missing)) stop("Unknown variation(s): ", paste(missing, collapse = ", "))
  
  # detect "all" and its plotted x-position
  has_all <- any(tolower(trimws(as.character(df$data))) == "all")
  x_all   <- if (has_all) max(df$data_num, na.rm = TRUE) else NA_real_
  x_max   <- max(df$data_num, na.rm = TRUE)
  
  # y label formatting
  y_format <- if (dataset_name == "wILI") {
    scales::label_number(accuracy = 0.01)
  } else {
    scales::label_number(accuracy = 1)
  }
  
  x_breaks <- if (dataset_name == "COVID-19") {
    brks <- c(
      seq(10, 100, by = 30),
      seq(130, x_max, by = 50),
      x_all
    )
    sort(unique(brks[!is.na(brks) & brks <= x_max]))
  } else {
    brks <- c(0, 5, 10, 20, 30, 40, 50, x_all)
    sort(unique(brks[!is.na(brks) & brks <= x_max]))
  }
  
  ggplot(df, aes(x = data_num, y = mean_cov95, group = variation, color = variation,linetype = variation)) +
    geom_line(linewidth = 0.5) +
    geom_vline(xintercept = 10, linetype = "dashed", color = "black") +
    geom_hline(yintercept = 0.95, linetype = "dotted", color = "grey40") +
    scale_color_manual(values = manual_colors) +
    scale_linetype_manual(values = manual_linetypes) +
    labs(
      x = NULL,
      y = NULL,
      title = if (show_title) dataset_name else NULL
    ) +
    scale_y_continuous(
      breaks = 0.95,
      labels = "0.95",
      limits = c(0, 1)
    ) +
    scale_x_continuous(
      breaks = x_breaks,
      labels = function(x) {
        if (has_all) ifelse(x == x_all, "all", as.character(x)) else as.character(x)
      }
    ) +
    coord_cartesian(clip = "off") +
    theme_text_aes +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.title       = element_text(hjust = 0.5, face = "bold", size = 14),
      legend.position  = "bottom",
      plot.margin      = margin(10, 20, 10, 10)
    )
}

# =========================
# Generate summary plots
# (legend will be extracted from ONE of them, then removed)
# =========================
plot_covid <- avg_all_plot %>% filter(dataset == "COVID-19")  %>% make_summary_plot("COVID-19",  show_title = TRUE)
plot_flu <- avg_all_plot %>% filter(dataset == "Influenza")  %>% make_summary_plot("Influenza",  show_title = TRUE)
plot_wili <- avg_all_plot %>% filter(dataset == "wILI")  %>% make_summary_plot("wILI",  show_title = TRUE)
plot_rsv <- avg_all_plot %>% filter(dataset == "RSV")  %>% make_summary_plot("RSV",  show_title = TRUE)

plot_covid  <- plot_covid  + theme(legend.position = "none")
plot_flu  <- plot_flu  + theme(legend.position = "none")
plot_wili <- plot_wili + theme(legend.position = "none")
plot_rsv  <- plot_rsv  + theme(legend.position = "none")


# =========================
# 2x2 layout (cowplot)
# =========================
two_by_two <- plot_grid(
  plot_covid, plot_flu,
  plot_wili,  plot_rsv,
  ncol = 2,
  align = "hv",
  axis = "tblr"
)

# =========================
# Custom legend (as a grob), vertical, RIGHT
# =========================
legend_df <- tibble(
  variation = factor(
    rep(names(manual_colors), each = 2),
    levels = names(manual_colors)
  ),
  x = rep(c(1, 2), times = length(manual_colors)),
  y = rep(seq_along(manual_colors), each = 2)
)

legend_plot <- ggplot(
  legend_df,
  aes(
    x = x, y = y,
    color = variation,
    linetype = variation,
    group = variation
  )
) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(values = manual_colors) +
  scale_linetype_manual(values = manual_linetypes) +
  guides(
    # keep ONLY ONE legend (color), but force it to display the correct linetypes
    color = guide_legend(
      ncol = 1,
      override.aes = list(
        linetype  = unname(manual_linetypes[names(manual_colors)]),
        linewidth = 1.2
      )
    ),
    linetype = "none"
  ) +
  theme_void() +
  theme(
    legend.title    = element_blank(),
    legend.text     = element_text(size = 12, face = "bold"),
    legend.position = "right"
  )

legend_grob <- gtable::gtable_filter(ggplotGrob(legend_plot), "guide-box")


# =========================
# Add A–D tags to ONLY the 2x2 panel (so legend column doesn't affect coordinates)
# If you want A C / B D ordering, set label = c("A","C","B","D")
# =========================
two_by_two_tagged <- ggdraw(two_by_two) +
  draw_plot_label(
    label = c("A", "B", "C", "D"),
    x = c(0.02, 0.52, 0.02, 0.52),
    y = c(0.98, 0.98, 0.49, 0.49),
    hjust = 0, vjust = 1,
    fontface = "bold", size = 12
  )

# =========================
# Combine 2x2 + legend (RIGHT-MIDDLE)
# =========================
final_with_legend <- plot_grid(
  two_by_two_tagged,
  plot_grid(
    NULL,
    legend_grob,
    NULL,
    ncol = 1,
    rel_heights = c(0.35, 0.30, 0.35)   # centers legend vertically
  ),
  ncol = 2,
  rel_widths = c(1, 0.22),
  align = "hv"
)

# =========================
# Add ONE global axis labels
final <- ggdraw(final_with_legend) +
  # Global Y label
  draw_label(
    "PIC",
    x = 0.012, y = 0.55,
    angle = 90,
    fontface = "bold", size = 14
  ) +
  # Global X label (centered under the 2x2 panel, not legend)
  draw_label(
    "Data Points",
    x = 0.42,    # <- centers under plots, avoids legend squeeze
    y = 0.015,
    fontface = "bold", size = 14
  )

final



# Save
ggsave("plots/paper/supp_summary_95_cov.png", 
       plot = final, height = 6, width = 12,
       units = "in", dpi = 400, bg = "white")
