
a_df_covid
a_df_flu
a_df_ili
a_df_rsv

suppressPackageStartupMessages({
  library(tidyverse)
  library(cowplot)
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(grid)
  library(patchwork)
  library(gtable)
  library(scales)
})

theme_text_aes <- list(
  theme_minimal(base_size = 14),
  theme_cowplot(),
  theme(
    text         = element_text(face = "plain"),
    axis.text    = element_text(face = "plain"),
    axis.title.x = element_text(face = "plain"),
    axis.title.y = element_text(
      face   = "plain",
      margin = margin(r = 15)
    ),
    legend.title = element_blank()
  )
)


avg_all <- bind_rows(
  a_df_ili   %>% mutate(dataset = "wILI"),
  a_df_covid %>% mutate(dataset = "COVID-19"),
  a_df_flu   %>% mutate(dataset = "Influenza"),
  a_df_rsv   %>% mutate(dataset = "RSV")
)


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

legend_order <- c(
  "Drift",
  "Drift (transformed)",
  "Flatline",
  "Flatline (transformed)"
)

manual_colors    <- manual_colors[legend_order]
manual_linetypes <- manual_linetypes[legend_order]

avg_all_plot <- avg_all %>%
  mutate(variation = factor(variation, levels = legend_order))


make_summary_plot <- function(df, dataset_name, show_title = FALSE) {
  
  # safety check: variations must match color map
  missing <- setdiff(unique(as.character(df$variation)), legend_order)
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
  
  # data-driven y limits with padding
  y_rng <- range(df$mean_cov50, na.rm = TRUE)
  y_pad <- 0.05 * diff(y_rng)
  if (!is.finite(y_pad) || y_pad == 0) y_pad <- 0.02
  y_lim <- c(y_rng[1] - y_pad, y_rng[2] + y_pad)
  
  ggplot(df, aes(
    x = data_num, y = mean_cov50,
    group = variation,
    color = variation,
    linetype = variation
  )) +
    
    # thicker model lines
    geom_line(linewidth = 1.2) +
    
    # NO vertical dashed line (removed)
    # 
    # # nominal PIC reference (thin, grey, dashed)
    # geom_hline(
    #   yintercept = 0.5,
    #   linetype = "dashed",
    #   color = "grey55",
    #   linewidth = 0.6
    # ) +
    
  scale_color_manual(values = manual_colors, breaks = legend_order) +
    scale_linetype_manual(values = manual_linetypes, breaks = legend_order) +
    
    labs(
      x = NULL,
      y = "PIC",
      title = if (show_title) dataset_name else NULL
    ) +
    
    scale_y_continuous(
      limits = y_lim,
      breaks = pretty_breaks(n = 5),
      labels = label_number(accuracy = 0.01)
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
      legend.position  = "none",  # remove per-panel legends
      plot.margin      = margin(15, 20, 15, 15)
    )
}


plot_covid <- avg_all_plot %>% filter(dataset == "COVID-19")  %>% make_summary_plot("COVID-19",  show_title = TRUE)
plot_flu   <- avg_all_plot %>% filter(dataset == "Influenza") %>% make_summary_plot("Influenza", show_title = TRUE)
plot_wili  <- avg_all_plot %>% filter(dataset == "wILI")      %>% make_summary_plot("wILI",      show_title = TRUE)
plot_rsv   <- avg_all_plot %>% filter(dataset == "RSV")       %>% make_summary_plot("RSV",       show_title = TRUE)


two_by_two <- plot_grid(
  plot_covid, plot_flu,
  plot_wili,  plot_rsv,
  ncol = 2,
  align = "hv",
  axis = "tblr"
)


legend_df <- tibble(
  variation = factor(
    rep(legend_order, each = 2),
    levels = legend_order
  ),
  x = rep(c(1, 2), times = length(legend_order)),
  y = rep(seq_along(legend_order), each = 2)
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
  geom_line(linewidth = 1.6) +
  scale_color_manual(values = manual_colors, breaks = legend_order) +
  scale_linetype_manual(values = manual_linetypes, breaks = legend_order) +
  guides(
    color = guide_legend(
      ncol = 1,
      override.aes = list(
        linetype  = unname(manual_linetypes[legend_order]),
        linewidth = 1.6
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


two_by_two_tagged <- ggdraw(two_by_two) +
  draw_plot_label(
    label = c("A", "B", "C", "D"),
    x = c(0.02, 0.52, 0.02, 0.52),
    y = c(0.98, 0.98, 0.49, 0.49),
    hjust = 0, vjust = 1,
    fontface = "bold", size = 12
  )


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


final <- ggdraw(final_with_legend) +
  draw_label(
    "Data Points",
    x = 0.42,
    y = 0.015,
    fontface = "bold",
    size = 14
  )

final


ggsave(
  "plots/paper/supp_summary_50_cov.png",
  plot   = final,
  height = 6,
  width  = 12,
  units  = "in",
  dpi    = 400,
  bg     = "white"
)

# ============================================================
# ============================================================


suppressPackageStartupMessages({
  library(tidyverse)
  library(cowplot)
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(grid)
  library(patchwork)
  library(gtable)
  library(scales)
})


theme_text_aes <- list(
  theme_minimal(base_size = 14),
  theme_cowplot(),
  theme(
    text         = element_text(face = "plain"),
    axis.text    = element_text(face = "plain"),
    axis.title.x = element_text(face = "plain"),
    axis.title.y = element_text(
      face   = "plain",
      margin = margin(r = 15)
    ),
    legend.title = element_blank()
  )
)


avg_all <- bind_rows(
  a_df_ili   %>% mutate(dataset = "wILI"),
  a_df_covid %>% mutate(dataset = "COVID-19"),
  a_df_flu   %>% mutate(dataset = "Influenza"),
  a_df_rsv   %>% mutate(dataset = "RSV")
)

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


legend_order <- c(
  "Drift",
  "Drift (transformed)",
  "Flatline",
  "Flatline (transformed)"
)

manual_colors    <- manual_colors[legend_order]
manual_linetypes <- manual_linetypes[legend_order]

avg_all_plot <- avg_all %>%
  mutate(variation = factor(variation, levels = legend_order))


make_summary_plot <- function(df, dataset_name, show_title = FALSE) {
  
  # safety check: variations must match color map
  missing <- setdiff(unique(as.character(df$variation)), legend_order)
  if (length(missing)) stop("Unknown variation(s): ", paste(missing, collapse = ", "))
  
  # detect "all" and its plotted x-position
  has_all <- any(tolower(trimws(as.character(df$data))) == "all")
  x_all   <- if (has_all) max(df$data_num, na.rm = TRUE) else NA_real_
  x_max   <- max(df$data_num, na.rm = TRUE)
  
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
  
  # data-driven y limits with padding (do NOT force 0–1)
  y_rng <- range(df$mean_cov95, na.rm = TRUE)
  y_pad <- 0.05 * diff(y_rng)
  if (!is.finite(y_pad) || y_pad == 0) y_pad <- 0.02
  y_lim <- c(y_rng[1] - y_pad, y_rng[2] + y_pad)
  
  ggplot(
    df,
    aes(
      x = data_num, y = mean_cov95,
      group = variation,
      color = variation,
      linetype = variation
    )
  ) +
    # thicker model lines
    geom_line(linewidth = 1.2) +
    
    # REMOVE vertical dashed line (Spencer request)
    # geom_vline(...)
    
    # # nominal PIC reference at 0.95 (thin grey dashed; distinct from model lines)
    # geom_hline(
    #   yintercept = 0.95,
    #   linetype = "dashed",
    #   color = "grey55",
    #   linewidth = 0.6
    # ) +
  
  scale_color_manual(values = manual_colors, breaks = legend_order) +
    scale_linetype_manual(values = manual_linetypes, breaks = legend_order) +
    
    labs(
      x = NULL,
      y = "PIC",  # PIC per panel (Spencer request)
      title = if (show_title) dataset_name else NULL
    ) +
    
    scale_y_continuous(
      limits = y_lim,
      breaks = pretty_breaks(n = 5),
      labels = label_number(accuracy = 0.01)
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
      legend.position  = "none",
      plot.margin      = margin(10, 20, 10, 10)
    )
}


plot_covid <- avg_all_plot %>% filter(dataset == "COVID-19")  %>% make_summary_plot("COVID-19",  show_title = TRUE)
plot_flu   <- avg_all_plot %>% filter(dataset == "Influenza") %>% make_summary_plot("Influenza", show_title = TRUE)
plot_wili  <- avg_all_plot %>% filter(dataset == "wILI")      %>% make_summary_plot("wILI",      show_title = TRUE)
plot_rsv   <- avg_all_plot %>% filter(dataset == "RSV")       %>% make_summary_plot("RSV",       show_title = TRUE)


two_by_two <- plot_grid(
  plot_covid, plot_flu,
  plot_wili,  plot_rsv,
  ncol = 2,
  align = "hv",
  axis = "tblr"
)


legend_df <- tibble(
  variation = factor(
    rep(legend_order, each = 2),
    levels = legend_order
  ),
  x = rep(c(1, 2), times = length(legend_order)),
  y = rep(seq_along(legend_order), each = 2)
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
  geom_line(linewidth = 1.6) +
  scale_color_manual(values = manual_colors, breaks = legend_order) +
  scale_linetype_manual(values = manual_linetypes, breaks = legend_order) +
  guides(
    color = guide_legend(
      ncol = 1,
      override.aes = list(
        linetype  = unname(manual_linetypes[legend_order]),
        linewidth = 1.6
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

two_by_two_tagged <- ggdraw(two_by_two) +
  draw_plot_label(
    label = c("A", "B", "C", "D"),
    x = c(0.02, 0.52, 0.02, 0.52),
    y = c(0.98, 0.98, 0.49, 0.49),
    hjust = 0, vjust = 1,
    fontface = "bold", size = 12
  )

final_with_legend <- plot_grid(
  two_by_two_tagged,
  plot_grid(
    NULL,
    legend_grob,
    NULL,
    ncol = 1,
    rel_heights = c(0.35, 0.30, 0.35)
  ),
  ncol = 2,
  rel_widths = c(1, 0.22),
  align = "hv"
)


final <- ggdraw(final_with_legend) +
  draw_label(
    "Data Points",
    x = 0.42,
    y = 0.015,
    fontface = "bold",
    size = 14
  )

final


# Save
ggsave(
  "plots/paper/supp_summary_95_cov.png",
  plot   = final,
  height = 6,
  width  = 12,
  units  = "in",
  dpi    = 400,
  bg     = "white"
)
