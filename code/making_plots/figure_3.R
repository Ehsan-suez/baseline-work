
# Load individual plotting scripts (these should create a_df_*)

source("code/running_models/flu_individual_plotting.R")
source("code/running_models/covid_individual_plotting.R")
source("code/running_models/ili_individual_plotting.R")
source("code/running_models/rsv_individual_plotting_new.R")

# Objects expected from sourced scripts:
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


# Shared theme bundle

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


# Build combined summary data

avg_all <- bind_rows(
  a_df_ili   %>% mutate(dataset = "wILI"),
  a_df_covid %>% mutate(dataset = "COVID-19"),
  a_df_flu   %>% mutate(dataset = "Influenza"),
  a_df_rsv   %>% mutate(dataset = "RSV")
)



# Manual color + linetype mapping (2 colors)

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

avg_all_plot <- avg_all %>%
  mutate(variation = factor(variation, levels = names(manual_colors)))


# Plot helper

make_summary_plot <- function(df, dataset_name, show_title = FALSE) {
  
  # safety check: variations must match color map
  missing <- setdiff(unique(as.character(df$variation)), names(manual_colors))
  if (length(missing)) stop("Unknown variation(s): ", paste(missing, collapse = ", "))
  
  # --- handle "all" even if data_num is NA ---
  df <- df %>%
    dplyr::mutate(
      data_clean = tolower(trimws(as.character(data)))
    )
  
  has_all <- any(df$data_clean == "all")
  
  if (has_all) {
    # define where "all" should appear on x: just to the right of the max non-all x
    x_non_all_max <- max(df$data_num[df$data_clean != "all"], na.rm = TRUE)
    x_all <- x_non_all_max + 2
    
    # fill missing x for "all" so it actually plots
    df <- df %>%
      dplyr::mutate(
        data_num = dplyr::if_else(data_clean == "all" & is.na(data_num), x_all, data_num)
      )
  } else {
    x_all <- NA_real_
  }
  
  x_max <- max(df$data_num, na.rm = TRUE)
  
  # y label formatting
  y_format <- if (dataset_name == "wILI") {
    scales::label_number(accuracy = 0.01)
  } else {
    scales::label_number(accuracy = 1)
  }
  
  # custom breaks for COVID; simpler breaks for others
  x_breaks <- if (dataset_name == "COVID-19") {
    brks <- c(
      seq(10, 100, by = 20),
      seq(120, x_max, by = 50),
      x_all
    )
    sort(unique(brks[!is.na(brks) & brks <= x_max]))
  } else {
    brks <- c(0, 5, 10, 20, 30, 40, 50, x_all)
    sort(unique(brks[!is.na(brks) & brks <= x_max]))
  }
  
  ggplot(df, aes(
    x = data_num,
    y = mean_wis,
    group = variation,
    color = variation,
    linetype = variation
  )) +
    geom_line(linewidth = 0.5, na.rm = TRUE) +
    geom_vline(xintercept = 10, linetype = "dashed", color = "black") +
    scale_color_manual(values = manual_colors) +
    scale_linetype_manual(values = manual_linetypes) +
    labs(
      x = NULL,
      y = NULL,
      title = if (show_title) dataset_name else NULL
    ) +
    scale_y_continuous(breaks = scales::pretty_breaks(), labels = y_format) +
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


# Generate summary plots
plot_covid <- avg_all_plot %>% filter(dataset == "COVID-19")  %>% make_summary_plot("COVID-19",  show_title = TRUE)
plot_flu   <- avg_all_plot %>% filter(dataset == "Influenza") %>% make_summary_plot("Influenza", show_title = TRUE)
plot_wili  <- avg_all_plot %>% filter(dataset == "wILI")      %>% make_summary_plot("wILI",      show_title = TRUE)
plot_rsv   <- avg_all_plot %>% filter(dataset == "RSV")       %>% make_summary_plot("RSV",       show_title = TRUE)

# Remove legends from each panel (we will add one global legend)
plot_covid <- plot_covid + theme(legend.position = "none")
plot_flu   <- plot_flu   + theme(legend.position = "none")
plot_wili  <- plot_wili  + theme(legend.position = "none")
plot_rsv   <- plot_rsv   + theme(legend.position = "none")


# 2x2 layout (cowplot)
two_by_two <- plot_grid(
  plot_covid, plot_flu,
  plot_wili,  plot_rsv,
  ncol = 2,
  align = "hv",
  axis = "tblr"
)

# Build a standalone legend grob

legend_df <- tidyr::expand_grid(
  variation = factor(names(manual_colors), levels = names(manual_colors)),
  x = c(1, 2)
) %>%
  dplyr::mutate(y = as.numeric(variation))

legend_plot <- ggplot(
  legend_df,
  aes(
    x = x, y = y,
    group = variation,
    color = variation,
    linetype = variation
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
    legend.position = "right",
    legend.title = element_blank(),
    legend.text  = element_text(size = 12, face = "bold")
  )

legend_grob <- gtable::gtable_filter(ggplotGrob(legend_plot), "guide-box")

# Put legend on the RIGHT and center it vertically (middle-right)

legend_centered <- plot_grid(
  NULL,
  legend_grob,
  NULL,
  ncol = 1,
  rel_heights = c(0.3, 0.4, 0.3)   # <<< controls vertical centering
)

final_with_legend <- plot_grid(
  two_by_two,
  legend_centered,
  nrow = 1,
  rel_widths = c(1, 0.22)          # <<< legend column width
)

final_with_legend

# Add ONE global axis labels + A–D tags

final <- ggdraw(final_with_legend) +
  # Global Y label
  draw_label(
    "WIS",
    x = 0.012, y = 0.6,
    angle = 90,
    fontface = "bold", size = 14
  ) +
  # Global X label (nudged left because legend occupies right)
  draw_label(
    "Data Points",
    x = 0.425, y = 0.012,
    fontface = "bold", size = 14
  ) +
  # Panel labels
  draw_plot_label(
    label = c("A", "B", "C", "D"),
    x = c(0.03, 0.44, 0.03, 0.44),
    y = c(0.98, 0.98, 0.50, 0.50),
    hjust = 0, vjust = 1,
    fontface = "bold", size = 12
  )

final

# Save

ggsave(
  "plots/paper/main_summary.png",
  plot = final,
  height = 6, width = 12,
  units = "in", dpi = 400, bg = "white"
)
