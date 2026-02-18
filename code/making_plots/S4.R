source("code/running_models/flu_individual_plotting.R")
source("code/running_models/covid_individual_plotting.R")
source("code/running_models/ili_individual_plotting.R")
source("code/running_models/rsv_individual_plotting_new.R")


d_df_covid
d_df_flu
d_df_ili
d_df_rsv
library(dplyr)

d_df_ili <- d_df_ili %>%
  mutate(
    location = if_else(location == "nat", "US", location)
  )

library(dplyr)
library(readr)

locations <- read_csv("data/locations.csv", show_col_types = FALSE)

library(dplyr)

d_df_flu <- d_df_flu %>%
  mutate(location = as.character(location)) %>%
  left_join(
    locations %>%
      select(location_name, location) %>%   # lookup: name -> code
      rename(location_code = location),
    by = c("location" = "location_name")
  ) %>%
  mutate(
    location = if_else(location == "US", "US", location_code)
  ) %>%
  select(-location_code)

head(d_df_flu$location)
unique(d_df_flu$location)



avg_all_loc <- bind_rows(
  d_df_ili %>% mutate(dataset = "wILI"),
  d_df_covid %>% mutate(dataset = "COVID-19"),
  d_df_flu %>% mutate(dataset = "Influenza"),
  d_df_rsv %>% mutate(dataset = "RSV")
) 

library(dplyr)

avg_us_nat <- avg_all_loc %>%
  filter(location %in% c("US"))

avg_other <- avg_all_loc %>%
  filter(!location %in% c("US"))

library(ggplot2)
library(cowplot)
avg_all_loc_flat_drift <- avg_all_loc %>%
  filter(variation %in% c("Flatline", "Drift"))

avg_all_loc_flat_drift <- avg_all_loc_flat_drift %>%
  left_join(
    locations %>% 
      select(location, location_name),
    by = "location"
  ) %>%
  mutate(
    location_label = if_else(
      is.na(location_name),
      location,        # fallback if no match
      location_name
    )
  ) %>%
  select(-location_name)



library(dplyr)
library(ggplot2)
library(cowplot)
library(tidytext)   # reorder_within(), scale_x_reordered()

avg_all_loc_flat_drift %>%
  group_by(dataset) %>%
  summarise(
    n_locations = n_distinct(location_label)
  )


library(dplyr)
library(ggplot2)
library(cowplot)

make_panel <- function(df, dataset_name, show_legend = FALSE) {
  ggplot(
    df %>% filter(dataset == dataset_name),
    aes(x = location_label, y = mean_wis, fill = variation)
  ) +
    geom_col(position = position_dodge(width = 0.8), width = 0.7) +
    scale_fill_manual(values = manual_colors) +
    # coord_flip() +
    # labs(x = NULL, y = "WIS", title = dataset_name) +
    # theme_cowplot() +
    coord_flip() +
    scale_y_log10(labels = scales::label_number()) +   # ← ADD THIS
    labs(x = NULL, y = "WIS", title = dataset_name) +
    theme_cowplot() +
    
    theme(
      legend.position = if (show_legend) "right" else "none",
      plot.title = element_text(face = "bold", hjust = 0.5),
      
      # ↓↓↓ THIS IS THE CHANGE ↓↓↓
      axis.text.x = element_text(size = 10),   # WIS axis
      axis.text.y = element_text(size = 11),   # location labels
      
      panel.grid = element_blank()
    )
}


# Panels
p_covid <- make_panel(avg_all_loc_flat_drift, "COVID-19", show_legend = TRUE)
p_flu   <- make_panel(avg_all_loc_flat_drift, "Influenza", show_legend = FALSE)
p_rsv   <- make_panel(avg_all_loc_flat_drift, "RSV", show_legend = FALSE)
p_wili  <- make_panel(avg_all_loc_flat_drift, "wILI", show_legend = FALSE)

# Extract legend from the COVID panel
leg <- get_legend(p_covid)

# Remove legend from covid panel for grid assembly
p_covid_noleg <- p_covid + theme(legend.position = "none")

# Top row (COVID + Flu), bottom row (RSV + wILI)
top_row    <- plot_grid(p_covid_noleg, p_flu,  nrow = 1, align = "h", axis = "tb")
bottom_row <- plot_grid(p_rsv,        p_wili, nrow = 1, align = "h", axis = "tb")

# 80/20 vertical split
main_grid <- plot_grid(
  top_row,
  bottom_row,
  ncol = 1,
  rel_heights = c(3.75, 1.25),   # 80/20 split
  align = "v",
  axis = "lr"
)

# Add legend on the right
final_plot <- plot_grid(
  main_grid,
  leg,
  nrow = 1,
  rel_widths = c(1, 0.18)  # tweak legend width as needed
)

final_plot

# Save
ggsave("plots/paper/loc_wis.png", 
       plot = final_plot, height = 12, width = 9,
       units = "in", dpi = 400, bg = "white")
