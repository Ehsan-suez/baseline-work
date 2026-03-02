

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(ggplot2)
  library(cowplot)
})

source("code/running_models/flu_individual_plotting.R")
source("code/running_models/covid_individual_plotting.R")
source("code/running_models/ili_individual_plotting.R")
source("code/running_models/rsv_individual_plotting_new.R")


d_df_ili <- d_df_ili %>%
  mutate(location = if_else(location == "nat", "US", location))

locations <- read_csv("data/locations.csv", show_col_types = FALSE) %>%
  mutate(location = as.character(location))

d_df_flu <- d_df_flu %>%
  mutate(location = as.character(location)) %>%
  left_join(
    locations %>%
      select(location_name, location) %>%
      rename(location_code = location),
    by = c("location" = "location_name")
  ) %>%
  mutate(location = if_else(location == "US", "US", location_code)) %>%
  select(-location_code)


avg_all_loc <- bind_rows(
  d_df_ili   %>% mutate(dataset = "wILI"),
  d_df_covid %>% mutate(dataset = "COVID-19"),
  d_df_flu   %>% mutate(dataset = "Influenza"),
  d_df_rsv   %>% mutate(dataset = "RSV")
)


avg_all_loc_flat_drift <- avg_all_loc %>%
  filter(variation %in% c("Flatline", "Drift")) %>%
  mutate(location = as.character(location))


avg_all_loc_flat_drift <- avg_all_loc_flat_drift %>%
  left_join(
    locations %>% select(location, location_name),
    by = "location"
  ) %>%
  mutate(
    location_label = case_when(
      location %in% c("US", "nat") ~ "US",
      grepl("^hhs\\d+$", location) ~ paste0("HHS ", sub("^hhs", "", location)),
      !is.na(location_name)        ~ location_name,
      TRUE                         ~ location
    )
  ) %>%
  select(-location_name)


wis_loc <- avg_all_loc_flat_drift %>%
  group_by(dataset, location, location_label, variation) %>%
  summarise(mean_wis = mean(mean_wis, na.rm = TRUE), .groups = "drop")


rw_loc <- wis_loc %>%
  select(dataset, location, location_label, variation, mean_wis) %>%
  pivot_wider(names_from = variation, values_from = mean_wis) %>%
  mutate(rWIS = Flatline / Drift) %>%
  filter(is.finite(rWIS), rWIS > 0)


make_panel_rwis <- function(df, dataset_name) {
  
  dfp <- df %>%
    filter(dataset == dataset_name) %>%
    mutate(
      location_label = case_when(
        grepl("^HHS \\d+$", location_label) ~
          factor(location_label, levels = paste0("HHS ", 1:10)),
        TRUE ~ reorder(location_label, rWIS)
      )
    )
  
  ggplot(dfp, aes(x = location_label, y = rWIS)) +
    geom_hline(yintercept = 1, linetype = "dashed", linewidth = 0.4) +
    geom_point(size = 1.9) +
    coord_flip() +
    labs(x = NULL, y = "rWIS", title = dataset_name) +
    theme_cowplot() +
    theme(
      plot.title  = element_text(face = "bold", hjust = 0.5),
      axis.text.x = element_text(size = 10),
      axis.text.y = element_text(size = 11),
      panel.grid.major.x = element_line(color = "grey85", linewidth = 0.35),
      panel.grid.minor = element_blank()
    )
}

p_covid <- make_panel_rwis(rw_loc, "COVID-19")
p_flu   <- make_panel_rwis(rw_loc, "Influenza")
p_rsv   <- make_panel_rwis(rw_loc, "RSV")
p_wili  <- make_panel_rwis(rw_loc, "wILI")

# Layout (2x2 with 80/20 vertical split)
top_row    <- plot_grid(p_covid, p_flu,  nrow = 1, align = "h", axis = "tb")
bottom_row <- plot_grid(p_rsv,  p_wili, nrow = 1, align = "h", axis = "tb")

final_plot <- plot_grid(
  top_row,
  bottom_row,
  ncol = 1,
  rel_heights = c(3.75, 1.25),
  align = "v",
  axis = "lr"
)

final_plot



ggsave(
  "plots/paper/loc_rwis.png",
  plot   = final_plot,
  height = 10,
  width  = 6.5,
  units  = "in",
  dpi    = 800,
  bg     = "white"
)


