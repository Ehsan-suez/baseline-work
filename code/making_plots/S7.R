library(readr)
flatline_sym_sqrt_10 <- read_csv("results/ili/flatline_sym_sqrt_10.csv")
seasonal_gam_forecasts_all <- read_csv("results/seasonal/forecasts/seasonal_gam_forecasts_all.csv")
combined_forecasts <- bind_rows(
  flatline_sym_sqrt_10,
  seasonal_gam_forecasts_all
)

truth_ili_seasonal <- read_csv("data/ili/truth_ili_seasonal.csv")

score_both <- covidHubUtils::score_forecasts(
  forecasts           = combined_forecasts,
  truth               = truth_ili_seasonal,
  return_format       = "wide",
  metrics             = c("wis", "interval_coverage"),
  use_median_as_point = TRUE
)

View(score_both)

p1 <- score_both %>%
  dplyr::group_by(model, horizon) %>%
  dplyr::summarise(wis = mean(wis)) %>%
  scoringutils::plot_heatmap(metric = "wis", x = "horizon")
p1

ggsave(
  filename = "plots/paper/seasonal_vs_flatline_supp_horizon.png",
  plot = p1,
  dpi = 600,          # High resolution
  width = 8,          # in inches
  height = 6,
  units = "in",
  bg = 'white'
)
