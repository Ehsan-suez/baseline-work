library(dplyr)
library(readr)
library(simplets)
library(purrr)
library(tidyr)
library(ggplot2)
library(furrr)
library(parallel)
library(covidHubUtils)
library(stringr)
library(lubridate)

theme_set(theme_bw())


# Location lookup
locations <- read_csv("data/locations.csv")

# Truth data (used for scoring every file)
truth_rsv <- read_csv("data/rsv/truth_rsv.csv")

truth_rsv_aligned <- truth_rsv %>%
  rename(target_end_date = date) %>%
  filter(
    age_group == "0-130",
    target == "inc hosp"
  ) %>%
  left_join(
    locations %>% select(location, location_name),
    by = "location"
  ) %>%
  mutate(
    target_variable = "inc hosp rsv",
    model = "truth"
  ) %>%
  select(model, target_variable, target_end_date,
         location_name, value) %>%
  mutate(value = round(value)) %>%
  rename(location = location_name)


############################################################
##Helper: quantile extractor
############################################################
get_quantiles_df <- function(predictions, taus) {
  purrr::map_dfr(seq_len(ncol(predictions)), function(h) {
    tibble(
      horizon = h,
      quantile = taus,
      value = pmax(
        0,
        ceiling(quantile(predictions[, h], probs = taus, na.rm = TRUE))
      )
    )
  })
}


############################################################


forecast_dates <- truth_rsv_aligned |>
  filter(
    year(target_end_date) >= 2022,
    month(target_end_date) %in% c(10,11,12,1,2,3,4),
    target_end_date < as.Date("2025-05-01")
  ) |>
  pull(target_end_date) |>
  unique() |>
  sort()

locs <- unique(truth_rsv_aligned$location)

# Hyperparams
transformations <- c("none", "sqrt")
offsets <- c(1)
sym_opts <- c(TRUE, FALSE)
taus <- c(0.01, 0.025, seq(0.05, 0.95, 0.05), 0.975, 0.99)

plan(multisession)

# ----------------------------
# 2. MAIN LOOP: all dates × all locations
# ----------------------------

baseline_all <- future_map_dfr(
  forecast_dates,
  function(fdate) {
    message("Running forecast for date: ", fdate)
    
    # Use only truth up to fdate
    truth_upto_fdate <- truth_rsv_aligned |>
      filter(target_end_date <= fdate)
    
    # Loop over locations
    map_dfr(locs, function(loc) {
      
      loc_df <- truth_upto_fdate |>
        filter(location == loc) |>
        arrange(target_end_date)
      
      # Skip if insufficient history
      if (sum(!is.na(loc_df$value)) < 10) {
        message("Skipping ", loc)
        return(NULL)
      }
      
      full_window <- sum(!is.na(loc_df$value))
      window_sizes <- unique(c(5:53, full_window))
      
      
      param_grid <- expand.grid(
        transformation = transformations,
        transform_offset = offsets,
        symmetrize = sym_opts,
        window_size = window_sizes,
        stringsAsFactors = FALSE
      ) |>
        mutate(
          model_id = paste0(
            transformation, "_",
            ifelse(symmetrize, "sym", "nonsym"), "_w",
            ifelse(window_size == full_window, "_all", window_size)
          )
        )
      
      
      # Run all baseline configs
      map_dfr(seq_len(nrow(param_grid)), function(i) {
        cfg <- param_grid[i, ]
        
        fit <- fit_simple_ts(
          y = loc_df$value,
          ts_frequency = 1,
          model = "quantile_baseline",
          transformation = cfg$transformation,
          transform_offset = cfg$transform_offset,
          symmetrize = cfg$symmetrize,
          window_size = cfg$window_size
        )
        
        preds <- predict(
          fit,
          nsim = 10000,
          horizon = 4,
          origin = "obs",
          force_nonneg = TRUE
        )
        
        get_quantiles_df(preds, taus) |>
          mutate(
            transformation = cfg$transformation,
            transform_offset = cfg$transform_offset,
            symmetrize = cfg$symmetrize,
            window_size = cfg$window_size,
            model = cfg$model_id,
            location = loc,
            reference_date = fdate
          )
      })
    })
  },
  .progress = TRUE,
  .options = furrr::furrr_options(seed = TRUE)
)

#Format final output

baseline_all_comb <- baseline_all |>
  mutate(
    forecast_date      = reference_date,
    target_variable    = "inc hosp rsv",
    target_end_date    = reference_date + horizon * 7L,
    type               = "quantile",
    temporal_resolution = "wk"
  ) %>%
  select(
    reference_date, forecast_date, horizon,
    target_variable, target_end_date,
    type, quantile, value, model,
    temporal_resolution, window_size, location
  ) 

baseline_all_comb <- baseline_all_comb %>%
  distinct(
    reference_date, forecast_date, location, horizon,
    target_variable, target_end_date,
    type, quantile, model, window_size,
    .keep_all = TRUE
  )

baseline_all_comb %>%
  group_by(model, location, horizon) %>%
  summarise(nonmonotonic = any(diff(value[order(quantile)]) < 0)) %>%
  filter(nonmonotonic)

write_csv(
  baseline_all_comb,
  "results/rsv/retro_full_final_data_redo.csv"
)


baseline_all_comb <- read_csv("results/rsv/retro_full_final_data_redo.csv")


ref_dates <- unique(baseline_all_comb$reference_date)
length(ref_dates)

library(dplyr)
library(purrr)
library(scoringutils)



library(purrr)
library(dplyr)
library(readr)

dir.create("results/rsv/scores_redo", recursive = TRUE, showWarnings = FALSE)

walk(
  ref_dates,
  function(fdate) {
    message("Scoring date: ", fdate)
    
    df <- baseline_all_comb %>% filter(reference_date == fdate)
    if (nrow(df) == 0) return(NULL)
    
    sc <- score_forecasts(
      df,
      truth_rsv_aligned,
      return_format = "wide",
      metrics = c("abs_error","wis","wis_components",
                  "interval_coverage","quantile_coverage"),
      use_median_as_point = TRUE
    ) %>% mutate(reference_date = fdate)
    
    write_csv(
      sc,
      paste0("results/rsv/scores_redo/rsv_baseline_scores_", fdate, ".csv")
    )
  }
)



