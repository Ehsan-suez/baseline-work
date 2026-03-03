library(dplyr)
library(lubridate)
library(purrr)
library(furrr)
library(simplets)
library(readr)
library(covidHubUtils)

get_quantiles_df <- function(predictions, taus) {
  purrr::map_dfr(seq_len(ncol(predictions)), function(h) {
    
    # raw quantiles
    q_raw <- quantile(
      predictions[, h],
      probs = taus,
      na.rm = TRUE
    )
    
    # enforce non-negative & monotonic increasing quantiles
    q_fixed <- pmax(0, cummax(q_raw))
    
    # output tibble
    tibble(
      horizon = h,
      quantile = taus,
      value = q_fixed
    )
  })
}

truth_ili <- read_csv("data/ili/truth_ili.csv")

# ----------------------------
# 1. Forecast dates to run
# ----------------------------

forecast_dates <- truth_ili |>
  filter(
    month(target_end_date) %in% c(10,11,12,1,2,3,4),
    year(target_end_date) >= 2014
  ) |>
  pull(target_end_date) |>
  unique() |>
  sort()

locs <- unique(truth_ili$location)

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
    truth_upto_fdate <- truth_ili |>
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
            model_id = cfg$model_id,
            location = loc,
            reference_date = fdate
          )
      })
    })
  },
  .progress = TRUE,
  .options = furrr::furrr_options(seed = TRUE)
)

# ----------------------------
# 3. Format final output
# ----------------------------

baseline_all_comb <- baseline_all |>
  mutate(
    forecast_date = reference_date,
    target_variable = "inc wili",
    target_end_date = reference_date + (horizon * 7L),
    type = "quantile",
    model = model_id,
    temporal_resolution = "wk"
  ) |>
  select(
    reference_date, forecast_date, location,
    horizon, target_variable, target_end_date,
    type, quantile, value, model,
    temporal_resolution, window_size
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
  "results/ili/retro_full.csv"
)


baseline_all_comb <- read_csv("results/ili/retro_full.csv")


ref_dates <- unique(baseline_all_comb$reference_date)
length(ref_dates)

library(dplyr)
library(purrr)
library(scoringutils)

truth_ili <- read_csv("data/ili/truth_ili.csv")

library(purrr)
library(dplyr)
library(readr)

dir.create("results/ili/scores", recursive = TRUE, showWarnings = FALSE)

walk(
  ref_dates,
  function(fdate) {
    message("Scoring date: ", fdate)
    
    df <- baseline_all_comb %>% filter(reference_date == fdate)
    if (nrow(df) == 0) return(NULL)
    
    sc <- score_forecasts(
      df,
      truth_ili,
      return_format = "wide",
      metrics = c("abs_error","wis","wis_components",
                  "interval_coverage","quantile_coverage"),
      use_median_as_point = TRUE
    ) %>% mutate(reference_date = fdate)
    
    write_csv(
      sc,
      paste0("results/ili/scores/ili_baseline_scores_", fdate, ".csv")
    )
  }
)




