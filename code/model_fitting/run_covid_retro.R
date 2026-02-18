
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

theme_set(theme_bw())

##  Load truth and locations

truth_covid_daily <- read_csv("data/covid/archive/truth_covid_daily.csv")

locations <- read_csv("data/locations.csv")

truth_covid_daily_aligned <- truth_covid_daily %>%
  rename(
    target_end_date = time_value   # <-- correct date column
  ) %>%
  mutate(
    model           = "truth",
    target_variable = "inc hosp covid"
  ) %>%
  # join to get names
  left_join(
    locations %>%
      select(location_fips = location,
             abbrev_upper  = abbreviation,
             location_name),
    by = c("location" = "location_fips")
  ) %>%
  select(
    model,
    target_variable,
    target_end_date,
    location,        # FIPS (character or numeric, but consistent)
    location_name,
    value
  ) %>%
  arrange(location, target_end_date)

# Quantile helper 

get_quantiles_df <- function(predictions, taus) {
  weekly_offsets <- c(7, 14, 21, 28)
  
  purrr::map_dfr(seq_along(weekly_offsets), function(i) {
    tibble(
      horizon = weekly_offsets[i],
      quantile = taus,
      value = pmax( #because even after forcing non-neg, still got some zero
        0,
        ceiling(quantile(predictions[, weekly_offsets[i]],
                         probs = taus,
                         na.rm = TRUE))
      )
    )
  })
}

## parallel plan 

plan(multisession, workers = max(1, parallel::detectCores() - 1))

## Find archive-matched files

files <- list.files(
  path   = "data/covid/archive/archive_matched/",
  pattern = "archived_target_data_\\d{4}-\\d{2}-\\d{2}\\.csv$",
  full.names = TRUE
) |> sort()

message("Found ", length(files), " Covid issue files.")

# For testing:
#files <- files[1]
files

## Output dirs 

dir.create("results/covid/forecasts", recursive = TRUE, showWarnings = FALSE)
dir.create("results/covid/scores",    recursive = TRUE, showWarnings = FALSE)

## Main loop over issue files 

for (file_path in files) {
  
  message("\n==============================")
  message("Processing file: ", file_path)
  message("==============================")
  
  # Extract issue date from filename
  issue_date <- as.Date(str_extract(file_path, "\\d{4}-\\d{2}-\\d{2}"))
  message("Issue date detected: ", issue_date)
  
  tryCatch({
    
    ## Load archive-matched data for this issue date 
    
    filtered_cov <- read_csv(
      file_path,
      col_types = cols(
        geo_value  = col_character(),   # lowercase abbrev: al, ak, az, ...
        time_value = col_date(),
        value      = col_double(),
        location   = col_character(),   # FIPS (may be present)
        issue_date = col_date()
      )
    ) %>%
      # Standardize column names
      rename(
        date         = time_value,
        abbrev_lower = geo_value         # al, ak, az
      ) %>%
      mutate(
        abbrev_upper = toupper(abbrev_lower)   # AL, AK, AZ
      ) %>%
      # Join to full location table to get clean FIPS + names
      left_join(
        locations %>%
          select(abbreviation,
                 location_fips = location,
                 location_name),
        by = c("abbrev_upper" = "abbreviation")
      ) %>%
      # Prefer FIPS from lookup (always clean)
      mutate(
        location = location_fips
      ) %>%
      # Keep only desired columns
      select(
        date,
        location,       # FIPS
        location_name,
        value,
        issue_date
      ) %>%
      arrange(date, location)
    
    ## Baseline config 
    
    transformations <- c("none", "sqrt")
    offsets         <- c(1)
    sym_opts        <- c(TRUE, FALSE)
    taus            <- c(0.01, 0.025, seq(0.05, 0.95, 0.05), 0.975, 0.99)
    
    locs <- sort(unique(filtered_cov$location_name))
    message("Locations found: ", paste(locs, collapse = ", "))
    
    ## Run baseline for each location (parallel)
    
    baseline_combined <- future_map_dfr(
      locs,
      function(loc) {
        
        loc_df <- filtered_cov %>%
          filter(location_name == loc) %>%
          arrange(date)
        
        # Too few non-missing points -> skip
        if (sum(!is.na(loc_df$value)) < 10) {
          message("Skipping ", loc, " — too few data points.")
          return(NULL)
        }
        
        full_window   <- sum(!is.na(loc_df$value))
        window_sizes <- c(
          seq(5, 60, by = 1),
          seq(60, 90, by = 5),
          seq(90, 150, by = 10),
          seq(150, 360, by = 30)
        ) |> unique() |> sort()
        
        # Ensure we include the full window at the end
        window_sizes <- c(window_sizes, full_window) |> unique() |> sort()
        
        
        param_grid <- expand.grid(
          transformation   = transformations,
          transform_offset = offsets,
          symmetrize       = sym_opts,
          window_size      = window_sizes,
          stringsAsFactors = FALSE
        ) %>%
          mutate(
            model_id = paste0(
              transformation, "_",
              ifelse(symmetrize, "sym", "nonsym"), "_w",
              ifelse(window_size == full_window, "_all", window_size)
            )
          )
        
        purrr::map_dfr(seq_len(nrow(param_grid)), function(i) {
          cfg <- param_grid[i, ]
          
          fit <- fit_simple_ts(
            y                = loc_df$value,
            ts_frequency     = 1,
            model            = "quantile_baseline",
            transformation   = cfg$transformation,
            transform_offset = cfg$transform_offset,
            symmetrize       = cfg$symmetrize,
            window_size      = cfg$window_size
          )
          
          preds <- predict(
            fit,
            nsim        = 10000,
            horizon     = 28,      # DAILY DATA → up to 28 days ahead
            origin      = "obs",
            force_nonneg = TRUE
          )
          
          get_quantiles_df(preds, taus) %>%
            mutate(
              transformation = cfg$transformation,
              symmetrize     = cfg$symmetrize,
              window_size    = cfg$window_size,
              model_id       = cfg$model_id,
              location_name  = loc,
              location       = unique(loc_df$location),# FIPS
              reference_date = max(loc_df$date[!is.na(loc_df$value)])
            )
        })
      },
      .progress = TRUE,
      .options  = furrr::furrr_options(seed = TRUE)
    )
    
    if (nrow(baseline_combined) == 0) {
      message("⚠ No baseline results produced — skipping.")
      next
    }
    
    ##Final formatting for hub-style forecasts 
    
    baseline_combined <- baseline_combined %>%
      mutate(
        forecast_date   = reference_date,
        target_variable = "inc hosp covid",
        target_end_date = reference_date + horizon,  # 7/14/21/28 days ahead
        type            = "quantile",
        model           = model_id,
        temporal_resolution = "day"
      ) %>%
      select(
        reference_date, forecast_date, horizon, target_variable,
        target_end_date, type, quantile,
        value, model, window_size,
        temporal_resolution, location, location_name
      )
    
    ## Score against truth ------------------------------------------
    scores <- covidHubUtils::score_forecasts(
      forecasts      = baseline_combined,
      truth          = truth_covid_daily_aligned,
      return_format  = "wide",
      metrics        = c("abs_error", "wis", "wis_components",
                         "interval_coverage", "quantile_coverage"),
      use_median_as_point = TRUE
    )
    
    ## Save outputs 
    
    forecast_path <- paste0(
      "results/covid/forecasts/covid_baseline_forecasts_",
      issue_date, ".csv"
    )
    scores_path   <- paste0(
      "results/covid/scores/covid_baseline_scores_",
      issue_date, ".csv"
    )
    
    write_csv(baseline_combined, forecast_path)
    write_csv(scores,           scores_path)
    
    message("✔ Saved: ", forecast_path)
    message("✔ Saved: ", scores_path)
    
  }, error = function(e) {
    message("❌ Error processing file ", file_path, ": ", e$message)
  })
}
message("\n🎉 DONE: Covid baseline pipeline complete!")

scores <- read_csv("results/covid/scores/covid_baseline_scores_2020-11-16.csv")
scores_summary <- scores %>%
  dplyr::group_by(model, horizon) %>%
  dplyr::summarise(wis = mean(wis), .groups = "drop")

plt_cov_dly <- ggplot(scores_summary, aes(x = factor(horizon), y = model, fill = wis)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(option = "plasma", direction = -1) +
  labs(
    title = "WIS",
    x = "Horizon (days ahead)",
    y = "Model",
    fill = "Mean WIS"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(face = "bold"),
    axis.text.y = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18)
  )
print(plt_cov_dly)


##see rwis
reference_model <- "sqrt_sym_w_all"

scores_summary2 <- scores_summary %>%
  group_by(horizon) %>%
  mutate(
    ref_wis = wis[model == reference_model],
    rwis = wis / ref_wis
  ) %>%
  ungroup()

scores_summary2 %>%
  filter(model == reference_model)

scores_summary3 <- scores_summary2 %>%
  tidyr::separate(
    model,
    into = c("transform", "symmetry", "window"),
    sep = "_",
    extra = "merge"         # <- keeps "w_all" intact
  ) %>%
  mutate(
    window = factor(window, levels = c("w10", "w50", "w_all")),
    transform = factor(transform, levels = c("none", "sqrt")),
    symmetry = factor(symmetry, levels = c("nonsym", "sym"))
  )

plt_cov_dly_rwis <- ggplot(scores_summary3, aes(
  x = factor(horizon),
  y = window,
  fill = rwis
)) +
  geom_tile(color = "white", linewidth = 0.3) +
  facet_grid(transform ~ symmetry) +
  scale_fill_viridis_c(
    option = "magma",
    direction = -1,
    limits = c(0.8, max(scores_summary3$rwis, na.rm = TRUE)),
    oob = scales::squish
  ) +
  labs(
    title = "Relative WIS",
    x = "Horizon (days ahead)",
    y = "Window size",
    fill = "rWIS"
  ) +
  theme_minimal(base_size = 14)
print(plt_cov_dly_rwis)
