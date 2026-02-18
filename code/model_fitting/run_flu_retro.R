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


# Truth data (used for scoring every file)
truth_flu <- read_csv("data/Influenza/archive/truth_flu.csv")

truth_flu_aligned <- truth_flu %>%
  rename(
    target_end_date = date,
    value = value
  ) %>%
  mutate(
    model = "truth",                      # 📘 label so it matches forecasts
    target_variable = "inc hosp flu"      # same as your forecast target
  ) %>%
  select(-location) %>%
  rename(location = location_name) |>
  select(model, target_variable, target_end_date, location, value)


############################################################ Helper
############################################################
get_quantiles_df <- function(predictions, taus) {
  purrr::map_dfr(seq_len(ncol(predictions)), function(h) {
    tibble(
      horizon = h,   # 1, 2, 3, ...
      quantile = taus,
      value = pmax(
        0,  # force non-negative
        ceiling(quantile(predictions[, h],
                         probs = taus,
                         na.rm = TRUE))
      )
    )
  })
}


############################################################Parallel backend 
############################################################
plan(multisession, workers = max(1, parallel::detectCores() - 1))


############################################################
# List all RSV hospitalization CSVs
############################################################
files <- list.files(
  path = "data/Influenza/archive/",
  pattern = "target-hospital-admissions_\\d{4}-\\d{2}-\\d{2}\\.csv$",
  full.names = TRUE
) |> sort()


message("Found ", length(files), " Influenza issue files.")

#files <- files[1] # testing
files


############################################################
## Output directories
############################################################
dir.create("results/flu/forecasts", recursive = TRUE, showWarnings = FALSE)
dir.create("results/flu/scores",    recursive = TRUE, showWarnings = FALSE)


############################################################
##MAIN LOOP — process each RSV file
############################################################
for (file_path in files) {
  
  message("\n==============================")
  message("Processing file: ", file_path)
  message("==============================")
  
  # Extract issue date from filename
  issue_date <- as.Date(str_extract(file_path, "\\d{4}-\\d{2}-\\d{2}"))
  message("Issue date detected: ", issue_date)
  
  tryCatch({
    
    ########################################################
    # Load flu issue-specific data
    ########################################################
    filtered_flu <- read_csv(
      file_path,
      col_types = cols(
        date = col_date(),
        location = col_character(),
        location_name = col_character(),
        value = col_double(),
        weekly_rate = col_double()
      )
    )
    
  
    
    
    ########################################################
    # Baseline model parameter grid (global)
    ########################################################
    transformations <- c("none", "sqrt")
    offsets <- 1
    sym_opts <- c(TRUE, FALSE)
    taus <- c(0.01, 0.025, seq(0.05, 0.95, 0.05), 0.975, 0.99)
    
    
    
    ########################################################
    # Fit baseline models for ALL LOCATIONS (parallel)
    ########################################################
    
    locs <- sort(unique(filtered_flu$location_name))
    message("Locations found: ", paste(locs, collapse = ", "))
    
    
    baseline_combined <- future_map_dfr(
      locs,
      function(loc) {
        
        loc_df <- filtered_flu %>%
          filter(location_name == loc) %>%
          arrange(date)
        
        if (sum(!is.na(loc_df$value)) < 10) {
          message("Skipping ", loc, " — too few data points.")
          return(NULL)
        }
        
        full_window <- sum(!is.na(loc_df$value))
        
        # Dynamic windows: 5:53 + full history window
        window_sizes <- unique(c(5:53, full_window))
        #window_sizes <- c(10)
        
        
        # Build model grid
        param_grid <- expand.grid(
          transformation   = transformations,
          transform_offset = offsets,
          symmetrize       = sym_opts,
          window_size      = window_sizes,
          stringsAsFactors = FALSE
        )
        
        param_grid <- param_grid %>%
          mutate(
            model_id = paste0(
              transformation, "_",
              ifelse(symmetrize, "sym", "nonsym"), "_w",
              ifelse(window_size == full_window, "_all", window_size)
            )
          )
        
        # Fit all variants for this location
        purrr::map_dfr(seq_len(nrow(param_grid)), function(i) {
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
          
          get_quantiles_df(preds, taus) %>%
            mutate(
              transformation = cfg$transformation,
              symmetrize     = cfg$symmetrize,
              window_size    = cfg$window_size,
              model_id      = cfg$model_id,
              location_name       = loc,
              reference_date = max(loc_df$date[!is.na(loc_df$value)])
            )
        })
      },
      .progress = TRUE,
      .options = furrr::furrr_options(seed = TRUE)
    )
    
    if (nrow(baseline_combined) == 0) {
      message("⚠ No baseline results produced — skipping.")
      next
    }
    
    #browser()
    ########################################################
    # Format forecasts
    ########################################################
    baseline_combined <- baseline_combined %>%
      mutate(
        forecast_date = reference_date,
        target_variable = "inc hosp flu",
        target_end_date = reference_date + (horizon * 7L),
        type = "quantile",
        model = model_id,
        temporal_resolution = "wk",
        location = location_name
      ) %>%
      select(
        reference_date, forecast_date, horizon,
        target_variable, target_end_date,
        type, quantile, value, model,
        temporal_resolution, window_size, location
      )
    
    
    ########################################################
    # Score forecasts
    ########################################################
    scores <- covidHubUtils::score_forecasts(
      baseline_combined,
      truth_flu_aligned,
      return_format = "wide",
      metrics = c(
        "abs_error", "wis", "wis_components",
        "interval_coverage", "quantile_coverage"
      ),
      use_median_as_point = TRUE
    )
    
    
    ########################################################
    # Save forecasts and scores for this issue date
    ########################################################
    forecast_path <- paste0("results/flu/forecasts/flu_baseline_forecasts_", issue_date, ".csv")
    scores_path   <- paste0("results/flu/scores/flu_baseline_scores_", issue_date, ".csv")
    
    write_csv(baseline_combined, forecast_path)
    write_csv(scores,           scores_path)
    
    message("✔ Saved: ", forecast_path)
    message("✔ Saved: ", scores_path)
    
  }, error = function(e) {
    message("❌ Error processing file ", file_path, ": ", e)
  })
}


message("\n🎉 DONE: Flu baseline pipeline complete!")

# 
# scores <- read_csv("results/flu/scores/flu_baseline_scores_2023-09-23.csv")
# scores_summary <- scores %>%
#   dplyr::group_by(model, horizon) %>%
#   dplyr::summarise(wis = mean(wis), .groups = "drop")
# 
# plt_flu_dly <- ggplot(scores_summary, aes(x = factor(horizon), y = model, fill = wis)) +
#   geom_tile(color = "white") +
#   scale_fill_viridis_c(option = "plasma", direction = -1) +
#   labs(
#     title = "WIS",
#     x = "Horizon (weeks ahead)",
#     y = "Model",
#     fill = "Mean WIS"
#   ) +
#   theme_minimal(base_size = 14) +
#   theme(
#     axis.text.x = element_text(face = "bold"),
#     axis.text.y = element_text(face = "bold"),
#     plot.title = element_text(hjust = 0.5, face = "bold", size = 18)
#   )
# print(plt_flu_dly)
# 
# 
# ##see rwis
# reference_model <- "sqrt_sym_w_all"
# 
# scores_summary2 <- scores_summary %>%
#   group_by(horizon) %>%
#   mutate(
#     ref_wis = wis[model == reference_model],
#     rwis = wis / ref_wis
#   ) %>%
#   ungroup()
# 
# scores_summary2 %>%
#   filter(model == reference_model)
# 
# scores_summary3 <- scores_summary2 %>%
#   tidyr::separate(
#     model,
#     into = c("transform", "symmetry", "window"),
#     sep = "_",
#     extra = "merge"         # <- keeps "w_all" intact
#   ) %>%
#   mutate(
#     window = factor(window, levels = c("w10", "w25", "w50", "w_all")),
#     transform = factor(transform, levels = c("none", "sqrt")),
#     symmetry = factor(symmetry, levels = c("nonsym", "sym"))
#   )
# 
# plt_flu_dly_rwis <- ggplot(scores_summary3, aes(
#   x = factor(horizon),
#   y = window,
#   fill = rwis
# )) +
#   geom_tile(color = "white", linewidth = 0.3) +
#   facet_grid(transform ~ symmetry) +
#   scale_fill_viridis_c(
#     option = "magma",
#     direction = -1,
#     limits = c(0.8, max(scores_summary3$rwis, na.rm = TRUE)),
#     oob = scales::squish
#   ) +
#   labs(
#     title = "Relative WIS",
#     x = "Horizon (weeks ahead)",
#     y = "Window size",
#     fill = "rWIS"
#   ) +
#   theme_minimal(base_size = 14)
# print(plt_flu_dly_rwis)
