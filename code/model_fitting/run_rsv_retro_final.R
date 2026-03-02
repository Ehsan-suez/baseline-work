#uses final data from 2025-10-24 (time of analyzing)

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

# ------------------------------------------------------------
# Location lookup
# ------------------------------------------------------------
locations <- read_csv("data/locations.csv", show_col_types = FALSE)

# ------------------------------------------------------------
# Truth data (final truth, used for scoring)
# ------------------------------------------------------------
truth_rsv <- read_csv("data/rsv/truth_rsv.csv", show_col_types = FALSE)

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

# ------------------------------------------------------------
# Helper: quantile extractor (kept EXACTLY your style)
# ------------------------------------------------------------
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

# ------------------------------------------------------------
# Parallel backend (set ONCE)
# ------------------------------------------------------------
plan(multisession, workers = max(1, parallel::detectCores() - 1))

# ------------------------------------------------------------
# FINAL DATA: pick the latest file in archive (by filename date)
# ------------------------------------------------------------
files <- list.files(
  path = "data/rsv/archive",
  pattern = "_rsvnet_hospitalization\\.csv$",
  full.names = TRUE
) |> sort()

stopifnot(length(files) > 0)

file_path <- tail(files, 1)  # <-- FINAL / latest archive snapshot
message("Using FINAL RSV file: ", file_path)

# Optional: still extract date from filename for naming outputs
issue_date <- as.Date(str_extract(file_path, "\\d{4}-\\d{2}-\\d{2}"))
message("Final file date detected: ", issue_date)

# ------------------------------------------------------------
# Output directories
# ------------------------------------------------------------
dir.create("results/rsv/forecasts", recursive = TRUE, showWarnings = FALSE)
dir.create("results/rsv/scores",    recursive = TRUE, showWarnings = FALSE)

# ============================================================
# MAIN — process FINAL RSV file only
# ============================================================
message("\n==============================")
message("Processing FINAL file: ", file_path)
message("==============================")

tryCatch({
  
  # ----------------------------------------------------------
  # Load FINAL RSV data
  # ----------------------------------------------------------
  df <- read_csv(
    file_path,
    col_types = cols(
      location    = col_character(),
      date        = col_date(),
      age_group   = col_character(),
      target      = col_character(),
      value       = col_double(),
      population  = col_double()
    ),
    show_col_types = FALSE
  )
  
  df <- df %>%
    left_join(locations %>% select(location, location_name),
              by = "location")
  
  # ----------------------------------------------------------
  # Filter RSV rows needed for baseline modeling (FINAL data)
  # ----------------------------------------------------------
  filtered_rsv <- df %>%
    filter(
      age_group == "0-130",
      target == "inc hosp",
      date >= as.Date("2022-01-01"),
      location != "37"
    ) %>%
    mutate(value = round(value)) %>%
    select(-age_group)
  
  if (nrow(filtered_rsv) == 0) {
    stop("⚠ No usable data in the FINAL file after filtering.")
  }
  
  locs <- sort(unique(filtered_rsv$location_name))
  message("Locations found: ", paste(locs, collapse = ", "))
  
  # ----------------------------------------------------------
  # Baseline model parameter grid (global)
  # ----------------------------------------------------------
  transformations <- c("none", "sqrt")
  offsets <- 1
  sym_opts <- c(TRUE, FALSE)
  taus <- c(0.01, 0.025, seq(0.05, 0.95, 0.05), 0.975, 0.99)
  
  # ----------------------------------------------------------
  # Fit baseline models for ALL LOCATIONS (parallel)
  # ----------------------------------------------------------
  baseline_combined <- future_map_dfr(
    locs,
    function(loc) {
      
      loc_df <- filtered_rsv %>%
        filter(location_name == loc) %>%
        arrange(date)
      
      if (sum(!is.na(loc_df$value)) < 10) {
        message("Skipping ", loc, " — too few data points.")
        return(NULL)
      }
      
      full_window <- sum(!is.na(loc_df$value))
      
      # Keep EXACT same window logic you used in as-of script
      window_sizes <- unique(c(5:53, full_window))
      # window_sizes <- c(10, 50, full_window)  # optional testing
      
      # Build model grid
      param_grid <- expand.grid(
        transformation   = transformations,
        transform_offset = offsets,
        symmetrize       = sym_opts,
        window_size      = window_sizes,
        stringsAsFactors = FALSE
      )
      
      # IMPORTANT: Fix model_id formatting (no double underscore)
      param_grid <- param_grid %>%
        mutate(
          model_id = paste0(
            transformation, "_",
            ifelse(symmetrize, "sym", "nonsym"), "_w",
            ifelse(window_size == full_window, "all", window_size)
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
            model          = cfg$model_id,
            location       = loc,
            reference_date = max(loc_df$date[!is.na(loc_df$value)])
          )
      })
    },
    .progress = TRUE,
    .options = furrr::furrr_options(seed = TRUE)
  )
  
  if (nrow(baseline_combined) == 0) {
    stop("⚠ No baseline results produced — stopping.")
  }
  
  # ----------------------------------------------------------
  # Format forecasts
  # ----------------------------------------------------------
  baseline_combined <- baseline_combined %>%
    mutate(
      forecast_date       = reference_date,
      target_variable     = "inc hosp rsv",
      target_end_date     = reference_date + horizon * 7L,
      type                = "quantile",
      temporal_resolution = "wk"
    ) %>%
    select(
      reference_date, forecast_date, horizon,
      target_variable, target_end_date,
      type, quantile, value, model,
      temporal_resolution, window_size, location
    )
  
  # ----------------------------------------------------------
  # Score forecasts vs FINAL truth
  # ----------------------------------------------------------
  scores <- covidHubUtils::score_forecasts(
    baseline_combined,
    truth_rsv_aligned,
    return_format = "wide",
    metrics = c(
      "abs_error", "wis", "wis_components",
      "interval_coverage", "quantile_coverage"
    ),
    use_median_as_point = TRUE
  )
  
  # ----------------------------------------------------------
  forecast_path <- paste0(
    "results/rsv/forecasts/rsv_baseline_forecasts_final_",
    issue_date,
    ".csv"
  )
  
  scores_path <- paste0(
    "results/rsv/scores/rsv_baseline_scores_final_",
    issue_date,
    ".csv"
  )
  
  write_csv(baseline_combined, forecast_path)
  write_csv(scores,           scores_path)
  
  message("✔ Saved: ", forecast_path)
  message("✔ Saved: ", scores_path)
  
}, error = function(e) {
  message("❌ Error processing FINAL file ", file_path, ": ", e)
})

message("\n🎉 DONE: RSV baseline pipeline complete (FINAL data only)!")

# ------------------------------------------------------------
# Optional sanity check (same as your style) — uncomment to use
# ------------------------------------------------------------
# scores <- read_csv("results/rsv/scores/rsv_baseline_scores_final_2025-02-14.csv")
#
# scores_summary <- scores %>%
#   dplyr::group_by(model, horizon) %>%
#   dplyr::summarise(wis = mean(wis), .groups = "drop")
#
# plt_rsv_wk <- ggplot(scores_summary, aes(x = factor(horizon), y = model, fill = wis)) +
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
# print(plt_rsv_wk)
#
# reference_model <- "sqrt_sym_wall"
#
# scores_summary2 <- scores_summary %>%
#   group_by(horizon) %>%
#   mutate(
#     ref_wis = wis[model == reference_model],
#     rwis = wis / ref_wis
#   ) %>%
#   ungroup()
#
# scores_summary3 <- scores_summary2 %>%
#   tidyr::separate(
#     model,
#     into = c("transform", "symmetry", "window"),
#     sep = "_",
#     extra = "merge"
#   ) %>%
#   mutate(
#     window = factor(window, levels = c("w10", "w50", "wall")),
#     transform = factor(transform, levels = c("none", "sqrt")),
#     symmetry = factor(symmetry, levels = c("nonsym", "sym"))
#   )
#
# plt_rsv_wk_rwis <- ggplot(scores_summary3, aes(
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
# print(plt_rsv_wk_rwis)