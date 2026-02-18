
library(dplyr)
library(purrr)
library(mgcv)
library(MMWRweek)
library(tibble)

------------------------------------------------------------------------

wrangle_ili_for_kde <- function(ili_data) {
  ili_data %>%
    # standardize names
    rename(
      time         = target_end_date,
      region       = location,
      weighted_ili = value
    ) %>%
    # MMWR year/week
    mutate(
      mmwr = MMWRweek(time),
      year = mmwr$MMWRyear,
      week = mmwr$MMWRweek
    ) %>%
    # flu season definition: week >= 40 is new season
    mutate(
      season = if_else(week >= 40, year, year - 1),
      season_week = if_else(
        week >= 40,
        week - 39L,   # 40 -> 1
        week + 13L    # 1–39 -> 14–52
      )
    ) %>%
    arrange(region, time) %>%
    group_by(region) %>%
    mutate(time_index = row_number()) %>%
    ungroup() %>%
    select(region, year, week, time, weighted_ili,
           time_index, season, season_week)
}


# ili_data <- readr::read_csv("path/to/your_ili_data.csv")
wrangled_ili <- wrangle_ili_for_kde(ili_data)

## seasonal GAM forecast function 

make_ili_gam_forecast <- function(ili_data,
                                  forecast_date,
                                  weeks_ahead,
                                  n_sim,
                                  quantiles = c(0.01, 0.025,
                                                seq(0.05, 0.95, 0.05),
                                                0.975, 0.99)) {
  forecast_date <- as.Date(forecast_date)
  
  ## Define future target_end_dates for this forecast ---
  target_end_dates <- seq.Date(
    from = forecast_date + 7,
    by   = "1 week",
    length.out = weeks_ahead
  )
  
  ## Map those dates to season_week using same logic as ili_data
  target_df <- tibble(target_end_date = target_end_dates) %>%
    mutate(
      mmwr = MMWRweek(target_end_date),
      year = mmwr$MMWRyear,
      week = mmwr$MMWRweek,
      season = if_else(week >= 40, year, year - 1),
      season_week = if_else(
        week >= 40,
        week - 39L,
        week + 13L
      )
    )
  
  season_weeks_new <- target_df$season_week
  
  ## Restrict training data to what is available at forecast_date ---
  clean_data <- ili_data %>%
    filter(time <= forecast_date) %>%
    filter(!is.na(season_week), !is.na(weighted_ili))
  
  ## Helper: fit GAM for one group (region) ---
  fit_gam <- function(df) {
    # avoid log(0)
    df <- df %>% mutate(count = pmax(weighted_ili, 0.001))
    gam(
      log(count) ~ s(season_week, bs = "cc"),
      data   = df
    )
  }
  
  ## helper: simulate predictive distribution & get monotone quantiles ---
  predict_gam <- function(fm, new_season_weeks) {
    beta <- coef(fm)
    Vb   <- vcov(fm)
    nb   <- length(beta)
    
    Cv <- chol(Vb)
    br <- t(Cv) %*% matrix(rnorm(n_sim * nb), nb, n_sim) + beta
    
    Xp <- predict(
      fm,
      newdata = data.frame(season_week = new_season_weeks),
      type    = "lpmatrix"
    )
    
    lp <- Xp %*% br
    
    # add observation noise on log scale
    tmp <- matrix(
      rnorm(length(lp), mean = lp, sd = sqrt(fm$sig2)),
      nrow = nrow(lp)
    )
    
    # enforce monotone quantiles on incidence scale
    apply(tmp, 1, function(x) {
      qs <- quantile(exp(x), probs = quantiles)
      qs <- cummax(qs)   # ensure non-decreasing
      qs
    })
  }
  
  ## Helper: format output (hub-style) ---
  create_output <- function(pred_matrix, region_label) {
    
    pred_matrix <- t(pred_matrix) + 1 / n_sim
    
    df <- expand.grid(
      target_end_date = target_end_dates,
      quantile        = quantiles
    ) %>%
      mutate(
        value    = as.vector(pred_matrix),
        location = region_label,
        horizon  = as.integer(difftime(target_end_date, forecast_date,
                                       units = "weeks")),
        type     = "quantile",
        target_variable     = "inc wili",
        reference_date      = forecast_date,
        forecast_date       = forecast_date,
        temporal_resolution = "wk",
        model               = "seasonal"
      )
    
    # point forecast = median
    point_df <- df %>%
      filter(quantile == 0.5) %>%
      mutate(type = "point")
    
    bind_rows(df, point_df) %>%
      distinct() %>%
      select(
        reference_date,
        forecast_date,
        location,
        horizon,
        target_variable,
        target_end_date,
        type,
        quantile,
        value,
        temporal_resolution,
        model
      )
  }
  
  ## Loop over regions (each region = one location) ---
  target_groups <- unique(clean_data$region)
  
  output_list <- map(target_groups, function(group) {
    group_data <- clean_data %>% filter(region == group)
    
    if (nrow(group_data) >= 10) {
      fm    <- fit_gam(group_data)
      preds <- predict_gam(fm, season_weeks_new)
      create_output(preds, group)
    } else {
      NULL
    }
  })
  
  bind_rows(output_list)
}

##Rolling weekly forecasts for 2014/15 season


start_fd <- wrangled_ili %>%
  dplyr::filter(lubridate::year(time) == 2022,
                lubridate::month(time) == 10) %>%
  dplyr::summarise(start_fd = min(time)) %>%
  dplyr::pull(start_fd)

end_fd <- wrangled_ili %>%
  dplyr::filter(lubridate::year(time) == 2023,
                lubridate::month(time) == 5) %>%
  dplyr::summarise(end_fd = max(time)) %>%
  dplyr::pull(end_fd)


ref_dates_2022_23 <- seq.Date(from = start_fd,
                              to   = end_fd,
                              by   = "1 week")

rolling_forecasts_2022_23 <- purrr::map_dfr(ref_dates_2022_23, function(fd) {
  make_ili_gam_forecast(
    ili_data      = wrangled_ili,
    forecast_date = fd,
    weeks_ahead   = 4,      # horizons 1–4
    n_sim         = 1000
  )
})

## Quick sanity checks


# Check structure
dplyr::glimpse(rolling_forecasts_2022_23)

rolling_forecasts_2022_23 %>%
  filter(reference_date == as.Date("2022-10-02"),
         location == "hhs1") %>%
  distinct(reference_date, horizon, target_end_date) %>%
  arrange(horizon)


# Check monotonicity of quantiles for a single reference_date (just to confirm)
rolling_forecasts_2022_23 %>%
  filter(type == "quantile") %>%
  group_by(model, location, reference_date, horizon) %>%
  summarise(
    nonmonotonic = any(diff(value[order(quantile)]) < 0),
    .groups = "drop"
  ) %>%
  filter(nonmonotonic)

## Prepare for scoring + run scoring + save outputs


library(readr)

# Prepare forecasts in covidHubUtils format
forecast_for_scoring <- rolling_forecasts_2022_23 %>%
  dplyr::select(
    model,
    reference_date,
    horizon, 
    forecast_date,
    target_variable,
    target_end_date,
    location,
    type,
    temporal_resolution,
    quantile,
    value
  )

# Prepare truth in covidHubUtils format
truth_for_scoring <- ili_data 
# Score this season
score_season_2022_2023 <- covidHubUtils::score_forecasts(
  forecasts           = forecast_for_scoring,
  truth               = truth_for_scoring,
  return_format       = "wide",
  metrics             = c("wis", "interval_coverage"),
  use_median_as_point = TRUE
)

# inspect if you like
score_season_2022_2023
# Save forecast + score to CSV
readr::write_csv(
  rolling_forecasts_2022_23,
  "results/seasonal/forecasts/seasonal_gam_forecasts_2022_2023.csv"
)

readr::write_csv(
  score_season_2022_2023,
  "results/seasonal/scores/seasonal_gam_scores_2022_2023.csv"
)
