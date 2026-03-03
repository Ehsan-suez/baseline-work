library(dplyr)
library(readr)
library(covidData)
library(parallel)


# Load required locations

required_locations <- readr::read_csv("./data/locations.csv") %>%
  dplyr::select(location, abbreviation)

# Get all available issue dates

issue_dates <- covidData::available_issue_dates(measure = "hospitalizations")

issue_dates <- issue_dates[ #skip for daily data
  issue_dates >  as.Date("2020-08-01") & ## before 08/2020 all NA,  
    issue_dates <= as.Date("2024-04-20") ## no data after 04/2020
]

issue_dates <- sort(issue_dates)


# Define function to process ONE issue date

process_one_issue <- function(issue_date) {
  
  message("Processing issue: ", as.Date(issue_date))
  
  nat_cov <- try(
    load_data(
      issues = issue_date,
      spatial_resolution = c("state", "national"),
      temporal_resolution = "daily", #or weekly
      measure = "hospitalizations",
      source = NULL,
      drop_last_date = FALSE
    ),
    silent = TRUE
  )
  
  if (inherits(nat_cov, "try-error")) {
    message("  -> FAILED: ", as.Date(issue_date), " not available. Skipping.")
    return(NULL)
  }
  
  nat_cov <- nat_cov %>%
    dplyr::mutate(issue_date = as.Date(issue_date)) %>%
    #dplyr::filter(date >= as.Date("2020-08-01")) %>% #to remove NA from weekly, still there will be some for some loc. skip for daily
    dplyr::left_join(covidData::fips_codes, by = "location") %>%
    dplyr::transmute(
      date,
      location,
      location_name = ifelse(location_name == "United States", "US", location_name),
      value = inc,
      issue_date = issue_date
    ) %>%
    dplyr::arrange(location, date) %>%
    dplyr::left_join(required_locations, by = "location") %>%
    dplyr::mutate(geo_value = tolower(abbreviation)) %>%
    dplyr::select(
      geo_value,
      time_value = date,
      value,
      location,
      issue_date
    )
  
  outfile <- paste0(
    "data/covid/archive/archive_daily/archived_target_data_",
    as.Date(issue_date),
    ".csv"
  )
  
  readr::write_csv(nat_cov, outfile)
  
  message("  -> Saved: ", outfile)
  return(NULL)
}


# Parallel execution
ncores <- max(1, detectCores() - 1)   # leave 1 core free

mclapply(
  issue_dates,
  process_one_issue,
  mc.cores = ncores
)
