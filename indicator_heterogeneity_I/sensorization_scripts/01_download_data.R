library(tidyverse)
library(covidcast)

args = commandArgs(trailingOnly=TRUE)
###############################################################################
# SET GLOBAL PARAMS
###############################################################################
PARCOMPUTE = TRUE
N_CORE = parallel::detectCores()

source_names = c("doctor-visits", "fb-survey", "fb-survey",
                 "hospital-admissions", "hospital-admissions")
signal_names = c("smoothed_adj_cli", "smoothed_cli", "smoothed_hh_cmnty_cli", 
            "smoothed_adj_covid19_from_claims", "smoothed_adj_covid19_from_claims")
pretty_names = c("Doctor visits", "Facebook CLI", "Facebook CLI-in-community", 
          "Hospitalizations", "Hospitalizations")

geo_level = args[1]
case_num = 500

# Get geo_values for analysis
if (geo_level == 'county') {
  geo_values = suppressWarnings(covidcast_signal("usa-facts", "confirmed_cumulative_num",
                                '2020-11-01', 
                                '2020-11-01')) %>%
    filter(value >= case_num) %>% pull(geo_value)
} else if (geo_level == 'state') {
  geo_values = stringr::str_to_lower(covidcast::state_geo$abbr)
}


###############################################################################
# DOWNLOAD INDICATOR AND TARGET DATA
###############################################################################
start_day = "2020-03-15"
end_day = NULL
cache_fname = sprintf('cached_data/01_core_indicators_%s.RDS',
                      geo_level)

if (!file.exists(cache_fname)) {
  df_signals = vector("list", length(signal_names))
  for (i in 1:length(signal_names)) {
    df_signals[[i]] = suppressWarnings(
                        covidcast_signal(source_names[i], signal_names[i],
                                         start_day, end_day,
                                         geo_type=geo_level)
      ) %>% filter (
        geo_value %in% geo_values,
      )
  }

  # Fetch USAFacts confirmed case incidence proportion (smoothed with 7-day 
  # trailing average)
  df_cases = suppressWarnings(
              covidcast_signal("usa-facts", "confirmed_7dav_incidence_prop",
                              start_day, end_day,
                              geo_type=geo_level)
    ) %>% filter (
      geo_value %in% geo_values,
    )

  df_deaths = suppressWarnings(
              covidcast_signal("usa-facts", "deaths_7dav_incidence_prop",
                              start_day, end_day,
                              geo_type=geo_level)
    ) %>% filter (
      geo_value %in% geo_values,
    )
  saveRDS(list(df_signals, df_cases, df_deaths), cache_fname)
} else {
  cached_data = readRDS(cache_fname)
  df_signals = cached_data[[1]]
  df_cases = cached_data[[2]]
  df_deaths = cached_data[[3]]
}


