library(covidcast)
library(tidyverse)
# Import
source('delphi_epidata.R')
# Fetch data
date_range = Epidata$range(20200415, 20201202)
res <- Epidata$covid_hosp(list('pa'),
                          list(date_range))

safe_subset = function(x) {
  if (is.null(x)) {
    return(NA)
  } else {
    return(x)
  }
}

state_abbrs = covidcast::state_geo$abbr

hhs_hosp_df = lapply(state_abbrs, function(state) {
}


state_df = lapply(res$epidata, function(row) {
  tibble(
      geo_value = row[['state']],
      # shift date backwards because data is reported about previous day
      time_value = lubridate::ymd(row[['date']])-1,
      issue = lubridate::ymd(row[['issue']]),
      adult_confirmed = safe_subset(
            row[['previous_day_admission_adult_covid_confirmed']]),
      adult_suspected = safe_subset(
            row[['previous_day_admission_adult_covid_suspected']]),
      pediatric_confirmed = safe_subset(
            row[['previous_day_admission_pediatric_covid_confirmed']]),
      pediatric_suspected = safe_subset(
            row[['previous_day_admission_pediatric_covid_suspected']]),
    )
}) %>% bind_rows
