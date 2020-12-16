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

###############################################################################
# READ IN INDICATOR AND TARGET DATA
###############################################################################
start_day = "2020-03-15"
end_day = NULL
cache_fname = sprintf('cached_data/01_core_indicators_%s.RDS',
                      geo_level)

cached_data = readRDS(cache_fname)
df_signals = cached_data[[1]]
df_cases = cached_data[[2]]
df_deaths = cached_data[[3]]

###############################################################################
# PERFORM DYNAMIC SENSORIZATION
###############################################################################
sensorize_time_ranges = list(
      c(-70, -8)
)
min_sensorization_period = 14

for (ind_idx in 1:length(source_names)) {
  if (target_names[ind_idx] == 'Cases') {
    df_target = df_cases
  } else if (target_names[ind_idx] == 'Deaths') {
    df_target = df_deaths
  } else {
    stop(sprintf("No matching dataframe for target %s.", target_names[ind_idx]))
  }
  ind_df = tibble(df_signals[[ind_idx]])
  ind_target = inner_join(ind_df, tibble(df_target),
                          by=c('geo_value', 'time_value')) %>% select (
        geo_value=geo_value,
        time_value=time_value,
        indicator_value=value.x,
        target_value=value.y,
      )
  sensorize_fname = sprintf('results/03_sensorize_cors_%s_%s_%s_%s.RDS',
                            geo_level,
                            source_names[ind_idx], signal_names[ind_idx],
                            target_names[ind_idx])
  sensorize_val_fname = sprintf('results/03_sensorize_vals_%s_%s_%s_%s.RDS',
                            geo_level,
                            source_names[ind_idx], signal_names[ind_idx],
                            target_names[ind_idx])
  if (!file.exists(sensorize_fname)) {
    sensorize_cors = vector('list', length(sensorize_time_ranges))
    ind_target_sensorized_list = vector('list', length(sensorize_time_ranges))
    for (outer_idx in 1:length(sensorize_time_ranges)) {
      sensorize_llim = sensorize_time_ranges[[outer_idx]][1]
      sensorize_ulim = sensorize_time_ranges[[outer_idx]][2]

      # Consider: include number of days in the sensorization regression as an
      # attribute of the output
      min_sensorize_date = lubridate::ymd(start_day) + min_sensorization_period
      max_sensorize_date = max(ind_target$time_value)
      sensorize_date_offsets = 0:(max_sensorize_date-min_sensorize_date)

      joiner_df_list = vector('list', length(sensorize_date_offsets))
      for (idx in 1:length(sensorize_date_offsets)) {
        dt = sensorize_date_offsets[idx]
        sensorize_date = min_sensorize_date + dt
        joiner_df_list[[idx]] = tibble(
                          sensorize_date = sensorize_date,
                          time_value = sensorize_date + sensorize_llim:sensorize_ulim)
        # Under the new setup, time_value could potentially be before there is
        # data avilable
        # However, the inner_join will drop all of the time_values where data is not
        # available.
      }
      joiner_df = bind_rows(joiner_df_list)

      if (!PARCOMPUTE) {
        ind_sensorized_lm =  ind_target %>% inner_join (
              joiner_df,
              on='time_value',
            ) %>%  group_by (
              geo_value,
              sensorize_date,
            ) %>% group_modify (
              ~ broom::tidy(lm(target_value ~ indicator_value, data = .x))
            ) %>% ungroup
      } else {
        ind_grouped_list =   ind_target %>% inner_join (
              joiner_df,
              on='time_value',
            ) %>%  group_by (
              geo_value,
              sensorize_date,
            ) %>% group_split
        ind_sensorized_lm = parallel::mclapply(ind_grouped_list, function(df) {
            broom::tidy(
              lm(target_value ~ indicator_value, data = df)
            ) %>% mutate (
              geo_value = unique(df$geo_value),
              sensorize_date = unique(df$sensorize_date),
              n_sensorize_days = nrow(df),
            )}, mc.cores = N_CORE) %>% bind_rows
      }
      ind_sensorized_wide = ind_sensorized_lm %>% select(
            geo_value,
            sensorize_date,
            n_sensorize_days,
            term,
            estimate,
          ) %>% mutate (
            term = sapply(term, function(x) {ifelse(x=='(Intercept)',
                                                    'intercept',
                                                    'slope')}),
          ) %>% pivot_wider (
            id_cols = c(geo_value, sensorize_date),
            names_from=term,
            values_from=estimate,
          )
      ind_target_sensorized = ind_target %>% inner_join (
            ind_sensorized_wide,
            by=c('time_value'='sensorize_date',
                 'geo_value'),
          ) %>% mutate (
            sensorized_value = intercept + indicator_value * slope,
          )
      df_sensorized = ind_target_sensorized %>% transmute (
            geo_value=geo_value,
            signal='ind_sensorized',
            time_value=time_value,
            direction=NA,
            issue=lubridate::ymd('2020-11-01'),
            lag=NA,
            value=sensorized_value,
            stderr=NA,
            sample_size=NA,
            data_source='linear_sensorization',
          )
      attributes(df_sensorized)$geo_type = 'county'
      class(df_sensorized) = c("covidcast_signal", "data.frame")

      df_cor_sensorized_ind = covidcast_cor(df_sensorized, df_target,
                                           by='time_value', method='spearman')
      df_cor_sensorized_ind$Indicator = sprintf('Sensorized (TS, %d:%d)',
                                               sensorize_llim,
                                               sensorize_ulim)
      sensorize_cors[[outer_idx]] = df_cor_sensorized_ind
      ind_target_sensorized_list[[outer_idx]] = ind_target_sensorized
    }

    saveRDS(sensorize_cors, sensorize_fname)
    saveRDS(ind_target_sensorized_list, sensorize_val_fname)
  } else {
    sensorize_cors = readRDS(sensorize_fname)
    ind_target_sensorized_list = readRDS(sensorize_val_fname)
  }
}
