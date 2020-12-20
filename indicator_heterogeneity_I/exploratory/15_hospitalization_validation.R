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

# Loop over states because Epidata only returns 3650 rows
hhs_hosp_df = lapply(state_abbrs, function(state) {
  res = Epidata$covid_hosp(list(state),
                           list(date_range))
  lapply(res$epidata, function(row) {
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
}) %>% bind_rows

hhs_hosp_df$geo_value = stringr::str_to_lower(hhs_hosp_df$geo_value)



availability = hhs_hosp_df %>% group_by (
    time_value,
  ) %>% summarise (
    prop_adult_confirmed = mean(!is.na(adult_confirmed)),
    prop_adult_suspected = mean(!is.na(adult_suspected)),
    prop_pediatric_confirmed = mean(!is.na(pediatric_confirmed)),
    prop_pediatric_suspected = mean(!is.na(pediatric_suspected)),
  )
plt = ggplot(
    availability,
    aes(x=time_value),
  ) + geom_line (
    aes(y=prop_adult_confirmed),
  ) + geom_line (
    aes(y=prop_adult_suspected),
  ) + geom_line (
    aes(y=prop_pediatric_confirmed),
  ) + geom_line (
    aes(y=prop_pediatric_suspected),
  )
# Availability really picks up in July - July 14th, to be precise

# calculate incidence
state_pop = covidcast::state_census %>% transmute (
    geo_value = stringr::str_to_lower(ABBR),
    population = POPESTIMATE2019,
  )
hhs_hosp_df = hhs_hosp_df %>% inner_join (
    state_pop,
    by='geo_value',
  ) %>% mutate (
    adult_confirmed_incidence = adult_confirmed / population * 100000,
    adult_suspected_incidence = adult_suspected / population * 100000,
    pediatric_confirmed_incidence = pediatric_confirmed / population * 100000,
    pediatric_suspected_incidence = pediatric_suspected / population * 100000,
  ) 

saveRDS(hhs_hosp_df, 'results/15_hosp_data.RDS')


hhs_signal_names = c('adult_confirmed_incidence',
                     'adult_confirmed_suspected_incidence',
                     'adult_pediatric_confirmed_incidence',
                     'adult_pediatric_confirmed_suspected_incidence')
hhs_signal_names_pretty = c('Adult: Confirmed',
                            'Adult: Confirmed & Suspected',
                            'Adult, Pediatric: Confirmed',
                            'Adult, Pediatric: Confirmed & Suspected')
hhs_signal_formulae = c('adult_confirmed_incidence',
                       'adult_confirmed_incidence+adult_suspected_incidence',
                       'adult_confirmed_incidence+pediatric_confirmed_incidence',
                       paste0('adult_confirmed_incidence',
                              '+pediatric_confirmed_incidence',
                              '+adult_suspected_incidence',
                              '+pediatric_suspected_incidence'))
cor_target_dfs = lapply(1:4, function(idx) {
  hhs_hosp_df %>% transmute (
      geo_value=geo_value,
      signal=hhs_signal_names[idx],
      time_value=time_value,
      direction=NA,
      issue=issue,
      lag=NA,
      value=eval(parse(text=hhs_signal_formulae[idx])),
      stderr=NA,
      sample_size=NA,
      data_source='hhs',
    ) %>% filter (
      !is.na(value),
    ) %>% filter (
      time_value > '2020-07-14',
    )
  })

# Read in sensorized hosp and calculate geo-wise correlations

source_names = c("doctor-visits", "fb-survey", "fb-survey",
                 "hospital-admissions", "hospital-admissions")
signal_names = c("smoothed_adj_cli", "smoothed_cli", "smoothed_hh_cmnty_cli", 
            "smoothed_adj_covid19_from_claims", "smoothed_adj_covid19_from_claims")
pretty_names = c("Doctor visits", "Facebook CLI", "Facebook CLI-in-community", 
          "Hospitalizations", "Hospitalizations")
target_names = c("Cases", "Cases", "Cases", "Cases", "Deaths")
geo_level = 'state'

cache_fname = sprintf('cached_data/12_heterogeneity_core_indicators_%s.RDS',
                      geo_level)

cached_data = readRDS(cache_fname)
df_signals = cached_data[[1]]
df_cases = cached_data[[2]]
df_deaths = cached_data[[3]]

sensorize_time_ranges = list(
      c(-42, -8),
      c(-49, -8),
      c(-56, -8),
      c(-63, -8),
      c(-70, -8)
)
splot_idx = 5

ind_idx = 4
cor_idx = 1

ind_idxs = 1:5
cor_idxs = 1:4

results_list = vector('list', length(ind_idxs) * length(cor_idxs))
total_idx = 1
for (ind_idx in ind_idxs) {
  for (cor_idx in cor_idxs) {
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
    ind_global_sensorized =  ind_target %>% group_by (
          geo_value,
        ) %>% group_modify ( ~ {
          fit = lm(target_value ~ indicator_value, data =.x);
          tibble(time_value=.x$time_value,
                 indicator_value=.x$indicator_value,
                 target_value=.x$target_value,
                 sensorized_value=fit$fitted.values)
        }) %>% ungroup
    df_global_sensorized = ind_global_sensorized %>% transmute (
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


    sensorize_val_fname = sprintf('results/12_sensorize_vals_%s_%s_%s_%s.RDS',
                              geo_level,
                              source_names[ind_idx], signal_names[ind_idx],
                              target_names[ind_idx])
    ind_target_sensorized_list = readRDS(sensorize_val_fname)
    ind_target_sensorized = ind_target_sensorized_list[[splot_idx]]
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

    raw_cor = covidcast_cor(df_signals[[ind_idx]],
                            cor_target_dfs[[cor_idx]],
                            by='time_value',
                            method='spearman')
    raw_cor$sensorization = 'raw'
    static_cor = covidcast_cor(df_global_sensorized,
                               cor_target_dfs[[cor_idx]],
                               by='time_value',
                               method='spearman')
    static_cor$sensorization = 'static'
    dynamic_cor = covidcast_cor(df_sensorized,
                                cor_target_dfs[[cor_idx]],
                                by='time_value',
                                method='spearman')
    dynamic_cor$sensorization = 'dynamic'
    result_df = bind_rows(raw_cor, static_cor, dynamic_cor)
    result_df$sensorization = factor(result_df$sensorization,
                                     levels=c('raw',
                                              'static',
                                              'dynamic'))
    result_df$indicator = pretty_names[ind_idx]
    result_df$sensorize_target = target_names[ind_idx]
    result_df$correlate_target = hhs_signal_names_pretty[cor_idx]
    results_list[[total_idx]] = result_df
    total_idx = total_idx + 1
  }
}
cor_df = bind_rows(results_list)
cor_df = cor_df %>% mutate (
    sensor_target = sprintf("Indicator: %s\nSensorization target: %s",
                            indicator,
                            sensorize_target)
  ) %>% filter (
    !is.na(value),
  )
cor_df$correlate_target = factor(cor_df$correlate_target,
                                 levels=hhs_signal_names_pretty)

saveRDS(cor_df, 'results/15_hhs_cor_df.RDS')


plt = ggplot(
    cor_df,
    aes(x=time_value,
        y=value,
        colour=sensorization)
  ) + geom_line (
  ) + facet_grid (
    rows = vars(correlate_target),
    cols = vars(sensor_target),
  ) + theme(
    legend.position = "bottom",
    legend.title = element_blank()
  )
