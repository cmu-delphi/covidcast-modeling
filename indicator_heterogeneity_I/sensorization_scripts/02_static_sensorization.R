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

if (args[1] == 'state') {
  geo_level = 'state'
} else {
  geo_level = 'county'
}

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
# PERFORM STATIC SENSORIZATION
###############################################################################
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
	attributes(df_global_sensorized)$geo_type = 'county'
	attributes(df_global_sensorized)$metadata$geo_type = 'county'
	class(df_global_sensorized) = c("covidcast_signal", "data.frame")

  base_cor_fname = sprintf('results/02_base_cors_%s_%s_%s_%s.RDS',
                           geo_level,
                           source_names[ind_idx], signal_names[ind_idx],
                           target_names[ind_idx])
  if (!file.exists(base_cor_fname)) {
    df_cor_base_ind = covidcast_cor(df_signals[[ind_idx]], df_target,
                                   by='time_value', method='spearman')
    df_cor_sensorized_ind = covidcast_cor(df_global_sensorized, df_target,
                                         by='time_value', method='spearman')
    df_cor_base = rbind(df_cor_base_ind, df_cor_sensorized_ind)
    df_cor_base$Indicator = as.factor(c(rep('raw', nrow(df_cor_base_ind)),
                                        rep('static',
                                            nrow(df_cor_sensorized_ind))))
    saveRDS(df_cor_base, base_cor_fname)
  } else {
    df_cor_base = readRDS(base_cor_fname)
  }
}


