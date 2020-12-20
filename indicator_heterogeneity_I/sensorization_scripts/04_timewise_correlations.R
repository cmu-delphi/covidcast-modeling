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
target_names = c("Cases", "Cases", "Cases", "Cases", "Deaths")

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

splot_idx = 1

ind_idx = 1
sensorize_val_fname = sprintf('results/03_sensorize_vals_%s_%s_%s_%s.RDS',
													geo_level,
													source_names[ind_idx], signal_names[ind_idx],
													target_names[ind_idx])
ind_target_sensorized_list = readRDS(sensorize_val_fname)

ind_target_sensorized = ind_target_sensorized_list[[splot_idx]]

correlate_llim = -41
correlate_ulim = 0

min_correlate_date = lubridate::ymd(start_day) - correlate_llim
max_correlate_date = max(ind_target_sensorized$time_value)
correlate_date_offsets = 0:(max_correlate_date-min_correlate_date)

joiner_df_list = vector('list', length(correlate_date_offsets))
for (idx in 1:length(correlate_date_offsets)) {
	dt = correlate_date_offsets[idx]
	correlate_date = min_correlate_date + dt
	joiner_df_list[[idx]] = tibble(
										correlate_date = correlate_date,
										time_value = correlate_date + correlate_llim:correlate_ulim)
}
joiner_df = bind_rows(joiner_df_list)


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

  sensorize_val_fname = sprintf('results/03_sensorize_vals_%s_%s_%s_%s.RDS',
                            geo_level,
                            source_names[ind_idx], signal_names[ind_idx],
                            target_names[ind_idx])
	ind_target_sensorized_list = readRDS(sensorize_val_fname)

  ind_target_sensorized = ind_target_sensorized_list[[splot_idx]] %>% inner_join (
      joiner_df,
      by='time_value',
    )

  timewise_cors_raw = parallel::mclapply(ind_df %>% inner_join (
      joiner_df,
      by='time_value',
    )%>% group_by (
      #geo_value,
      correlate_date,
    ) %>% group_split, function (df) {
        covidcast_cor(df, df_target,
                      by='geo_value', method='spearman') %>% mutate (
          correlate_date = unique(df$correlate_date)
        )
    }, mc.cores=N_CORE) %>% bind_rows
  timewise_cors_static = parallel::mclapply(
    ind_global_sensorized %>% inner_join (
      joiner_df,
      by='time_value',
    ) %>% group_by (
      #geo_value,
      correlate_date,
    ) %>% group_split, function(x) {
        df_ = x %>% transmute (
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
        attributes(df_)$geo_type = geo_level
        class(df_) = c("covidcast_signal", "data.frame")
        covidcast_cor(df_, df_target,
                      by='geo_value', method='spearman') %>% mutate (
          correlate_date = unique(x$correlate_date)
        )
    }, mc.cores=N_CORE) %>% bind_rows

  timewise_cors_dynamic = parallel::mclapply(
    ind_target_sensorized %>% group_by (
      correlate_date,
    ) %>% group_split, function(x) {
        df_ = x %>% transmute (
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
        attributes(df_)$geo_type = geo_level
        class(df_) = c("covidcast_signal", "data.frame")
        covidcast_cor(df_, df_target,
                      by='geo_value', method='spearman') %>% mutate (
          correlate_date = unique(x$correlate_date))
    }, mc.cores=N_CORE) %>% bind_rows
  timewise_cors = bind_rows(
      timewise_cors_raw %>% mutate(sensorization='raw'),
      timewise_cors_static %>% mutate(sensorization='static'),
      timewise_cors_dynamic %>% mutate(sensorization='dynamic'),
    )
  timewise_cors_summarized = timewise_cors %>% group_by (
      sensorization,
      correlate_date,
    ) %>% summarize (
      med = median(value, na.rm=TRUE),
      mad = mad(value, na.rm=TRUE),
      max = max(value, na.rm=TRUE),
      min = min(value, na.rm=TRUE),
    ) %>% ungroup

  timewise_cor_fname = sprintf('results/04_timewise_cors_%s_%s_%s_%s_%d_%d.RDS',
                               geo_level,
                               source_names[ind_idx], signal_names[ind_idx],
                               target_names[ind_idx],
                               correlate_llim, correlate_ulim)
  saveRDS(list(timewise_cors, timewise_cors_summarized), timewise_cor_fname)

}
