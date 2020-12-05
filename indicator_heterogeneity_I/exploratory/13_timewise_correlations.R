library(tidyverse)
library(covidcast)

source_names = c("doctor-visits", "fb-survey", "fb-survey",
                 "hospital-admissions", "hospital-admissions")
signal_names = c("smoothed_adj_cli", "smoothed_cli", "smoothed_hh_cmnty_cli", 
            "smoothed_adj_covid19_from_claims", "smoothed_adj_covid19_from_claims")
pretty_names = c("Doctor visits", "Facebook CLI", "Facebook CLI-in-community", 
          "Hospitalizations", "Hospitalizations")
target_names = c("Cases", "Cases", "Cases", "Cases", "Deaths")
geo_level = 'county'

cache_fname = sprintf('cached_data/12_heterogeneity_core_indicators_%s.RDS',
                      geo_level)

cached_data = readRDS(cache_fname)
df_signals = cached_data[[1]]
df_cases = cached_data[[2]]
df_deaths = cached_data[[3]]

case_num = 500

if (geo_level == 'county') {
  geo_values = suppressWarnings(covidcast_signal("usa-facts", "confirmed_cumulative_num",
                                '2020-11-01', 
                                '2020-11-01')) %>%
    filter(value >= case_num) %>% pull(geo_value)
} else if (geo_level == 'state') {
  geo_values = unique(df_signals[[1]]$geo_value)
}

# Read in dataframe of sensorized values
sensorize_time_ranges = list(
      c(-42, -8),
      c(-49, -8),
      c(-56, -8),
      c(-63, -8),
      c(-70, -8)
)
splot_idx = 5

ind_idx = 1
sensorize_val_fname = sprintf('results/12_sensorize_vals_%s_%s_%s_%s.RDS',
													geo_level,
													source_names[ind_idx], signal_names[ind_idx],
													target_names[ind_idx])
ind_target_sensorized_list = readRDS(sensorize_val_fname)

ind_target_sensorized = ind_target_sensorized_list[[5]]

correlate_llim = -42
correlate_ulim = -8

min_correlate_date = lubridate::ymd('2020-04-15') - correlate_llim
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

  ind_df = tibble(df_signals[[ind_idx]]) %>% filter(geo_value %in% geo_values)
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

  sensorize_val_fname = sprintf('results/12_sensorize_vals_%s_%s_%s_%s.RDS',
                            geo_level,
                            source_names[ind_idx], signal_names[ind_idx],
                            target_names[ind_idx])
	ind_target_sensorized_list = readRDS(sensorize_val_fname)

  ind_target_sensorized = ind_target_sensorized_list[[5]] %>% inner_join (
      joiner_df,
      by='time_value',
    )

  timewise_cors_raw = ind_df %>% inner_join (
      joiner_df,
      by='time_value',
    )%>% group_by (
      #geo_value,
      correlate_date,
    ) %>% group_modify (
      function(x, y) {
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
        return(covidcast_cor(df_, df_target,
                             by='geo_value', method='spearman'))
      }
    )
  timewise_cors_static = ind_global_sensorized %>% inner_join (
      joiner_df,
      by='time_value',
    )%>% group_by (
      #geo_value,
      correlate_date,
    ) %>% group_modify (
      function(x, y) {
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
        return(covidcast_cor(df_, df_target,
                             by='geo_value', method='spearman'))
      }
    )
  timewise_cors_dynamic = ind_target_sensorized %>% group_by (
      #geo_value,
      correlate_date,
    ) %>% group_modify (
      function(x, y) {
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
        return(covidcast_cor(df_, df_target,
                             by='geo_value', method='spearman'))
      }
    )
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

  plt = ggplot(timewise_cors_summarized,
               aes(x=correlate_date,
                   colour=sensorization),
    ) + geom_line (
      aes(y=med,
          linetype='median'),
    ) + geom_line (
      aes(y=med+mad,
          linetype='+/- mad'),
    ) + geom_line (
      aes(y=med-mad,
          linetype='+/- mad'),
    ) + geom_line (
      aes(y=max,
          linetype='extrema'),
    ) + geom_line (
      aes(y=min,
          linetype='extrema'),
    ) + scale_linetype_manual(
        values=c("median"="solid",
                 "+/- mad"="dashed",
                 "extrema"="dotted")
    )

}


# Choose the evaluation timeframe

# create a joiner df, create list of dfs

# for each df, massage it into a "covidcast" dataframe, 
# and then call the timewise correlation function on it

# resulting dataframe will have a value for every geo_value. 
# associate it with an correlation_date and bind all the rows
# into a massive dataframeo

# for each correlation date (and for each sensor), calculate 
# med and mad; then make the plot
