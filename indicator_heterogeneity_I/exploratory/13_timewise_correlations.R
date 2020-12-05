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

min_correlate_date = min(ind_target_sensorized$time_value) - correlate_llim
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

  base_cor_fname = sprintf('results/12_base_cors_%s_%s_%s_%s.RDS',
                           geo_level,
                           source_names[ind_idx], signal_names[ind_idx],
                           target_names[ind_idx])
	df_cor_base = readRDS(base_cor_fname)

  sensorize_val_fname = sprintf('results/12_sensorize_vals_%s_%s_%s_%s.RDS',
                            geo_level,
                            source_names[ind_idx], signal_names[ind_idx],
                            target_names[ind_idx])
	ind_target_sensorized_list = readRDS(sensorize_val_fname)

  ind_target_sensorized = ind_target_sensorized_list[[5]] %>% inner_join (
      joiner_df,
      by='time_value',
    )
  timewise_cors = ind_target_sensorized %>% group_by (
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
  plt = ggplot(timewise_cors,
               aes(x=correlate_date,
                   y=value),
    ) + stat_summary(
      aes(group=1,
          colour='median'),
      fun=median,
      geom="line",
      group=1,
    ) + stat_summary(
      aes(group=1,
          colour='+/- mad'),
      fun=function(x) {median(x) + mad(x)},
      geom="line",
      group=1,
    ) + stat_summary(
      aes(group=1,
          colour='+/- mad'),
      fun=function(x) {median(x) - mad(x)},
      geom="line",
      group=1,
    ) + scale_colour_manual(
        values=c("median"="maroon",
                 "+/- mad"="darkgreen")
    ) + labs(
      colour=''
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
