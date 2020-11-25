library(tidyverse)
library(covidcast)

source_names = c("doctor-visits", "fb-survey", "fb-survey",
                 "hospital-admissions", "hospital-admissions")
signal_names = c("smoothed_adj_cli", "smoothed_cli", "smoothed_hh_cmnty_cli", 
            "smoothed_adj_covid19_from_claims", "smoothed_adj_covid19_from_claims")
pretty_names = c("Doctor visits", "Facebook CLI", "Facebook CLI-in-community", 
          "Hospitalizations", "Hospitalizations")
target_names = c("Cases", "Cases", "Cases", "Cases", "Deaths")
geo_level = "county"

start_day = "2020-04-15"
end_day = NULL
cache_fname = 'cached_data/10_heterogeneity_core_indicators.RDS'

if (!file.exists(cache_fname)) {
  df_signals = vector("list", length(signal_names))
  for (i in 1:length(signal_names)) {
    df_signals[[i]] = suppressWarnings(
                        covidcast_signal(source_names[i], signal_names[i],
                                         start_day, end_day,
                                         geo_type=geo_level))
  }

  # Fetch USAFacts confirmed case incidence proportion (smoothed with 7-day 
  # trailing average)
  df_cases = suppressWarnings(
              covidcast_signal("usa-facts", "confirmed_7dav_incidence_prop",
                              start_day, end_day,
                              geo_type=geo_level))

  df_deaths = suppressWarnings(
              covidcast_signal("usa-facts", "deaths_7dav_incidence_prop",
                              start_day, end_day,
                              geo_type=geo_level))

  saveRDS(list(df_signals, df_cases, df_deaths), cache_fname)
} else {
  cached_data = readRDS(cache_fname)
  df_signals = cached_data[[1]]
  df_cases = cached_data[[2]]
  df_deaths = cached_data[[3]]
}

case_num = 500
geo_values = suppressWarnings(covidcast_signal("usa-facts", "confirmed_cumulative_num",
                              max(df_cases$time_value), 
                              max(df_cases$time_value))) %>%
  filter(value >= case_num) %>% pull(geo_value)

geo_values = suppressWarnings(covidcast_signal("usa-facts", "confirmed_cumulative_num",
                              '2020-11-01', 
                              '2020-11-01')) %>%
  filter(value >= case_num) %>% pull(geo_value)

sensorize_time_ranges = list(
      c(-7, -1),
      c(-10, -1),
      c(-14, -1),
      c(-21, -1),
      c(-14, -8),
      c(-21, -8),
      c(-28, -8),
      c(-35, -8),
      c(-42, -8))

plot_time_range_idxs = c(1, 5)
plot_time_range_idxs = c(1, 9)

n_plot = 40
plot_geo_values = sample(geo_values, n_plot)
sample_time_values = sample(1:150, n_plot) + lubridate::ymd('2020-06-15')

for (ind_idx in 1:length(source_names)) {
  if (target_names[ind_idx] == 'Cases') {
    df_target = df_cases
  } else if (target_names[ind_idx] == 'Deaths') {
    df_target = df_deaths
  } else {
    stop(sprintf("No matching dataframe for target %s.", target_names[ind_idx]))
  }

  base_cor_fname = sprintf('results/10_base_cors_%s_%s_%s.RDS',
                            source_names[ind_idx], signal_names[ind_idx],
                            target_names[ind_idx])
  sensorize_fname = sprintf('results/10_sensorize_cors_%s_%s_%s.RDS',
                            source_names[ind_idx], signal_names[ind_idx],
                            target_names[ind_idx])
  sensorize_val_fname = sprintf('results/10_sensorize_vals_%s_%s_%s.RDS',
                            source_names[ind_idx], signal_names[ind_idx],
                            target_names[ind_idx])

  df_cor_base = readRDS(base_cor_fname)
  sensorize_cors = readRDS(sensorize_fname)
  sensorized_vals = readRDS(sensorize_val_fname)

  # TODO: Loop through geo values, time values
  plot_geo_value = '36061'
  plot_time_value = lubridate::ymd('2020-08-01')

  data_df = lapply(plot_time_range_idxs, function(tidx) {
      df_signals[[ind_idx]] %>% filter (
        geo_value == plot_geo_value,
        time_value >= plot_time_value + sensorize_time_ranges[[tidx]][1],
        time_value <= plot_time_value + sensorize_time_ranges[[tidx]][2],
      ) %>% mutate (
        window = sprintf('{t%d, ..., t%d}',
                         sensorize_time_ranges[[tidx]][1],
                         sensorize_time_ranges[[tidx]][2])
      )
    }) %>% bind_rows (
    ) %>% select (
      geo_value,
      time_value,
      indicator=value,
      window,
    ) %>% inner_join (
      df_target %>% select (
        geo_value,
        time_value,
        target=value,
      ),
      by=c('geo_value', 'time_value'),
    ) %>% rename (
      time=window,
    )

  sensorized_df = lapply(plot_time_range_idxs, function(tidx) {
      sensorized_vals[[tidx]] %>% filter ( 
        geo_value == plot_geo_value,
        time_value == plot_time_value,
      ) %>% mutate (
        window = sprintf('{t%d, ..., t%d}',
                         sensorize_time_ranges[[tidx]][1],
                         sensorize_time_ranges[[tidx]][2])
      )
    }) %>% bind_rows

  data_df = bind_rows (
      data_df,
      sensorized_df[1, ] %>% transmute (
        geo_value=geo_value,
        time_value=time_value,
        indicator=indicator_value,
        target=target_value,
        time='t',
      ))

  data_df$time = factor(data_df$time,
                        levels=c('t',
                                 sapply(plot_time_range_idxs, function(tidx) {
                                  sprintf('{t%d, ..., t%d}',
                                   sensorize_time_ranges[[tidx]][1],
                                   sensorize_time_ranges[[tidx]][2])})))


  linetypes = c('y~x-1'='solid', 'y~x'='dotted')
  plt = ggplot(
      data_df,
      aes(x=indicator,
          y=target,
          colour=time),
    ) + geom_point (
      size=3,
      alpha=0.5,
    ) + stat_smooth (
      aes(linetype='y~x-1'),
      method='lm',
      se=FALSE,
      formula = y ~ x - 1,
      fullrange=TRUE,
    ) + stat_smooth (
      aes(linetype='y~x'),
      #linetype='dotted',
      method='lm',
      se=FALSE,
      formula = y ~ x,
      fullrange=TRUE,
    ) + scale_colour_manual (
      values = c('#000000',
                 '#00BFC4',
                 '#7CAE00'),
    ) + scale_alpha_manual (
      c(0.8, 0.5, 0.5),
    ) + expand_limits (
      x=0,
      y=0
    ) + geom_vline (
      xintercept = unique(sensorized_df$indicator_value)
    ) + scale_linetype_manual(
      values=linetypes,
    ) + labs (
      x=pretty_names[ind_idx],
      y=target_names[ind_idx],
      linetype='model'
    ) + ggtitle (
      sprintf('Location=%s, t=%s', plot_geo_value, plot_time_value)
    )

  # TODO: print pdf

}


ind_idx = 1
if (target_names[ind_idx] == 'Cases') {
	df_target = df_cases
} else if (target_names[ind_idx] == 'Deaths') {
	df_target = df_deaths
} else {
	stop(sprintf("No matching dataframe for target %s.", target_names[ind_idx]))
}

base_cor_fname = sprintf('results/10_base_cors_%s_%s_%s.RDS',
													source_names[ind_idx], signal_names[ind_idx],
													target_names[ind_idx])
sensorize_fname = sprintf('results/10_sensorize_cors_%s_%s_%s.RDS',
													source_names[ind_idx], signal_names[ind_idx],
													target_names[ind_idx])
sensorize_val_fname = sprintf('results/10_sensorize_vals_%s_%s_%s.RDS',
													source_names[ind_idx], signal_names[ind_idx],
													target_names[ind_idx])

df_cor_base = readRDS(base_cor_fname)
sensorize_cors = readRDS(sensorize_fname)
sensorized_vals = readRDS(sensorize_val_fname)

# TODO: randomly select geo_value
# TODO: randomly select time_value
plot_geo_value = '36061'
plot_time_value = lubridate::ymd('2020-08-01')

data_df = lapply(plot_time_range_idxs, function(tidx) {
		df_signals[[ind_idx]] %>% filter (
			geo_value == plot_geo_value,
			time_value >= plot_time_value + sensorize_time_ranges[[tidx]][1],
			time_value <= plot_time_value + sensorize_time_ranges[[tidx]][2],
		) %>% mutate (
			window = sprintf('{t%d, ..., t%d}',
											 sensorize_time_ranges[[tidx]][1],
											 sensorize_time_ranges[[tidx]][2])
		)
	}) %>% bind_rows (
	) %>% select (
		geo_value,
		time_value,
		indicator=value,
		window,
	) %>% inner_join (
		df_target %>% select (
			geo_value,
			time_value,
			target=value,
		),
		by=c('geo_value', 'time_value'),
	) %>% rename (
		time=window,
	)

sensorized_df = lapply(plot_time_range_idxs, function(tidx) {
		sensorized_vals[[tidx]] %>% filter ( 
			geo_value == plot_geo_value,
			time_value == plot_time_value,
		) %>% mutate (
			window = sprintf('{t%d, ..., t%d}',
											 sensorize_time_ranges[[tidx]][1],
											 sensorize_time_ranges[[tidx]][2])
		)
	}) %>% bind_rows

data_df = bind_rows (
		data_df,
		sensorized_df[1, ] %>% transmute (
			geo_value=geo_value,
			time_value=time_value,
			indicator=indicator_value,
			target=target_value,
			time='t',
		))

data_df$time = factor(data_df$time,
											levels=c('t',
															 sapply(plot_time_range_idxs, function(tidx) {
																sprintf('{t%d, ..., t%d}',
																 sensorize_time_ranges[[tidx]][1],
																 sensorize_time_ranges[[tidx]][2])})))


plt = ggplot(
		data_df,
		aes(x=indicator,
				y=target,
				colour=time),
	) + geom_point (
		size=5,
		alpha=0.5,
	) + stat_smooth (
		method='lm',
		se=FALSE,
		formula = y ~ x - 1,
		fullrange=TRUE,
	) + scale_colour_manual (
		values = c('#000000',
							 '#00BFC4',
							 '#7CAE00'),
	) + scale_alpha_manual (
		c(0.8, 0.5, 0.5),
	) + expand_limits (
		x=0,
		y=0
	) + geom_point (
		data=sensorized_df,
		aes(x=indicator_value,
				y=sensorized_value),
		colour='black',
		size=5,
		alpha=0.8,
		pch=1,
	)

