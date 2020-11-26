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

case_num = 5000
geo_values = suppressWarnings(covidcast_signal("usa-facts", "confirmed_cumulative_num",
                              max(df_cases$time_value), 
                              max(df_cases$time_value))) %>%
  filter(value >= case_num) %>% pull(geo_value)

county_tibble = covidcast::county_geo %>% transmute (
      geo_value = fips,
      county_name=county,
      state=abbr,
      county_name_fips = sprintf('%s, %s (%s)',
                                 county_name, state, geo_value),
    )

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
set.seed(0)
plot_geo_values = sample(geo_values, n_plot)
plot_time_values = sample(1:150, n_plot) + lubridate::ymd('2020-06-15')

nr=2
nc=2


for (ind_idx in 1:length(source_names)) {
  print(ind_idx)
  plot_list = vector('list', n_plot / (nr*nc))
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


  for (page_idx in 1:length(plot_list)) {
    data_df = lapply(1:4, function(plot_idx) {
      plot_geo_value = plot_geo_values[(page_idx-1)*nr*nc+plot_idx];
      plot_time_value = plot_time_values[(page_idx-1)*nr*nc+plot_idx];
      lapply(plot_time_range_idxs, function(tidx) {
          df_signals[[ind_idx]] %>% filter (
            geo_value == plot_geo_value,
            time_value >= plot_time_value + sensorize_time_ranges[[tidx]][1],
            time_value <= plot_time_value + sensorize_time_ranges[[tidx]][2],
          ) %>% mutate (
            time = sprintf('{t%d, ..., t%d}',
                             sensorize_time_ranges[[tidx]][1],
                             sensorize_time_ranges[[tidx]][2])
          )
        }) %>% bind_rows (
          df_signals[[ind_idx]] %>% filter (
            geo_value == plot_geo_value,
            time_value == plot_time_value,
          ) %>% mutate (
            time='t',
          )
        ) %>% select (
          geo_value,
          time_value,
          indicator=value,
          time,
        ) %>% inner_join (
          df_target %>% select (
            geo_value,
            time_value,
            target=value,
          ),
          by=c('geo_value', 'time_value'),
        ) %>% inner_join (
          county_tibble,
          by='geo_value',
        ) %>% mutate (
          county_time = sprintf('%s\nt = %s',
                                county_name_fips,
                                plot_time_value),
        )
    }) %>% bind_rows



    data_df$time = factor(data_df$time,
                          levels=c('t',
                                   sapply(plot_time_range_idxs, function(tidx) {
                                    sprintf('{t%d, ..., t%d}',
                                     sensorize_time_ranges[[tidx]][1],
                                     sensorize_time_ranges[[tidx]][2])})))


    linetypes = c('y~x-1'='solid', 'y~x'='dotted')
    data_df$t_indicator = data_df$indicator * ifelse(data_df$time=='t',
                                                     1,
                                                     NA)
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
        method='lm',
        se=FALSE,
        formula = y ~ x,
        fullrange=TRUE,
      ) + scale_colour_manual (
        values = c('#000000',
                   '#00BFC4',
                   '#7CAE00'),
      ) + scale_alpha_manual (
        c(0.7, 0.5, 0.5),
      ) + expand_limits (
        x=0,
        y=0
      ) + geom_vline (
        aes(xintercept = t_indicator),
      ) + scale_linetype_manual(
        values=linetypes,
      ) + labs (
        x=pretty_names[ind_idx],
        y=paste(target_names[ind_idx], 'Rate'),
        linetype='model'
      #) + ggtitle (
      #  sprintf('%s\n t=%s',
      #          unique(data_df$county_name_fips),
      #          plot_time_value)
      ) + facet_wrap (
        vars(county_time),
        nrow=nr,
        scales='free',
      ) + ggtitle(
        sprintf('%s, Page %d',
                pretty_names[ind_idx],
                page_idx)
      )
    plot_list[[page_idx]] = plt
  }
  plot_list = lapply(plot_list, ggplotGrob)
  ggsave(sprintf('fig/sanity_check_plots_%s_%s_%s.pdf',
                 source_names[ind_idx],
                 signal_names[ind_idx],
                 target_names[ind_idx]),
         plot=gridExtra::marrangeGrob(plot_list, nrow=1, ncol=1),
         width=15, height=8)
}
