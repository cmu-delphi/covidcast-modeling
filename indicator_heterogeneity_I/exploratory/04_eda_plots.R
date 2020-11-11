library(tidyverse)
library(covidcast)

## INGEST DATA
# Fetch the following sources and signals from the API 
# TODO: Add Google Symptoms "eventually"

source_names = c("doctor-visits", "fb-survey", "fb-survey", "hospital-admissions")
signal_names = c("smoothed_adj_cli", "smoothed_cli", "smoothed_hh_cmnty_cli", 
            "smoothed_adj_covid19")
pretty_names = c("Doctor visits", "Facebook CLI", "Facebook CLI-in-community", 
          "Hospitalizations")
target_names = c("Cases", "Cases", "Cases", "Deaths")
geo_level = "county"

start_day = "2020-04-15"
end_day = NULL
cache_fname = 'cached_data/03_heterogeneity_core_indicators.RDS'

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

  case_num = 500
  geo_values = suppressWarnings(covidcast_signal("usa-facts", "confirmed_cumulative_num",
                                max(df_cases$time_value), 
                                max(df_cases$time_value))) %>%
    filter(value >= case_num) %>% pull(geo_value)
  saveRDS(list(df_signals, df_cases, df_deaths), cache_fname)
} else {
  cached_data = readRDS(cache_fname)
  df_signals = cached_data[[1]]
  df_cases = cached_data[[2]]
  df_deaths = cached_data[[3]]
}

## HELPER FUNCTION
plot_points = function(plt) {
  plt + geom_point(
    aes(
      x=indicator_value,
      y=target_value,
      color=time_value,
    ),
    alpha=ALPHA,
  ) + scale_colour_viridis_c(
    trans='date',
  ) + facet_wrap (
    vars(county_name_fips),
    nrow=nr,
    scales='free',
  )
}

## PROCESS THE STUFF
pop_tibble = covidcast::county_census %>% filter (
      COUNTY != 0,
    ) %>% select (
      STATE,
      COUNTY,
      POPESTIMATE2019,
    ) %>% transmute (
      geo_value = sprintf('%02d%03d', STATE, COUNTY),
      population = POPESTIMATE2019,
    ) %>% tibble

county_tibble = covidcast::county_geo %>% transmute (
      geo_value = fips,
      county_name=county,
      state=abbr,
      county_name_fips = sprintf('FIPS: %s\n%s, %s',
                                 geo_value, county_name, state),
    ) %>% inner_join (
      pop_tibble,
      on='geo_value',
    )



ALPHA=0.8
nr=4
nc=8
top_counties = pop_tibble %>% arrange(
      -population,
    ) %>% head (
      nr * nc,
    ) %>% pull (
      geo_value,
    )


for (ind_idx in 1:length(signal_names)) {
  ## DO THE PLOTTING
  set.seed(20201111)
  n_random_counties = 5
  n_other_plots = 1

  #ind_idx = 1

  plot_list = vector('list', n_random_counties+n_other_plots)

  ind = tibble(df_signals[[1]])
  if (target_names[ind_idx] == 'Cases') {
    targets = tibble(df_cases)
  } else if (target_names[ind_idx] == 'Deaths') {
    targets = tibble(df_deaths)
  } else {
    stop(sprintf('No corresponding df for %s', target_names[ind_idx]))
  }
  ind_targets = inner_join(ind, targets, by=c('geo_value', 'time_value')) %>% select (
        geo_value=geo_value,
        time_value=time_value,
        indicator_value=value.x,
        target_value=value.y,
      ) %>% inner_join(
        county_tibble,
        on='geo_value',
      )



  ## TOP COUNTIES
  plt = ind_targets %>% filter (
        geo_value %in% top_counties,
      ) %>% ggplot (
      ) %>% plot_points (
      ) + ggtitle (
        sprintf("%s: Top 32 Counties", pretty_names[ind_idx])
        ) + ylab (
          sprintf("%s Rate", target_names[ind_idx])
      )
  plot_list[[1]] = plt


  ## RANDOM COUNTIES
  rand_fips = sample(unique(ind_targets$geo_value), nr*nc*5)
  for (idx in 1:n_random_counties) {

    plt = ind_targets %>% filter (
          geo_value %in% rand_fips[(nr*nc*(idx-1)+1):(nr*nc*idx)],
        ) %>% ggplot (
        ) + geom_point(
          aes(
            x=indicator_value,
            y=target_value,
            color=time_value,
          ),
          alpha=ALPHA,
        ) + scale_colour_viridis_c(
          trans='date',
        ) + facet_wrap (
          vars(county_name_fips),
          nrow=nr,
          scales='free',
        ) + ggtitle (
          sprintf("%s: Random Counties, Set %d",
                  pretty_names[ind_idx],
                  idx)
        ) + ylab (
          sprintf("%s Rate", target_names[ind_idx])
        )
    plot_list[[n_other_plots+idx]] = plt
  }


  ggsave(sprintf('fig/county_plots_%s_%s.pdf',
                 source_names[ind_idx], signal_names[ind_idx]),
         plot=gridExtra::marrangeGrob(plot_list, nrow=1, ncol=1),
         width=15, height=8)
}
