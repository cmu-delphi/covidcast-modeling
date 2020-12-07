library(covidcast)
library(dplyr)
library(tidyr)
library(ggplot2)

df <- structure(list(type = structure(1:6, .Label = c("a", "b", "c", "d", "e",
                                                      "f"), class = "factor"),
                     value = 1:6), .Names = c("type", "value"), class =
"data.frame", row.names = c(NA, -6L))
p <- ggplot(df, aes(x = value, y = value, color = type)) +
    geom_point(shape = 21, size = 4)
ggplot_colors =  ggplot_build(p)$data[[1]]$colour

source_names = c("doctor-visits", "fb-survey", "fb-survey",
                 "hospital-admissions", "hospital-admissions")
signal_names = c("smoothed_adj_cli", "smoothed_cli", "smoothed_hh_cmnty_cli", 
            "smoothed_adj_covid19_from_claims", "smoothed_adj_covid19_from_claims")
pretty_names = c("Doctor visits", "Facebook CLI", "Facebook CLI-in-community", 
          "Hospitalizations", "Hospitalizations")
target_names = c("Cases", "Cases", "Cases", "Cases", "Deaths")
sensor_target_names = sapply(1:5, function(ind_idx) {
  sprintf('%s\nTarget: %s', pretty_names[ind_idx], target_names[ind_idx])
         })
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


model_names = c('Targets',
                'Targets+Raw',
                'Targets+Static',
                'Targets+Dynamic')

lags = 1:2 * -7 
leads = 1:2 * 7

geo_list = vector('list', length(source_names))
for (ind_idx in 1:length(source_names)) {
  predictive_fname = sprintf('results/14_predictive_%s_%s_%s_%s.RDS', geo_level,
                               source_names[ind_idx], signal_names[ind_idx],
                               target_names[ind_idx])

  res = readRDS(predictive_fname)
  geo_list[[ind_idx]] = unique(res$geo_value)
}

geo_values = Reduce(intersect, geo_list)

plot_df_list = vector('list', length(source_names))
for (ind_idx in 1:length(source_names)) {
  predictive_fname = sprintf('results/14_predictive_%s_%s_%s_%s.RDS', geo_level,
                               source_names[ind_idx], signal_names[ind_idx],
                               target_names[ind_idx])

  res = readRDS(predictive_fname)


  # Restrict to common period for all 4 models, then calculate the scaled errors 
  # for each model, that is, the error relative to the strawman's error
  res_all4 = res %>%
  #res_all4 = res %>% filter(geo_value %in% geo_values) %>%
    drop_na() %>%                                       # Restrict to common time
    mutate(err1 = err1 / err0, err2 = err2 / err0,      # Compute relative error
           err3 = err3 / err0, err4 = err4 / err0) %>%  # to strawman model
    mutate(dif12 = err1 - err2, dif13 = err1 - err3,    # Compute differences
           dif14 = err1 - err4) %>%                     # relative to cases model
    ungroup() %>%
    select(-err0) 
           
  # Calculate and print median errors, for all 4 models, and just 7 days ahead
  if (ind_idx > 1) {res_all4 = rename(res_all4, lead=lead_)}
  res_err4 = res_all4 %>% 
    select(-starts_with("dif")) %>%
    pivot_longer(names_to = "model", values_to = "err",
                 cols = -c(geo_value, time_value, lead)) %>%
    mutate(lead = factor(lead, labels = paste(leads, "days ahead")),
           model = factor(model, labels = model_names))

  plot_df = res_err4 %>% group_by(
      model, lead, time_value
    ) %>% summarize(
      med = median(err),
      mad = mad(err),
      min = min(err),
      max = max(err),
    ) %>% ungroup()
  plot_df$sensor_target = sensor_target_names[ind_idx]
  plot_df_list[[ind_idx]] = plot_df
}
plot_df = bind_rows(plot_df_list)

plt = ggplot(
    plot_df,
    aes(x=time_value,
        colour=model)
  ) + geom_line (
    aes(y=med,
        linetype='median'),
  ) + geom_line (
    aes(y=med+mad,
        linetype='med±mad'),
  ) + geom_line (
    aes(y=med-mad,
        linetype='med±mad'),
  ) + scale_linetype_manual(
      values=c("median"="solid",
               "med±mad"="dashed"),
      breaks=c('median', "med±mad")
	) + scale_color_manual(
		values = c("black",
               '#F8766D',
               '#00BA38',
               '#619CFF')
  ) + geom_hline(
    yintercept = 1,
    linetype = 2,
    color = "gray"
  ) + facet_grid(
    cols=vars(lead),
    rows=vars(sensor_target),
    scales='free',
  ) + ylim (
    0, 2
  ) + labs(
    x = "Date",
    y = "Scaled error"
  ) + theme_bw (
  ) + theme(
    legend.pos = "bottom",
    legend.title = element_blank()
  )
# cases only model behaves differently because they are trained globlaly over
# all locations and it differs by sensor



