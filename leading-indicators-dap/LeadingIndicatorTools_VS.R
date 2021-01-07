# Tools for leading indicators DAP
# Includes exportable functions used in generating leading indicator notebooks

# Prepares two signals (cases and a given indicator) for leading indicator analysis.
# Fetches case and indicator data for a given time range, arranges the data 
# and returns only the locations which have enough data for both cases and the indicators based on given thresholds.
#
# @param start_day First day to get data
# @param end_day Last day to get data
# @param indicator_source The source for the signal data
# @param indicator_signal The signal for the given source.
# @param case_threshold The minumum number of cases a location should have over the time period to be included
# @param indicator_threshold The minimun number of days on which a location should have indicator data to be included
# @param geo_type What type of location to get data for. Deafault is "county".
#
# @return A list including: cases = list of dataframes for cases by location, indicator = list of dataframes for indicator by location
get_and_prepare_signals = function(start_day, end_day, indicator_source, case_signal, indicator_signal, 
                                   case_threshold, indicator_threshold, geo_type = "county", min_num_days=15) {
  # Get cases and indicator
  #cases = covidcast_signal(data_source = "usa-facts", signal = "confirmed_7dav_incidence_num", 
  #                         start_day = start_day, end_day = end_day, 
  #                         geo_type = geo_type)
  #indicator = covidcast_signal(data_source = indicator_source, signal = indicator_signal, 
  #                             start_day = start_day, end_day = end_day, 
  #                             geo_type = geo_type)
  
  # Split CASES into lists by geo
  cases=case_signal
  indicator=indicator_signal
  geos_case = cases %>%  group_keys(geo_value) %>% unlist()
  case_list = cases %>% group_by(geo_value) %>% select(geo_value, time_value, value) %>% arrange(time_value) %>% group_split() %>% setNames(geos_case)
  
  # Split INDICATOR into lists by geo
  indicator_list = indicator %>% group_by(geo_value) %>% select(geo_value, time_value, value) %>% arrange(time_value) %>% group_split()
  geos_indicator = lapply(indicator_list, function(a) a %>% select(geo_value) %>% unlist() %>% unique()) %>% unlist()
  indicator_list = indicator_list %>% setNames(geos_indicator)
  
  # Combine by overlapping geos
  case_list = case_list[geos_indicator]
  
  # Retain only the geos averaging more than "case_threshold" cases a day
  # and having greater than "indicator_threshold" days with indicator data
  large_geos = names(case_list)[which(sapply(case_list, function(a) a %>% summarize(sum(value))) > case_threshold)]
  negative_cases = unlist(lapply(case_list, function(x){ 1*any(x$value < 0 | var(x$value) == 0 | length(x) < min_num_days) }))
  large_geos=intersect(large_geos, names(negative_cases[which(negative_cases == 0)]))
  case_list = case_list[large_geos]
  large_geos = names(indicator_list)[which(sapply(indicator_list, function(a) a %>% summarize(nrow(a))) > indicator_threshold)]
  negative_indicator = unlist(lapply(indicator_list, function(x){ 1*any(x$value < 0 | var(x$value) == 0 | length(x) < min_num_days) }))
  large_geos=intersect(large_geos, names(negative_indicator[which(negative_indicator == 0)]))
  indicator_list = indicator_list[large_geos]
  return (list("cases" = case_list, "indicator" = indicator_list))
}

sm <- function(y, bandwidth = 2){
  n = length(y)
  return(ksmooth(x = 1:(n-1), y = y, bandwidth = bandwidth, x.points = 1:n, kernel="normal")$y)
}


#Get Increasing Period function
#(1)Calculate the first derivative and identify regions where this value is > 0 (i.e. increasing)
#(2)To get regions of substantial increase, look if the derivative is greater than the 75th percentile and signal exceeds a 
#threshold (e.g. cases during the period must rise 20% or more)
#(3) The selected region must be increasing for at-least x # of days 

#INPUT
#signal: will be a numeric vector of values corresponding to case counts, survey counts, etc. 

#OUTPUT
#returns list of minima for one specific location
get_increasing_points <- function(signal, bandwidth=10, quantile_threshold=0.75, threshold=0.2, period=5){
  smoothed_signal=sm(signal, bandwidth)
  tt = 1:length(signal)
  first_deriv = stats::splinefun(tt, smoothed_signal)(tt, 1)
  increasing_period= which(first_deriv>quantile(first_deriv, quantile_threshold) & first_deriv>0)
  s <- split(increasing_period, cumsum(c(TRUE, diff(increasing_period) != 1)))
  s <- lapply(s, function(x) {
    if(length(x) !=0)
    {
      if(smoothed_signal[max(x)]/smoothed_signal[min(x)] > (1+threshold) & length(x) >= period)
        min(x)
    }
  })
  s[lengths(s)!=0]
  return(s) #returns the minimum points list
}

trans = function(x, from_range, to_range) {
  (x - from_range[1]) / (from_range[2] - from_range[1]) *
    (to_range[2] - to_range[1]) + to_range[1]
}

library(dplyr)
library(ggplot2)
library(covidcast)
#the axis scaling needs work for both plotting codes
plot_one = function(geo_value, title = NULL, xlab = NULL,
                    ylab1 = "New COVID-19 cases", ylab2 = "% CLI-in-community", legend =  TRUE,
                    df_in=NULL, df_indicator=NULL) {
  ggplot_colors = c("#00AFBB", "#FC4E07")
  # Filter down the signal data frames
  given_geo_value = geo_value
  df_indicator_one = df_indicator %>% filter(geo_value == given_geo_value)
  df_in_one = df_in %>% filter(geo_value == given_geo_value)
  
  common_dates=intersect(as.character(df_indicator_one$time_value), as.character(df_in_one$time_value))
  df_indicator_one=df_indicator_one[which(as.character(df_indicator_one$time_value) %in% common_dates),]
  df_in_one=df_in_one[which(as.character(df_in_one$time_value) %in% common_dates),]
  
  # Compute ranges of the two signals
  range1 = df_in_one %>% select("value") %>% range
  range2 = df_indicator_one %>% select("value") %>% range
  
  # Convenience functions for our two signal ranges
  trans12 = function(x) trans(x, range1, range2)
  trans21 = function(x) trans(x, range2, range1)
  
  # Find state name, find abbreviation, then set title
  state_name = fips_to_name(paste0(substr(geo_value, 1, 2), "000"))
  state_abbr = name_to_abbr(state_name)
  title = paste0(fips_to_name(geo_value), ", ", state_abbr)
  
  # Transform the combined signal to the incidence range, then stack
  # these rowwise into one data frame
  df = select(rbind(df_indicator_one %>% mutate_at("value", trans21),
                    df_in_one), c("time_value", "value"))
  ylabel_axis1=ylab2
  ylabel_axis2=ylab1
  
  #ylabel_axis1="% CLI-in-community"
  #ylabel_axis2="New COVID-19 cases"
  df$signal = c(rep(ylabel_axis1, nrow(df_indicator_one)),
                rep(ylabel_axis2, nrow(df_in_one)))
  # Finally, plot both signals
  pos = ifelse(legend, "bottom", "none")
  return(ggplot(df, aes(x = time_value, y = value)) +
           geom_line(aes(color = signal), size = 1.5) +
           scale_color_manual(values = ggplot_colors[1:2]) +
           scale_y_continuous(name = ylabel_axis2, limits = range1,
                              sec.axis = sec_axis(trans = trans12,
                                                  name = ylabel_axis1)) +
           labs(title = title, x = xlab) + theme_bw() +
           theme(legend.pos = pos, legend.title = element_blank(), 
                 axis.text = element_text(size = 11),
                 legend.text = element_text(size = 11),
                 axis.title = element_text(size = 11),
                 title = element_text(size = 12)))
}

plot_one_old = function(geo_value, title = NULL, xlab = NULL,
                    ylab1 = NULL, ylab2 = NULL, legend =  TRUE,
                    df_in=NULL, df_fb=NULL) {
  ggplot_colors = c("#00AFBB", "#FC4E07")
  # Filter down the signal data frames
  given_geo_value = geo_value
  df_fb_one = df_fb %>% filter(geo_value == given_geo_value)
  df_in_one = df_in %>% filter(geo_value == given_geo_value)
  
  common_dates=intersect(as.character(df_fb_one$time_value), as.character(df_in_one$time_value))
  df_fb_one=df_fb_one[which(as.character(df_fb_one$time_value) %in% common_dates),]
  df_in_one=df_in_one[which(as.character(df_in_one$time_value) %in% common_dates),]
  
  # Compute ranges of the two signals
  range1 = df_in_one %>% select("value") %>% range
  range2 = df_fb_one %>% select("value") %>% range
  
  # Convenience functions for our two signal ranges
  trans12 = function(x) trans(x, range1, range2)
  trans21 = function(x) trans(x, range2, range1)
  
  # Find state name, find abbreviation, then set title
  state_name = fips_to_name(paste0(substr(geo_value, 1, 2), "000"))
  state_abbr = name_to_abbr(state_name)
  title = paste0(fips_to_name(geo_value), ", ", state_abbr)
  
  # Transform the combined signal to the incidence range, then stack
  # these rowwise into one data frame
  df = select(rbind(df_fb_one %>% mutate_at("value", trans21),
                    df_in_one), c("time_value", "value"))
  df$signal = c(rep("% CLI-in-community", nrow(df_fb_one)),
                rep("New COVID-19 cases", nrow(df_in_one)))
  # Finally, plot both signals
  pos = ifelse(legend, "bottom", "none")
  return(ggplot(df, aes(x = time_value, y = value)) +
           geom_line(aes(color = signal), size = 1.5) +
           scale_color_manual(values = ggplot_colors[1:2]) +
           scale_y_continuous(name = ylab1, limits = range1,
                              sec.axis = sec_axis(trans = trans12,
                                                  name = ylab2)) +
           labs(title = title, x = xlab) + theme_bw() +
           theme(legend.pos = pos, legend.title = element_blank(), 
                 axis.text = element_text(size = 11),
                 legend.text = element_text(size = 11),
                 axis.title = element_text(size = 11),
                 title = element_text(size = 12)))
}


case_signal = readRDS(file="../case_counts.RDS")
indicator_signal=readRDS(file="../cli.RDS")
list_with_increasing_points=identify_beginning_increase_points(case_signal = case_signal, indicator_signal = indicator_signal)
#plot_one("37125")
plot_one("37125", df_in = case_signal, df_indicator=indicator_signal)
plot_smoothed_signals_increase_point(list_with_increasing_points, county_fips = "37125")
plot_smoothed_signals_increase_point(list_with_increasing_points, county_fips= "06085")

plot_smoothed_signals_increase_point <- function(list_with_increasing_points, county_fips)
{
  county_fips_code = unlist(lapply(final_list_with_leading_indicator, function(x) x$geo_value.x[1]))
  list_elem = list_with_increasing_points[[which(county_fips_code %in% county_fips)]]
  ggplot_colors = c("#00AFBB", "#FC4E07")
  state_name = fips_to_name(paste0(substr(county_fips, 1, 2), "000"))
  state_abbr = name_to_abbr(state_name)
  title = paste0(fips_to_name(county_fips), ", ", state_abbr)
  range1 = range(list_elem$value)
  range2 = range(list_elem$ind.value)
  # Convenience functions for our two signal ranges
  #list_elem$ind.value=trans(list_elem$ind.value, range2, range1)
  list_elem$ind.value = trans(list_elem$ind.value, range2, range1)
  plot.df=data.frame(time_value = rep(list_elem$time_value, 2),
                     value = c(sm(list_elem$value,10), sm(list_elem$ind.value, 10)),
                     signal = c(rep("New COVID-19 Cases", length(list_elem$value)),
                                rep("% CLI-in-community", length(list_elem$value))),
                     increasing=c(list_elem$point_before_case_change, list_elem$point_before_indicator_change),
                     point_color=c(rep(ggplot_colors[2], length(list_elem$time_value)),
                                   rep(ggplot_colors[1], length(list_elem$time_value))),
                     stringsAsFactors = F)
  increasing_points=plot.df[which(plot.df$increasing==1),]
  # Compute ranges of the two signals
  trans12 = function(x) trans(x, range1, range2)
  trans21 = function(x) trans(x, range2, range1)
  
  
  p1=ggplot(plot.df, aes(x = time_value, y = value)) +
    geom_line(aes(color = signal), size = 1.5) +
    scale_color_manual(values = ggplot_colors[1:2]) +
    scale_y_continuous(name = "New COVID-19 Cases", limits = range1,
                       sec.axis = sec_axis(trans = trans12,
                                           name = "% CLI-in-community")) +
    labs(title = title, x = "Date") + theme_bw() + 
    geom_point(data = increasing_points, col = increasing_points$point_color, size = 5) + 
    theme(legend.pos = "bottom", legend.title = element_blank(), 
          axis.text = element_text(size = 12),
          legend.text = element_text(size = 11),
          axis.title = element_text(size = 12),
          title = element_text(size = 12))
  p1
}


#Identify leading indicator regions
#It takes in a dataframe of case and indicator signals from covidcast
#then returns a modified list of dataframes consisting of common dates, geovalue, case values, and indicator values
#each element of the list is uniquely determined by a county.
identify_beginning_increase_points <- function(case_signal, indicator_signal)
{
  processed_signals=get_and_prepare_signals(indicator_signal = indicator_signal,
                                            case_signal = case_signal,
                                            case_threshold = 2000,
                                            indicator_threshold = 0
                                            )
  #things that may need revision of get and prepare signals. It omits all the data on variance, sample size, which may be good to retain
  #even if we do not use it.
  caselist=processed_signals[[1]]
  indlist=processed_signals[[2]]
  final_list_with_leading_indicator=vector("list", length(caselist))
  for(i in 1:length(caselist))
  {
    cat(i,"\n")
    case_elem = caselist[[i]]
    #is location common to cases and fb
    if(!(names(caselist)[i] %in% names(indlist)))
      next
    ind_elem = indlist[[which(names(indlist) %in% names(caselist)[i])]]
    colnames(ind_elem)[3] <- c("ind.value")
    merged_df= merge(x = case_elem, y = ind_elem, by = "time_value")
    increasing_points_list_case_signal = get_increasing_points(merged_df$value, bandwidth=10, 
                                                               quantile_threshold=0.75, 
                                                               threshold=0.2, period=0)
    increasing_points_list_indicator_signal = get_increasing_points(merged_df$ind.value, bandwidth=10, 
                                                                    quantile_threshold=0.75, 
                                                                    threshold=0.2, period=0)
    increase_case_points = rep(0, length(merged_df$value))
    increase_case_points[as.vector(unlist(increasing_points_list_case_signal))]=1
    merged_df$point_before_case_change = increase_case_points
    increase_indicator_points = rep(0, length(merged_df$value))
    increase_indicator_points[as.vector(unlist(increasing_points_list_indicator_signal))]=1
    merged_df$point_before_indicator_change = increase_indicator_points
    final_list_with_leading_indicator[[i]]=merged_df
    #par(mfrow=c(3,1))
    #plot(merged_df$time_value, merged_df$value, pch = 20, col = 'black', xlab = "Date", ylab = "Case Value")
    #plot(merged_df$time_value, sm(merged_df$value, 10), pch = 20, 
    #     col = ifelse(1:length(merged_df$time_value) %in% unlist(increasing_points_list_case_signal), 'blue', 'black'),
    #     xlab = "Date", ylab = "Smoothed Case Value")
    #plot(merged_df$time_value, sm(merged_df$ind.value, 10), pch = 20, 
    #     col = ifelse(1:length(merged_df$time_value) %in% unlist(increasing_points_list_case_signal), 'red', 'black'),
    #     xlab = "Date", ylab = "Smoothed FB Value")
  }
  return(final_list_with_leading_indicator[lengths(final_list_with_leading_indicator)!=0])
}
