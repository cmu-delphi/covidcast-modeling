# Tools for leading indicators DAP
# Includes exportable functions used in generating leading indicator notebooks


# Get and prepare signals function
# Prepares two signals (cases and a given indicator) for leading indicator analysis.
# Fetches case and indicator data for a given time range, arranges the data 
# and returns only the locations which have enough data for both cases and the indicators based on given thresholds.
#
# INPUT
# @param start_day: First day to get data
# @param end_day: Last day to get data
# @param indicator_source: The source for the signal data
# @param indicator_signal: The signal for the given source.
# @param case_threshold: The minumum number of cases a location should have over the time period to be included
# @param indicator_threshold: The minimun number of days on which a location should have indicator data to be included
# @param geo_type: What type of location to get data for. Deafault is "county".
#
# OUTPUT
# @return A list including: cases = list of dataframes for cases by location, indicator = list of dataframes for indicator by location
get_and_parse_signals <- function(start_day, end_day, indicator_source, indicator_signal,
                                  case_threshold, indicator_threshold, case_source = "usa-facts", 
                                  case_signal = "confirmed_7dav_incidence_num", geo_type = "county") {
  # Get cases and indicator
  cases = covidcast_signal(data_source = case_source, signal = case_signal,
                           start_day = start_day, end_day = end_day,
                           geo_type = geo_type)
  indicator = covidcast_signal(data_source = indicator_source, signal = indicator_signal,
                               start_day = start_day, end_day = end_day,
                               geo_type = geo_type)
  
  # Split CASES into lists by geo
  geos_case = cases %>%  group_keys(geo_value) %>% unlist()
  case_list = cases %>% group_by(geo_value) %>% select(geo_value, time_value, value) %>% 
    arrange(time_value) %>% group_split() %>% setNames(geos_case)
  
  # Split INDICATOR into lists by geo
  indicator_list = indicator %>%
    group_by(geo_value) %>% 
    select(geo_value, time_value, value) %>% 
    arrange(time_value) %>% 
    group_split()
  geos_indicator = lapply(indicator_list, function(a) a %>% 
                            select(geo_value) %>% unlist() %>% unique()) %>% unlist()
  indicator_list = indicator_list %>% setNames(geos_indicator)
  
  # Retain only the geos that have more than "case_threshold" cases,
  # having greater than "indicator_threshold" days with indicator data,
  # and do not have any negative or zero values in either cases or indicators
  large_case_geos = names(case_list)[which(sapply(case_list, function(a) a %>% summarize(sum(value))) > case_threshold)]
  large_indicator_geos = names(indicator_list)[which(sapply(indicator_list, function(a) a %>% summarize(nrow(a))) > indicator_threshold)]
  negative_cases = unlist(lapply(case_list, function(x){ 1*any(x$value < 0 | var(x$value) == 0) }))
  negative_indicator = unlist(lapply(indicator_list, function(x){ 1*any(x$value < 0 | var(x$value) == 0) }))
  large_case_geos = intersect(large_case_geos, names(negative_cases[which(negative_cases == 0)]))
  large_indicator_geos = intersect(large_indicator_geos, names(negative_indicator[which(negative_indicator == 0)]))
  large_geos = intersect(large_case_geos, large_indicator_geos)
  return (list("cases" = case_list[large_geos], "indicator" = indicator_list[large_geos]))
}


# Get increase points
# Identify points at which the indicator and the case values begin to rise significantly
# INPUT
# @param case_signal: A list of dataframes of cases
# @param indicator_signal: A list of dataframes of indicators
# OUTPUT
# @return List of dataframes that include as cols:
#                 time_value, geo_value, case_value, ind_value, case_rise_point, indicator_rise_point
get_increase_points <- function(case_list, indicator_list)
{
  list_with_leading_indicator=vector("list", length(case_list))
  for(i in 1:length(case_list))
  {
    cat(i,"\n")
    case_elem = case_list[[i]]
    #is location common to cases and indicator
    if(!(names(case_list)[i] %in% names(indicator_list)))
      next
    ind_elem = indicator_list[[which(names(indicator_list) %in% names(case_list)[i])]]
    colnames(ind_elem)[3] <- c("ind_value")
    colnames(case_elem)[3] <- c("case_value")
    ind_elem = ind_elem[ -c(1) ] # drop the extra geo_value column
    merged_df= merge(x = case_elem, y = ind_elem, by = "time_value")
    increasing_points_list_case_signal = calculate_increasing_points(merged_df$case_value, bandwidth=10, 
                                                                     quantile_threshold=0.0, 
                                                                     threshold=0.4, period=10)
    increasing_points_list_indicator_signal = calculate_increasing_points(merged_df$ind_value, bandwidth=10, 
                                                                          quantile_threshold=0.0, 
                                                                          threshold=0.4, period=10)
    increase_case_points = rep(0, length(merged_df$case_value))
    increase_case_points[as.vector(unlist(increasing_points_list_case_signal))]=1
    merged_df$case_rise_point = increase_case_points
    increase_indicator_points = rep(0, length(merged_df$case_value))
    increase_indicator_points[as.vector(unlist(increasing_points_list_indicator_signal))]=1
    merged_df$indicator_rise_point = increase_indicator_points
    list_with_leading_indicator[[i]]=merged_df
  }
  return(list_with_leading_indicator[lengths(list_with_leading_indicator)!=0])
}


# Plot signals
# INPUT
# @param case_indicator_list: List of dataframes that include as cols:
#                             time_value, geo_value, case_value, ind_value, case_rise_point, indicator_rise_point
# @param county_fips: County fips code for the county to plot
# @param ylab2: Indicator label
# @param ylab1: Case label. Default is "New COVID-19 cases".
# @param xlab: x-axis label. Default is "Date".
# @param smooth_and_show_increase_points: Whether to produce smoothed plots with increase points, or raw data plots.
#                                         Default is FALSE.
# OUTPUT
# @return a plot for the given county
plot_signals <- function(case_indicator_list, county_fips, ylab2 = "",
                         ylab1 = "New COVID-19 cases", xlab = "Date",
                         smooth_and_show_increase_point=FALSE) {
  ggplot_colors = c("#00AFBB", "#FC4E07")
  county_fips_code = unlist(lapply(case_indicator_list, function(x) x$geo_value[1]))
  list_elem = case_indicator_list[[which(county_fips_code %in% county_fips)]]
  state_name = fips_to_name(paste0(substr(county_fips, 1, 2), "000"))
  state_abbr = name_to_abbr(state_name)
  title = paste0(fips_to_name(county_fips), ", ", state_abbr)
  
  # Compute ranges of the two signals
  range1 = range(list_elem$case_value)
  range2 = range(list_elem$ind_value)
  
  # Compute ranges of the two signals
  trans12 = function(x) trans(x, range1, range2)
  trans21 = function(x) trans(x, range2, range1)
  
  # Convenience functions for our two signal ranges
  list_elem$ind_value = trans(list_elem$ind_value, range2, range1)
  plot.df=data.frame(time_value = rep(list_elem$time_value, 2),
                     value = c(list_elem$case_value, list_elem$ind_value),
                     value_sm = c(sm(list_elem$case_value,12), sm(list_elem$ind_value, 12)),
                     signal = c(rep(ylab1, length(list_elem$case_value)),
                                rep(ylab2, length(list_elem$case_value))),
                     increasing=c(list_elem$case_rise_point, list_elem$indicator_rise_point),
                     point_color=c(rep(ggplot_colors[2], length(list_elem$time_value)),
                                   rep(ggplot_colors[1], length(list_elem$time_value))),
                     stringsAsFactors = F)
  
  if (smooth_and_show_increase_point) {
    increasing_points=plot.df[which(plot.df$increasing==1),]
    return(ggplot(plot.df, aes(x = time_value, y = value_sm)) +
             geom_line(aes(color = signal), size = 1.5) +
             scale_color_manual(values = ggplot_colors[1:2]) +
             scale_y_continuous(name = ylab1, limits = range1,
                                sec.axis = sec_axis(trans = trans12,
                                                    name = ylab2)) +
             labs(title = title, x = xlab) + theme_bw() + 
             geom_point(data = increasing_points, col = increasing_points$point_color, size = 5) + 
             theme(legend.pos = "bottom", legend.title = element_blank(), 
                   axis.text = element_text(size = 12),
                   legend.text = element_text(size = 11),
                   axis.title = element_text(size = 12),
                   title = element_text(size = 12)))
  }
  
  return(ggplot(plot.df, aes(x = time_value, y = value)) +
           geom_line(aes(color = signal), size = 1.5) +
           scale_color_manual(values = ggplot_colors[1:2]) +
           scale_y_continuous(name = ylab1, limits = range1,
                              sec.axis = sec_axis(trans = trans12,
                                                  name = ylab2)) +
           labs(title = title, x = xlab) + theme_bw() +
           theme(legend.pos = "bottom", legend.title = element_blank(), 
                 axis.text = element_text(size = 12),
                 legend.text = element_text(size = 11),
                 axis.title = element_text(size = 11),
                 title = element_text(size = 12)))
}

get_county_level_precision_recall <- function(final_cases_indicator_list, min_window=3, max_window=14)
{
  #does each case rise date have a preceding indicator rise point within window [3,14] (precision)?
  get_cases_preceding_indicators<-function(case_increase_dates, indicator_rise_dates)
  {
    sapply(case_increase_dates, function(date_case_increase) { 
      differences=as.integer(date_case_increase-indicator_rise_dates)
      1*(any(differences >= min_window & differences <= max_window))
    })
  }
  
  #for each indicator rise point, do cases increase within [3,14 days] (recall)
  get_indicator_preceding_cases<-function(indicator_rise_dates, case_increase_dates)
  {
    sapply(indicator_rise_dates, function(date_indicator_rise) { 
      differences=as.integer(case_increase_dates-date_indicator_rise)
      1*(any(differences >= min_window & differences <= max_window))
    })
  }
  
  #traverse the list of dataframes, where each element corresponds to a unique county
  precision_recall_list=lapply(final_cases_indicator_list, function(x){
    #does each case rise date have a preceding indicator rise point within window [3,14] (precision)?
    cases_preceded_by_indicators=get_cases_preceding_indicators(x$time_value[which(x$case_rise_point==1)], x$time_value[which(x$indicator_rise_point==1)])
    
    #for each indicator rise point, do cases increase within [3,14 days] (recall)
    each_indicator_rise_precedes_case_rise=get_indicator_preceding_cases(x$time_value[which(x$indicator_rise_point==1)], x$time_value[which(x$case_rise_point==1)])
    
    #precision=tp/(tp+fp), recall=tp/(tp+fn);
    #precision = tp/(all predicted positive cases)
    #true positive: for each substantial case rise, there is a substantial indicator rise before the case increase in the specified window
    #false positive: indicator rise, when there is not a substantial increase in cases
    #false negative: for substantial case rise, we predict indicator does not rise
    df=data.frame(County_Code=x$geo_value[1], 
                  Num_Case_Rises=length(x$time_value[which(x$case_rise_point==1)]), 
                  Num_Indicator_Rises=length(x$time_value[which(x$indicator_rise_point==1)]),
                  Num_Times_Indicator_Rise_Precedes_Case_Rise=ifelse(length(each_indicator_rise_precedes_case_rise)>0,sum(each_indicator_rise_precedes_case_rise),NA),
                  Num_Times_Case_Rise_Precedes_Indicator_Rise = ifelse(length(cases_preceded_by_indicators) > 0, sum(cases_preceded_by_indicators), NA),
                  TP_Num_Times_Case_Rise_Preceded_by_Indicator=ifelse(length(cases_preceded_by_indicators) > 0, sum(cases_preceded_by_indicators), NA),
                  FP_Num_Times_Indicator_Rise_Without_Case_Rise=ifelse(length(each_indicator_rise_precedes_case_rise) > 0, sum(1*(each_indicator_rise_precedes_case_rise == 0)), NA),
                  FN_Num_Times_Case_Rise_Without_Indicator_Rise = ifelse(length(each_indicator_rise_precedes_case_rise) > 0, sum(1*(cases_preceded_by_indicators == 0)), NA),
                  Leading_Indicator_Precision=-1,
                  Leading_Indicator_Recall=-1,
                  stringsAsFactors = F)
    df$Leading_Indicator_Precision = df$TP_Num_Times_Case_Rise_Preceded_by_Indicator/(df$TP_Num_Times_Case_Rise_Preceded_by_Indicator + df$FP_Num_Times_Indicator_Rise_Without_Case_Rise)
    df$Leading_Indicator_Recall = df$TP_Num_Times_Case_Rise_Preceded_by_Indicator/(df$TP_Num_Times_Case_Rise_Preceded_by_Indicator + df$FN_Num_Times_Case_Rise_Without_Indicator_Rise)
    df
    
  })
  precision_recall_df=rbindlist(precision_recall_list, use.names = T)
  precision_recall_df$F1_Score = 2*(precision_recall_df$Leading_Indicator_Recall*precision_recall_df$Leading_Indicator_Precision)/(precision_recall_df$Leading_Indicator_Precision + precision_recall_df$Leading_Indicator_Recall)
  global_precision_recall=data.frame(Precision_Leading_Indicator=sum(precision_recall_df$TP_Num_Times_Case_Rise_Preceded_by_Indicator,
                                                                     na.rm = T)/(sum(precision_recall_df$TP_Num_Times_Case_Rise_Preceded_by_Indicator, na.rm = T) + sum(precision_recall_df$FP_Num_Times_Indicator_Rise_Without_Case_Rise, na.rm = T)),
                                     Recall_Leading_Indicator=sum(precision_recall_df$TP_Num_Times_Case_Rise_Preceded_by_Indicator,
                                                                  na.rm = T)/(sum(precision_recall_df$TP_Num_Times_Case_Rise_Preceded_by_Indicator, na.rm = T) + sum(precision_recall_df$FN_Num_Times_Case_Rise_Without_Indicator_Rise, na.rm = T)),
                                     stringsAsFactors = F)
  return(list(precision_recall_df,global_precision_recall))
}


# Get success examples
#INPUT
# @param case_indicator_list: List of dataframes that include as cols:
#                             time_value, geo_value, case_value, ind_value, case_rise_point, indicator_rise_point
# @param success_window_max: Max number of days after an indicator rise and before a case rise to call the relationship a success
# @param success_window_min: Min number of days between an indicator rise and a case rise to call the relationship a success
# OUTPUT
# @return list of county fips codes where all indicator rise points precede a case rise point by success_window_max or fewer days, and more than success_window_min days
#         and all case rise points are preceded by an indicator rise point by success_window_max or fewer days, and more than success_window_min days
get_success_examples <- function(case_indicator_list, success_window_max = 14, success_window_min = 3) {
  success_counties = vector("list", length(case_indicator_list)) 
  
  for (i in (1:length(case_indicator_list))) {
    case_points = 1 %in% case_indicator_list[[i]]$case_rise_point
    indicator_points = 1 %in% case_indicator_list[[i]]$indicator_rise_point
    # County has at least one case rise point and at least one indicator rise point
    if(case_points && indicator_points) {
      case_dates = case_indicator_list[[i]]$time_value[case_indicator_list[[i]]$case_rise_point==1]
      indicator_dates = case_indicator_list[[i]]$time_value[case_indicator_list[[i]]$indicator_rise_point==1]
      # Check if all of the case points are preceded by an indicator point
      k = 1
      success = TRUE
      while(success && k <= length(case_dates)) {
        success = FALSE
        for (j in (1:length(indicator_dates))) {
          if(case_dates[k] - indicator_dates[j] <= success_window_max && case_dates[k] - indicator_dates[j] >= success_window_min) {
            success = TRUE
          }
        }
        k = k+1
      }
      # Check if all of the indicator points precede a case point
      if (success) {
        k = 1
        while(success && k <= length(indicator_dates)) {
          success = FALSE
          for (j in (1:length(case_dates))) {
            if(case_dates[j] - indicator_dates[k] <= success_window_max && case_dates[j] - indicator_dates[k] > success_window_min) {
              success = TRUE
            }
          }
          k = k+1
        }
      }
      if (success) {
        success_counties[[i]]=case_indicator_list[[i]]$geo_value[1]
      }
    }
  }
  success_counties = success_counties[lengths(success_counties)!=0]
  return (success_counties)
}


# Get recall strict
# INPUT
# @param case_indicator_list: List of dataframes that include as cols:
#                             time_value, geo_value, case_value, ind_value, case_rise_point, indicator_rise_point
# OUTPUT
# @return number of successful counties divided by number of total counties with at least one case rise point
get_recall_strict <- function(case_indicator_list) {
  success_examples = get_success_examples(case_indicator_list, 14, 3)
  cases_with_rise = unlist(lapply(case_indicator_list, function(x){ any(x$case_rise_point == 1)}))
  cases_with_rise = cases_with_rise[which(cases_with_rise == TRUE)]
  return (length(success_examples) / length(cases_with_rise))
}

# Get precision strict
# INPUT
# @param case_indicator_list: List of dataframes that include as cols:
#                             time_value, geo_value, case_value, ind_value, case_rise_point, indicator_rise_point
# OUTPUT
# @return number of successful counties divided by number of total counties with at least one indicator rise point
get_precision_strict <- function(case_indicator_list) {
  success_examples = get_success_examples(case_indicator_list, 14, 3)
  indicator_with_rise = unlist(lapply(case_indicator_list, function(x){ any(x$indicator_rise_point == 1)}))
  indicator_with_rise = indicator_with_rise[which(indicator_with_rise == TRUE)]
  return (length(success_examples) / length(indicator_with_rise))
}


# Generate strawman guessers, one selects random days as "rise points" one marks every day the indicator derivative > 0
# as a rise point.
# INPUT
# @param case_indicator_list: List of dataframes that include as cols:
#                             time_value, geo_value, case_value, ind_value, case_rise_point, indicator_rise_point
# OUTPUT
# @return list of dataframes with extra columns for the random guesser "rise points" and the first derivative guesser "rise points"
generate_competitors_get_scores<-function(final_cases_indicator_list)
{
  all_guessers=lapply(final_cases_indicator_list, function(x){
    random_guesser <- rbinom(nrow(x), 1, 0.5)
    case_first_deriv=get_signal_first_derivative(signal = x$case_value, bandwidth = 14)
    indicator_first_deriv=get_signal_first_derivative(signal = x$ind_value, bandwidth = 14)
    x$random_guesser=random_guesser
    x$case_first_deriv_guesser=ifelse(case_first_deriv > 0, 1, 0)
    x$indicator_first_deriv=ifelse(indicator_first_deriv > 0, 1, 0)
    x
  })
  return(all_guessers)
}


# Gets recall and precision scores for a given guesser
# INPUT 
# @param competitors: List of dataframes that include as cols:
#                             time_value, case_rise_point, indicator_rise_point, random_guesser, case_first_deriv_guesser
# @param guesser: Which guesser to get scores for
# OUTPUT
# @return List of: guesser name, recall score, precision score
get_recall_and_precision = function(competitors, guesser) {
  true_positives = 0
  false_positives = 0
  true_negatives = 0
  false_negatives = 0
  for (c in (1:length(competitors))) {
    for (i in (1:length(competitors[[c]]$time_value))) {
      guesser_points = competitors[[c]][guesser]
      guesser_point = as.numeric(unlist(guesser_points))[[i]]
      if (competitors[[c]]$case_rise_point[[i]] == 1 && guesser_point == 1) {
        true_positives = true_positives + 1
      }
      if (competitors[[c]]$case_rise_point[[i]] == 1 && guesser_point == 0) {
        false_negatives = false_negatives + 1
      } 
      if (competitors[[c]]$case_rise_point[[i]] == 0 && guesser_point == 0) {
        true_negatives = true_negatives + 1
      } 
      if (competitors[[c]]$case_rise_point[[i]] == 0 && guesser_point == 1) {
        false_positives = false_positives + 1
      } 
    }
  }
  recall = true_positives / (true_positives + false_negatives)
  precision = true_positives / (true_positives + false_positives)
  return(c(guesser, recall, precision))
}

###############################################################################################
# HELPER FUNCTIONS

# INPUT
# @param y: A vector of numeric values
# @param bandwidth: Default is 2
# OUTPUT
# @return: A smoothed version of the vector based on the bandwidth
sm <- function(y, bandwidth = 2){
  n = length(y)
  return(ksmooth(x = 1:(n-1), y = y, bandwidth = bandwidth, x.points = 1:n, kernel="normal")$y)
}


# TODO
# Trans
# INPUT
# @param x:
# @param from_range:
# @param to_range:
# OUTPUT
# @return: 
trans <- function(x, from_range, to_range) {
  (x - from_range[1]) / (from_range[2] - from_range[1]) *
    (to_range[2] - to_range[1]) + to_range[1]
}


# Calculate increasing points
# Identify periods where:
# 1. First derivative at each point is > 0
# 2. Period is > a certain number of days
# 3. Each first derivative is > a certain % of other derivatives
# 4. Magnitude of increase is > a certain threshold
# Return the first day of that period
# INPUT
# @param signal: A numeric vector of values corresponding to case counts or indicator counts for one location
# @param bandwidth: Bandwidth for smoothing. Default is 10.
# @param quantile_threshold: Deriv must be greater than quantile_threshold percent of other derivs. Default is 0.75.
# @param threshold: Min ratio of magnitude of increase from min point to max point. Default is 0.2.
# @param period: Min length of increase in days. Default is 5.
# OUTPUT
# @return List of points of increase for one location
calculate_increasing_points <- function(signal, bandwidth=10, quantile_threshold=0.0, threshold=0.4, period=10){
  first_deriv = get_signal_first_derivative(signal, bandwidth)
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
  return(s)
}


# Gets the first derivative of a signal at a certain point
# INPUT
# @param signal: A numeric vector of values corresponding to indicator/case counts for one location
# @param bandwidth: Bandwidth for smoothing
# OUTPUT
# @return First derivatives for signal's values
get_signal_first_derivative<-function(signal, bandwidth)
{
  smoothed_signal=sm(signal, bandwidth)
  tt = 1:length(signal)
  first_deriv = stats::splinefun(tt, smoothed_signal)(tt, 1)
  return(first_deriv)
}


# Sets the "rise points" to be spread over a time window for the per time point recall and precision analysis
# INPUT
# @param competitors_df: List of dataframes that include as cols:
#                             time_value, case_rise_point, indicator_rise_point
# @param window: How many days ahead or behind to replicate the "rise points" out from the original rise point
# OUTPUT
# @ return A list of dataframes that have rise points marked at original rise points and "window" number of days ahead (for indicators)
# and behind (for cases)
use_time_windows = function(competitors_df, window) {
  for (c in (1:length(competitors_df))) {
    for (i in (1:length(competitors_df[[c]]$time_value))) {
      if (competitors_df[[c]]$case_rise_point[[i]] == 1){
        for(j in (1:(window-1))) {
          if (i-j > 0) {
            competitors_df[[c]]$case_rise_point[[i-j]] = 1
          }
        }
      }
    }
    i = length(competitors_df[[c]]$time_value)
    while(i > 0) {
      if (competitors_df[[c]]$indicator_rise_point[[i]] == 1){
        for(j in (1:(window-1))) {
          if (i+j <= length(competitors_df[[c]]$time_value)) {
            competitors_df[[c]]$indicator_rise_point[[i+j]] = 1
          }
        }
      }
      i = i-1
    }
  }
  return (competitors_df)
}
