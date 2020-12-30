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
get_and_prepare_signals = function(start_day, end_day, indicator_source, indicator_signal, 
                                   case_threshold, indicator_threshold, geo_type = "county") {
  # Get cases and indicator
  cases = covidcast_signal(data_source = "usa-facts", signal = "confirmed_7dav_incidence_num", 
                   start_day = start_day, end_day = end_day, 
                   geo_type = geo_type)
  indicator = covidcast_signal(data_source = indicator_source, signal = indicator_signal, 
                   start_day = start_day, end_day = end_day, 
                   geo_type = geo_type)
  
  # Split CASES into lists by geo
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
  indicator_list = indicator_list[large_geos]
  large_geos = names(indicator_list)[which(sapply(indicator_list, function(a) a %>% summarize(nrow(a))) > indicator_threshold)]
  return (list("cases" = case_list[large_geos], "indicator" = indicator_list[large_geos]))
}