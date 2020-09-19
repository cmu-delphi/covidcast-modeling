load_data <- function(STARTDATE, ENDDATE, GEO_TYPE, GEO_VALUE, EXCLUDED_AREAS){
  ftime <- covidcast_signal(data_source = "safegraph", 
                            signal ="full_time_work_prop",
                            start_day = STARTDATE, 
                            end_day = ENDDATE,
                            geo_type = GEO_TYPE, 
                            geo_values = GEO_VALUE)
  
  ftime <- ftime %>%  filter(!(geo_value %in% EXCLUDED_AREAS))
  
  ############## New confirmed COVID19 cases ############
  
  # A composite signal from JHU and USA facts
  # New confirmed COVID19 cases on average per 7 days
  case_avg <- covidcast_signal(data_source = "indicator-combination",
                               signal ="confirmed_7dav_incidence_num",
                               start_day = STARTDATE, 
                               end_day = ENDDATE,
                               geo_type = GEO_TYPE, 
                               geo_values = GEO_VALUE)
  
  case_avg <- case_avg %>%  filter(!(geo_value %in% EXCLUDED_AREAS))
  
  # Cumulative confirmed COVID19 cases on average per 7 days
  cum_case <- covidcast_signal(data_source = "indicator-combination",
                               signal = "confirmed_7dav_cumulative_num",
                               start_day = STARTDATE,
                               end_day = ENDDATE,
                               geo_type = GEO_TYPE,
                               geo_values = GEO_VALUE)
  
  cum_case <- cum_case %>%  filter(!(geo_value %in% EXCLUDED_AREAS))
  
  # Cumulative confirmed COVID19 cases on average per 7 days, per 100,000 population
  
  cum_case_prop <- covidcast_signal(data_source = "indicator-combination",
                                    signal ="confirmed_7dav_cumulative_prop",
                                    start_day = STARTDATE, 
                                    end_day = ENDDATE,
                                    geo_type = GEO_TYPE, 
                                    geo_values = GEO_VALUE)
  
  cum_case_prop <- cum_case_prop %>%  filter(!(geo_value %in% EXCLUDED_AREAS))
  
  ########### Death cases ######################
  
  # Number of new confirmed deaths due to COVID-19, daily
  death_case <- covidcast_signal(data_source = "indicator-combination",
                                 signal ="deaths_7dav_incidence_num",
                                 start_day = STARTDATE, 
                                 end_day = ENDDATE,
                                 geo_type = GEO_TYPE, 
                                 geo_values = GEO_VALUE)
  
  death_case <- death_case %>%  filter(!(geo_value %in% EXCLUDED_AREAS))
  
  # Cumulative number of confirmed deaths due to COVID-19
  cum_death_case <- covidcast_signal(data_source = "indicator-combination",
                                     signal ="deaths_7dav_cumulative_num",
                                     start_day = STARTDATE, 
                                     end_day = ENDDATE,
                                     geo_type = GEO_TYPE, 
                                     geo_values = GEO_VALUE)
  cum_death_case  <- cum_death_case  %>%  filter(!(geo_value %in% EXCLUDED_AREAS))
  
  # State-level restaurant data
  res <- read.csv("data/state_restaurants_visit_num.csv")
  # Change date type
  res$date <- ymd(res$date)
  # Filter the data
  res <- res %>% filter(!(geo_id %in% EXCLUDED_AREAS) & (date >= STARTDATE & date <= ENDDATE))
  # drop some columns
  res <- res[c(1,2,4,5)]
  # Change column names
  colnames(res) <- c("geo_value", "value", "sample_size", "time_value")
  
  new_res <- res %>% transmute (
    geo_value = geo_value,
    signal = "state_restaurant_visit_num",
    time_value = time_value,
    direction = NA,
    issue = lubridate::today(),
    lag = issue - time_value,
    value = value,
    stderr = NA,
    sample_size = NA,
    data_source = 'Weekly Patterns')
  return(list(Full.Time.Mobility=ftime, Avg.Confirmed.Case.Count=case_avg,  Cum.Avg.Case.Count=cum_case, Cum.Avg.Case.Count.Prop = cum_case_prop, Avg.Death.Case.Count = death_case, Cum.Avg.Death.Count = cum_death_case, Restaurant.Visit.Count = new_res))
}