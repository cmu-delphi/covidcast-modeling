load_covidcast_data <- function(STARTDATE, ENDDATE, GEO_TYPE, GEO_VALUE, EXCLUDED_AREAS){
  
  # full-time away from home signal
  ftime <- covidcast_signal(data_source = "safegraph", 
                            signal ="full_time_work_prop",
                            start_day = STARTDATE, 
                            end_day = ENDDATE,
                            geo_type = GEO_TYPE, 
                            geo_values = GEO_VALUE)
  
  ftime <- ftime %>%  
    filter(!(geo_value %in% EXCLUDED_AREAS))
  
  # Part time away from home signal
  ptime <- covidcast_signal(data_source = "safegraph", 
                            signal ="part_time_work_prop",
                            start_day = STARTDATE, 
                            end_day = ENDDATE,
                            geo_type = GEO_TYPE, 
                            geo_values = GEO_VALUE)
  
  ptime <- ptime %>%  
    filter(!(geo_value %in% EXCLUDED_AREAS))
  
  # The fraction of mobile devices that 
  # did not leave the immediate area of their home
  # completely_home_prop <- covidcast_signal(data_source = "safegraph", 
  #                           signal ="completely_home_prop",
  #                           start_day = STARTDATE, 
  #                           end_day = ENDDATE,
  #                           geo_type = GEO_TYPE, 
  #                           geo_values = GEO_VALUE)
  # 
  # completely_home_prop <- completely_home_prop %>%  
  #   filter(!(geo_value %in% EXCLUDED_AREAS))
  # 
  # 
  # # The median time spent at home for all devices at this location 
  # # for this time period, in minutes
  # median_home_dwell_time <- covidcast_signal(data_source = "safegraph", 
  #                                            signal ="median_home_dwell_time",
  #                                            start_day = STARTDATE, 
  #                                            end_day = ENDDATE,
  #                                            geo_type = GEO_TYPE, 
  #                                            geo_values = GEO_VALUE)
  # 
  # median_home_dwell_time <- median_home_dwell_time %>%  
  #   filter(!(geo_value %in% EXCLUDED_AREAS))
  
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
  
  # doctor-visits with systematic day-of-week effects
  smoothed_cli <-covidcast_signal(data_source = "doctor-visits",
                                  signal ="smoothed_cli",
                                  start_day = STARTDATE, 
                                  end_day = ENDDATE,
                                  geo_type = GEO_TYPE, 
                                  geo_values = GEO_VALUE)
  
  smoothed_cli  <- smoothed_cli %>% filter(!(geo_value %in% EXCLUDED_AREAS))
  
  # with systematic day-of-week effects
  smoothed_adj_cli <- covidcast_signal(data_source = "doctor-visits",
                                    signal ="smoothed_adj_cli",
                                    start_day = STARTDATE, 
                                    end_day = ENDDATE,
                                    geo_type = GEO_TYPE, 
                                    geo_values = GEO_VALUE)
  
  
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
  return(list(full_time_work_prop=ftime, 
              part_time_work_prop=ptime,
              Avg.Confirmed.Case.Count=case_avg,  
              Cum.Avg.Case.Count=cum_case, 
              Cum.Avg.Case.Count.Prop = cum_case_prop, 
              Avg.Death.Case.Count = death_case, 
              Cum.Avg.Death.Count = cum_death_case, 
              Restaurant.Visit.Count = new_res, 
              smoothed_cli = smoothed_cli,   
              smoothed_adj_cli= smoothed_adj_cli))
}


load_policy <- function(){
  # Read government intervention data
  urlfile="https://raw.githubusercontent.com/COVID19StatePolicy/SocialDistancing/master/data/USstatesCov19distancingpolicy.csv"
  policy <- read.csv(url(urlfile))
  
  # Remove data that contains NA for every column
  idx <- apply(policy, 1, function(x) all(is.na(x)))
  # Override the loaded policy data
  policy <- policy[!idx, ]
  
  # Change to character
  policy$StateName <- as.character(policy$StateName)
  policy$StatePolicy <- as.character(policy$StatePolicy)
  # Convert to lower case
  policy$StatePostal <- tolower(policy$StatePostal)
  
  # First we convert the date to a proper format
  dated.policy <- lapply(policy[, c("DateIssued",
                    "DateEnacted", 
                    "DateExpiry" ,
                    "DateEased", 
                    "DateEnded", 
                    "DateReexpanded1", 
                    "DateReeased1")], 
         function(x) as.Date(as.character(x), 
                             "%Y%m%d"))
  
  policy[, c("DateIssued", 
             "DateEnacted", 
             "DateExpiry" ,
             "DateEased", 
             "DateEnded", 
             "DateReexpanded1", 
             "DateReeased1")] <- data.frame(dated.policy)
  
  return(policy)
}


