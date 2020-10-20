load_covidcast_data <- function(STARTDATE, ENDDATE, GEO_TYPE, GEO_VALUE, EXCLUDED_AREAS){
  ftime <- covidcast_signal(data_source = "safegraph", 
                            signal ="full_time_work_prop",
                            start_day = STARTDATE, 
                            end_day = ENDDATE,
                            geo_type = GEO_TYPE, 
                            geo_values = GEO_VALUE)
  
  ftime <- ftime %>%  filter(!(geo_value %in% EXCLUDED_AREAS))
  
  ptime <- covidcast_signal(data_source = "safegraph", 
                            signal ="part_time_work_prop",
                            start_day = STARTDATE, 
                            end_day = ENDDATE,
                            geo_type = GEO_TYPE, 
                            geo_values = GEO_VALUE)
  
  ptime <- ftime %>%  filter(!(geo_value %in% EXCLUDED_AREAS))
  
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
  return(list(Full.Time.Mobility=ftime, Part.Time.Mobility=ptime, Avg.Confirmed.Case.Count=case_avg,  Cum.Avg.Case.Count=cum_case, Cum.Avg.Case.Count.Prop = cum_case_prop, Avg.Death.Case.Count = death_case, Cum.Avg.Death.Count = cum_death_case, Restaurant.Visit.Count = new_res, smoothed_cli = smoothed_cli,   smoothed_adj_cli= smoothed_adj_cli))
}


load_policy_data <- function(STARTDATE, ENDDATE){
  # Read government intervention data
  urlfile="https://raw.githubusercontent.com/COVID19StatePolicy/SocialDistancing/master/data/USstatesCov19distancingpolicy.csv"
  policy <- read_csv(url(urlfile))
  
  # Convert to lower case
  policy$StatePostal <- tolower(policy$StatePostal)
  # First we convert the date to a proper format
  policy[, c("DateIssued", "DateEnacted", "DateExpiry" ,"DateEased", "DateEnded", "DateReexpanded1", "DateReeased1")] <- data.frame(lapply(policy[, c("DateIssued", "DateEnacted", "DateExpiry" ,"DateEased", "DateEnded", "DateReexpanded1", "DateReeased1")], function(x) as.Date(as.character(x), "%Y%m%d")))
  
  # Get the dates between start and end date
  all.dates <- seq(as.Date(STARTDATE), as.Date(ENDDATE), by="days")
  time_value <- sort(rep(all.dates, length(unique(policy$StatePostal)))) 
  
  # Generate geo_value
  geo_value <- rep(unique(policy$StatePostal), length(all.dates))
  policy_signal <- data.frame(time_value = time_value, geo_value = geo_value)
  
  # Create empty columns
  policy_signal[,unique(policy$StatePolicy)] <- 0
  
  # Fill in the count for each date
  # Get the policy name and state to filer policy signal 
  for (row in (1:nrow(policy))){
    current.policy <- policy[row,]$StatePolicy
    current.state <- policy[row,]$StatePostal
    
    if (is.na(policy[row,]$DateEnded)){
      
      # Filter the rows of dataframe to be the current state and the time value that is after the policy is enacted.
      policy_signal[policy_signal$geo_value == current.state & policy_signal$time_value > as.Date(policy[row,]$DateEnacted), current.policy] <- 1
    
      }else{
      
      # Get time range between Date Enacted and Date Ended
      if(policy[row,]$DateEnacted > as.Date(policy[row,]$DateEnded)){
        next
      }else{
      time.range <- seq(as.Date(policy[row,]$DateEnacted), as.Date(policy[row,]$DateEnded), by = "days")
      
      # Fill in the the rows that are in the current policy and fall between the time arrange to be 1
      policy_signal[policy_signal$time_value %in% time.range & policy_signal$geo_value == current.state, current.policy] <- 1
    }}
  }
  
  # Compute the sum of the number of policies for every day in the state
  policy_signal$total.num.policy <- rowSums(policy_signal[unique(policy$StatePolicy)])
  
  return(policy_signal)
  }