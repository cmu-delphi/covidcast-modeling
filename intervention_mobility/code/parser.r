################### shiftDays() ###########################

shiftDays <- function(filtered.df, original.df, shifted.days, columns.vec){
  # Check how many states
  if (length(unique(original.df$geo_value)) > 1){
    if(length(columns.vec)==1){
      # Get the data starting from the speificed date for CASE COUNT
      confounders.shifted.by.day <- filtered.df[1: (nrow(filtered.df)-shifted.days), columns.vec]
      # Get the number of rows 
      num_row <- length(original.df[original.df$geo_value == state,columns.vec])
      # replace all values by NA first
      original.df[original.df$geo_value == state,columns.vec] <- NA
      # Starting from the shifted time, we fill in the data
      original.df[original.df$geo_value == state,columns.vec][(shifted.days+1):num_row] <- confounders.shifted.by.day
    }else{
      # Get the data starting from the speificed date for CASE COUNT
      confounders.shifted.by.day <- filtered.df[1: (nrow(filtered.df)-shifted.days), columns.vec]
      # Get the number of rows 
      num_row <- nrow(original.df[original.df$geo_value == state,columns.vec])
      # replace all values by NA first
      original.df[original.df$geo_value == state,columns.vec] <- NA
      # Starting from the shifted time, we fill in the data
      original.df[original.df$geo_value == state,columns.vec][(shifted.days+1):num_row,] <- confounders.shifted.by.day[,columns.vec]
    }
  }else{
    if(length(columns.vec)==1){
      
      # Get the data starting from the speificed date for CASE COUNT
      confounders.shifted.by.day <- filtered.df[1: (nrow(filtered.df)-shifted.days), columns.vec]
      # Get the number of rows 
      num_row <- length(original.df[,columns.vec])
      # replace all values by NA first
      original.df[,columns.vec] <- NA
      # Starting from the shifted time, we fill in the data
      original.df[,columns.vec][(shifted.days+1):num_row] <- confounders.shifted.by.day
    }else{
      # Get the data starting from the speificed date for CASE COUNT
      confounders.shifted.by.day <- filtered.df[1: (nrow(filtered.df)-shifted.days), columns.vec]
      # Get the number of rows 
      num_row <- nrow(original.df[,columns.vec])
      # replace all values by NA first
      original.df[,columns.vec] <- NA
      # Starting from the shifted time, we fill in the data
      original.df[,columns.vec][(shifted.days+1):num_row,] <- confounders.shifted.by.day[,columns.vec]
    }
  }
  return(original.df)
}


####################### getSumOfPolicy() ###############
# Transform the policy data into another dataframe that counts
# number of policy for each day and computer 7-day rolling average

getSumOfPolicy<- function(policy, STARTDATE, ENDDATE){
  
  # Get the dates between start and end date
  all.dates <- seq(as.Date(STARTDATE), 
                   as.Date(ENDDATE), by="days")
  
  time_value <- sort(rep(all.dates, 
                         length(unique(policy$StatePostal)))) 
  
  # Generate geo_value
  geo_value <- rep(unique(policy$StatePostal), 
                   length(all.dates))
  
  policy_signal <- data.frame(time_value = time_value, 
                              geo_value = geo_value)
  
  # Create empty columns
  policy_signal[,unique(policy$StatePolicy)] <- 0
  
  # Fill in the count for each date
  # Get the policy name and state to filer policy signal 
  for (row in (1:nrow(policy))){
    current.policy <- policy[row,]$StatePolicy
    current.state <- policy[row,]$StatePostal
    
    if (is.na(policy[row,]$DateEnded)){
      
      # Filter the rows of dataframe to be the current state and the time value that is after the policy is enacted.
      rowCondition <- (policy_signal$geo_value == current.state) & 
        (policy_signal$time_value > as.Date(policy[row,]$DateEnacted))
      
      policy_signal[rowCondition, 
                    current.policy] <- 1
      
    }else{
      
      # Get time range between Date Enacted and Date Ended
      if(policy[row,]$DateEnacted > as.Date(policy[row,]$DateEnded)){
        
        # If the enacted date is after data ended, we skip this
        next
        
      }else{
        time.range <- seq(as.Date(policy[row,]$DateEnacted), 
                          as.Date(policy[row,]$DateEnded), 
                          by = "days")
        
        # Fill in the the rows that are in the current policy and fall between the time arrange to be 1
        rowCondition <- policy_signal$time_value %in% time.range & 
          policy_signal$geo_value == current.state
        
        policy_signal[rowCondition, 
                      current.policy] <- 1
      }}
  }
  
  # Compute the sum of the number of policies for every day in the state
  policy_signal$total.num.policy <- rowSums(policy_signal[unique(policy$StatePolicy)])
  
  # Compute the average on a 7day sliding window
  policy_signal <-policy_signal %>%
    arrange(desc(geo_value)) %>% 
    group_by(geo_value) %>% 
    mutate(num.policy.7avg = rollmean(total.num.policy, k = 7, fill = NA))%>%
    ungroup()
  
  return(policy_signal)
}


####################### get7daysRowSumPolicy() ###############
# Transform the policy data with the number of policy for each data
# into another dataframe that has covidcast dataframe format

transformToCovidcastLike <- function(policy_signal){
  
  # Finalize the covidcast-like signal for governemnt intervention
  covidcast.like.policy.signal <- policy_signal %>% transmute(
    geo_value = geo_value,
    signal = "policy_7dav_num",
    time_value = time_value,
    direction = NA,
    issue = lubridate::today(),
    lag = issue - time_value,
    value = num.policy.7avg,
    stderr = NA,
    sample_size = NA,
    data_source = 'University of Washington')
  
  return(covidcast.like.policy.signal)
}



################### getCorrByShift() #################
# A function to compute the corrlation by various number of shifts

# arguments:

# num_shift: specify how many days to forward signal_1
# signal_1: the first signal
# signal_2: the second signal
# corr_method: correlation method
# by: select either time_value or geo_value

getCorrByShift <- function(num_shift, signal_1, signal_2, corr_method, by){

  dt_vec <- 0:num_shift
  df_list <- vector("list", length(dt_vec))
  for (i in 1:length(dt_vec)) {
    df_list[[i]] <- covidcast_cor(signal_1,
                                  signal_2, 
                                  dt_x = dt_vec[i], 
                                  by = by, 
                                  method= corr_method)
    df_list[[i]]$dt <- dt_vec[i]
  }
  df <- do.call(rbind, df_list)
  return(df)
}

################### getMedian() #################

# A function to derive median of the signal from each shift 

# arguments:

# df: a covidcast like dataframe

# return:

# df_med: dataframe that containss the median of the value based on each
#         number of the shifts.

getMedian <-function(df){
  df_med <- df %>%
    group_by(dt) %>%
    summarize(median = median(value, na.rm = TRUE), .groups = "drop_last")
  return(df_med)
}

################### getMedianCorr() #################

# arguments: 
# shiftday: specify the number of days you want to shift
# covidcast.like_signal.1: a dataframe that has covidcast package ouput dataframe 
# covidcast.like_signal.2: another dataframe that has covidcast package ouput dataframe 
# corr.method: specify a correlation method
# name: specify the name of the signal

getMedianCorr <- function(shiftday, 
                          covidcast.like_signal.1, 
                          covidcast.like_signal.2, 
                          corr.method, 
                          name, 
                          by){

  
  df <- getCorrByShift(shiftday, 
                       covidcast.like_signal.1, 
                       covidcast.like_signal.2, 
                       corr.method, 
                       by)
  
  med <- getMedian(df)
  med$Comparison <- name
  return(med)
}

################## joinMobilityAndPolicy() ###################
# Left join mobility dataframe with a policy data frame by time
# arguments:
# mobility.df (data.frame): mobility dataframe
# policy.df (data.frame): policy dataframe
# confounders (list): a list of dataframes that contain each covidcast 
#                     case signals and doctor visits

joinMobilityAndPolicy <- function(mobility.df,
                                  policy.df,
                                  confounders){
  
  # Intervention left join mobility with policy signal
  intervention_mobility_case <- left_join(mobility.df, 
                                          policy.df, 
                                          by=c("time_value"))
  
  # Left join again with all other potential confounders
  for (confounder in confounders){
    intervention_mobility_case <- left_join(intervention_mobility_case, 
                                            confounder, 
                                            by=c("time_value"))
  }
  
  return(intervention_mobility_case)
}


################## getFirstDayOfIntervention() ###################
# get the first time stamp of the intervention in the data
# arguments:
# intervention (data.frame): policy dataframe
# geo_value(character): a string to specify the state name
# policy.name (character): a string to specify which policy to look at

getFirstDayOfIntervention <- function(intervention, 
                                      geo_value, 
                                      policy.name){
  
  # get intervention data for the specified state only
  policy.valid.time <- intervention %>% 
    filter(geo_value == geo_value & total.num.policy > 0)
  
  # Get the first date of the intervention
  # 1. Get the column index that corresponds to the policy 
  colIdx <- which(names(policy.valid.time) == policy.name)
  # 2. Get the row index that corresponds to the days that policy is enacted
  rowIdx <- policy.valid.time[,colIdx]==1

  # 3. Get the first day 
  intervention.first.day <-policy.valid.time[rowIdx, "time_value"]
  
  firstday <- as.data.frame(intervention.first.day)[1,1]
  
  return(firstday)
}

################## getLastDayOfIntervention() ###################

getLastDayOfIntervention <- function(intervention, 
                                      geo_value, 
                                      policy.name){
  
  # get intervention data for the specified state only
  policy.valid.time <- intervention %>% 
    filter(geo_value == geo_value & total.num.policy > 0)
  
  # Get the first date of the intervention
  # 1. Get the column index that corresponds to the policy 
  colIdx <- which(names(policy.valid.time) == policy.name)
  # 2. Get the row index that corresponds to the days that policy is ended
  rowIdx <- which(diff(policy.valid.time[,colIdx]) == -1) + 1
  
  # 3. Get the first day 
  lastday <-policy.valid.time[rowIdx, "time_value"]
  
  return(lastday)
}


################### getCovidcastLikePolicySignal() #################
# A wrapper to load the policy data and transform the data at the 
# same time

getCovidcastLikePolicySignal <- function(STARTDATE, ENDDATE){
  
  # load the policy data
  policy <- load_policy()
  
  # Compute number of policy and 7-day rolling average
  policy_signal <- getSumOfPolicy(policy, STARTDATE, ENDDATE)
  
  # Transform the policy data into a covidcast like signal dataframe
  policy_signal <-transformToCovidcastLike(policy_signal)
  
  return(policy_signal)
}


