library(ggplot2)
library (readr)
library(tidyverse)
library(dplyr)
library(covidcast)

########## plot.all.Corr.Median.by.shift() ################

plot.all.Corr.Median.by.shift <- function(other_signals, 
                                          main_signal, 
                                          shiftday, 
                                          names, 
                                          corr.method, 
                                          title, 
                                          by_method){
  
  # Compute correlation between other covidcast-like signals and mobility
  df.ls = list()
  for (i in 1:length(other_signals)){
    med.df <- getMedianCorr(shiftday, other_signals[[i]], main_signal, corr.method, names[[i]], by_method)
    df.ls[[i]] <- med.df
  }
  
  # Stack two dataframe row-wise
  df_all_signals <- bind_rows(df.ls)
  
  # plot the graph 
  ggplot(df_all_signals, aes(x = dt, y = median)) + geom_line(aes(color = Comparison)) + geom_point(aes(color = Comparison)) +
    labs(title = title,
         x = "Shift", y = "Correlation") +
    theme(legend.title = element_blank())
}


########## plotInterventionTime() ################
# Plot the first day of the intervention over time via ggplot
# arguments:

# intervention_mobility_case (data.frame): a dataframe that contains mobility
# left joined by specified policy dataframe

# mobility.name (character) : 
# intervention.first.day(character): a string that specify the first date of 
#                                   intervention

# ylab (character): name for y axis 
# xlab (character): name for x axis

plotInterventionTime <-function(intervention_mobility_case, 
                                mobility.name,
                                intervention.first.day,
                                ylab,
                                xlab){
  
  # Plot the time-series: mobility signal as y, time as x 
  intervention_mobility_case %>%
    ggplot(aes_string(x = "time_value", y = mobility.name)) + 
    geom_point() +
    geom_vline(xintercept = intervention.first.day) + 
    labs(y = ylab, 
         x = xlab)
  
}

################## plotRD () ############################
# a helper function to draw the regression discontinuty design
# arguments:

# mobility.df(data.frame): a covidcast like signal dataframe with single
#                           mobiility signal

# policy.df (data.frame): a policy dataframe that output by load_policy()

# policyName(character): the name of the policy you are looking at

# stateName(character): the name of the state postal, e.g. "ca"

# STARTDATE(date): the start date to filter the policy dataframe

# ENDDATE(date): the start date to filter the policy dataframe

plotRD <- function(mobility.df,
                   policy.df,
                   policyName,
                   stateName,
                   countyName,
                   STARTDATE,
                   ENDDATE,
                   policy.firstday,
                   dropDaysAfterIntervention,
                   showMultiplePolicies,
                   plotMultiple=F,
                   count=NULL){
  
  # filter mobility by the specified state
  mobility.df <- mobility.df %>% filter(geo_value == stateName)
  
  # Get the mobility signal name
  mobilityName <- unique(mobility.df$signal)
  
  # filter the policy data
  policy.df <- policy.df %>% filter(StatePostal == stateName)
  
  # compute the number of policies and rolling mean of the number
  # for each day between start and end dates
  policy_signal <- getSumOfPolicy(policy.df, STARTDATE, ENDDATE)
  
  # left join mobility with policy signal by time 
  df <- left_join(mobility.df, policy_signal, by = "time_value")
  
  # drop all the weekends in the data
  filtered.df <- df %>% 
    mutate(weekday= weekdays(as.Date(time_value)))%>% 
    filter(weekday %in% c("Saturday", "Sunday"))
  
  # Drop 2-weeks of data to account for the lag 
  if(dropDaysAfterIntervention){
    
    # Define the 2 weeks time interval 
    drop_period <- seq(as.Date(policy.firstday), 
        as.Date(policy.firstday)+14, by="days")
    
    # Filter the dataframe
    filtered.df<- filtered.df %>% 
      filter(!(time_value %in% drop_period))
  }
  
  # Compute the difference in means of the matched samples.
  diff <- as.numeric(as.Date(policy.firstday)-as.Date("2020-01-05"))
  
  first_sample_period <- seq(as.Date("2020-01-05"), 
                             as.Date(policy.firstday), by="days")
  
  first_sample <- filtered.df %>%
    filter(time_value %in% first_sample_period)
  
  num_data_points <- nrow(first_sample)
  
  idx <- which(filtered.df$time_value > as.Date(policy.firstday))
  
  selected_idx <- idx[1:num_data_points]

  second_sample <- filtered.df[selected_idx,]
  
  print("The number of data points:")
  print(num_data_points)
  print(t.test(first_sample$value, second_sample$value, paired=F))
  
  test.result <- t.test(first_sample$value, second_sample$value, paired=F)
  
  # Get the mean difference
  mean.diff <- as.numeric(test.result$estimate[1]-test.result$estimate[2])
  
  LCI <- test.result$conf.int[1]
  UCI <- test.result$conf.int[2]
    
  if(plotMultiple){
    if(showMultiplePolicies){
      # Plot the RD 
      p <- filtered.df %>% 
        mutate(intervention= as.factor(total.num.policy)) %>%
        ggplot(aes(x = time_value, 
                   y = value, 
                   color = intervention)) +
        geom_point() + 
        geom_smooth(method = "lm")+
        labs(title = as.character(count))+ 
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank(),
              legend.position = "none")
    }else{
      # Plot the RD 
      p <- filtered.df %>% 
        mutate(intervention= as.factor(eval(parse(text=policyName)))) %>%
        ggplot(aes(x = time_value, 
                   y = value, 
                   color = intervention)) +
        geom_point() + 
        geom_smooth(method = "lm")+
        labs(title = as.character(count))+ 
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank(),
              legend.position = "none")
    }
    
  }else{
    if(showMultiplePolicies){
      # Plot the RD 
      p <- filtered.df %>% 
        mutate(intervention= as.factor(total.num.policy)) %>%
        ggplot(aes(x = time_value, y = value, color = intervention)) +
        geom_point() + 
        geom_smooth(method = "lm")+
        labs(title = paste(" Mobility ~ time (", 
                           stateName,
                           "-",
                           countyName,
                           ")",
                           ",", 
                           policyName,
                           ";",
                           as.numeric(ENDDATE-STARTDATE)," day(s)"), 
             x = "Time", 
             y =  mobilityName)
    }else{
      # Plot the RD 
      p <- filtered.df %>% 
        mutate(intervention= as.factor(eval(parse(text=policyName)))) %>%
        ggplot(aes(x = time_value, y = value, color = intervention)) +
        geom_point() + 
        geom_smooth(method = "lm")+
        labs(title = paste(" Mobility ~ time (", 
                           stateName,
                           "-",
                           countyName,
                           ")",
                           ",", 
                           policyName,
                           ";",
                           as.numeric(ENDDATE-STARTDATE)," day(s)"), 
             x = "Time", 
             y =  mobilityName)
    }

  }

  
  return(list(p=p, mean.difference=mean.diff, LCI=LCI, UCI=UCI))
  
}
