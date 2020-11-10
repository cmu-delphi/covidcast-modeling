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
    labs(y = "Full time away home signal", 
         x = "time-value")
  
}

