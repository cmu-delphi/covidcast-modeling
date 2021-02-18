#vishnu shankar
#get performance of approaches

source("LeadingIndicatorToolsVS_v2.R")
library(covidcast)
library(magrittr)
library(data.table)
library(dplyr)
library(tidyverse)
library(assertthat)
library(lubridate)
fb_survey2 = get_and_prepare_signals(cases_deaths="case", 
                                     start_day="2020-05-01", 
                                     end_day="2021-01-31",  
                                     indicator_source="fb-survey", 
                                     indicator_signal="smoothed_hh_cmnty_cli", 
                                     case_death_threshold = 2000,
                                     indicator_threshold=80,
                                     geo_type = "county")
fb_survey2=readRDS(file="fb_survey.RDS")

fb_survey_cases_indicator_list = get_increase_points(case_list = fb_survey2$cases, 
                                                     indicator_list = fb_survey2$indicator,
                                                     local_bandwidth = 12,
                                                     local_quantile_threshold = 0.75,
                                                     local_threshold=0.2,
                                                     local_period=0)

guessers=generate_competitors_get_scores(final_cases_indicator_list = fb_survey_cases_indicator_list)
get_performances=get_county_level_precision_recall(final_cases_indicator_list = guessers, min_window = 3, max_window = 14)


#FB Survey Data: Community Signals
#Precision_Leading_Indicator Recall_Leading_Indicator Precision_Random Recall_Random Precision_First_Case_Deriv Recall_First_Case_Deriv
#                   0.2073503                0.2705113        0.9863884     0.1479479                  0.9278584               0.1259167
#Precision_First_Ind_Deriv Recall_First_Ind_Deriv
#                 0.8656987              0.1818735