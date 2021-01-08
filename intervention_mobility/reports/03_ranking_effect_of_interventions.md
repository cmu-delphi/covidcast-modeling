---
title: "Ranking the effect of intervention at countylevel"
author: "Kenneth Lee"
date: "06/01/2021"
output:
  html_document:
    code_folding: hide
    keep_md: yes
    toc: yes
    toc_float:
      collapsed: yes
      smooth_scroll: yes
---

# Abstract

This notebook has two objectives. Firstly, it aims to measure the effect of interventions on mobility with other potential confounding signals such as confirmed case counts. The motivation is to observe whether the effect of intervention is greater than the effect of case signals on mobility. Secondly, we will rank the effects of various government interventions.

Due to the limitation of computation, we focus on analyzing the state of California and its counties. We will regress mobility signals on the covariates of interests and measure the effect of the covariates based on its regression coefficients.


```r
library(dplyr)
library(lubridate)
library(covidcast)
library(RcppRoll)
library(tidycensus)
library(reshape2)
library(zoo)
library(readxl)

source("code/loader.r")
source("code/parser.r")

# Load files 
confirmed_7dav_cumulative_prop <- read_excel("data/case_signals/ca-county-confirmed_7dav_cumulative_prop.xlsx")
 
confirmed_7dav_incidence_prop <- read_excel("data/case_signals/ca-county-confirmed_7dav_incidence_prop.xlsx")
 
deaths_7dav_cumulative_prop <- read_excel("data/case_signals/ca-county-deaths_7dav_cumulative_prop.xlsx")
# 
deaths_7dav_incidence_prop <- read_excel("data/case_signals/ca-county-deaths_7dav_incidence_prop.xlsx")

STARTDATE <- "2020-02-20"
ENDDATE <- "2021-01-03"

# Get fips codes
ca.fips_codes <- fips_codes %>% filter(state=="CA") %>%
  mutate(fips = paste(state_code, county_code , sep=""))


temp.ca.ftime <- read.csv("data/20200220-ca-ftime.csv",
                                       stringsAsFactors = F)

#temp.ca.ftime <- covidcast_signal(data_source = "safegraph", 
#                            signal ="full_time_work_prop",
#                            start_day = STARTDATE, 
#                            end_day = ENDDATE,
#                            geo_type = "county", 
#                            geo_values = ca.fips_codes$fips)


# preprocess data types
 confirmed_7dav_cumulative_prop$time_value <- as.Date(confirmed_7dav_cumulative_prop$time_value)
 
 confirmed_7dav_incidence_prop$time_value <- as.Date(confirmed_7dav_incidence_prop$time_value)
 
 deaths_7dav_cumulative_prop$time_value <- as.Date(deaths_7dav_cumulative_prop$time_value)
 
 deaths_7dav_incidence_prop$time_value <- as.Date(deaths_7dav_incidence_prop$time_value)
 
 temp.ca.ftime$time_value <- as.Date(temp.ca.ftime$time_value)
 
 confirmed_7dav_cumulative_prop$geo_value <- paste("0", confirmed_7dav_cumulative_prop$geo_value, sep="" )
 
 confirmed_7dav_incidence_prop$geo_value <- paste("0", confirmed_7dav_incidence_prop$geo_value, sep="" )
 
 deaths_7dav_cumulative_prop$geo_value <- paste("0", deaths_7dav_cumulative_prop$geo_value, sep="" )
 
 deaths_7dav_incidence_prop$geo_value <- paste("0", deaths_7dav_incidence_prop$geo_value, sep="" )
 
temp.ca.ftime$geo_value <- paste("0",temp.ca.ftime$geo_value, sep="")


# Policy data
policy <- load_policy()
ca.policy <- policy %>% filter(StatePostal == "ca", StateWide == 1)

# filter all counties
# California mobility signal
ca.ftime <- read.csv("data/ca.ftime.csv")
ca.counties <- as.character(unique(ca.ftime$county))


ca.policy_signal<- getSumOfPolicy(ca.policy , STARTDATE, ENDDATE)

# Turn the polical signal to be factors
factored.ca.policy.signal <- cbind(ca.policy_signal[1], 
                                lapply(ca.policy_signal[3:14],
                                       as.factor),
                                ca.policy_signal[15:16])
```


```r
################ getForwardDays () ###################
# compute the number of days within a given day range that
# gives the highest spearman correlation between df1 and df2

# argument
# df1: a dataframe 
# df2: a dataframe that the data needs to be forwarded
# dt_vec: a vector contains a range of integers

getForwardDays <- function(df1, df2, dt_vec){
  # Empty list
  df_list <- vector("list", length(dt_vec))
  
  for (i in 1:length(dt_vec)) {
    df_list[[i]] <- covidcast_cor(df1, df2, dt_y = dt_vec[i],
                                  by = "geo_value", 
                                  method="spearman")

    df_list[[i]]$dt <- dt_vec[i]
  }
  
  # Stack into one big data frame
  df <- do.call(rbind, df_list)
  

  return(df)
}
```

# Lag analysis

We would like to forward mobility in time that is most correlated with the respective case count signals and use those case count signals in regression.


```r
# We want to select the day in which the case count is most correlated with the mobility in terms of spearman correlation in time

dt_vec = 0:100

# Confirmed cases
cum.confirmed.case.cor.df <- getForwardDays(temp.ca.ftime,confirmed_7dav_cumulative_prop, dt_vec)


daily.new.case.cor.df <- getForwardDays(temp.ca.ftime,confirmed_7dav_incidence_prop, dt_vec)

# Death case
cum.death.cor.df <- getForwardDays(temp.ca.ftime,deaths_7dav_cumulative_prop, dt_vec)

daily.death.case.cor.df <- getForwardDays(temp.ca.ftime,deaths_7dav_incidence_prop, dt_vec)
```



```r
# Combine case and mobility
cum.confirmed.case <- confirmed_7dav_cumulative_prop[c("time_value","geo_value","value")]
new.confirmed.case <- confirmed_7dav_incidence_prop[c("time_value","geo_value","value")]
cum.death.case <- deaths_7dav_cumulative_prop[c("time_value","geo_value","value")]
new.death.case <- deaths_7dav_incidence_prop[c("time_value","geo_value","value")]
mobility <- temp.ca.ftime

# change name
colnames(mobility)[7] <- "full_time_work_prop"
colnames(cum.confirmed.case)[3] <- "confirmed_7dav_cumulative_prop"
colnames(new.confirmed.case)[3] <- "confirmed_7dav_incidence_prop"
colnames(cum.death.case)[3] <- "deaths_7dav_cumulative_prop"
colnames(new.death.case)[3] <- "deaths_7dav_incidence_prop"

# A list of confounders
confounders <- list(cum.confirmed.case, 
                    new.confirmed.case, 
                    cum.death.case,
                    new.death.case)

# Left join again with all other potential confounders
for (confounder in confounders){
  mobility <- left_join(mobility, confounder, by=c("time_value", "geo_value"))
}

# combine with policy signal
mobility <- left_join(mobility, factored.ca.policy.signal, by=c("time_value"))
```

## Model 1:

$$Y_t = \beta_{0}+ \sum_{i}\beta_{i}S_{i_{t}} + \sum_{k}\beta_{k}P_{k_{t-r}}$$, where $r$ varies depending on the county and the correlation between case signal and response


```r
cof_ls <- list()
count <- 1

for(county in ca.fips_codes$fips){
  
  selected.df <- mobility %>% filter(geo_value==county)
  county.mobility <- mobility %>% filter(geo_value==county)
  
  # Get the days for most correlated
  
  # filter the data
  filtered.confirmed.case.df <- cum.confirmed.case.cor.df %>%
    filter(geo_value == county)
  
  filtered.daily.new.case.df <- daily.new.case.cor.df %>%
    filter(geo_value == county)
  
  filtered.cum.death.df <- cum.death.cor.df %>%
    filter(geo_value == county)
  
  filtered.daily.death.case.df <- daily.death.case.cor.df %>%
    filter(geo_value == county)
  
  # if there is no missing value, we go ahead to compute the switch time
  if(!any(is.na(filtered.confirmed.case.df$value))){
    
    days <- min(filtered.confirmed.case.df[abs(filtered.confirmed.case.df$value) == max(abs(filtered.confirmed.case.df$value)),]$dt)
    
    # switch the column 
    state<-county
    
    selected.df <- shiftDays(county.mobility, selected.df, days, c("confirmed_7dav_cumulative_prop"))
    
  }
  
  if(!any(is.na(filtered.daily.new.case.df$value))){
    
    days <- min(filtered.daily.new.case.df[abs(filtered.daily.new.case.df$value) == max(abs(filtered.daily.new.case.df$value)),]$dt)
      
    # switch the column 
    state<-county
    
    selected.df <- shiftDays(county.mobility, selected.df, days, c("confirmed_7dav_incidence_prop"))
    
  }
  
  if(!any(is.na(filtered.cum.death.df$value))){
    
    days <- min(filtered.cum.death.df[abs(filtered.cum.death.df$value) == max(abs(filtered.cum.death.df$value)),]$dt)
      
    # switch the column 
    state<-county
    
    selected.df <- shiftDays(county.mobility, selected.df, days, c("deaths_7dav_cumulative_prop"))
    
  }
  
  if(!any(is.na(filtered.daily.death.case.df$value))){
    
    days <- min(filtered.daily.death.case.df[abs(filtered.daily.death.case.df$value) == max(abs(filtered.daily.death.case.df$value)),]$dt)
      
    # switch the column 
    state<-county
    
    selected.df <- shiftDays(county.mobility, selected.df, days, c("deaths_7dav_incidence_prop"))
    
  }


  # fit the regression
   p <- selected.df  %>% 
     mutate(sumEmergDec = roll_sum(EmergDec, 2, align = "right", fill = NA),
            sumGathRestrict=roll_sum(GathRestrict, 2, align = "right", fill = NA),
            sumSchoolClose=roll_sum(SchoolClose, 2, align = "right", fill = NA),
            sumBarRestrict=roll_sum(BarRestrict, 2, align = "right", fill = NA),
            sumNEBusinessClose=roll_sum(NEBusinessClose, 2, align = "right", fill = NA),
            sumRestaurantRestrict=roll_sum(RestaurantRestrict, 2, align = "right", fill = NA),
            sumStayAtHome=roll_sum(StayAtHome, 2, align = "right", fill = NA),
            sumPublicMask=roll_sum(PublicMask, 2, align = "right", fill = NA),
            sumOtherBusinessClose=roll_sum(OtherBusinessClose, 2, align = "right", fill = NA),
            sumSchoolMask=roll_sum(SchoolMask, 2, align = "right", fill = NA),
            sumQuarantine=roll_sum(Quarantine, 2, align = "right", fill = NA),
            sumBusinessMask=roll_sum(BusinessMask, 2, align = "right", fill = NA)
            ) %>%
     lm(full_time_work_prop ~ confirmed_7dav_cumulative_prop+confirmed_7dav_incidence_prop+deaths_7dav_cumulative_prop+sumEmergDec + sumGathRestrict+sumSchoolClose+sumBarRestrict+sumNEBusinessClose+sumRestaurantRestrict+sumStayAtHome+sumPublicMask+sumOtherBusinessClose+sumBusinessMask+sumSchoolMask+sumQuarantine, data=.)
   
   # record the result
   cof_ls[[count]] <- p
   count <- count + 1
}

# Print all summaries
for(fit in cof_ls){
  print(summary(fit))
}
```

```
## 
## Call:
## lm(formula = full_time_work_prop ~ confirmed_7dav_cumulative_prop + 
##     confirmed_7dav_incidence_prop + deaths_7dav_cumulative_prop + 
##     sumEmergDec + sumGathRestrict + sumSchoolClose + sumBarRestrict + 
##     sumNEBusinessClose + sumRestaurantRestrict + sumStayAtHome + 
##     sumPublicMask + sumOtherBusinessClose + sumBusinessMask + 
##     sumSchoolMask + sumQuarantine, data = .)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -26.983  -6.628   1.327   6.745  15.896 
## 
## Coefficients: (8 not defined because of singularities)
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    50.320292  13.022457   3.864 0.000144 ***
## confirmed_7dav_cumulative_prop -0.015649   0.008934  -1.752 0.081123 .  
## confirmed_7dav_incidence_prop   1.715908   0.286789   5.983 8.14e-09 ***
## deaths_7dav_cumulative_prop    -0.239841   0.533946  -0.449 0.653714    
## sumEmergDec                           NA         NA      NA       NA    
## sumGathRestrict                       NA         NA      NA       NA    
## sumSchoolClose                        NA         NA      NA       NA    
## sumBarRestrict                        NA         NA      NA       NA    
## sumNEBusinessClose                    NA         NA      NA       NA    
## sumRestaurantRestrict                 NA         NA      NA       NA    
## sumStayAtHome                         NA         NA      NA       NA    
## sumPublicMask                         NA         NA      NA       NA    
## sumOtherBusinessClose          -5.450621   3.252743  -1.676 0.095133 .  
## sumBusinessMask                 4.549603   1.310891   3.471 0.000618 ***
## sumSchoolMask                   0.398314   1.070139   0.372 0.710075    
## sumQuarantine                  -0.784396   1.920093  -0.409 0.683266    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 9.037 on 234 degrees of freedom
##   (77 observations deleted due to missingness)
## Multiple R-squared:  0.4769,	Adjusted R-squared:  0.4612 
## F-statistic: 30.48 on 7 and 234 DF,  p-value: < 2.2e-16
## 
## 
## Call:
## lm(formula = full_time_work_prop ~ confirmed_7dav_cumulative_prop + 
##     confirmed_7dav_incidence_prop + deaths_7dav_cumulative_prop + 
##     sumEmergDec + sumGathRestrict + sumSchoolClose + sumBarRestrict + 
##     sumNEBusinessClose + sumRestaurantRestrict + sumStayAtHome + 
##     sumPublicMask + sumOtherBusinessClose + sumBusinessMask + 
##     sumSchoolMask + sumQuarantine, data = .)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -23.1035  -6.6255   0.3963   6.6419  15.4400 
## 
## Coefficients: (9 not defined because of singularities)
##                                  Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                     85.455149   3.678718  23.230  < 2e-16 ***
## confirmed_7dav_cumulative_prop  -0.021477   0.004067  -5.281 2.69e-07 ***
## confirmed_7dav_incidence_prop   -0.059034   0.012371  -4.772 3.03e-06 ***
## deaths_7dav_cumulative_prop            NA         NA      NA       NA    
## sumEmergDec                            NA         NA      NA       NA    
## sumGathRestrict                        NA         NA      NA       NA    
## sumSchoolClose                         NA         NA      NA       NA    
## sumBarRestrict                         NA         NA      NA       NA    
## sumNEBusinessClose                     NA         NA      NA       NA    
## sumRestaurantRestrict                  NA         NA      NA       NA    
## sumStayAtHome                          NA         NA      NA       NA    
## sumPublicMask                          NA         NA      NA       NA    
## sumOtherBusinessClose          -11.866307   1.324628  -8.958  < 2e-16 ***
## sumBusinessMask                  6.328407   1.252473   5.053 8.16e-07 ***
## sumSchoolMask                   -0.456363   0.732008  -0.623    0.534    
## sumQuarantine                   -6.513096   0.828263  -7.864 9.67e-14 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.758 on 263 degrees of freedom
##   (49 observations deleted due to missingness)
## Multiple R-squared:  0.5932,	Adjusted R-squared:  0.5839 
## F-statistic: 63.92 on 6 and 263 DF,  p-value: < 2.2e-16
## 
## 
## Call:
## lm(formula = full_time_work_prop ~ confirmed_7dav_cumulative_prop + 
##     confirmed_7dav_incidence_prop + deaths_7dav_cumulative_prop + 
##     sumEmergDec + sumGathRestrict + sumSchoolClose + sumBarRestrict + 
##     sumNEBusinessClose + sumRestaurantRestrict + sumStayAtHome + 
##     sumPublicMask + sumOtherBusinessClose + sumBusinessMask + 
##     sumSchoolMask + sumQuarantine, data = .)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -22.6589  -6.3906   0.5356   6.6678  15.9643 
## 
## Coefficients: (9 not defined because of singularities)
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    14.642722  12.256210   1.195 0.233501    
## confirmed_7dav_cumulative_prop  0.005131   0.004828   1.063 0.289065    
## confirmed_7dav_incidence_prop  -0.057315   0.015472  -3.704 0.000269 ***
## deaths_7dav_cumulative_prop    -0.471527   0.083688  -5.634 5.42e-08 ***
## sumEmergDec                           NA         NA      NA       NA    
## sumGathRestrict                       NA         NA      NA       NA    
## sumSchoolClose                        NA         NA      NA       NA    
## sumBarRestrict                        NA         NA      NA       NA    
## sumNEBusinessClose                    NA         NA      NA       NA    
## sumRestaurantRestrict                 NA         NA      NA       NA    
## sumStayAtHome                         NA         NA      NA       NA    
## sumPublicMask                         NA         NA      NA       NA    
## sumOtherBusinessClose                 NA         NA      NA       NA    
## sumBusinessMask                 9.699369   2.996154   3.237 0.001396 ** 
## sumSchoolMask                  -0.699353   0.793935  -0.881 0.379363    
## sumQuarantine                  -3.181016   1.677063  -1.897 0.059185 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.755 on 217 degrees of freedom
##   (95 observations deleted due to missingness)
## Multiple R-squared:  0.5377,	Adjusted R-squared:  0.5249 
## F-statistic: 42.06 on 6 and 217 DF,  p-value: < 2.2e-16
## 
## 
## Call:
## lm(formula = full_time_work_prop ~ confirmed_7dav_cumulative_prop + 
##     confirmed_7dav_incidence_prop + deaths_7dav_cumulative_prop + 
##     sumEmergDec + sumGathRestrict + sumSchoolClose + sumBarRestrict + 
##     sumNEBusinessClose + sumRestaurantRestrict + sumStayAtHome + 
##     sumPublicMask + sumOtherBusinessClose + sumBusinessMask + 
##     sumSchoolMask + sumQuarantine, data = .)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -22.5309  -5.9859   0.4447   6.3404  15.9432 
## 
## Coefficients: (9 not defined because of singularities)
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    27.596836   5.906074   4.673 5.06e-06 ***
## confirmed_7dav_cumulative_prop  0.017412   0.009596   1.815 0.070891 .  
## confirmed_7dav_incidence_prop   0.263710   0.164856   1.600 0.111052    
## deaths_7dav_cumulative_prop    -2.535808   0.655161  -3.871 0.000142 ***
## sumEmergDec                           NA         NA      NA       NA    
## sumGathRestrict                       NA         NA      NA       NA    
## sumSchoolClose                        NA         NA      NA       NA    
## sumBarRestrict                        NA         NA      NA       NA    
## sumNEBusinessClose                    NA         NA      NA       NA    
## sumRestaurantRestrict                 NA         NA      NA       NA    
## sumStayAtHome                         NA         NA      NA       NA    
## sumPublicMask                         NA         NA      NA       NA    
## sumOtherBusinessClose                 NA         NA      NA       NA    
## sumBusinessMask                 5.482896   1.223226   4.482 1.16e-05 ***
## sumSchoolMask                  -0.836587   0.780369  -1.072 0.284825    
## sumQuarantine                  -1.162193   1.806339  -0.643 0.520607    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.36 on 230 degrees of freedom
##   (82 observations deleted due to missingness)
## Multiple R-squared:  0.5585,	Adjusted R-squared:  0.547 
## F-statistic: 48.49 on 6 and 230 DF,  p-value: < 2.2e-16
## 
## 
## Call:
## lm(formula = full_time_work_prop ~ confirmed_7dav_cumulative_prop + 
##     confirmed_7dav_incidence_prop + deaths_7dav_cumulative_prop + 
##     sumEmergDec + sumGathRestrict + sumSchoolClose + sumBarRestrict + 
##     sumNEBusinessClose + sumRestaurantRestrict + sumStayAtHome + 
##     sumPublicMask + sumOtherBusinessClose + sumBusinessMask + 
##     sumSchoolMask + sumQuarantine, data = .)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -23.7397  -6.6685   0.6787   6.7294  15.3490 
## 
## Coefficients: (10 not defined because of singularities)
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    56.158015   4.036809  13.911  < 2e-16 ***
## confirmed_7dav_cumulative_prop -0.002422   0.003640  -0.665   0.5065    
## confirmed_7dav_incidence_prop  -0.088915   0.073519  -1.209   0.2278    
## deaths_7dav_cumulative_prop    -0.689464   0.259567  -2.656   0.0085 ** 
## sumEmergDec                           NA         NA      NA       NA    
## sumGathRestrict                       NA         NA      NA       NA    
## sumSchoolClose                        NA         NA      NA       NA    
## sumBarRestrict                        NA         NA      NA       NA    
## sumNEBusinessClose                    NA         NA      NA       NA    
## sumRestaurantRestrict                 NA         NA      NA       NA    
## sumStayAtHome                         NA         NA      NA       NA    
## sumPublicMask                         NA         NA      NA       NA    
## sumOtherBusinessClose                 NA         NA      NA       NA    
## sumBusinessMask                       NA         NA      NA       NA    
## sumSchoolMask                  -0.184220   1.107531  -0.166   0.8681    
## sumQuarantine                  -4.462958   1.116792  -3.996 8.87e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.631 on 213 degrees of freedom
##   (100 observations deleted due to missingness)
## Multiple R-squared:  0.5472,	Adjusted R-squared:  0.5365 
## F-statistic: 51.47 on 5 and 213 DF,  p-value: < 2.2e-16
## 
## 
## Call:
## lm(formula = full_time_work_prop ~ confirmed_7dav_cumulative_prop + 
##     confirmed_7dav_incidence_prop + deaths_7dav_cumulative_prop + 
##     sumEmergDec + sumGathRestrict + sumSchoolClose + sumBarRestrict + 
##     sumNEBusinessClose + sumRestaurantRestrict + sumStayAtHome + 
##     sumPublicMask + sumOtherBusinessClose + sumBusinessMask + 
##     sumSchoolMask + sumQuarantine, data = .)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -23.1093  -7.7926   0.7196   8.5620  16.7595 
## 
## Coefficients: (10 not defined because of singularities)
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    52.316222   5.981914   8.746 6.61e-16 ***
## confirmed_7dav_cumulative_prop -0.007998   0.002854  -2.802  0.00554 ** 
## confirmed_7dav_incidence_prop   0.199025   0.080424   2.475  0.01411 *  
## deaths_7dav_cumulative_prop    -0.018460   0.128030  -0.144  0.88549    
## sumEmergDec                           NA         NA      NA       NA    
## sumGathRestrict                       NA         NA      NA       NA    
## sumSchoolClose                        NA         NA      NA       NA    
## sumBarRestrict                        NA         NA      NA       NA    
## sumNEBusinessClose                    NA         NA      NA       NA    
## sumRestaurantRestrict                 NA         NA      NA       NA    
## sumStayAtHome                         NA         NA      NA       NA    
## sumPublicMask                         NA         NA      NA       NA    
## sumOtherBusinessClose                 NA         NA      NA       NA    
## sumBusinessMask                       NA         NA      NA       NA    
## sumSchoolMask                  -0.868083   0.942084  -0.921  0.35785    
## sumQuarantine                  -2.235364   2.551293  -0.876  0.38192    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 9.713 on 214 degrees of freedom
##   (99 observations deleted due to missingness)
## Multiple R-squared:  0.4278,	Adjusted R-squared:  0.4144 
## F-statistic:    32 on 5 and 214 DF,  p-value: < 2.2e-16
## 
## 
## Call:
## lm(formula = full_time_work_prop ~ confirmed_7dav_cumulative_prop + 
##     confirmed_7dav_incidence_prop + deaths_7dav_cumulative_prop + 
##     sumEmergDec + sumGathRestrict + sumSchoolClose + sumBarRestrict + 
##     sumNEBusinessClose + sumRestaurantRestrict + sumStayAtHome + 
##     sumPublicMask + sumOtherBusinessClose + sumBusinessMask + 
##     sumSchoolMask + sumQuarantine, data = .)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -19.593  -4.572  -0.544   5.376  17.892 
## 
## Coefficients: (8 not defined because of singularities)
##                                  Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    108.573730   7.176637  15.129  < 2e-16 ***
## confirmed_7dav_cumulative_prop  -0.081055   0.007682 -10.551  < 2e-16 ***
## confirmed_7dav_incidence_prop   -0.093706   0.192798  -0.486    0.627    
## deaths_7dav_cumulative_prop      8.281139   0.834917   9.919  < 2e-16 ***
## sumEmergDec                            NA         NA      NA       NA    
## sumGathRestrict                        NA         NA      NA       NA    
## sumSchoolClose                         NA         NA      NA       NA    
## sumBarRestrict                         NA         NA      NA       NA    
## sumNEBusinessClose                     NA         NA      NA       NA    
## sumRestaurantRestrict                  NA         NA      NA       NA    
## sumStayAtHome                          NA         NA      NA       NA    
## sumPublicMask                          NA         NA      NA       NA    
## sumOtherBusinessClose          -11.616689   1.496203  -7.764 2.24e-13 ***
## sumBusinessMask                  0.948130   1.170657   0.810    0.419    
## sumSchoolMask                   -8.011321   1.280684  -6.256 1.75e-09 ***
## sumQuarantine                  -10.105970   1.611177  -6.272 1.60e-09 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 7.918 on 245 degrees of freedom
##   (66 observations deleted due to missingness)
## Multiple R-squared:  0.5993,	Adjusted R-squared:  0.5878 
## F-statistic: 52.34 on 7 and 245 DF,  p-value: < 2.2e-16
## 
## 
## Call:
## lm(formula = full_time_work_prop ~ confirmed_7dav_cumulative_prop + 
##     confirmed_7dav_incidence_prop + deaths_7dav_cumulative_prop + 
##     sumEmergDec + sumGathRestrict + sumSchoolClose + sumBarRestrict + 
##     sumNEBusinessClose + sumRestaurantRestrict + sumStayAtHome + 
##     sumPublicMask + sumOtherBusinessClose + sumBusinessMask + 
##     sumSchoolMask + sumQuarantine, data = .)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -23.483  -7.079   1.544   7.537  16.100 
## 
## Coefficients: (10 not defined because of singularities)
##                                Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    60.13358    4.89753  12.278  < 2e-16 ***
## confirmed_7dav_cumulative_prop -0.04017    0.01421  -2.827 0.005147 ** 
## confirmed_7dav_incidence_prop   0.88959    0.22448   3.963 0.000101 ***
## deaths_7dav_cumulative_prop     2.00043    0.84559   2.366 0.018892 *  
## sumEmergDec                          NA         NA      NA       NA    
## sumGathRestrict                      NA         NA      NA       NA    
## sumSchoolClose                       NA         NA      NA       NA    
## sumBarRestrict                       NA         NA      NA       NA    
## sumNEBusinessClose                   NA         NA      NA       NA    
## sumRestaurantRestrict                NA         NA      NA       NA    
## sumStayAtHome                        NA         NA      NA       NA    
## sumPublicMask                        NA         NA      NA       NA    
## sumOtherBusinessClose                NA         NA      NA       NA    
## sumBusinessMask                      NA         NA      NA       NA    
## sumSchoolMask                  -1.14660    0.97981  -1.170 0.243217    
## sumQuarantine                  -5.67895    1.68892  -3.362 0.000916 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 9.447 on 213 degrees of freedom
##   (100 observations deleted due to missingness)
## Multiple R-squared:  0.4574,	Adjusted R-squared:  0.4447 
## F-statistic: 35.92 on 5 and 213 DF,  p-value: < 2.2e-16
## 
## 
## Call:
## lm(formula = full_time_work_prop ~ confirmed_7dav_cumulative_prop + 
##     confirmed_7dav_incidence_prop + deaths_7dav_cumulative_prop + 
##     sumEmergDec + sumGathRestrict + sumSchoolClose + sumBarRestrict + 
##     sumNEBusinessClose + sumRestaurantRestrict + sumStayAtHome + 
##     sumPublicMask + sumOtherBusinessClose + sumBusinessMask + 
##     sumSchoolMask + sumQuarantine, data = .)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -23.1206  -5.9590  -0.2002   6.6561  16.5021 
## 
## Coefficients: (8 not defined because of singularities)
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    55.425464   6.309324   8.785 2.88e-16 ***
## confirmed_7dav_cumulative_prop -0.049922   0.007951  -6.278 1.56e-09 ***
## confirmed_7dav_incidence_prop   0.966401   0.340076   2.842 0.004868 ** 
## deaths_7dav_cumulative_prop     8.122060   2.122569   3.827 0.000165 ***
## sumEmergDec                           NA         NA      NA       NA    
## sumGathRestrict                       NA         NA      NA       NA    
## sumSchoolClose                        NA         NA      NA       NA    
## sumBarRestrict                        NA         NA      NA       NA    
## sumNEBusinessClose                    NA         NA      NA       NA    
## sumRestaurantRestrict                 NA         NA      NA       NA    
## sumStayAtHome                         NA         NA      NA       NA    
## sumPublicMask                         NA         NA      NA       NA    
## sumOtherBusinessClose          -7.502236   1.692358  -4.433 1.41e-05 ***
## sumBusinessMask                 5.250356   1.223783   4.290 2.58e-05 ***
## sumSchoolMask                   2.242093   0.964199   2.325 0.020878 *  
## sumQuarantine                  -1.913826   1.391324  -1.376 0.170230    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.778 on 243 degrees of freedom
##   (68 observations deleted due to missingness)
## Multiple R-squared:  0.5042,	Adjusted R-squared:   0.49 
## F-statistic: 35.31 on 7 and 243 DF,  p-value: < 2.2e-16
## 
## 
## Call:
## lm(formula = full_time_work_prop ~ confirmed_7dav_cumulative_prop + 
##     confirmed_7dav_incidence_prop + deaths_7dav_cumulative_prop + 
##     sumEmergDec + sumGathRestrict + sumSchoolClose + sumBarRestrict + 
##     sumNEBusinessClose + sumRestaurantRestrict + sumStayAtHome + 
##     sumPublicMask + sumOtherBusinessClose + sumBusinessMask + 
##     sumSchoolMask + sumQuarantine, data = .)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -20.7174  -5.9482   0.1089   6.9343  22.0031 
## 
## Coefficients: (8 not defined because of singularities)
##                                  Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                     96.171385   4.594611  20.931  < 2e-16 ***
## confirmed_7dav_cumulative_prop  -0.036863   0.005538  -6.656 1.63e-10 ***
## confirmed_7dav_incidence_prop    0.616974   0.123750   4.986 1.12e-06 ***
## deaths_7dav_cumulative_prop      2.567964   0.385693   6.658 1.61e-10 ***
## sumEmergDec                            NA         NA      NA       NA    
## sumGathRestrict                        NA         NA      NA       NA    
## sumSchoolClose                         NA         NA      NA       NA    
## sumBarRestrict                         NA         NA      NA       NA    
## sumNEBusinessClose                     NA         NA      NA       NA    
## sumRestaurantRestrict                  NA         NA      NA       NA    
## sumStayAtHome                          NA         NA      NA       NA    
## sumPublicMask                          NA         NA      NA       NA    
## sumOtherBusinessClose          -13.150619   1.342528  -9.795  < 2e-16 ***
## sumBusinessMask                  4.072438   1.253168   3.250  0.00131 ** 
## sumSchoolMask                    0.335666   1.118188   0.300  0.76427    
## sumQuarantine                   -8.829002   1.238264  -7.130 9.68e-12 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.934 on 263 degrees of freedom
##   (48 observations deleted due to missingness)
## Multiple R-squared:  0.586,	Adjusted R-squared:  0.575 
## F-statistic: 53.18 on 7 and 263 DF,  p-value: < 2.2e-16
## 
## 
## Call:
## lm(formula = full_time_work_prop ~ confirmed_7dav_cumulative_prop + 
##     confirmed_7dav_incidence_prop + deaths_7dav_cumulative_prop + 
##     sumEmergDec + sumGathRestrict + sumSchoolClose + sumBarRestrict + 
##     sumNEBusinessClose + sumRestaurantRestrict + sumStayAtHome + 
##     sumPublicMask + sumOtherBusinessClose + sumBusinessMask + 
##     sumSchoolMask + sumQuarantine, data = .)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -22.630  -7.103   1.014   6.906  16.242 
## 
## Coefficients: (9 not defined because of singularities)
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    25.619432   6.747513   3.797 0.000189 ***
## confirmed_7dav_cumulative_prop -0.001693   0.001669  -1.014 0.311613    
## confirmed_7dav_incidence_prop  -0.023965   0.012103  -1.980 0.048915 *  
## deaths_7dav_cumulative_prop    -0.363270   0.055574  -6.537 4.19e-10 ***
## sumEmergDec                           NA         NA      NA       NA    
## sumGathRestrict                       NA         NA      NA       NA    
## sumSchoolClose                        NA         NA      NA       NA    
## sumBarRestrict                        NA         NA      NA       NA    
## sumNEBusinessClose                    NA         NA      NA       NA    
## sumRestaurantRestrict                 NA         NA      NA       NA    
## sumStayAtHome                         NA         NA      NA       NA    
## sumPublicMask                         NA         NA      NA       NA    
## sumOtherBusinessClose                 NA         NA      NA       NA    
## sumBusinessMask                 7.134512   1.596899   4.468 1.25e-05 ***
## sumSchoolMask                   2.015807   0.870019   2.317 0.021409 *  
## sumQuarantine                  -6.279364   1.368779  -4.588 7.46e-06 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.906 on 224 degrees of freedom
##   (88 observations deleted due to missingness)
## Multiple R-squared:  0.5117,	Adjusted R-squared:  0.4986 
## F-statistic: 39.12 on 6 and 224 DF,  p-value: < 2.2e-16
## 
## 
## Call:
## lm(formula = full_time_work_prop ~ confirmed_7dav_cumulative_prop + 
##     confirmed_7dav_incidence_prop + deaths_7dav_cumulative_prop + 
##     sumEmergDec + sumGathRestrict + sumSchoolClose + sumBarRestrict + 
##     sumNEBusinessClose + sumRestaurantRestrict + sumStayAtHome + 
##     sumPublicMask + sumOtherBusinessClose + sumBusinessMask + 
##     sumSchoolMask + sumQuarantine, data = .)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -25.145  -7.084   1.125   6.386  19.317 
## 
## Coefficients: (9 not defined because of singularities)
##                                Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    31.18690    6.48512   4.809 2.78e-06 ***
## confirmed_7dav_cumulative_prop  0.01813    0.01335   1.358 0.175969    
## confirmed_7dav_incidence_prop   2.29415    0.54591   4.202 3.81e-05 ***
## deaths_7dav_cumulative_prop    -4.38119    0.85268  -5.138 6.01e-07 ***
## sumEmergDec                          NA         NA      NA       NA    
## sumGathRestrict                      NA         NA      NA       NA    
## sumSchoolClose                       NA         NA      NA       NA    
## sumBarRestrict                       NA         NA      NA       NA    
## sumNEBusinessClose                   NA         NA      NA       NA    
## sumRestaurantRestrict                NA         NA      NA       NA    
## sumStayAtHome                        NA         NA      NA       NA    
## sumPublicMask                        NA         NA      NA       NA    
## sumOtherBusinessClose                NA         NA      NA       NA    
## sumBusinessMask                 5.76527    1.54607   3.729 0.000243 ***
## sumSchoolMask                   1.23498    0.98201   1.258 0.209837    
## sumQuarantine                  -7.04487    1.38569  -5.084 7.77e-07 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.929 on 225 degrees of freedom
##   (87 observations deleted due to missingness)
## Multiple R-squared:  0.5072,	Adjusted R-squared:  0.494 
## F-statistic: 38.59 on 6 and 225 DF,  p-value: < 2.2e-16
## 
## 
## Call:
## lm(formula = full_time_work_prop ~ confirmed_7dav_cumulative_prop + 
##     confirmed_7dav_incidence_prop + deaths_7dav_cumulative_prop + 
##     sumEmergDec + sumGathRestrict + sumSchoolClose + sumBarRestrict + 
##     sumNEBusinessClose + sumRestaurantRestrict + sumStayAtHome + 
##     sumPublicMask + sumOtherBusinessClose + sumBusinessMask + 
##     sumSchoolMask + sumQuarantine, data = .)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -25.0757  -7.6572   0.9127   7.7560  16.3654 
## 
## Coefficients: (8 not defined because of singularities)
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    57.850288   9.142171   6.328 1.22e-09 ***
## confirmed_7dav_cumulative_prop -0.004259   0.001443  -2.952  0.00347 ** 
## confirmed_7dav_incidence_prop   0.040941   0.040015   1.023  0.30728    
## deaths_7dav_cumulative_prop     0.097136   0.037947   2.560  0.01109 *  
## sumEmergDec                           NA         NA      NA       NA    
## sumGathRestrict                       NA         NA      NA       NA    
## sumSchoolClose                        NA         NA      NA       NA    
## sumBarRestrict                        NA         NA      NA       NA    
## sumNEBusinessClose                    NA         NA      NA       NA    
## sumRestaurantRestrict                 NA         NA      NA       NA    
## sumStayAtHome                         NA         NA      NA       NA    
## sumPublicMask                         NA         NA      NA       NA    
## sumOtherBusinessClose          -6.659183   2.108910  -3.158  0.00180 ** 
## sumBusinessMask                 6.421605   1.396500   4.598 6.91e-06 ***
## sumSchoolMask                   4.761634   2.053346   2.319  0.02124 *  
## sumQuarantine                  -8.791756   1.200725  -7.322 3.71e-12 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 9.451 on 239 degrees of freedom
##   (72 observations deleted due to missingness)
## Multiple R-squared:  0.4229,	Adjusted R-squared:  0.406 
## F-statistic: 25.03 on 7 and 239 DF,  p-value: < 2.2e-16
## 
## 
## Call:
## lm(formula = full_time_work_prop ~ confirmed_7dav_cumulative_prop + 
##     confirmed_7dav_incidence_prop + deaths_7dav_cumulative_prop + 
##     sumEmergDec + sumGathRestrict + sumSchoolClose + sumBarRestrict + 
##     sumNEBusinessClose + sumRestaurantRestrict + sumStayAtHome + 
##     sumPublicMask + sumOtherBusinessClose + sumBusinessMask + 
##     sumSchoolMask + sumQuarantine, data = .)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -23.4195  -6.4346  -0.3289   7.4015  15.5981 
## 
## Coefficients: (10 not defined because of singularities)
##                                Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    54.73077    6.36329   8.601 1.73e-15 ***
## confirmed_7dav_cumulative_prop  0.01886    0.01479   1.275  0.20385    
## confirmed_7dav_incidence_prop  -0.04744    0.05583  -0.850  0.39643    
## deaths_7dav_cumulative_prop    -0.49749    0.13981  -3.558  0.00046 ***
## sumEmergDec                          NA         NA      NA       NA    
## sumGathRestrict                      NA         NA      NA       NA    
## sumSchoolClose                       NA         NA      NA       NA    
## sumBarRestrict                       NA         NA      NA       NA    
## sumNEBusinessClose                   NA         NA      NA       NA    
## sumRestaurantRestrict                NA         NA      NA       NA    
## sumStayAtHome                        NA         NA      NA       NA    
## sumPublicMask                        NA         NA      NA       NA    
## sumOtherBusinessClose                NA         NA      NA       NA    
## sumBusinessMask                      NA         NA      NA       NA    
## sumSchoolMask                  -0.83388    1.04007  -0.802  0.42359    
## sumQuarantine                  -3.32175    2.38443  -1.393  0.16504    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.673 on 213 degrees of freedom
##   (100 observations deleted due to missingness)
## Multiple R-squared:  0.5427,	Adjusted R-squared:  0.5319 
## F-statistic: 50.55 on 5 and 213 DF,  p-value: < 2.2e-16
## 
## 
## Call:
## lm(formula = full_time_work_prop ~ confirmed_7dav_cumulative_prop + 
##     confirmed_7dav_incidence_prop + deaths_7dav_cumulative_prop + 
##     sumEmergDec + sumGathRestrict + sumSchoolClose + sumBarRestrict + 
##     sumNEBusinessClose + sumRestaurantRestrict + sumStayAtHome + 
##     sumPublicMask + sumOtherBusinessClose + sumBusinessMask + 
##     sumSchoolMask + sumQuarantine, data = .)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -21.790  -6.865   0.527   7.308  18.120 
## 
## Coefficients: (8 not defined because of singularities)
##                                  Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                     82.538169   5.879844  14.037  < 2e-16 ***
## confirmed_7dav_cumulative_prop  -0.006898   0.002061  -3.346 0.000946 ***
## confirmed_7dav_incidence_prop    0.028254   0.036526   0.774 0.439945    
## deaths_7dav_cumulative_prop      0.612362   0.186198   3.289 0.001152 ** 
## sumEmergDec                            NA         NA      NA       NA    
## sumGathRestrict                        NA         NA      NA       NA    
## sumSchoolClose                         NA         NA      NA       NA    
## sumBarRestrict                         NA         NA      NA       NA    
## sumNEBusinessClose                     NA         NA      NA       NA    
## sumRestaurantRestrict                  NA         NA      NA       NA    
## sumStayAtHome                          NA         NA      NA       NA    
## sumPublicMask                          NA         NA      NA       NA    
## sumOtherBusinessClose           -8.845509   1.616254  -5.473 1.08e-07 ***
## sumBusinessMask                  4.585885   1.301288   3.524 0.000506 ***
## sumSchoolMask                    0.166522   1.074340   0.155 0.876948    
## sumQuarantine                  -10.412627   1.327418  -7.844 1.30e-13 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 9.311 on 248 degrees of freedom
##   (63 observations deleted due to missingness)
## Multiple R-squared:  0.4544,	Adjusted R-squared:  0.439 
## F-statistic: 29.51 on 7 and 248 DF,  p-value: < 2.2e-16
## 
## 
## Call:
## lm(formula = full_time_work_prop ~ confirmed_7dav_cumulative_prop + 
##     confirmed_7dav_incidence_prop + deaths_7dav_cumulative_prop + 
##     sumEmergDec + sumGathRestrict + sumSchoolClose + sumBarRestrict + 
##     sumNEBusinessClose + sumRestaurantRestrict + sumStayAtHome + 
##     sumPublicMask + sumOtherBusinessClose + sumBusinessMask + 
##     sumSchoolMask + sumQuarantine, data = .)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -25.4320  -4.1171   0.6259   5.5835  17.1419 
## 
## Coefficients: (8 not defined because of singularities)
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    65.074721   6.287080  10.351  < 2e-16 ***
## confirmed_7dav_cumulative_prop -0.009786   0.001118  -8.750 3.78e-16 ***
## confirmed_7dav_incidence_prop   0.082941   0.036732   2.258  0.02484 *  
## deaths_7dav_cumulative_prop     0.816452   0.099875   8.175 1.69e-14 ***
## sumEmergDec                           NA         NA      NA       NA    
## sumGathRestrict                       NA         NA      NA       NA    
## sumSchoolClose                        NA         NA      NA       NA    
## sumBarRestrict                        NA         NA      NA       NA    
## sumNEBusinessClose                    NA         NA      NA       NA    
## sumRestaurantRestrict                 NA         NA      NA       NA    
## sumStayAtHome                         NA         NA      NA       NA    
## sumPublicMask                         NA         NA      NA       NA    
## sumOtherBusinessClose          -7.396887   1.658965  -4.459 1.26e-05 ***
## sumBusinessMask                 6.627027   1.143741   5.794 2.14e-08 ***
## sumSchoolMask                  -3.175919   1.108251  -2.866  0.00453 ** 
## sumQuarantine                  -3.513399   1.078790  -3.257  0.00129 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.084 on 241 degrees of freedom
##   (70 observations deleted due to missingness)
## Multiple R-squared:  0.578,	Adjusted R-squared:  0.5658 
## F-statistic: 47.17 on 7 and 241 DF,  p-value: < 2.2e-16
## 
## 
## Call:
## lm(formula = full_time_work_prop ~ confirmed_7dav_cumulative_prop + 
##     confirmed_7dav_incidence_prop + deaths_7dav_cumulative_prop + 
##     sumEmergDec + sumGathRestrict + sumSchoolClose + sumBarRestrict + 
##     sumNEBusinessClose + sumRestaurantRestrict + sumStayAtHome + 
##     sumPublicMask + sumOtherBusinessClose + sumBusinessMask + 
##     sumSchoolMask + sumQuarantine, data = .)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -22.6456  -6.9436   0.9695   7.0105  17.6938 
## 
## Coefficients: (9 not defined because of singularities)
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    11.646996  16.652892   0.699  0.48506    
## confirmed_7dav_cumulative_prop -0.014549   0.009519  -1.528  0.12789    
## confirmed_7dav_incidence_prop   1.817464   0.268032   6.781 1.13e-10 ***
## deaths_7dav_cumulative_prop    -0.260311   0.394772  -0.659  0.51034    
## sumEmergDec                           NA         NA      NA       NA    
## sumGathRestrict                       NA         NA      NA       NA    
## sumSchoolClose                        NA         NA      NA       NA    
## sumBarRestrict                        NA         NA      NA       NA    
## sumNEBusinessClose                    NA         NA      NA       NA    
## sumRestaurantRestrict                 NA         NA      NA       NA    
## sumStayAtHome                         NA         NA      NA       NA    
## sumPublicMask                         NA         NA      NA       NA    
## sumOtherBusinessClose                 NA         NA      NA       NA    
## sumBusinessMask                10.790570   4.107573   2.627  0.00923 ** 
## sumSchoolMask                   0.248516   1.092484   0.227  0.82027    
## sumQuarantine                  -4.762580   1.656243  -2.876  0.00444 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 9.027 on 216 degrees of freedom
##   (96 observations deleted due to missingness)
## Multiple R-squared:  0.5089,	Adjusted R-squared:  0.4953 
## F-statistic: 37.31 on 6 and 216 DF,  p-value: < 2.2e-16
## 
## 
## Call:
## lm(formula = full_time_work_prop ~ confirmed_7dav_cumulative_prop + 
##     confirmed_7dav_incidence_prop + deaths_7dav_cumulative_prop + 
##     sumEmergDec + sumGathRestrict + sumSchoolClose + sumBarRestrict + 
##     sumNEBusinessClose + sumRestaurantRestrict + sumStayAtHome + 
##     sumPublicMask + sumOtherBusinessClose + sumBusinessMask + 
##     sumSchoolMask + sumQuarantine, data = .)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -22.6559  -6.6711   0.5048   6.3833  19.6201 
## 
## Coefficients: (9 not defined because of singularities)
##                                  Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    23.9385384  6.3966217   3.742 0.000232 ***
## confirmed_7dav_cumulative_prop -0.0034939  0.0005924  -5.898 1.34e-08 ***
## confirmed_7dav_incidence_prop  -0.0069503  0.0145621  -0.477 0.633626    
## deaths_7dav_cumulative_prop    29.2838305  7.6350156   3.835 0.000163 ***
## sumEmergDec                            NA         NA      NA       NA    
## sumGathRestrict                        NA         NA      NA       NA    
## sumSchoolClose                         NA         NA      NA       NA    
## sumBarRestrict                         NA         NA      NA       NA    
## sumNEBusinessClose                     NA         NA      NA       NA    
## sumRestaurantRestrict                  NA         NA      NA       NA    
## sumStayAtHome                          NA         NA      NA       NA    
## sumPublicMask                          NA         NA      NA       NA    
## sumOtherBusinessClose                  NA         NA      NA       NA    
## sumBusinessMask                 7.1476077  1.6063863   4.449 1.36e-05 ***
## sumSchoolMask                   1.4686675  0.8381157   1.752 0.081083 .  
## sumQuarantine                  -4.9052183  1.0698020  -4.585 7.54e-06 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.959 on 224 degrees of freedom
##   (88 observations deleted due to missingness)
## Multiple R-squared:  0.5059,	Adjusted R-squared:  0.4926 
## F-statistic: 38.22 on 6 and 224 DF,  p-value: < 2.2e-16
## 
## 
## Call:
## lm(formula = full_time_work_prop ~ confirmed_7dav_cumulative_prop + 
##     confirmed_7dav_incidence_prop + deaths_7dav_cumulative_prop + 
##     sumEmergDec + sumGathRestrict + sumSchoolClose + sumBarRestrict + 
##     sumNEBusinessClose + sumRestaurantRestrict + sumStayAtHome + 
##     sumPublicMask + sumOtherBusinessClose + sumBusinessMask + 
##     sumSchoolMask + sumQuarantine, data = .)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -18.060  -5.622   0.019   6.212  20.230 
## 
## Coefficients: (8 not defined because of singularities)
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    80.374079   9.887115   8.129 2.44e-14 ***
## confirmed_7dav_cumulative_prop -0.030119   0.003778  -7.972 6.66e-14 ***
## confirmed_7dav_incidence_prop   0.009765   0.140624   0.069 0.944696    
## deaths_7dav_cumulative_prop     1.543073   0.209338   7.371 2.84e-12 ***
## sumEmergDec                           NA         NA      NA       NA    
## sumGathRestrict                       NA         NA      NA       NA    
## sumSchoolClose                        NA         NA      NA       NA    
## sumBarRestrict                        NA         NA      NA       NA    
## sumNEBusinessClose                    NA         NA      NA       NA    
## sumRestaurantRestrict                 NA         NA      NA       NA    
## sumStayAtHome                         NA         NA      NA       NA    
## sumPublicMask                         NA         NA      NA       NA    
## sumOtherBusinessClose          -8.047222   2.371618  -3.393 0.000810 ***
## sumBusinessMask                -1.637901   1.525280  -1.074 0.283993    
## sumSchoolMask                  -0.921499   1.206827  -0.764 0.445885    
## sumQuarantine                  -5.738707   1.593141  -3.602 0.000385 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.604 on 236 degrees of freedom
##   (75 observations deleted due to missingness)
## Multiple R-squared:  0.5234,	Adjusted R-squared:  0.5093 
## F-statistic: 37.03 on 7 and 236 DF,  p-value: < 2.2e-16
## 
## 
## Call:
## lm(formula = full_time_work_prop ~ confirmed_7dav_cumulative_prop + 
##     confirmed_7dav_incidence_prop + deaths_7dav_cumulative_prop + 
##     sumEmergDec + sumGathRestrict + sumSchoolClose + sumBarRestrict + 
##     sumNEBusinessClose + sumRestaurantRestrict + sumStayAtHome + 
##     sumPublicMask + sumOtherBusinessClose + sumBusinessMask + 
##     sumSchoolMask + sumQuarantine, data = .)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -23.310  -6.537   1.059   6.412  17.722 
## 
## Coefficients: (8 not defined because of singularities)
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    63.029154   7.062325   8.925  < 2e-16 ***
## confirmed_7dav_cumulative_prop -0.034199   0.005394  -6.340 1.11e-09 ***
## confirmed_7dav_incidence_prop   0.138288   0.088431   1.564 0.119175    
## deaths_7dav_cumulative_prop     2.153710   0.347881   6.191 2.54e-09 ***
## sumEmergDec                           NA         NA      NA       NA    
## sumGathRestrict                       NA         NA      NA       NA    
## sumSchoolClose                        NA         NA      NA       NA    
## sumBarRestrict                        NA         NA      NA       NA    
## sumNEBusinessClose                    NA         NA      NA       NA    
## sumRestaurantRestrict                 NA         NA      NA       NA    
## sumStayAtHome                         NA         NA      NA       NA    
## sumPublicMask                         NA         NA      NA       NA    
## sumOtherBusinessClose          -7.651820   1.761949  -4.343 2.07e-05 ***
## sumBusinessMask                 5.448286   1.234993   4.412 1.55e-05 ***
## sumSchoolMask                   1.730933   1.038557   1.667 0.096873 .  
## sumQuarantine                  -6.566787   1.698663  -3.866 0.000142 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.877 on 242 degrees of freedom
##   (69 observations deleted due to missingness)
## Multiple R-squared:  0.492,	Adjusted R-squared:  0.4773 
## F-statistic: 33.48 on 7 and 242 DF,  p-value: < 2.2e-16
## 
## 
## Call:
## lm(formula = full_time_work_prop ~ confirmed_7dav_cumulative_prop + 
##     confirmed_7dav_incidence_prop + deaths_7dav_cumulative_prop + 
##     sumEmergDec + sumGathRestrict + sumSchoolClose + sumBarRestrict + 
##     sumNEBusinessClose + sumRestaurantRestrict + sumStayAtHome + 
##     sumPublicMask + sumOtherBusinessClose + sumBusinessMask + 
##     sumSchoolMask + sumQuarantine, data = .)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -23.8945  -8.0590   0.9209   8.0816  16.5381 
## 
## Coefficients: (9 not defined because of singularities)
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    32.292821   7.608558   4.244 3.21e-05 ***
## confirmed_7dav_cumulative_prop  0.007557   0.004165   1.814   0.0709 .  
## confirmed_7dav_incidence_prop  -0.018778   0.037283  -0.504   0.6150    
## deaths_7dav_cumulative_prop    -0.533907   0.282108  -1.893   0.0597 .  
## sumEmergDec                           NA         NA      NA       NA    
## sumGathRestrict                       NA         NA      NA       NA    
## sumSchoolClose                        NA         NA      NA       NA    
## sumBarRestrict                        NA         NA      NA       NA    
## sumNEBusinessClose                    NA         NA      NA       NA    
## sumRestaurantRestrict                 NA         NA      NA       NA    
## sumStayAtHome                         NA         NA      NA       NA    
## sumPublicMask                         NA         NA      NA       NA    
## sumOtherBusinessClose                 NA         NA      NA       NA    
## sumBusinessMask                 7.753520   1.782508   4.350 2.07e-05 ***
## sumSchoolMask                  -0.440425   0.966477  -0.456   0.6490    
## sumQuarantine                  -7.795805   1.587163  -4.912 1.74e-06 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 9.777 on 224 degrees of freedom
##   (88 observations deleted due to missingness)
## Multiple R-squared:  0.4115,	Adjusted R-squared:  0.3958 
## F-statistic: 26.11 on 6 and 224 DF,  p-value: < 2.2e-16
## 
## 
## Call:
## lm(formula = full_time_work_prop ~ confirmed_7dav_cumulative_prop + 
##     confirmed_7dav_incidence_prop + deaths_7dav_cumulative_prop + 
##     sumEmergDec + sumGathRestrict + sumSchoolClose + sumBarRestrict + 
##     sumNEBusinessClose + sumRestaurantRestrict + sumStayAtHome + 
##     sumPublicMask + sumOtherBusinessClose + sumBusinessMask + 
##     sumSchoolMask + sumQuarantine, data = .)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -16.1187  -6.0315  -0.2629   5.7631  23.4999 
## 
## Coefficients: (8 not defined because of singularities)
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                     82.47015    4.30726  19.147  < 2e-16 ***
## confirmed_7dav_cumulative_prop  -0.03994    0.01102  -3.623 0.000349 ***
## confirmed_7dav_incidence_prop   -1.00959    0.09773 -10.330  < 2e-16 ***
## deaths_7dav_cumulative_prop      1.34689    0.43497   3.097 0.002171 ** 
## sumEmergDec                           NA         NA      NA       NA    
## sumGathRestrict                       NA         NA      NA       NA    
## sumSchoolClose                        NA         NA      NA       NA    
## sumBarRestrict                        NA         NA      NA       NA    
## sumNEBusinessClose                    NA         NA      NA       NA    
## sumRestaurantRestrict                 NA         NA      NA       NA    
## sumStayAtHome                         NA         NA      NA       NA    
## sumPublicMask                         NA         NA      NA       NA    
## sumOtherBusinessClose          -11.56722    1.19565  -9.674  < 2e-16 ***
## sumBusinessMask                  4.47255    1.12475   3.976 9.05e-05 ***
## sumSchoolMask                   -0.18752    1.04717  -0.179 0.858016    
## sumQuarantine                   -3.73511    1.01016  -3.698 0.000265 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 7.9 on 262 degrees of freedom
##   (49 observations deleted due to missingness)
## Multiple R-squared:  0.6703,	Adjusted R-squared:  0.6615 
## F-statistic: 76.09 on 7 and 262 DF,  p-value: < 2.2e-16
## 
## 
## Call:
## lm(formula = full_time_work_prop ~ confirmed_7dav_cumulative_prop + 
##     confirmed_7dav_incidence_prop + deaths_7dav_cumulative_prop + 
##     sumEmergDec + sumGathRestrict + sumSchoolClose + sumBarRestrict + 
##     sumNEBusinessClose + sumRestaurantRestrict + sumStayAtHome + 
##     sumPublicMask + sumOtherBusinessClose + sumBusinessMask + 
##     sumSchoolMask + sumQuarantine, data = .)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -23.7864  -5.6203   0.0384   5.7345  18.8648 
## 
## Coefficients: (4 not defined because of singularities)
##                                  Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    166.982283  10.032027  16.645  < 2e-16 ***
## confirmed_7dav_cumulative_prop  -0.024290   0.004846  -5.012 9.40e-07 ***
## confirmed_7dav_incidence_prop   -0.307833   0.114694  -2.684   0.0077 ** 
## deaths_7dav_cumulative_prop      1.245695   0.244509   5.095 6.32e-07 ***
## sumEmergDec                            NA         NA      NA       NA    
## sumGathRestrict                 -1.384507   4.427200  -0.313   0.7547    
## sumSchoolClose                  -1.956738   4.067301  -0.481   0.6308    
## sumBarRestrict                  -4.711714   2.199715  -2.142   0.0330 *  
## sumNEBusinessClose                     NA         NA      NA       NA    
## sumRestaurantRestrict                  NA         NA      NA       NA    
## sumStayAtHome                          NA         NA      NA       NA    
## sumPublicMask                  -12.571718   1.392914  -9.025  < 2e-16 ***
## sumOtherBusinessClose          -13.859169   1.234970 -11.222  < 2e-16 ***
## sumBusinessMask                  5.638338   1.175850   4.795 2.61e-06 ***
## sumSchoolMask                   -0.210035   1.121574  -0.187   0.8516    
## sumQuarantine                   -1.979561   1.355232  -1.461   0.1452    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.454 on 289 degrees of freedom
##   (18 observations deleted due to missingness)
## Multiple R-squared:  0.824,	Adjusted R-squared:  0.8173 
## F-statistic:   123 on 11 and 289 DF,  p-value: < 2.2e-16
## 
## 
## Call:
## lm(formula = full_time_work_prop ~ confirmed_7dav_cumulative_prop + 
##     confirmed_7dav_incidence_prop + deaths_7dav_cumulative_prop + 
##     sumEmergDec + sumGathRestrict + sumSchoolClose + sumBarRestrict + 
##     sumNEBusinessClose + sumRestaurantRestrict + sumStayAtHome + 
##     sumPublicMask + sumOtherBusinessClose + sumBusinessMask + 
##     sumSchoolMask + sumQuarantine, data = .)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -22.514  -7.300   1.213   7.519  17.080 
## 
## Coefficients: (8 not defined because of singularities)
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    67.979755   7.448447   9.127  < 2e-16 ***
## confirmed_7dav_cumulative_prop -0.014643   0.004402  -3.326 0.001017 ** 
## confirmed_7dav_incidence_prop   0.026389   0.061263   0.431 0.667039    
## deaths_7dav_cumulative_prop     0.808579   0.264069   3.062 0.002448 ** 
## sumEmergDec                           NA         NA      NA       NA    
## sumGathRestrict                       NA         NA      NA       NA    
## sumSchoolClose                        NA         NA      NA       NA    
## sumBarRestrict                        NA         NA      NA       NA    
## sumNEBusinessClose                    NA         NA      NA       NA    
## sumRestaurantRestrict                 NA         NA      NA       NA    
## sumStayAtHome                         NA         NA      NA       NA    
## sumPublicMask                         NA         NA      NA       NA    
## sumOtherBusinessClose          -7.418883   1.926907  -3.850 0.000151 ***
## sumBusinessMask                 5.041752   1.303557   3.868 0.000141 ***
## sumSchoolMask                   2.725896   1.324293   2.058 0.040629 *  
## sumQuarantine                  -9.308487   1.700660  -5.473 1.11e-07 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 9.385 on 241 degrees of freedom
##   (70 observations deleted due to missingness)
## Multiple R-squared:  0.4314,	Adjusted R-squared:  0.4149 
## F-statistic: 26.12 on 7 and 241 DF,  p-value: < 2.2e-16
## 
## 
## Call:
## lm(formula = full_time_work_prop ~ confirmed_7dav_cumulative_prop + 
##     confirmed_7dav_incidence_prop + deaths_7dav_cumulative_prop + 
##     sumEmergDec + sumGathRestrict + sumSchoolClose + sumBarRestrict + 
##     sumNEBusinessClose + sumRestaurantRestrict + sumStayAtHome + 
##     sumPublicMask + sumOtherBusinessClose + sumBusinessMask + 
##     sumSchoolMask + sumQuarantine, data = .)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -22.7448  -5.3255  -0.3817   5.7659  18.0603 
## 
## Coefficients: (10 not defined because of singularities)
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    45.235490   3.853395  11.739  < 2e-16 ***
## confirmed_7dav_cumulative_prop -0.017625   0.002433  -7.244 7.68e-12 ***
## confirmed_7dav_incidence_prop   0.368201   0.263867   1.395    0.164    
## deaths_7dav_cumulative_prop    -0.818657   0.841679  -0.973    0.332    
## sumEmergDec                           NA         NA      NA       NA    
## sumGathRestrict                       NA         NA      NA       NA    
## sumSchoolClose                        NA         NA      NA       NA    
## sumBarRestrict                        NA         NA      NA       NA    
## sumNEBusinessClose                    NA         NA      NA       NA    
## sumRestaurantRestrict                 NA         NA      NA       NA    
## sumStayAtHome                         NA         NA      NA       NA    
## sumPublicMask                         NA         NA      NA       NA    
## sumOtherBusinessClose                 NA         NA      NA       NA    
## sumBusinessMask                       NA         NA      NA       NA    
## sumSchoolMask                   0.288776   0.724419   0.399    0.691    
## sumQuarantine                  -0.034136   1.452794  -0.023    0.981    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.514 on 215 degrees of freedom
##   (98 observations deleted due to missingness)
## Multiple R-squared:  0.561,	Adjusted R-squared:  0.5508 
## F-statistic: 54.95 on 5 and 215 DF,  p-value: < 2.2e-16
## 
## 
## Call:
## lm(formula = full_time_work_prop ~ confirmed_7dav_cumulative_prop + 
##     confirmed_7dav_incidence_prop + deaths_7dav_cumulative_prop + 
##     sumEmergDec + sumGathRestrict + sumSchoolClose + sumBarRestrict + 
##     sumNEBusinessClose + sumRestaurantRestrict + sumStayAtHome + 
##     sumPublicMask + sumOtherBusinessClose + sumBusinessMask + 
##     sumSchoolMask + sumQuarantine, data = .)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -22.8564  -6.2723   0.2456   6.4176  18.3173 
## 
## Coefficients: (6 not defined because of singularities)
##                                  Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    147.256018  12.246899  12.024  < 2e-16 ***
## confirmed_7dav_cumulative_prop  -0.006829   0.001469  -4.648 5.16e-06 ***
## confirmed_7dav_incidence_prop    0.051720   0.014268   3.625 0.000343 ***
## deaths_7dav_cumulative_prop     -0.308448   0.252471  -1.222 0.222835    
## sumEmergDec                            NA         NA      NA       NA    
## sumGathRestrict                        NA         NA      NA       NA    
## sumSchoolClose                         NA         NA      NA       NA    
## sumBarRestrict                  -4.103189   3.213978  -1.277 0.202769    
## sumNEBusinessClose                     NA         NA      NA       NA    
## sumRestaurantRestrict                  NA         NA      NA       NA    
## sumStayAtHome                          NA         NA      NA       NA    
## sumPublicMask                  -12.273935   1.453617  -8.444 1.64e-15 ***
## sumOtherBusinessClose          -12.792175   1.412685  -9.055  < 2e-16 ***
## sumBusinessMask                  5.289816   1.212087   4.364 1.79e-05 ***
## sumSchoolMask                    2.175645   0.843605   2.579 0.010416 *  
## sumQuarantine                   -3.413852   1.206212  -2.830 0.004986 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.72 on 282 degrees of freedom
##   (27 observations deleted due to missingness)
## Multiple R-squared:  0.7612,	Adjusted R-squared:  0.7536 
## F-statistic: 99.89 on 9 and 282 DF,  p-value: < 2.2e-16
## 
## 
## Call:
## lm(formula = full_time_work_prop ~ confirmed_7dav_cumulative_prop + 
##     confirmed_7dav_incidence_prop + deaths_7dav_cumulative_prop + 
##     sumEmergDec + sumGathRestrict + sumSchoolClose + sumBarRestrict + 
##     sumNEBusinessClose + sumRestaurantRestrict + sumStayAtHome + 
##     sumPublicMask + sumOtherBusinessClose + sumBusinessMask + 
##     sumSchoolMask + sumQuarantine, data = .)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -23.355  -6.721   1.261   7.210  18.032 
## 
## Coefficients: (8 not defined because of singularities)
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    75.016066   5.491922  13.659  < 2e-16 ***
## confirmed_7dav_cumulative_prop  0.035016   0.009462   3.701 0.000263 ***
## confirmed_7dav_incidence_prop  -0.038139   0.122519  -0.311 0.755834    
## deaths_7dav_cumulative_prop    -4.895051   1.199660  -4.080 6.02e-05 ***
## sumEmergDec                           NA         NA      NA       NA    
## sumGathRestrict                       NA         NA      NA       NA    
## sumSchoolClose                        NA         NA      NA       NA    
## sumBarRestrict                        NA         NA      NA       NA    
## sumNEBusinessClose                    NA         NA      NA       NA    
## sumRestaurantRestrict                 NA         NA      NA       NA    
## sumStayAtHome                         NA         NA      NA       NA    
## sumPublicMask                         NA         NA      NA       NA    
## sumOtherBusinessClose          -9.707085   1.478558  -6.565 2.89e-10 ***
## sumBusinessMask                 6.059319   1.296565   4.673 4.80e-06 ***
## sumSchoolMask                  -2.637721   1.284587  -2.053 0.041058 *  
## sumQuarantine                  -2.252248   1.810330  -1.244 0.214602    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 9.157 on 255 degrees of freedom
##   (56 observations deleted due to missingness)
## Multiple R-squared:  0.5064,	Adjusted R-squared:  0.4929 
## F-statistic: 37.37 on 7 and 255 DF,  p-value: < 2.2e-16
## 
## 
## Call:
## lm(formula = full_time_work_prop ~ confirmed_7dav_cumulative_prop + 
##     confirmed_7dav_incidence_prop + deaths_7dav_cumulative_prop + 
##     sumEmergDec + sumGathRestrict + sumSchoolClose + sumBarRestrict + 
##     sumNEBusinessClose + sumRestaurantRestrict + sumStayAtHome + 
##     sumPublicMask + sumOtherBusinessClose + sumBusinessMask + 
##     sumSchoolMask + sumQuarantine, data = .)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -22.1099  -6.6402   0.5376   7.3952  18.1203 
## 
## Coefficients: (10 not defined because of singularities)
##                                Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    67.05338    6.49376  10.326  < 2e-16 ***
## confirmed_7dav_cumulative_prop -0.08021    0.01271  -6.310 1.59e-09 ***
## confirmed_7dav_incidence_prop   0.58007    0.17512   3.312  0.00109 ** 
## deaths_7dav_cumulative_prop     8.85985    1.72324   5.141 6.16e-07 ***
## sumEmergDec                          NA         NA      NA       NA    
## sumGathRestrict                      NA         NA      NA       NA    
## sumSchoolClose                       NA         NA      NA       NA    
## sumBarRestrict                       NA         NA      NA       NA    
## sumNEBusinessClose                   NA         NA      NA       NA    
## sumRestaurantRestrict                NA         NA      NA       NA    
## sumStayAtHome                        NA         NA      NA       NA    
## sumPublicMask                        NA         NA      NA       NA    
## sumOtherBusinessClose                NA         NA      NA       NA    
## sumBusinessMask                      NA         NA      NA       NA    
## sumSchoolMask                  -6.72519    1.45392  -4.626 6.48e-06 ***
## sumQuarantine                  -4.24655    2.40335  -1.767  0.07867 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 9.004 on 213 degrees of freedom
##   (100 observations deleted due to missingness)
## Multiple R-squared:  0.5072,	Adjusted R-squared:  0.4956 
## F-statistic: 43.84 on 5 and 213 DF,  p-value: < 2.2e-16
## 
## 
## Call:
## lm(formula = full_time_work_prop ~ confirmed_7dav_cumulative_prop + 
##     confirmed_7dav_incidence_prop + deaths_7dav_cumulative_prop + 
##     sumEmergDec + sumGathRestrict + sumSchoolClose + sumBarRestrict + 
##     sumNEBusinessClose + sumRestaurantRestrict + sumStayAtHome + 
##     sumPublicMask + sumOtherBusinessClose + sumBusinessMask + 
##     sumSchoolMask + sumQuarantine, data = .)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -23.0280  -5.0467   0.3738   5.6403  15.6454 
## 
## Coefficients: (10 not defined because of singularities)
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    43.012048   3.803562  11.308  < 2e-16 ***
## confirmed_7dav_cumulative_prop -0.016917   0.005199  -3.254 0.001324 ** 
## confirmed_7dav_incidence_prop   1.069976   0.360185   2.971 0.003313 ** 
## deaths_7dav_cumulative_prop    -3.603016   0.926813  -3.888 0.000135 ***
## sumEmergDec                           NA         NA      NA       NA    
## sumGathRestrict                       NA         NA      NA       NA    
## sumSchoolClose                        NA         NA      NA       NA    
## sumBarRestrict                        NA         NA      NA       NA    
## sumNEBusinessClose                    NA         NA      NA       NA    
## sumRestaurantRestrict                 NA         NA      NA       NA    
## sumStayAtHome                         NA         NA      NA       NA    
## sumPublicMask                         NA         NA      NA       NA    
## sumOtherBusinessClose                 NA         NA      NA       NA    
## sumBusinessMask                       NA         NA      NA       NA    
## sumSchoolMask                   2.021805   0.873132   2.316 0.021533 *  
## sumQuarantine                  -0.168607   1.209947  -0.139 0.889304    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.281 on 213 degrees of freedom
##   (100 observations deleted due to missingness)
## Multiple R-squared:  0.5831,	Adjusted R-squared:  0.5733 
## F-statistic: 59.58 on 5 and 213 DF,  p-value: < 2.2e-16
## 
## 
## Call:
## lm(formula = full_time_work_prop ~ confirmed_7dav_cumulative_prop + 
##     confirmed_7dav_incidence_prop + deaths_7dav_cumulative_prop + 
##     sumEmergDec + sumGathRestrict + sumSchoolClose + sumBarRestrict + 
##     sumNEBusinessClose + sumRestaurantRestrict + sumStayAtHome + 
##     sumPublicMask + sumOtherBusinessClose + sumBusinessMask + 
##     sumSchoolMask + sumQuarantine, data = .)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -22.1534  -6.8151   0.7327   6.7062  18.7504 
## 
## Coefficients: (8 not defined because of singularities)
##                                  Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                     72.256684   7.114434  10.156  < 2e-16 ***
## confirmed_7dav_cumulative_prop  -0.032059   0.005377  -5.962 8.81e-09 ***
## confirmed_7dav_incidence_prop    0.099622   0.125689   0.793 0.428787    
## deaths_7dav_cumulative_prop      1.318940   0.224270   5.881 1.35e-08 ***
## sumEmergDec                            NA         NA      NA       NA    
## sumGathRestrict                        NA         NA      NA       NA    
## sumSchoolClose                         NA         NA      NA       NA    
## sumBarRestrict                         NA         NA      NA       NA    
## sumNEBusinessClose                     NA         NA      NA       NA    
## sumRestaurantRestrict                  NA         NA      NA       NA    
## sumStayAtHome                          NA         NA      NA       NA    
## sumPublicMask                          NA         NA      NA       NA    
## sumOtherBusinessClose           -7.143898   1.831277  -3.901 0.000124 ***
## sumBusinessMask                  4.776799   1.245393   3.836 0.000160 ***
## sumSchoolMask                    1.888299   1.171929   1.611 0.108429    
## sumQuarantine                  -10.470250   1.410323  -7.424 1.95e-12 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.924 on 241 degrees of freedom
##   (70 observations deleted due to missingness)
## Multiple R-squared:  0.4859,	Adjusted R-squared:  0.4709 
## F-statistic: 32.53 on 7 and 241 DF,  p-value: < 2.2e-16
## 
## 
## Call:
## lm(formula = full_time_work_prop ~ confirmed_7dav_cumulative_prop + 
##     confirmed_7dav_incidence_prop + deaths_7dav_cumulative_prop + 
##     sumEmergDec + sumGathRestrict + sumSchoolClose + sumBarRestrict + 
##     sumNEBusinessClose + sumRestaurantRestrict + sumStayAtHome + 
##     sumPublicMask + sumOtherBusinessClose + sumBusinessMask + 
##     sumSchoolMask + sumQuarantine, data = .)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -22.6312  -5.9889   0.8798   6.4845  17.5474 
## 
## Coefficients: (8 not defined because of singularities)
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    58.416607   8.079243   7.230 6.53e-12 ***
## confirmed_7dav_cumulative_prop -0.051315   0.007432  -6.905 4.54e-11 ***
## confirmed_7dav_incidence_prop   0.594465   0.279081   2.130 0.034191 *  
## deaths_7dav_cumulative_prop     3.264074   0.648306   5.035 9.45e-07 ***
## sumEmergDec                           NA         NA      NA       NA    
## sumGathRestrict                       NA         NA      NA       NA    
## sumSchoolClose                        NA         NA      NA       NA    
## sumBarRestrict                        NA         NA      NA       NA    
## sumNEBusinessClose                    NA         NA      NA       NA    
## sumRestaurantRestrict                 NA         NA      NA       NA    
## sumStayAtHome                         NA         NA      NA       NA    
## sumPublicMask                         NA         NA      NA       NA    
## sumOtherBusinessClose          -8.025681   2.085352  -3.849 0.000153 ***
## sumBusinessMask                 4.543096   1.219918   3.724 0.000245 ***
## sumSchoolMask                   4.002556   1.075553   3.721 0.000247 ***
## sumQuarantine                  -5.764814   1.694564  -3.402 0.000785 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.761 on 238 degrees of freedom
##   (73 observations deleted due to missingness)
## Multiple R-squared:  0.5044,	Adjusted R-squared:  0.4899 
## F-statistic: 34.61 on 7 and 238 DF,  p-value: < 2.2e-16
## 
## 
## Call:
## lm(formula = full_time_work_prop ~ confirmed_7dav_cumulative_prop + 
##     confirmed_7dav_incidence_prop + deaths_7dav_cumulative_prop + 
##     sumEmergDec + sumGathRestrict + sumSchoolClose + sumBarRestrict + 
##     sumNEBusinessClose + sumRestaurantRestrict + sumStayAtHome + 
##     sumPublicMask + sumOtherBusinessClose + sumBusinessMask + 
##     sumSchoolMask + sumQuarantine, data = .)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -22.4437  -6.7513   0.5835   6.8692  16.3720 
## 
## Coefficients: (8 not defined because of singularities)
##                                  Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    79.9296153  6.1430895  13.011  < 2e-16 ***
## confirmed_7dav_cumulative_prop  0.0002421  0.0102545   0.024 0.981187    
## confirmed_7dav_incidence_prop  -0.4946947  0.2326796  -2.126 0.034473 *  
## deaths_7dav_cumulative_prop    -2.2677215  0.5849753  -3.877 0.000135 ***
## sumEmergDec                            NA         NA      NA       NA    
## sumGathRestrict                        NA         NA      NA       NA    
## sumSchoolClose                         NA         NA      NA       NA    
## sumBarRestrict                         NA         NA      NA       NA    
## sumNEBusinessClose                     NA         NA      NA       NA    
## sumRestaurantRestrict                  NA         NA      NA       NA    
## sumStayAtHome                          NA         NA      NA       NA    
## sumPublicMask                          NA         NA      NA       NA    
## sumOtherBusinessClose          -9.7025544  1.4624906  -6.634 1.99e-10 ***
## sumBusinessMask                 5.1155366  1.2220363   4.186 3.93e-05 ***
## sumSchoolMask                  -0.1402645  1.1467607  -0.122 0.902748    
## sumQuarantine                  -7.7433517  1.6139234  -4.798 2.75e-06 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.794 on 251 degrees of freedom
##   (60 observations deleted due to missingness)
## Multiple R-squared:  0.5246,	Adjusted R-squared:  0.5113 
## F-statistic: 39.57 on 7 and 251 DF,  p-value: < 2.2e-16
## 
## 
## Call:
## lm(formula = full_time_work_prop ~ confirmed_7dav_cumulative_prop + 
##     confirmed_7dav_incidence_prop + deaths_7dav_cumulative_prop + 
##     sumEmergDec + sumGathRestrict + sumSchoolClose + sumBarRestrict + 
##     sumNEBusinessClose + sumRestaurantRestrict + sumStayAtHome + 
##     sumPublicMask + sumOtherBusinessClose + sumBusinessMask + 
##     sumSchoolMask + sumQuarantine, data = .)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -23.2932  -8.1742   0.8741   7.8747  18.7012 
## 
## Coefficients: (10 not defined because of singularities)
##                                Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    55.37716    5.89858   9.388  < 2e-16 ***
## confirmed_7dav_cumulative_prop  0.02071    0.01042   1.988 0.048132 *  
## confirmed_7dav_incidence_prop   0.12955    0.18660   0.694 0.488285    
## deaths_7dav_cumulative_prop    -1.36256    0.61081  -2.231 0.026741 *  
## sumEmergDec                          NA         NA      NA       NA    
## sumGathRestrict                      NA         NA      NA       NA    
## sumSchoolClose                       NA         NA      NA       NA    
## sumBarRestrict                       NA         NA      NA       NA    
## sumNEBusinessClose                   NA         NA      NA       NA    
## sumRestaurantRestrict                NA         NA      NA       NA    
## sumStayAtHome                        NA         NA      NA       NA    
## sumPublicMask                        NA         NA      NA       NA    
## sumOtherBusinessClose                NA         NA      NA       NA    
## sumBusinessMask                      NA         NA      NA       NA    
## sumSchoolMask                   2.21070    1.25778   1.758 0.080248 .  
## sumQuarantine                  -6.84409    1.88457  -3.632 0.000353 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 9.611 on 213 degrees of freedom
##   (100 observations deleted due to missingness)
## Multiple R-squared:  0.4384,	Adjusted R-squared:  0.4252 
## F-statistic: 33.26 on 5 and 213 DF,  p-value: < 2.2e-16
## 
## 
## Call:
## lm(formula = full_time_work_prop ~ confirmed_7dav_cumulative_prop + 
##     confirmed_7dav_incidence_prop + deaths_7dav_cumulative_prop + 
##     sumEmergDec + sumGathRestrict + sumSchoolClose + sumBarRestrict + 
##     sumNEBusinessClose + sumRestaurantRestrict + sumStayAtHome + 
##     sumPublicMask + sumOtherBusinessClose + sumBusinessMask + 
##     sumSchoolMask + sumQuarantine, data = .)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -24.084  -5.144   1.013   5.581  18.516 
## 
## Coefficients: (10 not defined because of singularities)
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    41.153710   4.628106   8.892 2.59e-16 ***
## confirmed_7dav_cumulative_prop  0.040383   0.008477   4.764 3.51e-06 ***
## confirmed_7dav_incidence_prop  -0.063337   0.116098  -0.546 0.585947    
## deaths_7dav_cumulative_prop    -3.978739   0.514848  -7.728 4.25e-13 ***
## sumEmergDec                           NA         NA      NA       NA    
## sumGathRestrict                       NA         NA      NA       NA    
## sumSchoolClose                        NA         NA      NA       NA    
## sumBarRestrict                        NA         NA      NA       NA    
## sumNEBusinessClose                    NA         NA      NA       NA    
## sumRestaurantRestrict                 NA         NA      NA       NA    
## sumStayAtHome                         NA         NA      NA       NA    
## sumPublicMask                         NA         NA      NA       NA    
## sumOtherBusinessClose                 NA         NA      NA       NA    
## sumBusinessMask                       NA         NA      NA       NA    
## sumSchoolMask                   3.405286   0.953952   3.570 0.000442 ***
## sumQuarantine                  -0.408098   1.791294  -0.228 0.820003    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.313 on 213 degrees of freedom
##   (100 observations deleted due to missingness)
## Multiple R-squared:  0.5799,	Adjusted R-squared:   0.57 
## F-statistic:  58.8 on 5 and 213 DF,  p-value: < 2.2e-16
## 
## 
## Call:
## lm(formula = full_time_work_prop ~ confirmed_7dav_cumulative_prop + 
##     confirmed_7dav_incidence_prop + deaths_7dav_cumulative_prop + 
##     sumEmergDec + sumGathRestrict + sumSchoolClose + sumBarRestrict + 
##     sumNEBusinessClose + sumRestaurantRestrict + sumStayAtHome + 
##     sumPublicMask + sumOtherBusinessClose + sumBusinessMask + 
##     sumSchoolMask + sumQuarantine, data = .)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -23.574  -7.588   1.352   6.975  16.969 
## 
## Coefficients: (8 not defined because of singularities)
##                                  Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                     75.387750   5.750804  13.109  < 2e-16 ***
## confirmed_7dav_cumulative_prop  -0.019843   0.004739  -4.187 3.90e-05 ***
## confirmed_7dav_incidence_prop    0.234335   0.138863   1.688 0.092734 .  
## deaths_7dav_cumulative_prop      1.830517   0.463945   3.946 0.000103 ***
## sumEmergDec                            NA         NA      NA       NA    
## sumGathRestrict                        NA         NA      NA       NA    
## sumSchoolClose                         NA         NA      NA       NA    
## sumBarRestrict                         NA         NA      NA       NA    
## sumNEBusinessClose                     NA         NA      NA       NA    
## sumRestaurantRestrict                  NA         NA      NA       NA    
## sumStayAtHome                          NA         NA      NA       NA    
## sumPublicMask                          NA         NA      NA       NA    
## sumOtherBusinessClose          -10.939313   1.507515  -7.257 4.86e-12 ***
## sumBusinessMask                  5.511644   1.283397   4.295 2.50e-05 ***
## sumSchoolMask                    3.487750   1.332677   2.617 0.009402 ** 
## sumQuarantine                   -9.399109   1.774697  -5.296 2.57e-07 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 9.202 on 253 degrees of freedom
##   (58 observations deleted due to missingness)
## Multiple R-squared:  0.4898,	Adjusted R-squared:  0.4757 
## F-statistic: 34.69 on 7 and 253 DF,  p-value: < 2.2e-16
## 
## 
## Call:
## lm(formula = full_time_work_prop ~ confirmed_7dav_cumulative_prop + 
##     confirmed_7dav_incidence_prop + deaths_7dav_cumulative_prop + 
##     sumEmergDec + sumGathRestrict + sumSchoolClose + sumBarRestrict + 
##     sumNEBusinessClose + sumRestaurantRestrict + sumStayAtHome + 
##     sumPublicMask + sumOtherBusinessClose + sumBusinessMask + 
##     sumSchoolMask + sumQuarantine, data = .)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -20.6259  -4.4298   0.3518   6.1352  19.0428 
## 
## Coefficients: (8 not defined because of singularities)
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    65.903796   7.362996   8.951  < 2e-16 ***
## confirmed_7dav_cumulative_prop -0.019497   0.002441  -7.987 5.74e-14 ***
## confirmed_7dav_incidence_prop   0.100854   0.117402   0.859 0.391170    
## deaths_7dav_cumulative_prop     1.135834   0.165672   6.856 5.95e-11 ***
## sumEmergDec                           NA         NA      NA       NA    
## sumGathRestrict                       NA         NA      NA       NA    
## sumSchoolClose                        NA         NA      NA       NA    
## sumBarRestrict                        NA         NA      NA       NA    
## sumNEBusinessClose                    NA         NA      NA       NA    
## sumRestaurantRestrict                 NA         NA      NA       NA    
## sumStayAtHome                         NA         NA      NA       NA    
## sumPublicMask                         NA         NA      NA       NA    
## sumOtherBusinessClose          -7.572882   1.823565  -4.153 4.57e-05 ***
## sumBusinessMask                 3.223985   1.218336   2.646 0.008678 ** 
## sumSchoolMask                   2.204389   1.015108   2.172 0.030867 *  
## sumQuarantine                  -6.092838   1.694766  -3.595 0.000394 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.537 on 240 degrees of freedom
##   (71 observations deleted due to missingness)
## Multiple R-squared:  0.5292,	Adjusted R-squared:  0.5154 
## F-statistic: 38.54 on 7 and 240 DF,  p-value: < 2.2e-16
## 
## 
## Call:
## lm(formula = full_time_work_prop ~ confirmed_7dav_cumulative_prop + 
##     confirmed_7dav_incidence_prop + deaths_7dav_cumulative_prop + 
##     sumEmergDec + sumGathRestrict + sumSchoolClose + sumBarRestrict + 
##     sumNEBusinessClose + sumRestaurantRestrict + sumStayAtHome + 
##     sumPublicMask + sumOtherBusinessClose + sumBusinessMask + 
##     sumSchoolMask + sumQuarantine, data = .)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -18.5687  -5.6572  -0.4365   6.5965  20.7942 
## 
## Coefficients: (8 not defined because of singularities)
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    81.401714   9.808155   8.299 8.10e-15 ***
## confirmed_7dav_cumulative_prop -0.051210   0.006452  -7.938 8.32e-14 ***
## confirmed_7dav_incidence_prop  -0.109125   0.238046  -0.458 0.647074    
## deaths_7dav_cumulative_prop     4.008988   0.586029   6.841 6.70e-11 ***
## sumEmergDec                           NA         NA      NA       NA    
## sumGathRestrict                       NA         NA      NA       NA    
## sumSchoolClose                        NA         NA      NA       NA    
## sumBarRestrict                        NA         NA      NA       NA    
## sumNEBusinessClose                    NA         NA      NA       NA    
## sumRestaurantRestrict                 NA         NA      NA       NA    
## sumStayAtHome                         NA         NA      NA       NA    
## sumPublicMask                         NA         NA      NA       NA    
## sumOtherBusinessClose          -8.121786   2.367612  -3.430 0.000711 ***
## sumBusinessMask                -1.406357   1.521220  -0.924 0.356174    
## sumSchoolMask                  -2.659796   1.209357  -2.199 0.028824 *  
## sumQuarantine                  -4.999211   1.300054  -3.845 0.000155 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.576 on 236 degrees of freedom
##   (75 observations deleted due to missingness)
## Multiple R-squared:  0.5265,	Adjusted R-squared:  0.5125 
## F-statistic:  37.5 on 7 and 236 DF,  p-value: < 2.2e-16
## 
## 
## Call:
## lm(formula = full_time_work_prop ~ confirmed_7dav_cumulative_prop + 
##     confirmed_7dav_incidence_prop + deaths_7dav_cumulative_prop + 
##     sumEmergDec + sumGathRestrict + sumSchoolClose + sumBarRestrict + 
##     sumNEBusinessClose + sumRestaurantRestrict + sumStayAtHome + 
##     sumPublicMask + sumOtherBusinessClose + sumBusinessMask + 
##     sumSchoolMask + sumQuarantine, data = .)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -22.0515  -7.4967   0.9946   6.6549  16.3748 
## 
## Coefficients: (8 not defined because of singularities)
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    43.833494  17.738918   2.471 0.014189 *  
## confirmed_7dav_cumulative_prop  0.001693   0.007991   0.212 0.832426    
## confirmed_7dav_incidence_prop   1.332827   0.296561   4.494  1.1e-05 ***
## deaths_7dav_cumulative_prop    -2.359781   0.962304  -2.452 0.014933 *  
## sumEmergDec                           NA         NA      NA       NA    
## sumGathRestrict                       NA         NA      NA       NA    
## sumSchoolClose                        NA         NA      NA       NA    
## sumBarRestrict                        NA         NA      NA       NA    
## sumNEBusinessClose                    NA         NA      NA       NA    
## sumRestaurantRestrict                 NA         NA      NA       NA    
## sumStayAtHome                         NA         NA      NA       NA    
## sumPublicMask                         NA         NA      NA       NA    
## sumOtherBusinessClose          -5.088932   4.311208  -1.180 0.239046    
## sumBusinessMask                 5.733683   1.529096   3.750 0.000223 ***
## sumSchoolMask                   1.052209   1.074686   0.979 0.328553    
## sumQuarantine                   0.697284   2.322924   0.300 0.764311    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 9.165 on 233 degrees of freedom
##   (78 observations deleted due to missingness)
## Multiple R-squared:  0.4635,	Adjusted R-squared:  0.4474 
## F-statistic: 28.76 on 7 and 233 DF,  p-value: < 2.2e-16
## 
## 
## Call:
## lm(formula = full_time_work_prop ~ confirmed_7dav_cumulative_prop + 
##     confirmed_7dav_incidence_prop + deaths_7dav_cumulative_prop + 
##     sumEmergDec + sumGathRestrict + sumSchoolClose + sumBarRestrict + 
##     sumNEBusinessClose + sumRestaurantRestrict + sumStayAtHome + 
##     sumPublicMask + sumOtherBusinessClose + sumBusinessMask + 
##     sumSchoolMask + sumQuarantine, data = .)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -22.626  -6.267   1.048   6.761  17.940 
## 
## Coefficients: (8 not defined because of singularities)
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    68.925712   5.908425  11.666  < 2e-16 ***
## confirmed_7dav_cumulative_prop -0.024424   0.004184  -5.837 1.65e-08 ***
## confirmed_7dav_incidence_prop   0.167541   0.082135   2.040 0.042428 *  
## deaths_7dav_cumulative_prop     0.943016   0.161699   5.832 1.70e-08 ***
## sumEmergDec                           NA         NA      NA       NA    
## sumGathRestrict                       NA         NA      NA       NA    
## sumSchoolClose                        NA         NA      NA       NA    
## sumBarRestrict                        NA         NA      NA       NA    
## sumNEBusinessClose                    NA         NA      NA       NA    
## sumRestaurantRestrict                 NA         NA      NA       NA    
## sumStayAtHome                         NA         NA      NA       NA    
## sumPublicMask                         NA         NA      NA       NA    
## sumOtherBusinessClose          -9.272493   1.549045  -5.986 7.50e-09 ***
## sumBusinessMask                 4.716605   1.240153   3.803 0.000180 ***
## sumSchoolMask                   4.522523   1.333239   3.392 0.000807 ***
## sumQuarantine                  -7.867718   1.359005  -5.789 2.13e-08 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.919 on 248 degrees of freedom
##   (63 observations deleted due to missingness)
## Multiple R-squared:  0.4994,	Adjusted R-squared:  0.4852 
## F-statistic: 35.34 on 7 and 248 DF,  p-value: < 2.2e-16
## 
## 
## Call:
## lm(formula = full_time_work_prop ~ confirmed_7dav_cumulative_prop + 
##     confirmed_7dav_incidence_prop + deaths_7dav_cumulative_prop + 
##     sumEmergDec + sumGathRestrict + sumSchoolClose + sumBarRestrict + 
##     sumNEBusinessClose + sumRestaurantRestrict + sumStayAtHome + 
##     sumPublicMask + sumOtherBusinessClose + sumBusinessMask + 
##     sumSchoolMask + sumQuarantine, data = .)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -23.5190  -8.2265   0.4463   8.1426  16.2884 
## 
## Coefficients: (10 not defined because of singularities)
##                                Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    51.21844    5.46922   9.365   <2e-16 ***
## confirmed_7dav_cumulative_prop  0.01902    0.01490   1.276   0.2033    
## confirmed_7dav_incidence_prop  -0.22926    0.18183  -1.261   0.2087    
## deaths_7dav_cumulative_prop    -4.46837    1.97071  -2.267   0.0244 *  
## sumEmergDec                          NA         NA      NA       NA    
## sumGathRestrict                      NA         NA      NA       NA    
## sumSchoolClose                       NA         NA      NA       NA    
## sumBarRestrict                       NA         NA      NA       NA    
## sumNEBusinessClose                   NA         NA      NA       NA    
## sumRestaurantRestrict                NA         NA      NA       NA    
## sumStayAtHome                        NA         NA      NA       NA    
## sumPublicMask                        NA         NA      NA       NA    
## sumOtherBusinessClose                NA         NA      NA       NA    
## sumBusinessMask                      NA         NA      NA       NA    
## sumSchoolMask                   0.33218    1.06849   0.311   0.7562    
## sumQuarantine                  -2.54308    2.11827  -1.201   0.2313    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 9.549 on 213 degrees of freedom
##   (100 observations deleted due to missingness)
## Multiple R-squared:  0.4457,	Adjusted R-squared:  0.4327 
## F-statistic: 34.25 on 5 and 213 DF,  p-value: < 2.2e-16
## 
## 
## Call:
## lm(formula = full_time_work_prop ~ confirmed_7dav_cumulative_prop + 
##     confirmed_7dav_incidence_prop + deaths_7dav_cumulative_prop + 
##     sumEmergDec + sumGathRestrict + sumSchoolClose + sumBarRestrict + 
##     sumNEBusinessClose + sumRestaurantRestrict + sumStayAtHome + 
##     sumPublicMask + sumOtherBusinessClose + sumBusinessMask + 
##     sumSchoolMask + sumQuarantine, data = .)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -19.2243  -7.1220   0.2939   7.1781  19.2396 
## 
## Coefficients: (8 not defined because of singularities)
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    75.678032  10.489722   7.214 7.19e-12 ***
## confirmed_7dav_cumulative_prop -0.026433   0.005444  -4.856 2.18e-06 ***
## confirmed_7dav_incidence_prop   1.701719   0.351549   4.841 2.33e-06 ***
## deaths_7dav_cumulative_prop     1.540520   0.604645   2.548  0.01147 *  
## sumEmergDec                           NA         NA      NA       NA    
## sumGathRestrict                       NA         NA      NA       NA    
## sumSchoolClose                        NA         NA      NA       NA    
## sumBarRestrict                        NA         NA      NA       NA    
## sumNEBusinessClose                    NA         NA      NA       NA    
## sumRestaurantRestrict                 NA         NA      NA       NA    
## sumStayAtHome                         NA         NA      NA       NA    
## sumPublicMask                         NA         NA      NA       NA    
## sumOtherBusinessClose          -7.228025   2.177100  -3.320  0.00104 ** 
## sumBusinessMask                -0.453974   2.020502  -0.225  0.82242    
## sumSchoolMask                  -2.349062   1.598463  -1.470  0.14300    
## sumQuarantine                  -3.529236   1.603828  -2.201  0.02873 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 9.081 on 238 degrees of freedom
##   (73 observations deleted due to missingness)
## Multiple R-squared:  0.4676,	Adjusted R-squared:  0.4519 
## F-statistic: 29.86 on 7 and 238 DF,  p-value: < 2.2e-16
## 
## 
## Call:
## lm(formula = full_time_work_prop ~ confirmed_7dav_cumulative_prop + 
##     confirmed_7dav_incidence_prop + deaths_7dav_cumulative_prop + 
##     sumEmergDec + sumGathRestrict + sumSchoolClose + sumBarRestrict + 
##     sumNEBusinessClose + sumRestaurantRestrict + sumStayAtHome + 
##     sumPublicMask + sumOtherBusinessClose + sumBusinessMask + 
##     sumSchoolMask + sumQuarantine, data = .)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -22.3399  -6.5370   0.6908   7.1642  15.8622 
## 
## Coefficients: (10 not defined because of singularities)
##                                Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    34.80573    6.02617   5.776 2.69e-08 ***
## confirmed_7dav_cumulative_prop  0.01750    0.00299   5.851 1.83e-08 ***
## confirmed_7dav_incidence_prop  -0.35188    0.11493  -3.062  0.00248 ** 
## deaths_7dav_cumulative_prop    -3.20212    0.43920  -7.291 5.95e-12 ***
## sumEmergDec                          NA         NA      NA       NA    
## sumGathRestrict                      NA         NA      NA       NA    
## sumSchoolClose                       NA         NA      NA       NA    
## sumBarRestrict                       NA         NA      NA       NA    
## sumNEBusinessClose                   NA         NA      NA       NA    
## sumRestaurantRestrict                NA         NA      NA       NA    
## sumStayAtHome                        NA         NA      NA       NA    
## sumPublicMask                        NA         NA      NA       NA    
## sumOtherBusinessClose                NA         NA      NA       NA    
## sumBusinessMask                      NA         NA      NA       NA    
## sumSchoolMask                  -2.75922    1.13304  -2.435  0.01570 *  
## sumQuarantine                   7.47852    2.56414   2.917  0.00392 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.819 on 213 degrees of freedom
##   (100 observations deleted due to missingness)
## Multiple R-squared:  0.5272,	Adjusted R-squared:  0.5161 
## F-statistic:  47.5 on 5 and 213 DF,  p-value: < 2.2e-16
## 
## 
## Call:
## lm(formula = full_time_work_prop ~ confirmed_7dav_cumulative_prop + 
##     confirmed_7dav_incidence_prop + deaths_7dav_cumulative_prop + 
##     sumEmergDec + sumGathRestrict + sumSchoolClose + sumBarRestrict + 
##     sumNEBusinessClose + sumRestaurantRestrict + sumStayAtHome + 
##     sumPublicMask + sumOtherBusinessClose + sumBusinessMask + 
##     sumSchoolMask + sumQuarantine, data = .)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -24.924  -6.495   1.047   6.773  16.785 
## 
## Coefficients: (8 not defined because of singularities)
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    52.125646  13.196594   3.950 0.000103 ***
## confirmed_7dav_cumulative_prop -0.028003   0.007434  -3.767 0.000209 ***
## confirmed_7dav_incidence_prop   1.563217   0.252754   6.185 2.75e-09 ***
## deaths_7dav_cumulative_prop     0.508745   0.723450   0.703 0.482618    
## sumEmergDec                           NA         NA      NA       NA    
## sumGathRestrict                       NA         NA      NA       NA    
## sumSchoolClose                        NA         NA      NA       NA    
## sumBarRestrict                        NA         NA      NA       NA    
## sumNEBusinessClose                    NA         NA      NA       NA    
## sumRestaurantRestrict                 NA         NA      NA       NA    
## sumStayAtHome                         NA         NA      NA       NA    
## sumPublicMask                         NA         NA      NA       NA    
## sumOtherBusinessClose          -6.014440   3.228822  -1.863 0.063753 .  
## sumBusinessMask                 3.147380   1.615657   1.948 0.052605 .  
## sumSchoolMask                   2.908371   1.024162   2.840 0.004912 ** 
## sumQuarantine                  -1.937082   2.002382  -0.967 0.334348    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.898 on 234 degrees of freedom
##   (77 observations deleted due to missingness)
## Multiple R-squared:  0.4928,	Adjusted R-squared:  0.4776 
## F-statistic: 32.48 on 7 and 234 DF,  p-value: < 2.2e-16
## 
## 
## Call:
## lm(formula = full_time_work_prop ~ confirmed_7dav_cumulative_prop + 
##     confirmed_7dav_incidence_prop + deaths_7dav_cumulative_prop + 
##     sumEmergDec + sumGathRestrict + sumSchoolClose + sumBarRestrict + 
##     sumNEBusinessClose + sumRestaurantRestrict + sumStayAtHome + 
##     sumPublicMask + sumOtherBusinessClose + sumBusinessMask + 
##     sumSchoolMask + sumQuarantine, data = .)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -22.3408  -5.8968   0.5193   6.7671  16.0590 
## 
## Coefficients: (8 not defined because of singularities)
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    44.681443   9.266983   4.822 2.54e-06 ***
## confirmed_7dav_cumulative_prop -0.008758   0.006140  -1.426  0.15512    
## confirmed_7dav_incidence_prop   1.410877   0.282118   5.001 1.11e-06 ***
## deaths_7dav_cumulative_prop    -3.155402   1.115986  -2.827  0.00509 ** 
## sumEmergDec                           NA         NA      NA       NA    
## sumGathRestrict                       NA         NA      NA       NA    
## sumSchoolClose                        NA         NA      NA       NA    
## sumBarRestrict                        NA         NA      NA       NA    
## sumNEBusinessClose                    NA         NA      NA       NA    
## sumRestaurantRestrict                 NA         NA      NA       NA    
## sumStayAtHome                         NA         NA      NA       NA    
## sumPublicMask                         NA         NA      NA       NA    
## sumOtherBusinessClose          -5.970235   2.072336  -2.881  0.00433 ** 
## sumBusinessMask                 5.101817   1.234105   4.134 4.94e-05 ***
## sumSchoolMask                   0.159161   0.935669   0.170  0.86507    
## sumQuarantine                   2.793420   2.643016   1.057  0.29163    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.774 on 238 degrees of freedom
##   (73 observations deleted due to missingness)
## Multiple R-squared:  0.5029,	Adjusted R-squared:  0.4883 
## F-statistic:  34.4 on 7 and 238 DF,  p-value: < 2.2e-16
## 
## 
## Call:
## lm(formula = full_time_work_prop ~ confirmed_7dav_cumulative_prop + 
##     confirmed_7dav_incidence_prop + deaths_7dav_cumulative_prop + 
##     sumEmergDec + sumGathRestrict + sumSchoolClose + sumBarRestrict + 
##     sumNEBusinessClose + sumRestaurantRestrict + sumStayAtHome + 
##     sumPublicMask + sumOtherBusinessClose + sumBusinessMask + 
##     sumSchoolMask + sumQuarantine, data = .)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -19.9297  -5.2015   0.0432   5.7566  17.8302 
## 
## Coefficients: (7 not defined because of singularities)
##                                  Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    143.004787   6.486770  22.046  < 2e-16 ***
## confirmed_7dav_cumulative_prop  -0.031322   0.004509  -6.947 2.62e-11 ***
## confirmed_7dav_incidence_prop    0.049105   0.086671   0.567  0.57146    
## deaths_7dav_cumulative_prop      1.994424   0.403067   4.948 1.30e-06 ***
## sumEmergDec                            NA         NA      NA       NA    
## sumGathRestrict                        NA         NA      NA       NA    
## sumSchoolClose                         NA         NA      NA       NA    
## sumBarRestrict                         NA         NA      NA       NA    
## sumNEBusinessClose                     NA         NA      NA       NA    
## sumRestaurantRestrict                  NA         NA      NA       NA    
## sumStayAtHome                          NA         NA      NA       NA    
## sumPublicMask                  -12.790909   1.394268  -9.174  < 2e-16 ***
## sumOtherBusinessClose          -14.172975   1.227775 -11.544  < 2e-16 ***
## sumBusinessMask                  3.764337   1.197804   3.143  0.00185 ** 
## sumSchoolMask                   -0.667917   0.845889  -0.790  0.43043    
## sumQuarantine                   -3.259112   1.581362  -2.061  0.04023 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.398 on 280 degrees of freedom
##   (30 observations deleted due to missingness)
## Multiple R-squared:  0.7597,	Adjusted R-squared:  0.7528 
## F-statistic: 110.6 on 8 and 280 DF,  p-value: < 2.2e-16
## 
## 
## Call:
## lm(formula = full_time_work_prop ~ confirmed_7dav_cumulative_prop + 
##     confirmed_7dav_incidence_prop + deaths_7dav_cumulative_prop + 
##     sumEmergDec + sumGathRestrict + sumSchoolClose + sumBarRestrict + 
##     sumNEBusinessClose + sumRestaurantRestrict + sumStayAtHome + 
##     sumPublicMask + sumOtherBusinessClose + sumBusinessMask + 
##     sumSchoolMask + sumQuarantine, data = .)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -23.1714  -5.7961   0.2775   6.0878  16.7766 
## 
## Coefficients: (10 not defined because of singularities)
##                                Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    38.24267    6.49987   5.884 1.43e-08 ***
## confirmed_7dav_cumulative_prop  0.02641    0.01866   1.415    0.158    
## confirmed_7dav_incidence_prop  -0.83047    0.10113  -8.212 1.64e-14 ***
## deaths_7dav_cumulative_prop          NA         NA      NA       NA    
## sumEmergDec                          NA         NA      NA       NA    
## sumGathRestrict                      NA         NA      NA       NA    
## sumSchoolClose                       NA         NA      NA       NA    
## sumBarRestrict                       NA         NA      NA       NA    
## sumNEBusinessClose                   NA         NA      NA       NA    
## sumRestaurantRestrict                NA         NA      NA       NA    
## sumStayAtHome                        NA         NA      NA       NA    
## sumPublicMask                        NA         NA      NA       NA    
## sumOtherBusinessClose                NA         NA      NA       NA    
## sumBusinessMask                 6.86539    1.41615   4.848 2.31e-06 ***
## sumSchoolMask                  -0.68990    0.80391  -0.858    0.392    
## sumQuarantine                  -9.07651    1.70645  -5.319 2.50e-07 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.569 on 227 degrees of freedom
##   (86 observations deleted due to missingness)
## Multiple R-squared:  0.5421,	Adjusted R-squared:  0.532 
## F-statistic: 53.74 on 5 and 227 DF,  p-value: < 2.2e-16
## 
## 
## Call:
## lm(formula = full_time_work_prop ~ confirmed_7dav_cumulative_prop + 
##     confirmed_7dav_incidence_prop + deaths_7dav_cumulative_prop + 
##     sumEmergDec + sumGathRestrict + sumSchoolClose + sumBarRestrict + 
##     sumNEBusinessClose + sumRestaurantRestrict + sumStayAtHome + 
##     sumPublicMask + sumOtherBusinessClose + sumBusinessMask + 
##     sumSchoolMask + sumQuarantine, data = .)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -22.5074  -5.6969   0.8431   5.7504  16.1652 
## 
## Coefficients: (8 not defined because of singularities)
##                                Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    55.04880   15.30119   3.598 0.000392 ***
## confirmed_7dav_cumulative_prop  0.01250    0.01102   1.134 0.257870    
## confirmed_7dav_incidence_prop  -0.07075    0.43107  -0.164 0.869778    
## deaths_7dav_cumulative_prop    -1.60259    0.17855  -8.976  < 2e-16 ***
## sumEmergDec                          NA         NA      NA       NA    
## sumGathRestrict                      NA         NA      NA       NA    
## sumSchoolClose                       NA         NA      NA       NA    
## sumBarRestrict                       NA         NA      NA       NA    
## sumNEBusinessClose                   NA         NA      NA       NA    
## sumRestaurantRestrict                NA         NA      NA       NA    
## sumStayAtHome                        NA         NA      NA       NA    
## sumPublicMask                        NA         NA      NA       NA    
## sumOtherBusinessClose          -5.98184    3.89906  -1.534 0.126344    
## sumBusinessMask                 4.92847    1.15447   4.269 2.86e-05 ***
## sumSchoolMask                  -0.91970    0.82017  -1.121 0.263290    
## sumQuarantine                  -1.72102    1.64042  -1.049 0.295202    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.301 on 233 degrees of freedom
##   (78 observations deleted due to missingness)
## Multiple R-squared:  0.5599,	Adjusted R-squared:  0.5467 
## F-statistic: 42.35 on 7 and 233 DF,  p-value: < 2.2e-16
## 
## 
## Call:
## lm(formula = full_time_work_prop ~ confirmed_7dav_cumulative_prop + 
##     confirmed_7dav_incidence_prop + deaths_7dav_cumulative_prop + 
##     sumEmergDec + sumGathRestrict + sumSchoolClose + sumBarRestrict + 
##     sumNEBusinessClose + sumRestaurantRestrict + sumStayAtHome + 
##     sumPublicMask + sumOtherBusinessClose + sumBusinessMask + 
##     sumSchoolMask + sumQuarantine, data = .)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -24.415  -7.202   2.073   7.475  16.864 
## 
## Coefficients: (10 not defined because of singularities)
##                                Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    63.72748    5.32287  11.972  < 2e-16 ***
## confirmed_7dav_cumulative_prop -0.03516    0.01019  -3.449 0.000678 ***
## confirmed_7dav_incidence_prop  -0.97055    0.30249  -3.208 0.001540 ** 
## deaths_7dav_cumulative_prop     4.07386    1.33214   3.058 0.002513 ** 
## sumEmergDec                          NA         NA      NA       NA    
## sumGathRestrict                      NA         NA      NA       NA    
## sumSchoolClose                       NA         NA      NA       NA    
## sumBarRestrict                       NA         NA      NA       NA    
## sumNEBusinessClose                   NA         NA      NA       NA    
## sumRestaurantRestrict                NA         NA      NA       NA    
## sumStayAtHome                        NA         NA      NA       NA    
## sumPublicMask                        NA         NA      NA       NA    
## sumOtherBusinessClose                NA         NA      NA       NA    
## sumBusinessMask                      NA         NA      NA       NA    
## sumSchoolMask                  -1.31059    1.17936  -1.111 0.267706    
## sumQuarantine                  -6.47421    1.95726  -3.308 0.001104 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 9.53 on 213 degrees of freedom
##   (100 observations deleted due to missingness)
## Multiple R-squared:  0.4479,	Adjusted R-squared:  0.4349 
## F-statistic: 34.56 on 5 and 213 DF,  p-value: < 2.2e-16
## 
## 
## Call:
## lm(formula = full_time_work_prop ~ confirmed_7dav_cumulative_prop + 
##     confirmed_7dav_incidence_prop + deaths_7dav_cumulative_prop + 
##     sumEmergDec + sumGathRestrict + sumSchoolClose + sumBarRestrict + 
##     sumNEBusinessClose + sumRestaurantRestrict + sumStayAtHome + 
##     sumPublicMask + sumOtherBusinessClose + sumBusinessMask + 
##     sumSchoolMask + sumQuarantine, data = .)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -23.7172  -4.7088   0.0202   5.9801  16.9498 
## 
## Coefficients: (10 not defined because of singularities)
##                                Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    47.25009    4.52393  10.444  < 2e-16 ***
## confirmed_7dav_cumulative_prop  0.04707    0.01344   3.503 0.000561 ***
## confirmed_7dav_incidence_prop  -0.03253    0.13434  -0.242 0.808893    
## deaths_7dav_cumulative_prop    -4.43440    1.06121  -4.179 4.28e-05 ***
## sumEmergDec                          NA         NA      NA       NA    
## sumGathRestrict                      NA         NA      NA       NA    
## sumSchoolClose                       NA         NA      NA       NA    
## sumBarRestrict                       NA         NA      NA       NA    
## sumNEBusinessClose                   NA         NA      NA       NA    
## sumRestaurantRestrict                NA         NA      NA       NA    
## sumStayAtHome                        NA         NA      NA       NA    
## sumPublicMask                        NA         NA      NA       NA    
## sumOtherBusinessClose                NA         NA      NA       NA    
## sumBusinessMask                      NA         NA      NA       NA    
## sumSchoolMask                  -1.65490    1.19172  -1.389 0.166386    
## sumQuarantine                   1.44156    1.90909   0.755 0.451021    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.292 on 213 degrees of freedom
##   (100 observations deleted due to missingness)
## Multiple R-squared:  0.582,	Adjusted R-squared:  0.5722 
## F-statistic: 59.31 on 5 and 213 DF,  p-value: < 2.2e-16
## 
## 
## Call:
## lm(formula = full_time_work_prop ~ confirmed_7dav_cumulative_prop + 
##     confirmed_7dav_incidence_prop + deaths_7dav_cumulative_prop + 
##     sumEmergDec + sumGathRestrict + sumSchoolClose + sumBarRestrict + 
##     sumNEBusinessClose + sumRestaurantRestrict + sumStayAtHome + 
##     sumPublicMask + sumOtherBusinessClose + sumBusinessMask + 
##     sumSchoolMask + sumQuarantine, data = .)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -21.7560  -6.8485   0.1986   7.3238  18.1764 
## 
## Coefficients: (8 not defined because of singularities)
##                                  Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                     82.828636   5.719405  14.482  < 2e-16 ***
## confirmed_7dav_cumulative_prop  -0.010269   0.003798  -2.704 0.007327 ** 
## confirmed_7dav_incidence_prop    0.042911   0.064424   0.666 0.505986    
## deaths_7dav_cumulative_prop      0.472196   0.166811   2.831 0.005023 ** 
## sumEmergDec                            NA         NA      NA       NA    
## sumGathRestrict                        NA         NA      NA       NA    
## sumSchoolClose                         NA         NA      NA       NA    
## sumBarRestrict                         NA         NA      NA       NA    
## sumNEBusinessClose                     NA         NA      NA       NA    
## sumRestaurantRestrict                  NA         NA      NA       NA    
## sumStayAtHome                          NA         NA      NA       NA    
## sumPublicMask                          NA         NA      NA       NA    
## sumOtherBusinessClose           -9.157860   1.600358  -5.722 3.01e-08 ***
## sumBusinessMask                  4.545252   1.312235   3.464 0.000627 ***
## sumSchoolMask                    1.041129   1.131999   0.920 0.358606    
## sumQuarantine                  -10.830053   1.489079  -7.273 4.55e-12 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 9.365 on 249 degrees of freedom
##   (62 observations deleted due to missingness)
## Multiple R-squared:  0.4519,	Adjusted R-squared:  0.4365 
## F-statistic: 29.33 on 7 and 249 DF,  p-value: < 2.2e-16
## 
## 
## Call:
## lm(formula = full_time_work_prop ~ confirmed_7dav_cumulative_prop + 
##     confirmed_7dav_incidence_prop + deaths_7dav_cumulative_prop + 
##     sumEmergDec + sumGathRestrict + sumSchoolClose + sumBarRestrict + 
##     sumNEBusinessClose + sumRestaurantRestrict + sumStayAtHome + 
##     sumPublicMask + sumOtherBusinessClose + sumBusinessMask + 
##     sumSchoolMask + sumQuarantine, data = .)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -23.2252  -4.5396   0.8631   4.7827  17.0714 
## 
## Coefficients: (8 not defined because of singularities)
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                     83.51161    4.85895  17.187  < 2e-16 ***
## confirmed_7dav_cumulative_prop  -0.03583    0.00356 -10.064  < 2e-16 ***
## confirmed_7dav_incidence_prop    0.33737    0.12697   2.657  0.00838 ** 
## deaths_7dav_cumulative_prop      6.12470    0.74997   8.167 1.47e-14 ***
## sumEmergDec                           NA         NA      NA       NA    
## sumGathRestrict                       NA         NA      NA       NA    
## sumSchoolClose                        NA         NA      NA       NA    
## sumBarRestrict                        NA         NA      NA       NA    
## sumNEBusinessClose                    NA         NA      NA       NA    
## sumRestaurantRestrict                 NA         NA      NA       NA    
## sumStayAtHome                         NA         NA      NA       NA    
## sumPublicMask                         NA         NA      NA       NA    
## sumOtherBusinessClose          -15.64296    1.41010 -11.094  < 2e-16 ***
## sumBusinessMask                  5.30668    1.11209   4.772 3.08e-06 ***
## sumSchoolMask                   -0.96234    0.91222  -1.055  0.29245    
## sumQuarantine                   -3.02118    1.46907  -2.057  0.04075 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.005 on 255 degrees of freedom
##   (56 observations deleted due to missingness)
## Multiple R-squared:  0.6229,	Adjusted R-squared:  0.6125 
## F-statistic: 60.16 on 7 and 255 DF,  p-value: < 2.2e-16
## 
## 
## Call:
## lm(formula = full_time_work_prop ~ confirmed_7dav_cumulative_prop + 
##     confirmed_7dav_incidence_prop + deaths_7dav_cumulative_prop + 
##     sumEmergDec + sumGathRestrict + sumSchoolClose + sumBarRestrict + 
##     sumNEBusinessClose + sumRestaurantRestrict + sumStayAtHome + 
##     sumPublicMask + sumOtherBusinessClose + sumBusinessMask + 
##     sumSchoolMask + sumQuarantine, data = .)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -22.6019  -5.7397   0.7291   5.8702  15.4006 
## 
## Coefficients: (8 not defined because of singularities)
##                                  Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    53.5972421 10.0285269   5.344 2.14e-07 ***
## confirmed_7dav_cumulative_prop -0.0004705  0.0036748  -0.128   0.8982    
## confirmed_7dav_incidence_prop   0.2480783  0.2338680   1.061   0.2899    
## deaths_7dav_cumulative_prop    -2.9080811  0.4432577  -6.561 3.38e-10 ***
## sumEmergDec                            NA         NA      NA       NA    
## sumGathRestrict                        NA         NA      NA       NA    
## sumSchoolClose                         NA         NA      NA       NA    
## sumBarRestrict                         NA         NA      NA       NA    
## sumNEBusinessClose                     NA         NA      NA       NA    
## sumRestaurantRestrict                  NA         NA      NA       NA    
## sumStayAtHome                          NA         NA      NA       NA    
## sumPublicMask                          NA         NA      NA       NA    
## sumOtherBusinessClose          -5.9320117  2.5648032  -2.313   0.0216 *  
## sumBusinessMask                 4.9521211  1.1651160   4.250 3.08e-05 ***
## sumSchoolMask                   0.9084962  0.8490866   1.070   0.2857    
## sumQuarantine                  -2.9460219  1.5357835  -1.918   0.0563 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.387 on 235 degrees of freedom
##   (76 observations deleted due to missingness)
## Multiple R-squared:  0.5482,	Adjusted R-squared:  0.5347 
## F-statistic: 40.73 on 7 and 235 DF,  p-value: < 2.2e-16
## 
## 
## Call:
## lm(formula = full_time_work_prop ~ confirmed_7dav_cumulative_prop + 
##     confirmed_7dav_incidence_prop + deaths_7dav_cumulative_prop + 
##     sumEmergDec + sumGathRestrict + sumSchoolClose + sumBarRestrict + 
##     sumNEBusinessClose + sumRestaurantRestrict + sumStayAtHome + 
##     sumPublicMask + sumOtherBusinessClose + sumBusinessMask + 
##     sumSchoolMask + sumQuarantine, data = .)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -22.6286  -5.2250  -0.1139   6.2143  16.0970 
## 
## Coefficients: (8 not defined because of singularities)
##                                Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    63.40351   11.83309   5.358 2.01e-07 ***
## confirmed_7dav_cumulative_prop  0.04324    0.03877   1.115  0.26590    
## confirmed_7dav_incidence_prop  -0.14384    0.50257  -0.286  0.77497    
## deaths_7dav_cumulative_prop    -1.35967    0.17492  -7.773 2.42e-13 ***
## sumEmergDec                          NA         NA      NA       NA    
## sumGathRestrict                      NA         NA      NA       NA    
## sumSchoolClose                       NA         NA      NA       NA    
## sumBarRestrict                       NA         NA      NA       NA    
## sumNEBusinessClose                   NA         NA      NA       NA    
## sumRestaurantRestrict                NA         NA      NA       NA    
## sumStayAtHome                        NA         NA      NA       NA    
## sumPublicMask                        NA         NA      NA       NA    
## sumOtherBusinessClose          -5.83330    2.95177  -1.976  0.04931 *  
## sumBusinessMask                 4.96429    1.14285   4.344 2.09e-05 ***
## sumSchoolMask                  -0.91309    0.80166  -1.139  0.25587    
## sumQuarantine                  -6.23633    1.91367  -3.259  0.00128 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.226 on 234 degrees of freedom
##   (77 observations deleted due to missingness)
## Multiple R-squared:  0.5666,	Adjusted R-squared:  0.5536 
## F-statistic:  43.7 on 7 and 234 DF,  p-value: < 2.2e-16
## 
## 
## Call:
## lm(formula = full_time_work_prop ~ confirmed_7dav_cumulative_prop + 
##     confirmed_7dav_incidence_prop + deaths_7dav_cumulative_prop + 
##     sumEmergDec + sumGathRestrict + sumSchoolClose + sumBarRestrict + 
##     sumNEBusinessClose + sumRestaurantRestrict + sumStayAtHome + 
##     sumPublicMask + sumOtherBusinessClose + sumBusinessMask + 
##     sumSchoolMask + sumQuarantine, data = .)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -20.937  -7.089   0.348   7.629  18.843 
## 
## Coefficients: (8 not defined because of singularities)
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    87.602109   8.126802  10.779  < 2e-16 ***
## confirmed_7dav_cumulative_prop -0.005577   0.003753  -1.486   0.1385    
## confirmed_7dav_incidence_prop   0.067703   0.089143   0.759   0.4483    
## deaths_7dav_cumulative_prop     0.425737   0.309991   1.373   0.1709    
## sumEmergDec                           NA         NA      NA       NA    
## sumGathRestrict                       NA         NA      NA       NA    
## sumSchoolClose                        NA         NA      NA       NA    
## sumBarRestrict                        NA         NA      NA       NA    
## sumNEBusinessClose                    NA         NA      NA       NA    
## sumRestaurantRestrict                 NA         NA      NA       NA    
## sumStayAtHome                         NA         NA      NA       NA    
## sumPublicMask                         NA         NA      NA       NA    
## sumOtherBusinessClose          -9.453723   1.630616  -5.798 2.03e-08 ***
## sumBusinessMask                 3.219808   1.824604   1.765   0.0788 .  
## sumSchoolMask                  -1.959057   1.600237  -1.224   0.2220    
## sumQuarantine                  -8.600449   1.618627  -5.313 2.39e-07 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 9.477 on 249 degrees of freedom
##   (62 observations deleted due to missingness)
## Multiple R-squared:  0.4388,	Adjusted R-squared:  0.423 
## F-statistic: 27.81 on 7 and 249 DF,  p-value: < 2.2e-16
## 
## 
## Call:
## lm(formula = full_time_work_prop ~ confirmed_7dav_cumulative_prop + 
##     confirmed_7dav_incidence_prop + deaths_7dav_cumulative_prop + 
##     sumEmergDec + sumGathRestrict + sumSchoolClose + sumBarRestrict + 
##     sumNEBusinessClose + sumRestaurantRestrict + sumStayAtHome + 
##     sumPublicMask + sumOtherBusinessClose + sumBusinessMask + 
##     sumSchoolMask + sumQuarantine, data = .)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -23.5546  -7.9419   0.3273   8.1642  16.7706 
## 
## Coefficients: (10 not defined because of singularities)
##                                Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    55.98225    4.85737  11.525  < 2e-16 ***
## confirmed_7dav_cumulative_prop  0.00968    0.01320   0.734  0.46401    
## confirmed_7dav_incidence_prop   0.25798    0.30996   0.832  0.40618    
## deaths_7dav_cumulative_prop    -3.41521    1.04167  -3.279  0.00122 ** 
## sumEmergDec                          NA         NA      NA       NA    
## sumGathRestrict                      NA         NA      NA       NA    
## sumSchoolClose                       NA         NA      NA       NA    
## sumBarRestrict                       NA         NA      NA       NA    
## sumNEBusinessClose                   NA         NA      NA       NA    
## sumRestaurantRestrict                NA         NA      NA       NA    
## sumStayAtHome                        NA         NA      NA       NA    
## sumPublicMask                        NA         NA      NA       NA    
## sumOtherBusinessClose                NA         NA      NA       NA    
## sumBusinessMask                      NA         NA      NA       NA    
## sumSchoolMask                  -1.20351    0.91169  -1.320  0.18822    
## sumQuarantine                  -3.51034    1.88098  -1.866  0.06338 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 9.497 on 213 degrees of freedom
##   (100 observations deleted due to missingness)
## Multiple R-squared:  0.4517,	Adjusted R-squared:  0.4388 
## F-statistic: 35.09 on 5 and 213 DF,  p-value: < 2.2e-16
## 
## 
## Call:
## lm(formula = full_time_work_prop ~ confirmed_7dav_cumulative_prop + 
##     confirmed_7dav_incidence_prop + deaths_7dav_cumulative_prop + 
##     sumEmergDec + sumGathRestrict + sumSchoolClose + sumBarRestrict + 
##     sumNEBusinessClose + sumRestaurantRestrict + sumStayAtHome + 
##     sumPublicMask + sumOtherBusinessClose + sumBusinessMask + 
##     sumSchoolMask + sumQuarantine, data = .)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -24.260  -6.680   1.083   6.673  20.574 
## 
## Coefficients: (10 not defined because of singularities)
##                                Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    47.82135    5.05804   9.455  < 2e-16 ***
## confirmed_7dav_cumulative_prop  0.05443    0.01018   5.344 2.33e-07 ***
## confirmed_7dav_incidence_prop   0.51756    0.18434   2.808  0.00545 ** 
## deaths_7dav_cumulative_prop    -6.69023    1.07318  -6.234 2.40e-09 ***
## sumEmergDec                          NA         NA      NA       NA    
## sumGathRestrict                      NA         NA      NA       NA    
## sumSchoolClose                       NA         NA      NA       NA    
## sumBarRestrict                       NA         NA      NA       NA    
## sumNEBusinessClose                   NA         NA      NA       NA    
## sumRestaurantRestrict                NA         NA      NA       NA    
## sumStayAtHome                        NA         NA      NA       NA    
## sumPublicMask                        NA         NA      NA       NA    
## sumOtherBusinessClose                NA         NA      NA       NA    
## sumBusinessMask                      NA         NA      NA       NA    
## sumSchoolMask                   3.51420    1.16237   3.023  0.00281 ** 
## sumQuarantine                  -4.28427    1.74853  -2.450  0.01508 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.963 on 213 degrees of freedom
##   (100 observations deleted due to missingness)
## Multiple R-squared:  0.5116,	Adjusted R-squared:  0.5002 
## F-statistic: 44.63 on 5 and 213 DF,  p-value: < 2.2e-16
## 
## 
## Call:
## lm(formula = full_time_work_prop ~ confirmed_7dav_cumulative_prop + 
##     confirmed_7dav_incidence_prop + deaths_7dav_cumulative_prop + 
##     sumEmergDec + sumGathRestrict + sumSchoolClose + sumBarRestrict + 
##     sumNEBusinessClose + sumRestaurantRestrict + sumStayAtHome + 
##     sumPublicMask + sumOtherBusinessClose + sumBusinessMask + 
##     sumSchoolMask + sumQuarantine, data = .)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -21.408  -7.727   0.789   7.720  17.278 
## 
## Coefficients: (8 not defined because of singularities)
##                                  Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                     82.663076   8.233884  10.039  < 2e-16 ***
## confirmed_7dav_cumulative_prop  -0.012537   0.009693  -1.293    0.197    
## confirmed_7dav_incidence_prop    0.005948   0.216441   0.027    0.978    
## deaths_7dav_cumulative_prop      0.534342   0.756906   0.706    0.481    
## sumEmergDec                            NA         NA      NA       NA    
## sumGathRestrict                        NA         NA      NA       NA    
## sumSchoolClose                         NA         NA      NA       NA    
## sumBarRestrict                         NA         NA      NA       NA    
## sumNEBusinessClose                     NA         NA      NA       NA    
## sumRestaurantRestrict                  NA         NA      NA       NA    
## sumStayAtHome                          NA         NA      NA       NA    
## sumPublicMask                          NA         NA      NA       NA    
## sumOtherBusinessClose          -11.173752   1.713218  -6.522 3.70e-10 ***
## sumBusinessMask                  3.486971   2.633793   1.324    0.187    
## sumSchoolMask                    1.554730   1.091663   1.424    0.156    
## sumQuarantine                   -6.749566   1.610737  -4.190 3.84e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 9.385 on 255 degrees of freedom
##   (56 observations deleted due to missingness)
## Multiple R-squared:  0.4816,	Adjusted R-squared:  0.4674 
## F-statistic: 33.84 on 7 and 255 DF,  p-value: < 2.2e-16
## 
## 
## Call:
## lm(formula = full_time_work_prop ~ confirmed_7dav_cumulative_prop + 
##     confirmed_7dav_incidence_prop + deaths_7dav_cumulative_prop + 
##     sumEmergDec + sumGathRestrict + sumSchoolClose + sumBarRestrict + 
##     sumNEBusinessClose + sumRestaurantRestrict + sumStayAtHome + 
##     sumPublicMask + sumOtherBusinessClose + sumBusinessMask + 
##     sumSchoolMask + sumQuarantine, data = .)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -22.6193  -6.6898   0.1504   6.9037  19.6462 
## 
## Coefficients: (10 not defined because of singularities)
##                                Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    46.21370    4.44067  10.407  < 2e-16 ***
## confirmed_7dav_cumulative_prop  0.01769    0.00318   5.564 7.85e-08 ***
## confirmed_7dav_incidence_prop  -0.90738    0.14997  -6.050 6.38e-09 ***
## deaths_7dav_cumulative_prop    -6.49655    0.88769  -7.318 4.99e-12 ***
## sumEmergDec                          NA         NA      NA       NA    
## sumGathRestrict                      NA         NA      NA       NA    
## sumSchoolClose                       NA         NA      NA       NA    
## sumBarRestrict                       NA         NA      NA       NA    
## sumNEBusinessClose                   NA         NA      NA       NA    
## sumRestaurantRestrict                NA         NA      NA       NA    
## sumStayAtHome                        NA         NA      NA       NA    
## sumPublicMask                        NA         NA      NA       NA    
## sumOtherBusinessClose                NA         NA      NA       NA    
## sumBusinessMask                      NA         NA      NA       NA    
## sumSchoolMask                   3.59501    0.99060   3.629 0.000356 ***
## sumQuarantine                  -3.75863    1.49483  -2.514 0.012659 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.665 on 214 degrees of freedom
##   (99 observations deleted due to missingness)
## Multiple R-squared:  0.5446,	Adjusted R-squared:  0.534 
## F-statistic: 51.19 on 5 and 214 DF,  p-value: < 2.2e-16
```

## Model 2:

$$Y_t = \beta_{0}+ \sum_{i}\beta_{i}S_{i_{t}} + \sum_{k}\beta_{k}P_{k_{t}}$$


```r
mod2_cof_ls <- list()
count <- 1
for(county in ca.fips_codes$fips){
  p <- mobility%>%filter(geo_value==county)%>%
    lm(full_time_work_prop ~ confirmed_7dav_cumulative_prop+confirmed_7dav_incidence_prop+deaths_7dav_cumulative_prop+deaths_7dav_incidence_prop+EmergDec + GathRestrict+BarRestrict+NEBusinessClose+RestaurantRestrict+StayAtHome+PublicMask+OtherBusinessClose+BusinessMask+SchoolMask+Quarantine, data=.)
  mod2_cof_ls[[count]] <- p
  count <- count + 1
}

# Show all results for county level mobility
counter <- 1
for (i in mod2_cof_ls){
  print(ca.fips_codes$county[counter])
  print(summary(i))
  counter <- counter + 1
}
```

```
## [1] "Alameda County"
## 
## Call:
## lm(formula = full_time_work_prop ~ confirmed_7dav_cumulative_prop + 
##     confirmed_7dav_incidence_prop + deaths_7dav_cumulative_prop + 
##     deaths_7dav_incidence_prop + EmergDec + GathRestrict + BarRestrict + 
##     NEBusinessClose + RestaurantRestrict + StayAtHome + PublicMask + 
##     OtherBusinessClose + BusinessMask + SchoolMask + Quarantine, 
##     data = .)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -24.0727  -5.4272   0.6368   5.5965  17.4565 
## 
## Coefficients: (3 not defined because of singularities)
##                                  Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    117.500397   2.246442  52.305  < 2e-16 ***
## confirmed_7dav_cumulative_prop  -0.011262   0.005822  -1.934  0.05400 .  
## confirmed_7dav_incidence_prop   -0.307930   0.122394  -2.516  0.01239 *  
## deaths_7dav_cumulative_prop      0.314580   0.323239   0.973  0.33122    
## deaths_7dav_incidence_prop      -7.554951   6.154672  -1.228  0.22057    
## EmergDec1                      -10.494294   3.890952  -2.697  0.00738 ** 
## GathRestrict1                   -7.459218   4.350233  -1.715  0.08742 .  
## BarRestrict1                    -9.938724   3.782765  -2.627  0.00904 ** 
## NEBusinessClose1                       NA         NA      NA       NA    
## RestaurantRestrict1                    NA         NA      NA       NA    
## StayAtHome1                            NA         NA      NA       NA    
## PublicMask1                    -24.055767   2.791538  -8.617 3.72e-16 ***
## OtherBusinessClose1            -27.387045   2.469349 -11.091  < 2e-16 ***
## BusinessMask1                   10.864508   2.333450   4.656 4.81e-06 ***
## SchoolMask1                      5.677862   2.066953   2.747  0.00637 ** 
## Quarantine1                     -3.849793   2.650329  -1.453  0.14737    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.405 on 306 degrees of freedom
## Multiple R-squared:  0.8899,	Adjusted R-squared:  0.8856 
## F-statistic: 206.1 on 12 and 306 DF,  p-value: < 2.2e-16
## 
## [1] "Alpine County"
## 
## Call:
## lm(formula = full_time_work_prop ~ confirmed_7dav_cumulative_prop + 
##     confirmed_7dav_incidence_prop + deaths_7dav_cumulative_prop + 
##     deaths_7dav_incidence_prop + EmergDec + GathRestrict + BarRestrict + 
##     NEBusinessClose + RestaurantRestrict + StayAtHome + PublicMask + 
##     OtherBusinessClose + BusinessMask + SchoolMask + Quarantine, 
##     data = .)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -22.4038  -6.7491   0.0451   7.2509  17.7682 
## 
## Coefficients: (5 not defined because of singularities)
##                                  Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                     1.175e+02  2.449e+00  47.971  < 2e-16 ***
## confirmed_7dav_cumulative_prop -2.653e-03  7.317e-04  -3.625 0.000338 ***
## confirmed_7dav_incidence_prop  -4.645e-03  3.692e-03  -1.258 0.209395    
## deaths_7dav_cumulative_prop            NA         NA      NA       NA    
## deaths_7dav_incidence_prop             NA         NA      NA       NA    
## EmergDec1                      -1.050e+01  4.243e+00  -2.475 0.013864 *  
## GathRestrict1                  -7.500e+00  4.743e+00  -1.581 0.114862    
## BarRestrict1                   -1.047e+01  4.118e+00  -2.543 0.011495 *  
## NEBusinessClose1                       NA         NA      NA       NA    
## RestaurantRestrict1                    NA         NA      NA       NA    
## StayAtHome1                            NA         NA      NA       NA    
## PublicMask1                    -2.460e+01  2.957e+00  -8.321 2.86e-15 ***
## OtherBusinessClose1            -2.745e+01  2.634e+00 -10.424  < 2e-16 ***
## BusinessMask1                   8.904e+00  2.506e+00   3.553 0.000441 ***
## SchoolMask1                    -6.547e-01  1.524e+00  -0.430 0.667772    
## Quarantine1                    -1.138e+01  2.327e+00  -4.890 1.63e-06 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 9.165 on 308 degrees of freedom
## Multiple R-squared:  0.8682,	Adjusted R-squared:  0.864 
## F-statistic: 202.9 on 10 and 308 DF,  p-value: < 2.2e-16
## 
## [1] "Amador County"
## 
## Call:
## lm(formula = full_time_work_prop ~ confirmed_7dav_cumulative_prop + 
##     confirmed_7dav_incidence_prop + deaths_7dav_cumulative_prop + 
##     deaths_7dav_incidence_prop + EmergDec + GathRestrict + BarRestrict + 
##     NEBusinessClose + RestaurantRestrict + StayAtHome + PublicMask + 
##     OtherBusinessClose + BusinessMask + SchoolMask + Quarantine, 
##     data = .)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -22.5358  -4.9867   0.4302   5.5193  17.9379 
## 
## Coefficients: (3 not defined because of singularities)
##                                  Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                     1.175e+02  2.216e+00  53.035  < 2e-16 ***
## confirmed_7dav_cumulative_prop -6.199e-03  9.464e-04  -6.550 2.43e-10 ***
## confirmed_7dav_incidence_prop   2.209e-04  1.145e-02   0.019  0.98462    
## deaths_7dav_cumulative_prop     7.885e-02  4.646e-02   1.697  0.09071 .  
## deaths_7dav_incidence_prop     -1.931e+00  6.383e-01  -3.025  0.00270 ** 
## EmergDec1                      -1.050e+01  3.837e+00  -2.736  0.00658 ** 
## GathRestrict1                  -7.497e+00  4.290e+00  -1.747  0.08156 .  
## BarRestrict1                   -1.048e+01  3.725e+00  -2.812  0.00524 ** 
## NEBusinessClose1                       NA         NA      NA       NA    
## RestaurantRestrict1                    NA         NA      NA       NA    
## StayAtHome1                            NA         NA      NA       NA    
## PublicMask1                    -2.493e+01  2.673e+00  -9.327  < 2e-16 ***
## OtherBusinessClose1            -2.746e+01  2.382e+00 -11.526  < 2e-16 ***
## BusinessMask1                   9.051e+00  2.267e+00   3.993 8.19e-05 ***
## SchoolMask1                     1.077e+00  1.765e+00   0.610  0.54222    
## Quarantine1                    -9.431e+00  1.869e+00  -5.046 7.76e-07 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.29 on 306 degrees of freedom
## Multiple R-squared:  0.8929,	Adjusted R-squared:  0.8887 
## F-statistic: 212.6 on 12 and 306 DF,  p-value: < 2.2e-16
## 
## [1] "Butte County"
## 
## Call:
## lm(formula = full_time_work_prop ~ confirmed_7dav_cumulative_prop + 
##     confirmed_7dav_incidence_prop + deaths_7dav_cumulative_prop + 
##     deaths_7dav_incidence_prop + EmergDec + GathRestrict + BarRestrict + 
##     NEBusinessClose + RestaurantRestrict + StayAtHome + PublicMask + 
##     OtherBusinessClose + BusinessMask + SchoolMask + Quarantine, 
##     data = .)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -23.214  -4.987   1.078   5.490  17.959 
## 
## Coefficients: (3 not defined because of singularities)
##                                  Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    117.500000   2.217587  52.986  < 2e-16 ***
## confirmed_7dav_cumulative_prop  -0.014011   0.005994  -2.337 0.020066 *  
## confirmed_7dav_incidence_prop   -0.024947   0.084736  -0.294 0.768645    
## deaths_7dav_cumulative_prop      0.655457   0.342761   1.912 0.056773 .  
## deaths_7dav_incidence_prop     -17.081613   4.013559  -4.256 2.77e-05 ***
## EmergDec1                      -10.500000   3.840974  -2.734 0.006628 ** 
## GathRestrict1                   -7.500000   4.294339  -1.746 0.081730 .  
## BarRestrict1                   -10.476319   3.728560  -2.810 0.005277 ** 
## NEBusinessClose1                       NA         NA      NA       NA    
## RestaurantRestrict1                    NA         NA      NA       NA    
## StayAtHome1                            NA         NA      NA       NA    
## PublicMask1                    -24.928974   2.675465  -9.318  < 2e-16 ***
## OtherBusinessClose1            -27.435888   2.384528 -11.506  < 2e-16 ***
## BusinessMask1                    9.832865   2.281873   4.309 2.21e-05 ***
## SchoolMask1                      6.638051   1.887124   3.518 0.000502 ***
## Quarantine1                     -9.391836   2.516149  -3.733 0.000226 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.297 on 306 degrees of freedom
## Multiple R-squared:  0.8927,	Adjusted R-squared:  0.8885 
## F-statistic: 212.1 on 12 and 306 DF,  p-value: < 2.2e-16
## 
## [1] "Calaveras County"
## 
## Call:
## lm(formula = full_time_work_prop ~ confirmed_7dav_cumulative_prop + 
##     confirmed_7dav_incidence_prop + deaths_7dav_cumulative_prop + 
##     deaths_7dav_incidence_prop + EmergDec + GathRestrict + BarRestrict + 
##     NEBusinessClose + RestaurantRestrict + StayAtHome + PublicMask + 
##     OtherBusinessClose + BusinessMask + SchoolMask + Quarantine, 
##     data = .)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -23.0944  -5.1593   0.2324   5.7993  17.7180 
## 
## Coefficients: (3 not defined because of singularities)
##                                  Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    117.500000   2.247027  52.291  < 2e-16 ***
## confirmed_7dav_cumulative_prop  -0.018595   0.002963  -6.276 1.18e-09 ***
## confirmed_7dav_incidence_prop   -0.030766   0.054320  -0.566 0.571549    
## deaths_7dav_cumulative_prop      0.216153   0.059619   3.626 0.000338 ***
## deaths_7dav_incidence_prop      -0.425245   1.085642  -0.392 0.695553    
## EmergDec1                      -10.500000   3.891964  -2.698 0.007366 ** 
## GathRestrict1                   -7.442737   4.351424  -1.710 0.088203 .  
## BarRestrict1                   -10.458628   3.778124  -2.768 0.005980 ** 
## NEBusinessClose1                       NA         NA      NA       NA    
## RestaurantRestrict1                    NA         NA      NA       NA    
## StayAtHome1                            NA         NA      NA       NA    
## PublicMask1                    -24.695148   2.710953  -9.109  < 2e-16 ***
## OtherBusinessClose1            -27.375176   2.416590 -11.328  < 2e-16 ***
## BusinessMask1                    9.681238   2.301508   4.206 3.41e-05 ***
## SchoolMask1                      4.260296   1.787051   2.384 0.017736 *  
## Quarantine1                     -8.319100   2.370120  -3.510 0.000515 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.408 on 306 degrees of freedom
## Multiple R-squared:  0.8898,	Adjusted R-squared:  0.8855 
## F-statistic:   206 on 12 and 306 DF,  p-value: < 2.2e-16
## 
## [1] "Colusa County"
## 
## Call:
## lm(formula = full_time_work_prop ~ confirmed_7dav_cumulative_prop + 
##     confirmed_7dav_incidence_prop + deaths_7dav_cumulative_prop + 
##     deaths_7dav_incidence_prop + EmergDec + GathRestrict + BarRestrict + 
##     NEBusinessClose + RestaurantRestrict + StayAtHome + PublicMask + 
##     OtherBusinessClose + BusinessMask + SchoolMask + Quarantine, 
##     data = .)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -23.1356  -5.1166   0.0042   5.4230  18.0986 
## 
## Coefficients: (3 not defined because of singularities)
##                                  Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    117.500000   2.235022  52.572  < 2e-16 ***
## confirmed_7dav_cumulative_prop  -0.015059   0.001982  -7.596 3.75e-13 ***
## confirmed_7dav_incidence_prop    0.102795   0.047414   2.168  0.03093 *  
## deaths_7dav_cumulative_prop      1.033475   0.213456   4.842 2.05e-06 ***
## deaths_7dav_incidence_prop      -5.469233   2.101311  -2.603  0.00970 ** 
## EmergDec1                      -10.500000   3.871172  -2.712  0.00706 ** 
## GathRestrict1                   -7.500000   4.328102  -1.733  0.08413 .  
## BarRestrict1                   -10.515328   3.757868  -2.798  0.00546 ** 
## NEBusinessClose1                       NA         NA      NA       NA    
## RestaurantRestrict1                    NA         NA      NA       NA    
## StayAtHome1                            NA         NA      NA       NA    
## PublicMask1                    -24.823418   2.696345  -9.206  < 2e-16 ***
## OtherBusinessClose1            -27.473178   2.403208 -11.432  < 2e-16 ***
## BusinessMask1                    9.796952   2.321554   4.220 3.22e-05 ***
## SchoolMask1                      5.056535   3.209073   1.576  0.11613    
## Quarantine1                     -3.013921   2.483268  -1.214  0.22580    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.363 on 306 degrees of freedom
## Multiple R-squared:  0.891,	Adjusted R-squared:  0.8867 
## F-statistic: 208.4 on 12 and 306 DF,  p-value: < 2.2e-16
## 
## [1] "Contra Costa County"
## 
## Call:
## lm(formula = full_time_work_prop ~ confirmed_7dav_cumulative_prop + 
##     confirmed_7dav_incidence_prop + deaths_7dav_cumulative_prop + 
##     deaths_7dav_incidence_prop + EmergDec + GathRestrict + BarRestrict + 
##     NEBusinessClose + RestaurantRestrict + StayAtHome + PublicMask + 
##     OtherBusinessClose + BusinessMask + SchoolMask + Quarantine, 
##     data = .)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -21.5136  -5.1280   0.5922   5.2672  19.5537 
## 
## Coefficients: (3 not defined because of singularities)
##                                  Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    117.500170   2.246547  52.303  < 2e-16 ***
## confirmed_7dav_cumulative_prop  -0.023922   0.004655  -5.139 4.93e-07 ***
## confirmed_7dav_incidence_prop   -0.168207   0.106582  -1.578  0.11556    
## deaths_7dav_cumulative_prop      2.058214   0.514318   4.002 7.89e-05 ***
## deaths_7dav_incidence_prop       1.510371   9.797831   0.154  0.87759    
## EmergDec1                      -10.481193   3.891141  -2.694  0.00746 ** 
## GathRestrict1                   -7.426873   4.350471  -1.707  0.08881 .  
## BarRestrict1                   -10.467393   3.780848  -2.769  0.00597 ** 
## NEBusinessClose1                       NA         NA      NA       NA    
## RestaurantRestrict1                    NA         NA      NA       NA    
## StayAtHome1                            NA         NA      NA       NA    
## PublicMask1                    -26.500185   2.802592  -9.456  < 2e-16 ***
## OtherBusinessClose1            -29.404877   2.472197 -11.894  < 2e-16 ***
## BusinessMask1                    7.824151   2.546803   3.072  0.00232 ** 
## SchoolMask1                      1.363272   2.565638   0.531  0.59556    
## Quarantine1                     -1.318300   2.859254  -0.461  0.64508    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.406 on 306 degrees of freedom
## Multiple R-squared:  0.8899,	Adjusted R-squared:  0.8856 
## F-statistic: 206.1 on 12 and 306 DF,  p-value: < 2.2e-16
## 
## [1] "Del Norte County"
## 
## Call:
## lm(formula = full_time_work_prop ~ confirmed_7dav_cumulative_prop + 
##     confirmed_7dav_incidence_prop + deaths_7dav_cumulative_prop + 
##     deaths_7dav_incidence_prop + EmergDec + GathRestrict + BarRestrict + 
##     NEBusinessClose + RestaurantRestrict + StayAtHome + PublicMask + 
##     OtherBusinessClose + BusinessMask + SchoolMask + Quarantine, 
##     data = .)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -22.6978  -5.1373   0.2823   5.5519  17.8887 
## 
## Coefficients: (3 not defined because of singularities)
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    117.50000    2.24578  52.320  < 2e-16 ***
## confirmed_7dav_cumulative_prop  -0.01656    0.00261  -6.344 8.03e-10 ***
## confirmed_7dav_incidence_prop   -0.01140    0.05468  -0.208  0.83506    
## deaths_7dav_cumulative_prop      0.33112    0.61676   0.537  0.59175    
## deaths_7dav_incidence_prop       2.52451    4.70055   0.537  0.59161    
## EmergDec1                      -10.50000    3.88980  -2.699  0.00733 ** 
## GathRestrict1                   -7.50000    4.34893  -1.725  0.08562 .  
## BarRestrict1                   -10.50000    3.77593  -2.781  0.00576 ** 
## NEBusinessClose1                      NA         NA      NA       NA    
## RestaurantRestrict1                   NA         NA      NA       NA    
## StayAtHome1                           NA         NA      NA       NA    
## PublicMask1                    -24.87439    2.70928  -9.181  < 2e-16 ***
## OtherBusinessClose1            -27.04815    2.42463 -11.156  < 2e-16 ***
## BusinessMask1                   11.28365    2.33889   4.824 2.22e-06 ***
## SchoolMask1                      3.23305    1.90766   1.695  0.09114 .  
## Quarantine1                      0.10731    2.72497   0.039  0.96861    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.403 on 306 degrees of freedom
## Multiple R-squared:   0.89,	Adjusted R-squared:  0.8856 
## F-statistic: 206.2 on 12 and 306 DF,  p-value: < 2.2e-16
## 
## [1] "El Dorado County"
## 
## Call:
## lm(formula = full_time_work_prop ~ confirmed_7dav_cumulative_prop + 
##     confirmed_7dav_incidence_prop + deaths_7dav_cumulative_prop + 
##     deaths_7dav_incidence_prop + EmergDec + GathRestrict + BarRestrict + 
##     NEBusinessClose + RestaurantRestrict + StayAtHome + PublicMask + 
##     OtherBusinessClose + BusinessMask + SchoolMask + Quarantine, 
##     data = .)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -22.9527  -4.8777   0.1801   5.6890  17.8900 
## 
## Coefficients: (3 not defined because of singularities)
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    117.50000    2.23614  52.546  < 2e-16 ***
## confirmed_7dav_cumulative_prop  -0.01186    0.00443  -2.678  0.00780 ** 
## confirmed_7dav_incidence_prop   -0.05664    0.08001  -0.708  0.47951    
## deaths_7dav_cumulative_prop      0.65026    1.34991   0.482  0.63036    
## deaths_7dav_incidence_prop     -16.67748    6.99258  -2.385  0.01769 *  
## EmergDec1                      -10.50000    3.87311  -2.711  0.00709 ** 
## GathRestrict1                   -7.50000    4.33027  -1.732  0.08428 .  
## BarRestrict1                   -10.44021    3.75993  -2.777  0.00583 ** 
## NEBusinessClose1                      NA         NA      NA       NA    
## RestaurantRestrict1                   NA         NA      NA       NA    
## StayAtHome1                           NA         NA      NA       NA    
## PublicMask1                    -24.81465    2.69850  -9.196  < 2e-16 ***
## OtherBusinessClose1            -27.32783    2.40513 -11.362  < 2e-16 ***
## BusinessMask1                    9.61839    2.29691   4.188 3.69e-05 ***
## SchoolMask1                      3.94419    1.71028   2.306  0.02177 *  
## Quarantine1                     -1.33267    2.90150  -0.459  0.64634    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.367 on 306 degrees of freedom
## Multiple R-squared:  0.8909,	Adjusted R-squared:  0.8866 
## F-statistic: 208.2 on 12 and 306 DF,  p-value: < 2.2e-16
## 
## [1] "Fresno County"
## 
## Call:
## lm(formula = full_time_work_prop ~ confirmed_7dav_cumulative_prop + 
##     confirmed_7dav_incidence_prop + deaths_7dav_cumulative_prop + 
##     deaths_7dav_incidence_prop + EmergDec + GathRestrict + BarRestrict + 
##     NEBusinessClose + RestaurantRestrict + StayAtHome + PublicMask + 
##     OtherBusinessClose + BusinessMask + SchoolMask + Quarantine, 
##     data = .)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -23.9112  -5.5715   0.4164   5.9229  18.1197 
## 
## Coefficients: (3 not defined because of singularities)
##                                  Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    117.500000   2.258253  52.031  < 2e-16 ***
## confirmed_7dav_cumulative_prop  -0.014059   0.002616  -5.374 1.53e-07 ***
## confirmed_7dav_incidence_prop   -0.036648   0.028943  -1.266 0.206395    
## deaths_7dav_cumulative_prop      0.796404   0.187914   4.238 2.99e-05 ***
## deaths_7dav_incidence_prop       4.779256   3.016964   1.584 0.114198    
## EmergDec1                      -10.499413   3.911410  -2.684 0.007664 ** 
## GathRestrict1                   -7.497931   4.373089  -1.715 0.087438 .  
## BarRestrict1                   -10.463480   3.796913  -2.756 0.006206 ** 
## NEBusinessClose1                       NA         NA      NA       NA    
## RestaurantRestrict1                    NA         NA      NA       NA    
## StayAtHome1                            NA         NA      NA       NA    
## PublicMask1                    -25.040507   2.725954  -9.186  < 2e-16 ***
## OtherBusinessClose1            -27.307231   2.433051 -11.223  < 2e-16 ***
## BusinessMask1                    9.330671   2.360713   3.952 9.61e-05 ***
## SchoolMask1                      7.944322   2.187911   3.631 0.000331 ***
## Quarantine1                     -6.766957   2.254849  -3.001 0.002912 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.45 on 306 degrees of freedom
## Multiple R-squared:  0.8887,	Adjusted R-squared:  0.8844 
## F-statistic: 203.7 on 12 and 306 DF,  p-value: < 2.2e-16
## 
## [1] "Glenn County"
## 
## Call:
## lm(formula = full_time_work_prop ~ confirmed_7dav_cumulative_prop + 
##     confirmed_7dav_incidence_prop + deaths_7dav_cumulative_prop + 
##     deaths_7dav_incidence_prop + EmergDec + GathRestrict + BarRestrict + 
##     NEBusinessClose + RestaurantRestrict + StayAtHome + PublicMask + 
##     OtherBusinessClose + BusinessMask + SchoolMask + Quarantine, 
##     data = .)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -24.5985  -6.1979   0.6232   6.1879  20.0108 
## 
## Coefficients: (3 not defined because of singularities)
##                                  Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    117.500000   2.339418  50.226  < 2e-16 ***
## confirmed_7dav_cumulative_prop  -0.006592   0.001297  -5.083 6.48e-07 ***
## confirmed_7dav_incidence_prop   -0.274479   0.060912  -4.506 9.41e-06 ***
## deaths_7dav_cumulative_prop      0.250120   0.067462   3.708 0.000248 ***
## deaths_7dav_incidence_prop       2.297673   0.518686   4.430 1.31e-05 ***
## EmergDec1                      -10.500000   4.051991  -2.591 0.010019 *  
## GathRestrict1                   -7.500000   4.530263  -1.656 0.098842 .  
## BarRestrict1                   -10.397502   3.933427  -2.643 0.008631 ** 
## NEBusinessClose1                       NA         NA      NA       NA    
## RestaurantRestrict1                    NA         NA      NA       NA    
## StayAtHome1                            NA         NA      NA       NA    
## PublicMask1                    -24.912918   2.822219  -8.827  < 2e-16 ***
## OtherBusinessClose1            -27.120460   2.516193 -10.778  < 2e-16 ***
## BusinessMask1                   11.406794   2.475062   4.609 5.96e-06 ***
## SchoolMask1                      9.647573   2.354160   4.098 5.34e-05 ***
## Quarantine1                     -3.343003   2.828590  -1.182 0.238178    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.753 on 306 degrees of freedom
## Multiple R-squared:  0.8806,	Adjusted R-squared:  0.8759 
## F-statistic:   188 on 12 and 306 DF,  p-value: < 2.2e-16
## 
## [1] "Humboldt County"
## 
## Call:
## lm(formula = full_time_work_prop ~ confirmed_7dav_cumulative_prop + 
##     confirmed_7dav_incidence_prop + deaths_7dav_cumulative_prop + 
##     deaths_7dav_incidence_prop + EmergDec + GathRestrict + BarRestrict + 
##     NEBusinessClose + RestaurantRestrict + StayAtHome + PublicMask + 
##     OtherBusinessClose + BusinessMask + SchoolMask + Quarantine, 
##     data = .)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -19.9525  -5.8729   0.3846   5.7316  18.3756 
## 
## Coefficients: (3 not defined because of singularities)
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    117.50818    2.24813  52.269  < 2e-16 ***
## confirmed_7dav_cumulative_prop  -0.02775    0.01210  -2.293  0.02251 *  
## confirmed_7dav_incidence_prop    0.12225    0.24297   0.503  0.61521    
## deaths_7dav_cumulative_prop      0.83983    0.79552   1.056  0.29194    
## deaths_7dav_incidence_prop     -28.06979    6.90377  -4.066 6.09e-05 ***
## EmergDec1                      -10.48771    3.89386  -2.693  0.00746 ** 
## GathRestrict1                   -7.50000    4.35344  -1.723  0.08594 .  
## BarRestrict1                   -10.51668    3.78711  -2.777  0.00583 ** 
## NEBusinessClose1                      NA         NA      NA       NA    
## RestaurantRestrict1                   NA         NA      NA       NA    
## StayAtHome1                           NA         NA      NA       NA    
## PublicMask1                    -24.09426    2.74613  -8.774  < 2e-16 ***
## OtherBusinessClose1            -25.20894    2.49095 -10.120  < 2e-16 ***
## BusinessMask1                    6.51401    2.73403   2.383  0.01780 *  
## SchoolMask1                      3.79559    2.03430   1.866  0.06303 .  
## Quarantine1                     -5.74013    3.17126  -1.810  0.07127 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.412 on 306 degrees of freedom
## Multiple R-squared:  0.8897,	Adjusted R-squared:  0.8854 
## F-statistic: 205.7 on 12 and 306 DF,  p-value: < 2.2e-16
## 
## [1] "Imperial County"
## 
## Call:
## lm(formula = full_time_work_prop ~ confirmed_7dav_cumulative_prop + 
##     confirmed_7dav_incidence_prop + deaths_7dav_cumulative_prop + 
##     deaths_7dav_incidence_prop + EmergDec + GathRestrict + BarRestrict + 
##     NEBusinessClose + RestaurantRestrict + StayAtHome + PublicMask + 
##     OtherBusinessClose + BusinessMask + SchoolMask + Quarantine, 
##     data = .)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -25.1365  -4.7099   0.8258   5.3616  18.5469 
## 
## Coefficients: (3 not defined because of singularities)
##                                  Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    117.500000   2.205007  53.288  < 2e-16 ***
## confirmed_7dav_cumulative_prop  -0.009666   0.001108  -8.721  < 2e-16 ***
## confirmed_7dav_incidence_prop   -0.014429   0.029008  -0.497 0.619257    
## deaths_7dav_cumulative_prop      0.387870   0.051417   7.544 5.26e-13 ***
## deaths_7dav_incidence_prop       5.687579   0.998362   5.697 2.87e-08 ***
## EmergDec1                      -10.500000   3.819184  -2.749 0.006328 ** 
## GathRestrict1                   -7.497529   4.269978  -1.756 0.080111 .  
## BarRestrict1                   -10.427317   3.707492  -2.812 0.005234 ** 
## NEBusinessClose1                       NA         NA      NA       NA    
## RestaurantRestrict1                    NA         NA      NA       NA    
## StayAtHome1                            NA         NA      NA       NA    
## PublicMask1                    -25.888170   2.670068  -9.696  < 2e-16 ***
## OtherBusinessClose1            -27.754687   2.429928 -11.422  < 2e-16 ***
## BusinessMask1                   13.939284   2.800679   4.977 1.08e-06 ***
## SchoolMask1                    -11.266421   3.378657  -3.335 0.000959 ***
## Quarantine1                     -0.397511   2.764633  -0.144 0.885765    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.25 on 306 degrees of freedom
## Multiple R-squared:  0.8939,	Adjusted R-squared:  0.8897 
## F-statistic: 214.9 on 12 and 306 DF,  p-value: < 2.2e-16
## 
## [1] "Inyo County"
## 
## Call:
## lm(formula = full_time_work_prop ~ confirmed_7dav_cumulative_prop + 
##     confirmed_7dav_incidence_prop + deaths_7dav_cumulative_prop + 
##     deaths_7dav_incidence_prop + EmergDec + GathRestrict + BarRestrict + 
##     NEBusinessClose + RestaurantRestrict + StayAtHome + PublicMask + 
##     OtherBusinessClose + BusinessMask + SchoolMask + Quarantine, 
##     data = .)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -23.097  -4.722   0.000   5.718  16.456 
## 
## Coefficients: (3 not defined because of singularities)
##                                  Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    117.500000   2.217533  52.987  < 2e-16 ***
## confirmed_7dav_cumulative_prop  -0.024735   0.003387  -7.302 2.47e-12 ***
## confirmed_7dav_incidence_prop    0.179689   0.072791   2.469 0.014112 *  
## deaths_7dav_cumulative_prop      0.266938   0.050867   5.248 2.88e-07 ***
## deaths_7dav_incidence_prop      -2.601518   0.703595  -3.697 0.000258 ***
## EmergDec1                      -10.500000   3.840879  -2.734 0.006626 ** 
## GathRestrict1                   -7.500000   4.294233  -1.747 0.081723 .  
## BarRestrict1                   -10.756018   3.732654  -2.882 0.004236 ** 
## NEBusinessClose1                       NA         NA      NA       NA    
## RestaurantRestrict1                    NA         NA      NA       NA    
## StayAtHome1                            NA         NA      NA       NA    
## PublicMask1                    -23.062582   2.691559  -8.568 5.25e-16 ***
## OtherBusinessClose1            -28.055933   2.387698 -11.750  < 2e-16 ***
## BusinessMask1                    9.494225   2.271070   4.181 3.80e-05 ***
## SchoolMask1                      3.869412   1.927664   2.007 0.045596 *  
## Quarantine1                     -3.940483   2.312177  -1.704 0.089353 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.297 on 306 degrees of freedom
## Multiple R-squared:  0.8927,	Adjusted R-squared:  0.8885 
## F-statistic: 212.2 on 12 and 306 DF,  p-value: < 2.2e-16
## 
## [1] "Kern County"
## 
## Call:
## lm(formula = full_time_work_prop ~ confirmed_7dav_cumulative_prop + 
##     confirmed_7dav_incidence_prop + deaths_7dav_cumulative_prop + 
##     deaths_7dav_incidence_prop + EmergDec + GathRestrict + BarRestrict + 
##     NEBusinessClose + RestaurantRestrict + StayAtHome + PublicMask + 
##     OtherBusinessClose + BusinessMask + SchoolMask + Quarantine, 
##     data = .)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -22.825  -5.769   0.553   6.023  17.673 
## 
## Coefficients: (3 not defined because of singularities)
##                                  Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    117.500000   2.294380  51.212  < 2e-16 ***
## confirmed_7dav_cumulative_prop  -0.009075   0.001463  -6.203 1.79e-09 ***
## confirmed_7dav_incidence_prop   -0.036724   0.028622  -1.283 0.200442    
## deaths_7dav_cumulative_prop      0.539857   0.155820   3.465 0.000607 ***
## deaths_7dav_incidence_prop       1.267862   4.792523   0.265 0.791534    
## EmergDec1                      -10.500000   3.973983  -2.642 0.008661 ** 
## GathRestrict1                   -7.499637   4.443048  -1.688 0.092440 .  
## BarRestrict1                   -10.481618   3.857879  -2.717 0.006964 ** 
## NEBusinessClose1                       NA         NA      NA       NA    
## RestaurantRestrict1                    NA         NA      NA       NA    
## StayAtHome1                            NA         NA      NA       NA    
## PublicMask1                    -24.615082   2.771224  -8.882  < 2e-16 ***
## OtherBusinessClose1            -27.676246   2.519558 -10.985  < 2e-16 ***
## BusinessMask1                    9.114798   2.442983   3.731 0.000227 ***
## SchoolMask1                      9.714268   3.022712   3.214 0.001450 ** 
## Quarantine1                     -6.578732   2.677344  -2.457 0.014558 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.585 on 306 degrees of freedom
## Multiple R-squared:  0.8851,	Adjusted R-squared:  0.8806 
## F-statistic: 196.5 on 12 and 306 DF,  p-value: < 2.2e-16
## 
## [1] "Kings County"
## 
## Call:
## lm(formula = full_time_work_prop ~ confirmed_7dav_cumulative_prop + 
##     confirmed_7dav_incidence_prop + deaths_7dav_cumulative_prop + 
##     deaths_7dav_incidence_prop + EmergDec + GathRestrict + BarRestrict + 
##     NEBusinessClose + RestaurantRestrict + StayAtHome + PublicMask + 
##     OtherBusinessClose + BusinessMask + SchoolMask + Quarantine, 
##     data = .)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -21.769  -5.281  -0.099   5.298  21.121 
## 
## Coefficients: (3 not defined because of singularities)
##                                  Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                     1.175e+02  2.223e+00  52.860  < 2e-16 ***
## confirmed_7dav_cumulative_prop -8.822e-03  9.929e-04  -8.886  < 2e-16 ***
## confirmed_7dav_incidence_prop  -8.001e-03  3.116e-02  -0.257 0.797508    
## deaths_7dav_cumulative_prop     7.649e-01  1.112e-01   6.880 3.39e-11 ***
## deaths_7dav_incidence_prop      1.354e+00  9.156e-01   1.479 0.140189    
## EmergDec1                      -1.050e+01  3.850e+00  -2.727 0.006756 ** 
## GathRestrict1                  -7.500e+00  4.304e+00  -1.742 0.082450 .  
## BarRestrict1                   -1.050e+01  3.737e+00  -2.809 0.005294 ** 
## NEBusinessClose1                       NA         NA      NA       NA    
## RestaurantRestrict1                    NA         NA      NA       NA    
## StayAtHome1                            NA         NA      NA       NA    
## PublicMask1                    -2.504e+01  2.684e+00  -9.330  < 2e-16 ***
## OtherBusinessClose1            -2.605e+01  2.408e+00 -10.817  < 2e-16 ***
## BusinessMask1                   9.705e+00  2.474e+00   3.923 0.000108 ***
## SchoolMask1                    -3.416e+00  2.746e+00  -1.244 0.214347    
## Quarantine1                     5.788e+00  3.280e+00   1.765 0.078629 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.317 on 306 degrees of freedom
## Multiple R-squared:  0.8922,	Adjusted R-squared:  0.888 
## F-statistic:   211 on 12 and 306 DF,  p-value: < 2.2e-16
## 
## [1] "Lake County"
## 
## Call:
## lm(formula = full_time_work_prop ~ confirmed_7dav_cumulative_prop + 
##     confirmed_7dav_incidence_prop + deaths_7dav_cumulative_prop + 
##     deaths_7dav_incidence_prop + EmergDec + GathRestrict + BarRestrict + 
##     NEBusinessClose + RestaurantRestrict + StayAtHome + PublicMask + 
##     OtherBusinessClose + BusinessMask + SchoolMask + Quarantine, 
##     data = .)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -23.1621  -5.0750   0.0582   5.8352  17.8505 
## 
## Coefficients: (3 not defined because of singularities)
##                                  Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    117.500000   2.196974  53.483  < 2e-16 ***
## confirmed_7dav_cumulative_prop  -0.033119   0.003917  -8.455 1.15e-15 ***
## confirmed_7dav_incidence_prop    0.244537   0.083924   2.914  0.00383 ** 
## deaths_7dav_cumulative_prop      1.103519   0.160456   6.877 3.44e-11 ***
## deaths_7dav_incidence_prop       5.451341   3.253644   1.675  0.09487 .  
## EmergDec1                      -10.500000   3.805270  -2.759  0.00614 ** 
## GathRestrict1                   -7.500000   4.254421  -1.763  0.07892 .  
## BarRestrict1                   -10.500000   3.693872  -2.843  0.00478 ** 
## NEBusinessClose1                       NA         NA      NA       NA    
## RestaurantRestrict1                    NA         NA      NA       NA    
## StayAtHome1                            NA         NA      NA       NA    
## PublicMask1                    -24.850518   2.650412  -9.376  < 2e-16 ***
## OtherBusinessClose1            -27.277845   2.362411 -11.547  < 2e-16 ***
## BusinessMask1                    9.750132   2.255408   4.323 2.08e-05 ***
## SchoolMask1                      3.252894   1.766910   1.841  0.06659 .  
## Quarantine1                     -7.956131   2.414969  -3.295  0.00110 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.22 on 306 degrees of freedom
## Multiple R-squared:  0.8947,	Adjusted R-squared:  0.8906 
## F-statistic: 216.6 on 12 and 306 DF,  p-value: < 2.2e-16
## 
## [1] "Lassen County"
## 
## Call:
## lm(formula = full_time_work_prop ~ confirmed_7dav_cumulative_prop + 
##     confirmed_7dav_incidence_prop + deaths_7dav_cumulative_prop + 
##     deaths_7dav_incidence_prop + EmergDec + GathRestrict + BarRestrict + 
##     NEBusinessClose + RestaurantRestrict + StayAtHome + PublicMask + 
##     OtherBusinessClose + BusinessMask + SchoolMask + Quarantine, 
##     data = .)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -22.4783  -4.7620   0.1662   5.1107  18.0000 
## 
## Coefficients: (3 not defined because of singularities)
##                                  Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                     1.175e+02  2.204e+00  53.317  < 2e-16 ***
## confirmed_7dav_cumulative_prop -8.293e-04  8.246e-04  -1.006  0.31539    
## confirmed_7dav_incidence_prop   4.240e-03  9.260e-03   0.458  0.64738    
## deaths_7dav_cumulative_prop    -9.451e-01  3.578e-01  -2.642  0.00867 ** 
## deaths_7dav_incidence_prop      4.312e-02  2.656e+00   0.016  0.98706    
## EmergDec1                      -1.050e+01  3.817e+00  -2.751  0.00630 ** 
## GathRestrict1                  -7.500e+00  4.268e+00  -1.757  0.07985 .  
## BarRestrict1                   -1.050e+01  3.705e+00  -2.834  0.00491 ** 
## NEBusinessClose1                       NA         NA      NA       NA    
## RestaurantRestrict1                    NA         NA      NA       NA    
## StayAtHome1                            NA         NA      NA       NA    
## PublicMask1                    -2.500e+01  2.659e+00  -9.403  < 2e-16 ***
## OtherBusinessClose1            -2.750e+01  2.370e+00 -11.605  < 2e-16 ***
## BusinessMask1                   8.990e+00  2.255e+00   3.986 8.39e-05 ***
## SchoolMask1                     1.372e+00  1.873e+00   0.733  0.46426    
## Quarantine1                    -8.545e-01  2.912e+00  -0.293  0.76940    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.246 on 306 degrees of freedom
## Multiple R-squared:  0.894,	Adjusted R-squared:  0.8899 
## F-statistic: 215.1 on 12 and 306 DF,  p-value: < 2.2e-16
## 
## [1] "Los Angeles County"
## 
## Call:
## lm(formula = full_time_work_prop ~ confirmed_7dav_cumulative_prop + 
##     confirmed_7dav_incidence_prop + deaths_7dav_cumulative_prop + 
##     deaths_7dav_incidence_prop + EmergDec + GathRestrict + BarRestrict + 
##     NEBusinessClose + RestaurantRestrict + StayAtHome + PublicMask + 
##     OtherBusinessClose + BusinessMask + SchoolMask + Quarantine, 
##     data = .)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -24.1281  -5.1676   0.5595   5.3928  16.4080 
## 
## Coefficients: (3 not defined because of singularities)
##                                  Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    117.500021   2.155564  54.510  < 2e-16 ***
## confirmed_7dav_cumulative_prop   0.001425   0.002570   0.554  0.57968    
## confirmed_7dav_incidence_prop   -0.059347   0.046719  -1.270  0.20495    
## deaths_7dav_cumulative_prop     -0.311353   0.170584  -1.825  0.06894 .  
## deaths_7dav_incidence_prop     -22.331285   4.279097  -5.219 3.33e-07 ***
## EmergDec1                      -10.494251   3.733547  -2.811  0.00526 ** 
## GathRestrict1                   -7.469011   4.174236  -1.789  0.07455 .  
## BarRestrict1                    -9.747940   3.626407  -2.688  0.00758 ** 
## NEBusinessClose1                       NA         NA      NA       NA    
## RestaurantRestrict1                    NA         NA      NA       NA    
## StayAtHome1                            NA         NA      NA       NA    
## PublicMask1                    -16.087613   3.148062  -5.110 5.67e-07 ***
## OtherBusinessClose1            -22.590945   2.839621  -7.956 3.49e-14 ***
## BusinessMask1                   11.296051   2.532303   4.461 1.15e-05 ***
## SchoolMask1                      4.216830   2.353425   1.792  0.07416 .  
## Quarantine1                     -4.556063   2.541144  -1.793  0.07397 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.065 on 306 degrees of freedom
## Multiple R-squared:  0.8986,	Adjusted R-squared:  0.8946 
## F-statistic:   226 on 12 and 306 DF,  p-value: < 2.2e-16
## 
## [1] "Madera County"
## 
## Call:
## lm(formula = full_time_work_prop ~ confirmed_7dav_cumulative_prop + 
##     confirmed_7dav_incidence_prop + deaths_7dav_cumulative_prop + 
##     deaths_7dav_incidence_prop + EmergDec + GathRestrict + BarRestrict + 
##     NEBusinessClose + RestaurantRestrict + StayAtHome + PublicMask + 
##     OtherBusinessClose + BusinessMask + SchoolMask + Quarantine, 
##     data = .)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -24.5578  -4.5463   0.4121   4.9230  20.0872 
## 
## Coefficients: (3 not defined because of singularities)
##                                  Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    117.500000   2.177170  53.969  < 2e-16 ***
## confirmed_7dav_cumulative_prop  -0.012574   0.003095  -4.062 6.18e-05 ***
## confirmed_7dav_incidence_prop   -0.189209   0.041388  -4.572 7.03e-06 ***
## deaths_7dav_cumulative_prop      0.823667   0.222546   3.701 0.000255 ***
## deaths_7dav_incidence_prop       6.574545   2.062261   3.188 0.001581 ** 
## EmergDec1                      -10.485281   3.770970  -2.781 0.005763 ** 
## GathRestrict1                   -7.500284   4.216072  -1.779 0.076237 .  
## BarRestrict1                   -10.791245   3.661648  -2.947 0.003454 ** 
## NEBusinessClose1                       NA         NA      NA       NA    
## RestaurantRestrict1                    NA         NA      NA       NA    
## StayAtHome1                            NA         NA      NA       NA    
## PublicMask1                    -25.412165   2.633507  -9.650  < 2e-16 ***
## OtherBusinessClose1            -27.099081   2.341512 -11.573  < 2e-16 ***
## BusinessMask1                   11.454225   2.251615   5.087 6.35e-07 ***
## SchoolMask1                      0.686939   2.457232   0.280 0.780005    
## Quarantine1                     -5.313054   2.349612  -2.261 0.024446 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.146 on 306 degrees of freedom
## Multiple R-squared:  0.8966,	Adjusted R-squared:  0.8925 
## F-statistic:   221 on 12 and 306 DF,  p-value: < 2.2e-16
## 
## [1] "Marin County"
## 
## Call:
## lm(formula = full_time_work_prop ~ confirmed_7dav_cumulative_prop + 
##     confirmed_7dav_incidence_prop + deaths_7dav_cumulative_prop + 
##     deaths_7dav_incidence_prop + EmergDec + GathRestrict + BarRestrict + 
##     NEBusinessClose + RestaurantRestrict + StayAtHome + PublicMask + 
##     OtherBusinessClose + BusinessMask + SchoolMask + Quarantine, 
##     data = .)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -23.0308  -6.1572   0.8058   6.6612  19.1988 
## 
## Coefficients: (3 not defined because of singularities)
##                                  Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    117.500000   2.466205  47.644  < 2e-16 ***
## confirmed_7dav_cumulative_prop  -0.009788   0.003496  -2.799  0.00545 ** 
## confirmed_7dav_incidence_prop    0.112586   0.033348   3.376  0.00083 ***
## deaths_7dav_cumulative_prop      0.517258   0.210934   2.452  0.01476 *  
## deaths_7dav_incidence_prop      -1.552773   2.920907  -0.532  0.59538    
## EmergDec1                      -10.500000   4.271592  -2.458  0.01452 *  
## GathRestrict1                   -7.542285   4.775808  -1.579  0.11531    
## BarRestrict1                   -10.513091   4.152002  -2.532  0.01184 *  
## NEBusinessClose1                       NA         NA      NA       NA    
## RestaurantRestrict1                    NA         NA      NA       NA    
## StayAtHome1                            NA         NA      NA       NA    
## PublicMask1                    -26.255409   3.052482  -8.601 4.17e-16 ***
## OtherBusinessClose1            -28.165940   2.668184 -10.556  < 2e-16 ***
## BusinessMask1                    9.880753   2.701553   3.657  0.00030 ***
## SchoolMask1                      2.120793   3.476201   0.610  0.54226    
## Quarantine1                    -17.513016   2.035369  -8.604 4.08e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 9.228 on 306 degrees of freedom
## Multiple R-squared:  0.8673,	Adjusted R-squared:  0.8621 
## F-statistic: 166.6 on 12 and 306 DF,  p-value: < 2.2e-16
## 
## [1] "Mariposa County"
## 
## Call:
## lm(formula = full_time_work_prop ~ confirmed_7dav_cumulative_prop + 
##     confirmed_7dav_incidence_prop + deaths_7dav_cumulative_prop + 
##     deaths_7dav_incidence_prop + EmergDec + GathRestrict + BarRestrict + 
##     NEBusinessClose + RestaurantRestrict + StayAtHome + PublicMask + 
##     OtherBusinessClose + BusinessMask + SchoolMask + Quarantine, 
##     data = .)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -23.1793  -5.7897   0.4628   5.4846  17.2781 
## 
## Coefficients: (3 not defined because of singularities)
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    117.50000    2.29935  51.101  < 2e-16 ***
## confirmed_7dav_cumulative_prop  -0.01580    0.01017  -1.553  0.12139    
## confirmed_7dav_incidence_prop   -0.24336    0.12396  -1.963  0.05052 .  
## deaths_7dav_cumulative_prop     -0.89281    0.44792  -1.993  0.04712 *  
## deaths_7dav_incidence_prop       0.05418    2.33921   0.023  0.98153    
## EmergDec1                      -10.50000    3.98259  -2.636  0.00880 ** 
## GathRestrict1                   -7.50000    4.45267  -1.684  0.09313 .  
## BarRestrict1                   -10.50000    3.86600  -2.716  0.00698 ** 
## NEBusinessClose1                      NA         NA      NA       NA    
## RestaurantRestrict1                   NA         NA      NA       NA    
## StayAtHome1                           NA         NA      NA       NA    
## PublicMask1                    -24.27810    2.78503  -8.717  < 2e-16 ***
## OtherBusinessClose1            -24.19287    2.67983  -9.028  < 2e-16 ***
## BusinessMask1                   12.49828    2.74250   4.557 7.50e-06 ***
## SchoolMask1                      8.38518    1.91140   4.387 1.58e-05 ***
## Quarantine1                     -1.69745    2.91604  -0.582  0.56092    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.603 on 306 degrees of freedom
## Multiple R-squared:  0.8846,	Adjusted R-squared:  0.8801 
## F-statistic: 195.5 on 12 and 306 DF,  p-value: < 2.2e-16
## 
## [1] "Mendocino County"
## 
## Call:
## lm(formula = full_time_work_prop ~ confirmed_7dav_cumulative_prop + 
##     confirmed_7dav_incidence_prop + deaths_7dav_cumulative_prop + 
##     deaths_7dav_incidence_prop + EmergDec + GathRestrict + BarRestrict + 
##     NEBusinessClose + RestaurantRestrict + StayAtHome + PublicMask + 
##     OtherBusinessClose + BusinessMask + SchoolMask + Quarantine, 
##     data = .)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -23.6090  -5.4851   0.4589   5.1872  17.9018 
## 
## Coefficients: (3 not defined because of singularities)
##                                  Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    117.500000   2.269890  51.765  < 2e-16 ***
## confirmed_7dav_cumulative_prop  -0.029169   0.003648  -7.995 2.68e-14 ***
## confirmed_7dav_incidence_prop   -0.020132   0.106309  -0.189  0.84993    
## deaths_7dav_cumulative_prop      1.498231   0.258402   5.798 1.67e-08 ***
## deaths_7dav_incidence_prop      -4.981233   3.508573  -1.420  0.15670    
## EmergDec1                      -10.500000   3.931565  -2.671  0.00797 ** 
## GathRestrict1                   -7.497370   4.395626  -1.706  0.08909 .  
## BarRestrict1                   -10.442589   3.816541  -2.736  0.00658 ** 
## NEBusinessClose1                       NA         NA      NA       NA    
## RestaurantRestrict1                    NA         NA      NA       NA    
## StayAtHome1                            NA         NA      NA       NA    
## PublicMask1                    -24.835172   2.738379  -9.069  < 2e-16 ***
## OtherBusinessClose1            -27.245965   2.440932 -11.162  < 2e-16 ***
## BusinessMask1                   10.567834   2.340083   4.516 9.01e-06 ***
## SchoolMask1                     -2.851350   3.576180  -0.797  0.42589    
## Quarantine1                      0.860783   3.024635   0.285  0.77615    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.493 on 306 degrees of freedom
## Multiple R-squared:  0.8876,	Adjusted R-squared:  0.8832 
## F-statistic: 201.3 on 12 and 306 DF,  p-value: < 2.2e-16
## 
## [1] "Merced County"
## 
## Call:
## lm(formula = full_time_work_prop ~ confirmed_7dav_cumulative_prop + 
##     confirmed_7dav_incidence_prop + deaths_7dav_cumulative_prop + 
##     deaths_7dav_incidence_prop + EmergDec + GathRestrict + BarRestrict + 
##     NEBusinessClose + RestaurantRestrict + StayAtHome + PublicMask + 
##     OtherBusinessClose + BusinessMask + SchoolMask + Quarantine, 
##     data = .)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -24.6477  -4.9813  -0.2871   5.9225  18.5339 
## 
## Coefficients: (3 not defined because of singularities)
##                                  Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    117.500000   2.256134  52.080  < 2e-16 ***
## confirmed_7dav_cumulative_prop  -0.022995   0.003332  -6.901 2.98e-11 ***
## confirmed_7dav_incidence_prop    0.094005   0.044964   2.091 0.037382 *  
## deaths_7dav_cumulative_prop      1.225572   0.211048   5.807 1.59e-08 ***
## deaths_7dav_incidence_prop      -0.465448   2.903916  -0.160 0.872764    
## EmergDec1                      -10.500000   3.907738  -2.687 0.007604 ** 
## GathRestrict1                   -7.500000   4.368984  -1.717 0.087056 .  
## BarRestrict1                   -10.499630   3.793351  -2.768 0.005986 ** 
## NEBusinessClose1                       NA         NA      NA       NA    
## RestaurantRestrict1                    NA         NA      NA       NA    
## StayAtHome1                            NA         NA      NA       NA    
## PublicMask1                    -25.511848   2.726219  -9.358  < 2e-16 ***
## OtherBusinessClose1            -27.591627   2.428874 -11.360  < 2e-16 ***
## BusinessMask1                   10.877930   2.364809   4.600 6.20e-06 ***
## SchoolMask1                      8.835223   2.581067   3.423 0.000703 ***
## Quarantine1                     -9.015410   2.496820  -3.611 0.000357 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.442 on 306 degrees of freedom
## Multiple R-squared:  0.8889,	Adjusted R-squared:  0.8846 
## F-statistic: 204.1 on 12 and 306 DF,  p-value: < 2.2e-16
## 
## [1] "Modoc County"
## 
## Call:
## lm(formula = full_time_work_prop ~ confirmed_7dav_cumulative_prop + 
##     confirmed_7dav_incidence_prop + deaths_7dav_cumulative_prop + 
##     deaths_7dav_incidence_prop + EmergDec + GathRestrict + BarRestrict + 
##     NEBusinessClose + RestaurantRestrict + StayAtHome + PublicMask + 
##     OtherBusinessClose + BusinessMask + SchoolMask + Quarantine, 
##     data = .)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -22.4038  -5.3536   0.5962   6.4135  18.0000 
## 
## Coefficients: (3 not defined because of singularities)
##                                  Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    117.500000   2.281086  51.511  < 2e-16 ***
## confirmed_7dav_cumulative_prop  -0.012648   0.002373  -5.330 1.91e-07 ***
## confirmed_7dav_incidence_prop    0.186184   0.041240   4.515 9.06e-06 ***
## deaths_7dav_cumulative_prop      0.872474   1.694508   0.515 0.607006    
## deaths_7dav_incidence_prop     -11.845515   5.760161  -2.056 0.040586 *  
## EmergDec1                      -10.500000   3.950956  -2.658 0.008284 ** 
## GathRestrict1                   -7.500000   4.417304  -1.698 0.090549 .  
## BarRestrict1                   -10.500000   3.835294  -2.738 0.006549 ** 
## NEBusinessClose1                       NA         NA      NA       NA    
## RestaurantRestrict1                    NA         NA      NA       NA    
## StayAtHome1                            NA         NA      NA       NA    
## PublicMask1                    -25.000000   2.751808  -9.085  < 2e-16 ***
## OtherBusinessClose1            -27.500000   2.452731 -11.212  < 2e-16 ***
## BusinessMask1                    8.903846   2.334085   3.815 0.000165 ***
## SchoolMask1                      0.303663   1.503146   0.202 0.840037    
## Quarantine1                     -1.444968   3.731201  -0.387 0.698828    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.535 on 306 degrees of freedom
## Multiple R-squared:  0.8865,	Adjusted R-squared:  0.882 
## F-statistic: 199.1 on 12 and 306 DF,  p-value: < 2.2e-16
## 
## [1] "Mono County"
## 
## Call:
## lm(formula = full_time_work_prop ~ confirmed_7dav_cumulative_prop + 
##     confirmed_7dav_incidence_prop + deaths_7dav_cumulative_prop + 
##     deaths_7dav_incidence_prop + EmergDec + GathRestrict + BarRestrict + 
##     NEBusinessClose + RestaurantRestrict + StayAtHome + PublicMask + 
##     OtherBusinessClose + BusinessMask + SchoolMask + Quarantine, 
##     data = .)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -22.495  -6.789   0.000   6.539  17.092 
## 
## Coefficients: (3 not defined because of singularities)
##                                  Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    117.500000   2.369034  49.598  < 2e-16 ***
## confirmed_7dav_cumulative_prop  -0.002730   0.001543  -1.770 0.077742 .  
## confirmed_7dav_incidence_prop   -0.009909   0.009580  -1.034 0.301774    
## deaths_7dav_cumulative_prop     -0.885290   0.276126  -3.206 0.001488 ** 
## deaths_7dav_incidence_prop       1.465799   1.951266   0.751 0.453107    
## EmergDec1                      -10.500000   4.103288  -2.559 0.010981 *  
## GathRestrict1                   -7.500000   4.587615  -1.635 0.103112    
## BarRestrict1                   -10.386464   3.994715  -2.600 0.009773 ** 
## NEBusinessClose1                       NA         NA      NA       NA    
## RestaurantRestrict1                    NA         NA      NA       NA    
## StayAtHome1                            NA         NA      NA       NA    
## PublicMask1                    -18.967750   3.293645  -5.759 2.06e-08 ***
## OtherBusinessClose1            -26.885421   2.562616 -10.491  < 2e-16 ***
## BusinessMask1                    9.057451   2.425639   3.734 0.000225 ***
## SchoolMask1                      6.006014   1.828689   3.284 0.001141 ** 
## Quarantine1                     -2.938623   3.202450  -0.918 0.359542    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.864 on 306 degrees of freedom
## Multiple R-squared:  0.8775,	Adjusted R-squared:  0.8727 
## F-statistic: 182.7 on 12 and 306 DF,  p-value: < 2.2e-16
## 
## [1] "Monterey County"
## 
## Call:
## lm(formula = full_time_work_prop ~ confirmed_7dav_cumulative_prop + 
##     confirmed_7dav_incidence_prop + deaths_7dav_cumulative_prop + 
##     deaths_7dav_incidence_prop + EmergDec + GathRestrict + BarRestrict + 
##     NEBusinessClose + RestaurantRestrict + StayAtHome + PublicMask + 
##     OtherBusinessClose + BusinessMask + SchoolMask + Quarantine, 
##     data = .)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -24.1698  -5.6601   0.5162   5.7562  18.3763 
## 
## Coefficients: (3 not defined because of singularities)
##                                  Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    117.500000   2.280392  51.526  < 2e-16 ***
## confirmed_7dav_cumulative_prop  -0.014640   0.003936  -3.719 0.000238 ***
## confirmed_7dav_incidence_prop   -0.096359   0.028322  -3.402 0.000757 ***
## deaths_7dav_cumulative_prop      1.501394   0.513255   2.925 0.003699 ** 
## deaths_7dav_incidence_prop      -6.494095   5.816930  -1.116 0.265121    
## EmergDec1                      -10.500000   3.949755  -2.658 0.008264 ** 
## GathRestrict1                   -7.498053   4.415960  -1.698 0.090535 .  
## BarRestrict1                   -10.397612   3.835509  -2.711 0.007089 ** 
## NEBusinessClose1                       NA         NA      NA       NA    
## RestaurantRestrict1                    NA         NA      NA       NA    
## StayAtHome1                            NA         NA      NA       NA    
## PublicMask1                    -25.493374   2.762133  -9.230  < 2e-16 ***
## OtherBusinessClose1            -27.835713   2.461736 -11.307  < 2e-16 ***
## BusinessMask1                   11.010092   2.348575   4.688 4.16e-06 ***
## SchoolMask1                      7.741977   2.177103   3.556 0.000436 ***
## Quarantine1                     -8.097964   2.719640  -2.978 0.003137 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.532 on 306 degrees of freedom
## Multiple R-squared:  0.8865,	Adjusted R-squared:  0.8821 
## F-statistic: 199.2 on 12 and 306 DF,  p-value: < 2.2e-16
## 
## [1] "Napa County"
## 
## Call:
## lm(formula = full_time_work_prop ~ confirmed_7dav_cumulative_prop + 
##     confirmed_7dav_incidence_prop + deaths_7dav_cumulative_prop + 
##     deaths_7dav_incidence_prop + EmergDec + GathRestrict + BarRestrict + 
##     NEBusinessClose + RestaurantRestrict + StayAtHome + PublicMask + 
##     OtherBusinessClose + BusinessMask + SchoolMask + Quarantine, 
##     data = .)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -23.7709  -5.3654   0.2946   5.6609  19.6703 
## 
## Coefficients: (3 not defined because of singularities)
##                                  Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    117.500000   2.303183  51.016  < 2e-16 ***
## confirmed_7dav_cumulative_prop  -0.015159   0.004475  -3.387 0.000799 ***
## confirmed_7dav_incidence_prop   -0.062577   0.091640  -0.683 0.495214    
## deaths_7dav_cumulative_prop      1.447103   0.891085   1.624 0.105410    
## deaths_7dav_incidence_prop      -5.838716   6.535609  -0.893 0.372361    
## EmergDec1                      -10.500000   3.989231  -2.632 0.008917 ** 
## GathRestrict1                   -7.500000   4.460095  -1.682 0.093671 .  
## BarRestrict1                   -10.408179   3.872743  -2.688 0.007592 ** 
## NEBusinessClose1                       NA         NA      NA       NA    
## RestaurantRestrict1                    NA         NA      NA       NA    
## StayAtHome1                            NA         NA      NA       NA    
## PublicMask1                    -26.243080   2.993897  -8.766  < 2e-16 ***
## OtherBusinessClose1            -27.963472   2.517805 -11.106  < 2e-16 ***
## BusinessMask1                   10.007978   2.403528   4.164 4.07e-05 ***
## SchoolMask1                      4.245376   2.903262   1.462 0.144691    
## Quarantine1                      0.277669   2.947554   0.094 0.925009    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.618 on 306 degrees of freedom
## Multiple R-squared:  0.8843,	Adjusted R-squared:  0.8797 
## F-statistic: 194.8 on 12 and 306 DF,  p-value: < 2.2e-16
## 
## [1] "Nevada County"
## 
## Call:
## lm(formula = full_time_work_prop ~ confirmed_7dav_cumulative_prop + 
##     confirmed_7dav_incidence_prop + deaths_7dav_cumulative_prop + 
##     deaths_7dav_incidence_prop + EmergDec + GathRestrict + BarRestrict + 
##     NEBusinessClose + RestaurantRestrict + StayAtHome + PublicMask + 
##     OtherBusinessClose + BusinessMask + SchoolMask + Quarantine, 
##     data = .)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -22.3447  -5.1699   0.0855   5.9131  17.4942 
## 
## Coefficients: (3 not defined because of singularities)
##                                  Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    117.500000   2.228605  52.724  < 2e-16 ***
## confirmed_7dav_cumulative_prop   0.008847   0.009633   0.918 0.359126    
## confirmed_7dav_incidence_prop   -0.126482   0.142679  -0.886 0.376058    
## deaths_7dav_cumulative_prop     -0.985935   0.385298  -2.559 0.010982 *  
## deaths_7dav_incidence_prop      -3.224998   2.013978  -1.601 0.110341    
## EmergDec1                      -10.500000   3.860057  -2.720 0.006898 ** 
## GathRestrict1                   -7.492527   4.315684  -1.736 0.083550 .  
## BarRestrict1                   -10.418915   3.748587  -2.779 0.005782 ** 
## NEBusinessClose1                       NA         NA      NA       NA    
## RestaurantRestrict1                    NA         NA      NA       NA    
## StayAtHome1                            NA         NA      NA       NA    
## PublicMask1                    -24.331193   2.689682  -9.046  < 2e-16 ***
## OtherBusinessClose1            -27.632641   2.400287 -11.512  < 2e-16 ***
## BusinessMask1                    8.844697   2.302069   3.842 0.000148 ***
## SchoolMask1                      0.249959   2.412784   0.104 0.917556    
## Quarantine1                     -5.285974   3.907466  -1.353 0.177122    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.339 on 306 degrees of freedom
## Multiple R-squared:  0.8916,	Adjusted R-squared:  0.8874 
## F-statistic: 209.8 on 12 and 306 DF,  p-value: < 2.2e-16
## 
## [1] "Orange County"
## 
## Call:
## lm(formula = full_time_work_prop ~ confirmed_7dav_cumulative_prop + 
##     confirmed_7dav_incidence_prop + deaths_7dav_cumulative_prop + 
##     deaths_7dav_incidence_prop + EmergDec + GathRestrict + BarRestrict + 
##     NEBusinessClose + RestaurantRestrict + StayAtHome + PublicMask + 
##     OtherBusinessClose + BusinessMask + SchoolMask + Quarantine, 
##     data = .)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -25.4195  -4.7554   0.0284   5.2036  17.9589 
## 
## Coefficients: (3 not defined because of singularities)
##                                  Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    117.500375   2.183947  53.802  < 2e-16 ***
## confirmed_7dav_cumulative_prop  -0.012234   0.002907  -4.208 3.39e-05 ***
## confirmed_7dav_incidence_prop    0.027606   0.067905   0.407 0.684627    
## deaths_7dav_cumulative_prop      0.343304   0.135820   2.528 0.011987 *  
## deaths_7dav_incidence_prop     -19.161945   7.273930  -2.634 0.008859 ** 
## EmergDec1                      -10.499690   3.782708  -2.776 0.005847 ** 
## GathRestrict1                   -7.498295   4.229198  -1.773 0.077226 .  
## BarRestrict1                   -10.255157   3.672969  -2.792 0.005567 ** 
## NEBusinessClose1                       NA         NA      NA       NA    
## RestaurantRestrict1                    NA         NA      NA       NA    
## StayAtHome1                            NA         NA      NA       NA    
## PublicMask1                    -24.104731   2.649389  -9.098  < 2e-16 ***
## OtherBusinessClose1            -26.087800   2.397440 -10.882  < 2e-16 ***
## BusinessMask1                   11.359763   2.413226   4.707 3.81e-06 ***
## SchoolMask1                      6.727624   2.759603   2.438 0.015342 *  
## Quarantine1                     -8.797719   2.643874  -3.328 0.000983 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.172 on 306 degrees of freedom
## Multiple R-squared:  0.8959,	Adjusted R-squared:  0.8918 
## F-statistic: 219.5 on 12 and 306 DF,  p-value: < 2.2e-16
## 
## [1] "Placer County"
## 
## Call:
## lm(formula = full_time_work_prop ~ confirmed_7dav_cumulative_prop + 
##     confirmed_7dav_incidence_prop + deaths_7dav_cumulative_prop + 
##     deaths_7dav_incidence_prop + EmergDec + GathRestrict + BarRestrict + 
##     NEBusinessClose + RestaurantRestrict + StayAtHome + PublicMask + 
##     OtherBusinessClose + BusinessMask + SchoolMask + Quarantine, 
##     data = .)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -24.1019  -4.9681  -0.0113   5.7920  18.8792 
## 
## Coefficients: (3 not defined because of singularities)
##                                  Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    117.503451   2.266384  51.846  < 2e-16 ***
## confirmed_7dav_cumulative_prop  -0.022843   0.005162  -4.425 1.34e-05 ***
## confirmed_7dav_incidence_prop   -0.007065   0.091489  -0.077  0.93850    
## deaths_7dav_cumulative_prop      1.199745   0.447935   2.678  0.00780 ** 
## deaths_7dav_incidence_prop      -2.358761   7.700865  -0.306  0.75959    
## EmergDec1                      -10.620914   3.932068  -2.701  0.00730 ** 
## GathRestrict1                   -7.641759   4.394753  -1.739  0.08307 .  
## BarRestrict1                   -10.403483   3.811034  -2.730  0.00670 ** 
## NEBusinessClose1                       NA         NA      NA       NA    
## RestaurantRestrict1                    NA         NA      NA       NA    
## StayAtHome1                            NA         NA      NA       NA    
## PublicMask1                    -25.780737   2.780314  -9.273  < 2e-16 ***
## OtherBusinessClose1            -27.986979   2.453536 -11.407  < 2e-16 ***
## BusinessMask1                   10.468999   2.345705   4.463 1.14e-05 ***
## SchoolMask1                      5.997931   1.853798   3.235  0.00135 ** 
## Quarantine1                     -1.719153   2.652791  -0.648  0.51744    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.48 on 306 degrees of freedom
## Multiple R-squared:  0.8879,	Adjusted R-squared:  0.8835 
## F-statistic:   202 on 12 and 306 DF,  p-value: < 2.2e-16
## 
## [1] "Plumas County"
## 
## Call:
## lm(formula = full_time_work_prop ~ confirmed_7dav_cumulative_prop + 
##     confirmed_7dav_incidence_prop + deaths_7dav_cumulative_prop + 
##     deaths_7dav_incidence_prop + EmergDec + GathRestrict + BarRestrict + 
##     NEBusinessClose + RestaurantRestrict + StayAtHome + PublicMask + 
##     OtherBusinessClose + BusinessMask + SchoolMask + Quarantine, 
##     data = .)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -22.5468  -5.5701   0.0728   6.0477  17.9322 
## 
## Coefficients: (3 not defined because of singularities)
##                                  Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    117.500000   2.280158  51.532  < 2e-16 ***
## confirmed_7dav_cumulative_prop  -0.007750   0.003566  -2.174 0.030504 *  
## confirmed_7dav_incidence_prop   -0.051445   0.023048  -2.232 0.026332 *  
## deaths_7dav_cumulative_prop     -0.487510   1.646497  -0.296 0.767363    
## deaths_7dav_incidence_prop      -5.638690   8.039590  -0.701 0.483608    
## EmergDec1                      -10.500000   3.949349  -2.659 0.008258 ** 
## GathRestrict1                   -7.500000   4.415506  -1.699 0.090419 .  
## BarRestrict1                   -10.492630   3.833734  -2.737 0.006564 ** 
## NEBusinessClose1                       NA         NA      NA       NA    
## RestaurantRestrict1                    NA         NA      NA       NA    
## StayAtHome1                            NA         NA      NA       NA    
## PublicMask1                    -24.837829   2.751327  -9.028  < 2e-16 ***
## OtherBusinessClose1            -27.504708   2.451818 -11.218  < 2e-16 ***
## BusinessMask1                    9.046833   2.333369   3.877 0.000129 ***
## SchoolMask1                      1.047032   1.604826   0.652 0.514615    
## Quarantine1                     -9.957920   1.991347  -5.001 9.65e-07 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.532 on 306 degrees of freedom
## Multiple R-squared:  0.8866,	Adjusted R-squared:  0.8821 
## F-statistic: 199.3 on 12 and 306 DF,  p-value: < 2.2e-16
## 
## [1] "Riverside County"
## 
## Call:
## lm(formula = full_time_work_prop ~ confirmed_7dav_cumulative_prop + 
##     confirmed_7dav_incidence_prop + deaths_7dav_cumulative_prop + 
##     deaths_7dav_incidence_prop + EmergDec + GathRestrict + BarRestrict + 
##     NEBusinessClose + RestaurantRestrict + StayAtHome + PublicMask + 
##     OtherBusinessClose + BusinessMask + SchoolMask + Quarantine, 
##     data = .)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -23.5042  -5.3644   0.2701   5.8029  16.8272 
## 
## Coefficients: (3 not defined because of singularities)
##                                  Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    117.500000   2.225331  52.801  < 2e-16 ***
## confirmed_7dav_cumulative_prop  -0.006774   0.001982  -3.417 0.000718 ***
## confirmed_7dav_incidence_prop   -0.020090   0.035602  -0.564 0.572968    
## deaths_7dav_cumulative_prop      0.255613   0.157251   1.626 0.105084    
## deaths_7dav_incidence_prop      -8.281668   5.135112  -1.613 0.107829    
## EmergDec1                      -10.499361   3.854387  -2.724 0.006820 ** 
## GathRestrict1                   -7.436011   4.309534  -1.725 0.085451 .  
## BarRestrict1                   -10.378965   3.742799  -2.773 0.005894 ** 
## NEBusinessClose1                       NA         NA      NA       NA    
## RestaurantRestrict1                    NA         NA      NA       NA    
## StayAtHome1                            NA         NA      NA       NA    
## PublicMask1                    -23.702583   2.855030  -8.302 3.33e-15 ***
## OtherBusinessClose1            -27.833380   2.574257 -10.812  < 2e-16 ***
## BusinessMask1                    9.383728   2.381942   3.940 0.000101 ***
## SchoolMask1                      3.863260   2.520317   1.533 0.126347    
## Quarantine1                     -5.090886   2.270975  -2.242 0.025696 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.326 on 306 degrees of freedom
## Multiple R-squared:  0.8919,	Adjusted R-squared:  0.8877 
## F-statistic: 210.5 on 12 and 306 DF,  p-value: < 2.2e-16
## 
## [1] "Sacramento County"
## 
## Call:
## lm(formula = full_time_work_prop ~ confirmed_7dav_cumulative_prop + 
##     confirmed_7dav_incidence_prop + deaths_7dav_cumulative_prop + 
##     deaths_7dav_incidence_prop + EmergDec + GathRestrict + BarRestrict + 
##     NEBusinessClose + RestaurantRestrict + StayAtHome + PublicMask + 
##     OtherBusinessClose + BusinessMask + SchoolMask + Quarantine, 
##     data = .)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -23.4477  -4.6226   0.0328   5.5130  19.3098 
## 
## Coefficients: (3 not defined because of singularities)
##                                  Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    117.501973   2.229957  52.692  < 2e-16 ***
## confirmed_7dav_cumulative_prop  -0.028839   0.005142  -5.609 4.56e-08 ***
## confirmed_7dav_incidence_prop   -0.012500   0.121085  -0.103  0.91785    
## deaths_7dav_cumulative_prop      1.364160   0.294773   4.628 5.46e-06 ***
## deaths_7dav_incidence_prop      11.283637   6.928598   1.629  0.10444    
## EmergDec1                      -10.531097   3.862439  -2.727  0.00677 ** 
## GathRestrict1                   -7.652077   4.318784  -1.772  0.07742 .  
## BarRestrict1                   -10.759378   3.751088  -2.868  0.00441 ** 
## NEBusinessClose1                       NA         NA      NA       NA    
## RestaurantRestrict1                    NA         NA      NA       NA    
## StayAtHome1                            NA         NA      NA       NA    
## PublicMask1                    -26.462865   2.732601  -9.684  < 2e-16 ***
## OtherBusinessClose1            -28.365998   2.410693 -11.767  < 2e-16 ***
## BusinessMask1                   10.316685   2.335991   4.416 1.39e-05 ***
## SchoolMask1                      4.082302   2.136321   1.911  0.05695 .  
## Quarantine1                     -0.069352   3.149616  -0.022  0.98245    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.344 on 306 degrees of freedom
## Multiple R-squared:  0.8915,	Adjusted R-squared:  0.8872 
## F-statistic: 209.5 on 12 and 306 DF,  p-value: < 2.2e-16
## 
## [1] "San Benito County"
## 
## Call:
## lm(formula = full_time_work_prop ~ confirmed_7dav_cumulative_prop + 
##     confirmed_7dav_incidence_prop + deaths_7dav_cumulative_prop + 
##     deaths_7dav_incidence_prop + EmergDec + GathRestrict + BarRestrict + 
##     NEBusinessClose + RestaurantRestrict + StayAtHome + PublicMask + 
##     OtherBusinessClose + BusinessMask + SchoolMask + Quarantine, 
##     data = .)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -24.1895  -5.1760  -0.0467   5.5331  18.2272 
## 
## Coefficients: (3 not defined because of singularities)
##                                  Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    117.510329   2.268661  51.797  < 2e-16 ***
## confirmed_7dav_cumulative_prop  -0.003244   0.004911  -0.661  0.50939    
## confirmed_7dav_incidence_prop   -0.211515   0.075194  -2.813  0.00523 ** 
## deaths_7dav_cumulative_prop      0.009996   0.440381   0.023  0.98191    
## deaths_7dav_incidence_prop      -0.378245   3.430318  -0.110  0.91227    
## EmergDec1                      -10.500000   3.929343  -2.672  0.00794 ** 
## GathRestrict1                   -7.387577   4.402772  -1.678  0.09438 .  
## BarRestrict1                   -10.254135   3.863486  -2.654  0.00837 ** 
## NEBusinessClose1                       NA         NA      NA       NA    
## RestaurantRestrict1                    NA         NA      NA       NA    
## StayAtHome1                            NA         NA      NA       NA    
## PublicMask1                    -24.845255   2.775328  -8.952  < 2e-16 ***
## OtherBusinessClose1            -27.433021   2.445281 -11.219  < 2e-16 ***
## BusinessMask1                   11.309369   2.386636   4.739  3.3e-06 ***
## SchoolMask1                      5.037460   2.742249   1.837  0.06718 .  
## Quarantine1                     -1.904633   2.645680  -0.720  0.47213    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.488 on 306 degrees of freedom
## Multiple R-squared:  0.8877,	Adjusted R-squared:  0.8833 
## F-statistic: 201.6 on 12 and 306 DF,  p-value: < 2.2e-16
## 
## [1] "San Bernardino County"
## 
## Call:
## lm(formula = full_time_work_prop ~ confirmed_7dav_cumulative_prop + 
##     confirmed_7dav_incidence_prop + deaths_7dav_cumulative_prop + 
##     deaths_7dav_incidence_prop + EmergDec + GathRestrict + BarRestrict + 
##     NEBusinessClose + RestaurantRestrict + StayAtHome + PublicMask + 
##     OtherBusinessClose + BusinessMask + SchoolMask + Quarantine, 
##     data = .)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -23.0003  -4.8725   0.4075   5.3641  17.7251 
## 
## Coefficients: (3 not defined because of singularities)
##                                  Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    117.500000   2.218710  52.959  < 2e-16 ***
## confirmed_7dav_cumulative_prop  -0.005726   0.001285  -4.458 1.16e-05 ***
## confirmed_7dav_incidence_prop   -0.022470   0.034012  -0.661  0.50933    
## deaths_7dav_cumulative_prop      0.250387   0.108862   2.300  0.02212 *  
## deaths_7dav_incidence_prop      -5.714213   3.635405  -1.572  0.11703    
## EmergDec1                      -10.500000   3.842918  -2.732  0.00665 ** 
## GathRestrict1                   -7.499564   4.296513  -1.746  0.08190 .  
## BarRestrict1                   -10.417068   3.730718  -2.792  0.00556 ** 
## NEBusinessClose1                       NA         NA      NA       NA    
## RestaurantRestrict1                    NA         NA      NA       NA    
## StayAtHome1                            NA         NA      NA       NA    
## PublicMask1                    -24.595710   2.717229  -9.052  < 2e-16 ***
## OtherBusinessClose1            -27.599378   2.427142 -11.371  < 2e-16 ***
## BusinessMask1                    9.384437   2.318267   4.048 6.55e-05 ***
## SchoolMask1                      4.370741   2.343431   1.865  0.06312 .  
## Quarantine1                     -2.920540   2.643774  -1.105  0.27016    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.302 on 306 degrees of freedom
## Multiple R-squared:  0.8926,	Adjusted R-squared:  0.8884 
## F-statistic: 211.9 on 12 and 306 DF,  p-value: < 2.2e-16
## 
## [1] "San Diego County"
## 
## Call:
## lm(formula = full_time_work_prop ~ confirmed_7dav_cumulative_prop + 
##     confirmed_7dav_incidence_prop + deaths_7dav_cumulative_prop + 
##     deaths_7dav_incidence_prop + EmergDec + GathRestrict + BarRestrict + 
##     NEBusinessClose + RestaurantRestrict + StayAtHome + PublicMask + 
##     OtherBusinessClose + BusinessMask + SchoolMask + Quarantine, 
##     data = .)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -25.2011  -5.0564   0.5313   5.6330  15.4819 
## 
## Coefficients: (3 not defined because of singularities)
##                                  Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    117.499733   2.183463  53.813  < 2e-16 ***
## confirmed_7dav_cumulative_prop   0.004455   0.006664   0.669  0.50426    
## confirmed_7dav_incidence_prop    0.061075   0.149117   0.410  0.68240    
## deaths_7dav_cumulative_prop     -0.925398   0.620034  -1.492  0.13660    
## deaths_7dav_incidence_prop     -58.266263  12.455419  -4.678 4.35e-06 ***
## EmergDec1                      -10.500338   3.781870  -2.776  0.00583 ** 
## GathRestrict1                   -7.511893   4.228318  -1.777  0.07663 .  
## BarRestrict1                    -9.518574   3.675206  -2.590  0.01006 *  
## NEBusinessClose1                       NA         NA      NA       NA    
## RestaurantRestrict1                    NA         NA      NA       NA    
## StayAtHome1                            NA         NA      NA       NA    
## PublicMask1                    -17.654347   3.145602  -5.612 4.47e-08 ***
## OtherBusinessClose1            -22.273117   3.095713  -7.195 4.85e-12 ***
## BusinessMask1                    9.166183   2.792111   3.283  0.00115 ** 
## SchoolMask1                      6.019935   2.562253   2.349  0.01944 *  
## Quarantine1                     -4.538310   3.138234  -1.446  0.14916    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.17 on 306 degrees of freedom
## Multiple R-squared:  0.896,	Adjusted R-squared:  0.8919 
## F-statistic: 219.6 on 12 and 306 DF,  p-value: < 2.2e-16
## 
## [1] "San Francisco County"
## 
## Call:
## lm(formula = full_time_work_prop ~ confirmed_7dav_cumulative_prop + 
##     confirmed_7dav_incidence_prop + deaths_7dav_cumulative_prop + 
##     deaths_7dav_incidence_prop + EmergDec + GathRestrict + BarRestrict + 
##     NEBusinessClose + RestaurantRestrict + StayAtHome + PublicMask + 
##     OtherBusinessClose + BusinessMask + SchoolMask + Quarantine, 
##     data = .)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -23.929  -5.218   0.038   5.819  17.869 
## 
## Coefficients: (3 not defined because of singularities)
##                                  Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    117.500000   2.271943  51.718  < 2e-16 ***
## confirmed_7dav_cumulative_prop  -0.018038   0.005253  -3.434 0.000677 ***
## confirmed_7dav_incidence_prop   -0.431992   0.161792  -2.670 0.007989 ** 
## deaths_7dav_cumulative_prop      1.064283   0.488862   2.177 0.030240 *  
## deaths_7dav_incidence_prop     -31.499250  13.578089  -2.320 0.021006 *  
## EmergDec1                      -10.421643   3.935199  -2.648 0.008509 ** 
## GathRestrict1                   -7.333266   4.399776  -1.667 0.096590 .  
## BarRestrict1                    -8.161459   3.880959  -2.103 0.036287 *  
## NEBusinessClose1                       NA         NA      NA       NA    
## RestaurantRestrict1                    NA         NA      NA       NA    
## StayAtHome1                            NA         NA      NA       NA    
## PublicMask1                    -23.211615   2.860719  -8.114 1.20e-14 ***
## OtherBusinessClose1            -28.602547   2.535081 -11.283  < 2e-16 ***
## BusinessMask1                    9.644899   2.402901   4.014 7.52e-05 ***
## SchoolMask1                     10.078347   2.179452   4.624 5.55e-06 ***
## Quarantine1                     -5.344267   3.248102  -1.645 0.100924    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.501 on 306 degrees of freedom
## Multiple R-squared:  0.8874,	Adjusted R-squared:  0.883 
## F-statistic: 200.9 on 12 and 306 DF,  p-value: < 2.2e-16
## 
## [1] "San Joaquin County"
## 
## Call:
## lm(formula = full_time_work_prop ~ confirmed_7dav_cumulative_prop + 
##     confirmed_7dav_incidence_prop + deaths_7dav_cumulative_prop + 
##     deaths_7dav_incidence_prop + EmergDec + GathRestrict + BarRestrict + 
##     NEBusinessClose + RestaurantRestrict + StayAtHome + PublicMask + 
##     OtherBusinessClose + BusinessMask + SchoolMask + Quarantine, 
##     data = .)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -25.224  -4.830   0.088   5.496  17.895 
## 
## Coefficients: (3 not defined because of singularities)
##                                  Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    117.500000   2.206144  53.260  < 2e-16 ***
## confirmed_7dav_cumulative_prop  -0.016016   0.003263  -4.908 1.50e-06 ***
## confirmed_7dav_incidence_prop   -0.055757   0.056572  -0.986 0.325108    
## deaths_7dav_cumulative_prop      0.636857   0.150635   4.228 3.12e-05 ***
## deaths_7dav_incidence_prop       9.998004   2.719203   3.677 0.000279 ***
## EmergDec1                      -10.500000   3.821153  -2.748 0.006354 ** 
## GathRestrict1                   -7.585628   4.272264  -1.776 0.076801 .  
## BarRestrict1                   -10.940629   3.711945  -2.947 0.003451 ** 
## NEBusinessClose1                       NA         NA      NA       NA    
## RestaurantRestrict1                    NA         NA      NA       NA    
## StayAtHome1                            NA         NA      NA       NA    
## PublicMask1                    -25.834167   2.671614  -9.670  < 2e-16 ***
## OtherBusinessClose1            -27.717925   2.374571 -11.673  < 2e-16 ***
## BusinessMask1                   12.141192   2.308100   5.260 2.71e-07 ***
## SchoolMask1                      1.182070   2.723747   0.434 0.664604    
## Quarantine1                     -2.527894   2.322860  -1.088 0.277333    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.255 on 306 degrees of freedom
## Multiple R-squared:  0.8938,	Adjusted R-squared:  0.8896 
## F-statistic: 214.6 on 12 and 306 DF,  p-value: < 2.2e-16
## 
## [1] "San Luis Obispo County"
## 
## Call:
## lm(formula = full_time_work_prop ~ confirmed_7dav_cumulative_prop + 
##     confirmed_7dav_incidence_prop + deaths_7dav_cumulative_prop + 
##     deaths_7dav_incidence_prop + EmergDec + GathRestrict + BarRestrict + 
##     NEBusinessClose + RestaurantRestrict + StayAtHome + PublicMask + 
##     OtherBusinessClose + BusinessMask + SchoolMask + Quarantine, 
##     data = .)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -24.4989  -5.2902   0.2596   5.7631  17.9822 
## 
## Coefficients: (3 not defined because of singularities)
##                                  Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    117.500000   2.291099  51.285  < 2e-16 ***
## confirmed_7dav_cumulative_prop  -0.008257   0.008933  -0.924  0.35601    
## confirmed_7dav_incidence_prop   -0.256266   0.102568  -2.498  0.01300 *  
## deaths_7dav_cumulative_prop      0.433956   1.047934   0.414  0.67909    
## deaths_7dav_incidence_prop     -10.619317   6.091904  -1.743  0.08231 .  
## EmergDec1                      -10.500000   3.968299  -2.646  0.00857 ** 
## GathRestrict1                   -7.450156   4.436730  -1.679  0.09413 .  
## BarRestrict1                    -9.985614   3.855704  -2.590  0.01006 *  
## NEBusinessClose1                       NA         NA      NA       NA    
## RestaurantRestrict1                    NA         NA      NA       NA    
## StayAtHome1                            NA         NA      NA       NA    
## PublicMask1                    -24.874697   2.766750  -8.991  < 2e-16 ***
## OtherBusinessClose1            -27.403439   2.479087 -11.054  < 2e-16 ***
## BusinessMask1                   10.977989   2.409464   4.556 7.53e-06 ***
## SchoolMask1                      5.513371   2.061835   2.674  0.00790 ** 
## Quarantine1                     -0.027002   4.324362  -0.006  0.99502    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.573 on 306 degrees of freedom
## Multiple R-squared:  0.8855,	Adjusted R-squared:  0.881 
## F-statistic: 197.1 on 12 and 306 DF,  p-value: < 2.2e-16
## 
## [1] "San Mateo County"
## 
## Call:
## lm(formula = full_time_work_prop ~ confirmed_7dav_cumulative_prop + 
##     confirmed_7dav_incidence_prop + deaths_7dav_cumulative_prop + 
##     deaths_7dav_incidence_prop + EmergDec + GathRestrict + BarRestrict + 
##     NEBusinessClose + RestaurantRestrict + StayAtHome + PublicMask + 
##     OtherBusinessClose + BusinessMask + SchoolMask + Quarantine, 
##     data = .)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -24.3225  -5.4456   0.2147   5.3992  18.6516 
## 
## Coefficients: (3 not defined because of singularities)
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    117.50325    2.10341  55.863  < 2e-16 ***
## confirmed_7dav_cumulative_prop   0.01635    0.00448   3.649 0.000309 ***
## confirmed_7dav_incidence_prop   -0.51774    0.10418  -4.969 1.12e-06 ***
## deaths_7dav_cumulative_prop     -2.95131    0.59440  -4.965 1.14e-06 ***
## deaths_7dav_incidence_prop     -50.89740    8.79224  -5.789 1.75e-08 ***
## EmergDec1                      -10.45154    3.64323  -2.869 0.004408 ** 
## GathRestrict1                   -6.52522    4.07459  -1.601 0.110311    
## BarRestrict1                    -6.33256    3.56821  -1.775 0.076939 .  
## NEBusinessClose1                      NA         NA      NA       NA    
## RestaurantRestrict1                   NA         NA      NA       NA    
## StayAtHome1                           NA         NA      NA       NA    
## PublicMask1                    -10.32520    3.24713  -3.180 0.001625 ** 
## OtherBusinessClose1            -15.41268    3.28043  -4.698 3.97e-06 ***
## BusinessMask1                   16.47477    3.03243   5.433 1.13e-07 ***
## SchoolMask1                      2.15879    2.03992   1.058 0.290767    
## Quarantine1                     -3.11620    2.48451  -1.254 0.210709    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 7.87 on 306 degrees of freedom
## Multiple R-squared:  0.9035,	Adjusted R-squared:  0.8997 
## F-statistic: 238.6 on 12 and 306 DF,  p-value: < 2.2e-16
## 
## [1] "Santa Barbara County"
## 
## Call:
## lm(formula = full_time_work_prop ~ confirmed_7dav_cumulative_prop + 
##     confirmed_7dav_incidence_prop + deaths_7dav_cumulative_prop + 
##     deaths_7dav_incidence_prop + EmergDec + GathRestrict + BarRestrict + 
##     NEBusinessClose + RestaurantRestrict + StayAtHome + PublicMask + 
##     OtherBusinessClose + BusinessMask + SchoolMask + Quarantine, 
##     data = .)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -22.5468  -3.9647  -0.0285   5.6364  18.5038 
## 
## Coefficients: (3 not defined because of singularities)
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    117.50000    2.21994  52.929  < 2e-16 ***
## confirmed_7dav_cumulative_prop  -0.03598    0.00548  -6.565 2.23e-10 ***
## confirmed_7dav_incidence_prop    0.16961    0.10445   1.624 0.105438    
## deaths_7dav_cumulative_prop      2.17383    0.38711   5.616 4.40e-08 ***
## deaths_7dav_incidence_prop       8.24249    3.48819   2.363 0.018754 *  
## EmergDec1                      -10.50000    3.84505  -2.731 0.006685 ** 
## GathRestrict1                   -7.50575    4.29890  -1.746 0.081820 .  
## BarRestrict1                   -10.47756    3.73376  -2.806 0.005335 ** 
## NEBusinessClose1                      NA         NA      NA       NA    
## RestaurantRestrict1                   NA         NA      NA       NA    
## StayAtHome1                           NA         NA      NA       NA    
## PublicMask1                    -25.21807    2.69548  -9.356  < 2e-16 ***
## OtherBusinessClose1            -24.17950    2.44899  -9.873  < 2e-16 ***
## BusinessMask1                   12.58237    2.35033   5.353 1.70e-07 ***
## SchoolMask1                      9.36078    2.62705   3.563 0.000425 ***
## Quarantine1                     -6.65578    2.31632  -2.873 0.004345 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.306 on 306 degrees of freedom
## Multiple R-squared:  0.8925,	Adjusted R-squared:  0.8883 
## F-statistic: 211.6 on 12 and 306 DF,  p-value: < 2.2e-16
## 
## [1] "Santa Clara County"
## 
## Call:
## lm(formula = full_time_work_prop ~ confirmed_7dav_cumulative_prop + 
##     confirmed_7dav_incidence_prop + deaths_7dav_cumulative_prop + 
##     deaths_7dav_incidence_prop + EmergDec + GathRestrict + BarRestrict + 
##     NEBusinessClose + RestaurantRestrict + StayAtHome + PublicMask + 
##     OtherBusinessClose + BusinessMask + SchoolMask + Quarantine, 
##     data = .)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -23.4909  -5.4786   0.6083   5.5954  18.4966 
## 
## Coefficients: (3 not defined because of singularities)
##                                  Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    117.487235   2.276836  51.601  < 2e-16 ***
## confirmed_7dav_cumulative_prop  -0.009534   0.006155  -1.549  0.12239    
## confirmed_7dav_incidence_prop   -0.234910   0.106525  -2.205  0.02818 *  
## deaths_7dav_cumulative_prop      0.264405   0.456580   0.579  0.56295    
## deaths_7dav_incidence_prop       0.104076  11.740751   0.009  0.99293    
## EmergDec1                      -10.421791   3.943396  -2.643  0.00864 ** 
## GathRestrict1                   -7.179529   4.411704  -1.627  0.10469    
## BarRestrict1                   -10.111947   3.916842  -2.582  0.01030 *  
## NEBusinessClose1                       NA         NA      NA       NA    
## RestaurantRestrict1                    NA         NA      NA       NA    
## StayAtHome1                            NA         NA      NA       NA    
## PublicMask1                    -25.528579   2.999882  -8.510 7.90e-16 ***
## OtherBusinessClose1            -28.158513   2.748225 -10.246  < 2e-16 ***
## BusinessMask1                   10.096556   2.377222   4.247 2.88e-05 ***
## SchoolMask1                      5.489495   1.942863   2.825  0.00503 ** 
## Quarantine1                     -1.151602   3.244206  -0.355  0.72286    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.516 on 306 degrees of freedom
## Multiple R-squared:  0.887,	Adjusted R-squared:  0.8825 
## F-statistic: 200.1 on 12 and 306 DF,  p-value: < 2.2e-16
## 
## [1] "Santa Cruz County"
## 
## Call:
## lm(formula = full_time_work_prop ~ confirmed_7dav_cumulative_prop + 
##     confirmed_7dav_incidence_prop + deaths_7dav_cumulative_prop + 
##     deaths_7dav_incidence_prop + EmergDec + GathRestrict + BarRestrict + 
##     NEBusinessClose + RestaurantRestrict + StayAtHome + PublicMask + 
##     OtherBusinessClose + BusinessMask + SchoolMask + Quarantine, 
##     data = .)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -23.2546  -5.2765   0.4794   5.3679  17.8808 
## 
## Coefficients: (3 not defined because of singularities)
##                                  Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                     1.175e+02  2.260e+00  51.993  < 2e-16 ***
## confirmed_7dav_cumulative_prop -9.491e-04  5.510e-03  -0.172 0.863361    
## confirmed_7dav_incidence_prop  -3.878e-01  1.033e-01  -3.754 0.000208 ***
## deaths_7dav_cumulative_prop    -4.840e-01  4.860e-01  -0.996 0.320067    
## deaths_7dav_incidence_prop      2.470e+00  3.860e+00   0.640 0.522676    
## EmergDec1                      -1.048e+01  3.914e+00  -2.678 0.007806 ** 
## GathRestrict1                  -7.353e+00  4.376e+00  -1.680 0.093934 .  
## BarRestrict1                   -1.032e+01  3.800e+00  -2.716 0.006983 ** 
## NEBusinessClose1                       NA         NA      NA       NA    
## RestaurantRestrict1                    NA         NA      NA       NA    
## StayAtHome1                            NA         NA      NA       NA    
## PublicMask1                    -2.470e+01  2.730e+00  -9.049  < 2e-16 ***
## OtherBusinessClose1            -2.734e+01  2.431e+00 -11.244  < 2e-16 ***
## BusinessMask1                   9.805e+00  2.337e+00   4.195 3.58e-05 ***
## SchoolMask1                     3.190e+00  2.381e+00   1.340 0.181245    
## Quarantine1                     6.064e-01  2.902e+00   0.209 0.834602    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.456 on 306 degrees of freedom
## Multiple R-squared:  0.8886,	Adjusted R-squared:  0.8842 
## F-statistic: 203.3 on 12 and 306 DF,  p-value: < 2.2e-16
## 
## [1] "Shasta County"
## 
## Call:
## lm(formula = full_time_work_prop ~ confirmed_7dav_cumulative_prop + 
##     confirmed_7dav_incidence_prop + deaths_7dav_cumulative_prop + 
##     deaths_7dav_incidence_prop + EmergDec + GathRestrict + BarRestrict + 
##     NEBusinessClose + RestaurantRestrict + StayAtHome + PublicMask + 
##     OtherBusinessClose + BusinessMask + SchoolMask + Quarantine, 
##     data = .)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -22.5801  -5.2664  -0.1693   5.2446  17.5191 
## 
## Coefficients: (3 not defined because of singularities)
##                                  Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    117.500000   2.179524  53.911  < 2e-16 ***
## confirmed_7dav_cumulative_prop  -0.016015   0.003765  -4.254 2.79e-05 ***
## confirmed_7dav_incidence_prop    0.243206   0.048977   4.966 1.14e-06 ***
## deaths_7dav_cumulative_prop      0.945970   0.400814   2.360 0.018896 *  
## deaths_7dav_incidence_prop     -10.951803   4.858287  -2.254 0.024887 *  
## EmergDec1                      -10.507180   3.775046  -2.783 0.005715 ** 
## GathRestrict1                   -7.494527   4.220630  -1.776 0.076778 .  
## BarRestrict1                   -10.249212   3.670654  -2.792 0.005564 ** 
## NEBusinessClose1                       NA         NA      NA       NA    
## RestaurantRestrict1                    NA         NA      NA       NA    
## StayAtHome1                            NA         NA      NA       NA    
## PublicMask1                    -25.395704   2.633400  -9.644  < 2e-16 ***
## OtherBusinessClose1            -28.892591   2.422783 -11.925  < 2e-16 ***
## BusinessMask1                    8.790557   2.233895   3.935 0.000103 ***
## SchoolMask1                     -1.034774   1.767646  -0.585 0.558713    
## Quarantine1                     -3.812235   4.243231  -0.898 0.369664    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.155 on 306 degrees of freedom
## Multiple R-squared:  0.8963,	Adjusted R-squared:  0.8923 
## F-statistic: 220.5 on 12 and 306 DF,  p-value: < 2.2e-16
## 
## [1] "Sierra County"
## 
## Call:
## lm(formula = full_time_work_prop ~ confirmed_7dav_cumulative_prop + 
##     confirmed_7dav_incidence_prop + deaths_7dav_cumulative_prop + 
##     deaths_7dav_incidence_prop + EmergDec + GathRestrict + BarRestrict + 
##     NEBusinessClose + RestaurantRestrict + StayAtHome + PublicMask + 
##     OtherBusinessClose + BusinessMask + SchoolMask + Quarantine, 
##     data = .)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -22.322  -6.028   1.000   6.046  18.000 
## 
## Coefficients: (5 not defined because of singularities)
##                                  Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    117.500000   2.354312  49.908  < 2e-16 ***
## confirmed_7dav_cumulative_prop  -0.041012   0.008428  -4.866 1.82e-06 ***
## confirmed_7dav_incidence_prop    0.259282   0.109535   2.367   0.0185 *  
## deaths_7dav_cumulative_prop            NA         NA      NA       NA    
## deaths_7dav_incidence_prop             NA         NA      NA       NA    
## EmergDec1                      -10.500000   4.077788  -2.575   0.0105 *  
## GathRestrict1                   -7.500000   4.559106  -1.645   0.1010    
## BarRestrict1                   -10.500000   3.958413  -2.653   0.0084 ** 
## NEBusinessClose1                       NA         NA      NA       NA    
## RestaurantRestrict1                    NA         NA      NA       NA    
## StayAtHome1                            NA         NA      NA       NA    
## PublicMask1                    -25.000000   2.840145  -8.802  < 2e-16 ***
## OtherBusinessClose1            -27.665599   2.534599 -10.915  < 2e-16 ***
## BusinessMask1                   10.351883   2.420603   4.277 2.54e-05 ***
## SchoolMask1                      4.303340   1.936713   2.222   0.0270 *  
## Quarantine1                      0.054969   4.807594   0.011   0.9909    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.809 on 308 degrees of freedom
## Multiple R-squared:  0.8783,	Adjusted R-squared:  0.8743 
## F-statistic: 222.2 on 10 and 308 DF,  p-value: < 2.2e-16
## 
## [1] "Siskiyou County"
## 
## Call:
## lm(formula = full_time_work_prop ~ confirmed_7dav_cumulative_prop + 
##     confirmed_7dav_incidence_prop + deaths_7dav_cumulative_prop + 
##     deaths_7dav_incidence_prop + EmergDec + GathRestrict + BarRestrict + 
##     NEBusinessClose + RestaurantRestrict + StayAtHome + PublicMask + 
##     OtherBusinessClose + BusinessMask + SchoolMask + Quarantine, 
##     data = .)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -22.1490  -5.3152   0.2216   5.3436  18.0261 
## 
## Coefficients: (3 not defined because of singularities)
##                                  Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    117.500000   2.206177  53.260  < 2e-16 ***
## confirmed_7dav_cumulative_prop   0.006460   0.005626   1.148 0.251767    
## confirmed_7dav_incidence_prop    0.029048   0.116756   0.249 0.803687    
## deaths_7dav_cumulative_prop     -1.889231   0.540186  -3.497 0.000539 ***
## deaths_7dav_incidence_prop       1.243411   3.694722   0.337 0.736697    
## EmergDec1                      -10.500000   3.821211  -2.748 0.006355 ** 
## GathRestrict1                   -7.500000   4.272244  -1.756 0.080171 .  
## BarRestrict1                   -10.543929   3.709723  -2.842 0.004781 ** 
## NEBusinessClose1                       NA         NA      NA       NA    
## RestaurantRestrict1                    NA         NA      NA       NA    
## StayAtHome1                            NA         NA      NA       NA    
## PublicMask1                    -25.026645   2.662393  -9.400  < 2e-16 ***
## OtherBusinessClose1            -27.512264   2.372208 -11.598  < 2e-16 ***
## BusinessMask1                    8.626919   2.264906   3.809 0.000169 ***
## SchoolMask1                     -2.759089   1.996115  -1.382 0.167909    
## Quarantine1                     -6.293992   4.605087  -1.367 0.172708    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.255 on 306 degrees of freedom
## Multiple R-squared:  0.8938,	Adjusted R-squared:  0.8896 
## F-statistic: 214.6 on 12 and 306 DF,  p-value: < 2.2e-16
## 
## [1] "Solano County"
## 
## Call:
## lm(formula = full_time_work_prop ~ confirmed_7dav_cumulative_prop + 
##     confirmed_7dav_incidence_prop + deaths_7dav_cumulative_prop + 
##     deaths_7dav_incidence_prop + EmergDec + GathRestrict + BarRestrict + 
##     NEBusinessClose + RestaurantRestrict + StayAtHome + PublicMask + 
##     OtherBusinessClose + BusinessMask + SchoolMask + Quarantine, 
##     data = .)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -23.717  -4.961  -0.009   5.643  18.252 
## 
## Coefficients: (3 not defined because of singularities)
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    117.50000    2.24738  52.283  < 2e-16 ***
## confirmed_7dav_cumulative_prop  -0.01380    0.00319  -4.326 2.06e-05 ***
## confirmed_7dav_incidence_prop   -0.10743    0.08068  -1.332 0.184007    
## deaths_7dav_cumulative_prop      1.33007    0.39697   3.351 0.000908 ***
## deaths_7dav_incidence_prop     -11.90666    7.54451  -1.578 0.115555    
## EmergDec1                      -10.49095    3.89258  -2.695 0.007425 ** 
## GathRestrict1                   -7.47542    4.35203  -1.718 0.086866 .  
## BarRestrict1                   -10.40538    3.77870  -2.754 0.006245 ** 
## NEBusinessClose1                      NA         NA      NA       NA    
## RestaurantRestrict1                   NA         NA      NA       NA    
## StayAtHome1                           NA         NA      NA       NA    
## PublicMask1                    -24.81410    2.73434  -9.075  < 2e-16 ***
## OtherBusinessClose1            -28.34403    2.73356 -10.369  < 2e-16 ***
## BusinessMask1                    6.94312    2.63794   2.632 0.008918 ** 
## SchoolMask1                      5.00271    2.14742   2.330 0.020475 *  
## Quarantine1                     -3.33587    2.58180  -1.292 0.197306    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.409 on 306 degrees of freedom
## Multiple R-squared:  0.8898,	Adjusted R-squared:  0.8855 
## F-statistic: 205.9 on 12 and 306 DF,  p-value: < 2.2e-16
## 
## [1] "Sonoma County"
## 
## Call:
## lm(formula = full_time_work_prop ~ confirmed_7dav_cumulative_prop + 
##     confirmed_7dav_incidence_prop + deaths_7dav_cumulative_prop + 
##     deaths_7dav_incidence_prop + EmergDec + GathRestrict + BarRestrict + 
##     NEBusinessClose + RestaurantRestrict + StayAtHome + PublicMask + 
##     OtherBusinessClose + BusinessMask + SchoolMask + Quarantine, 
##     data = .)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -23.3993  -4.7355  -0.0006   5.4909  18.1327 
## 
## Coefficients: (3 not defined because of singularities)
##                                  Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    117.500321   2.234681  52.580  < 2e-16 ***
## confirmed_7dav_cumulative_prop  -0.027540   0.004439  -6.203 1.79e-09 ***
## confirmed_7dav_incidence_prop    0.003305   0.113486   0.029  0.97678    
## deaths_7dav_cumulative_prop      1.757839   0.296779   5.923 8.49e-09 ***
## deaths_7dav_incidence_prop       8.818790   5.547202   1.590  0.11292    
## EmergDec1                      -10.494204   3.870583  -2.711  0.00708 ** 
## GathRestrict1                   -7.486244   4.327444  -1.730  0.08465 .  
## BarRestrict1                   -10.758682   3.759131  -2.862  0.00450 ** 
## NEBusinessClose1                       NA         NA      NA       NA    
## RestaurantRestrict1                    NA         NA      NA       NA    
## StayAtHome1                            NA         NA      NA       NA    
## PublicMask1                    -24.520194   2.696619  -9.093  < 2e-16 ***
## OtherBusinessClose1            -27.201947   2.404436 -11.313  < 2e-16 ***
## BusinessMask1                   10.884546   2.301148   4.730 3.43e-06 ***
## SchoolMask1                     -2.803539   2.539896  -1.104  0.27055    
## Quarantine1                     -0.852291   2.628229  -0.324  0.74595    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.361 on 306 degrees of freedom
## Multiple R-squared:  0.891,	Adjusted R-squared:  0.8868 
## F-statistic: 208.5 on 12 and 306 DF,  p-value: < 2.2e-16
## 
## [1] "Stanislaus County"
## 
## Call:
## lm(formula = full_time_work_prop ~ confirmed_7dav_cumulative_prop + 
##     confirmed_7dav_incidence_prop + deaths_7dav_cumulative_prop + 
##     deaths_7dav_incidence_prop + EmergDec + GathRestrict + BarRestrict + 
##     NEBusinessClose + RestaurantRestrict + StayAtHome + PublicMask + 
##     OtherBusinessClose + BusinessMask + SchoolMask + Quarantine, 
##     data = .)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -24.4931  -5.5135   0.0983   5.5573  18.2050 
## 
## Coefficients: (3 not defined because of singularities)
##                                  Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    117.500000   2.277336  51.595  < 2e-16 ***
## confirmed_7dav_cumulative_prop  -0.021674   0.005137  -4.219 3.23e-05 ***
## confirmed_7dav_incidence_prop    0.070051   0.055256   1.268 0.205848    
## deaths_7dav_cumulative_prop      0.758171   0.218492   3.470 0.000595 ***
## deaths_7dav_incidence_prop       1.702571   3.116657   0.546 0.585271    
## EmergDec1                      -10.500000   3.944462  -2.662 0.008179 ** 
## GathRestrict1                   -7.498969   4.410044  -1.700 0.090066 .  
## BarRestrict1                   -10.476540   3.829011  -2.736 0.006580 ** 
## NEBusinessClose1                       NA         NA      NA       NA    
## RestaurantRestrict1                    NA         NA      NA       NA    
## StayAtHome1                            NA         NA      NA       NA    
## PublicMask1                    -25.149466   2.762900  -9.103  < 2e-16 ***
## OtherBusinessClose1            -28.747240   2.500733 -11.496  < 2e-16 ***
## BusinessMask1                   10.995282   2.421311   4.541 8.06e-06 ***
## SchoolMask1                     13.766910   2.522655   5.457 1.00e-07 ***
## Quarantine1                     -4.490854   2.707022  -1.659 0.098147 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.521 on 306 degrees of freedom
## Multiple R-squared:  0.8868,	Adjusted R-squared:  0.8824 
## F-statistic: 199.8 on 12 and 306 DF,  p-value: < 2.2e-16
## 
## [1] "Sutter County"
## 
## Call:
## lm(formula = full_time_work_prop ~ confirmed_7dav_cumulative_prop + 
##     confirmed_7dav_incidence_prop + deaths_7dav_cumulative_prop + 
##     deaths_7dav_incidence_prop + EmergDec + GathRestrict + BarRestrict + 
##     NEBusinessClose + RestaurantRestrict + StayAtHome + PublicMask + 
##     OtherBusinessClose + BusinessMask + SchoolMask + Quarantine, 
##     data = .)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -23.068  -5.273   0.329   5.888  15.998 
## 
## Coefficients: (3 not defined because of singularities)
##                                  Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    117.500000   2.231151  52.663  < 2e-16 ***
## confirmed_7dav_cumulative_prop   0.006140   0.002952   2.080  0.03837 *  
## confirmed_7dav_incidence_prop   -0.044348   0.060336  -0.735  0.46290    
## deaths_7dav_cumulative_prop     -1.372739   0.320198  -4.287 2.43e-05 ***
## deaths_7dav_incidence_prop       0.691874   2.758437   0.251  0.80212    
## EmergDec1                      -10.500000   3.864466  -2.717  0.00696 ** 
## GathRestrict1                   -7.500000   4.320605  -1.736  0.08359 .  
## BarRestrict1                   -10.480879   3.751428  -2.794  0.00554 ** 
## NEBusinessClose1                       NA         NA      NA       NA    
## RestaurantRestrict1                    NA         NA      NA       NA    
## StayAtHome1                            NA         NA      NA       NA    
## PublicMask1                    -22.654623   2.742168  -8.262 4.40e-15 ***
## OtherBusinessClose1            -27.239960   2.403595 -11.333  < 2e-16 ***
## BusinessMask1                    9.543625   2.314695   4.123 4.82e-05 ***
## SchoolMask1                      0.665377   2.337834   0.285  0.77613    
## Quarantine1                     -7.307664   4.524892  -1.615  0.10734    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.348 on 306 degrees of freedom
## Multiple R-squared:  0.8914,	Adjusted R-squared:  0.8871 
## F-statistic: 209.3 on 12 and 306 DF,  p-value: < 2.2e-16
## 
## [1] "Tehama County"
## 
## Call:
## lm(formula = full_time_work_prop ~ confirmed_7dav_cumulative_prop + 
##     confirmed_7dav_incidence_prop + deaths_7dav_cumulative_prop + 
##     deaths_7dav_incidence_prop + EmergDec + GathRestrict + BarRestrict + 
##     NEBusinessClose + RestaurantRestrict + StayAtHome + PublicMask + 
##     OtherBusinessClose + BusinessMask + SchoolMask + Quarantine, 
##     data = .)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -23.4743  -5.3640   0.4095   5.4510  19.8204 
## 
## Coefficients: (3 not defined because of singularities)
##                                  Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    117.500000   2.213945  53.073  < 2e-16 ***
## confirmed_7dav_cumulative_prop  -0.014971   0.002403  -6.229 1.55e-09 ***
## confirmed_7dav_incidence_prop   -0.036834   0.063819  -0.577 0.564251    
## deaths_7dav_cumulative_prop      0.575216   0.226414   2.541 0.011562 *  
## deaths_7dav_incidence_prop       6.111020   1.985853   3.077 0.002278 ** 
## EmergDec1                      -10.500000   3.834665  -2.738 0.006540 ** 
## GathRestrict1                   -7.500000   4.287285  -1.749 0.081232 .  
## BarRestrict1                   -10.500000   3.722406  -2.821 0.005104 ** 
## NEBusinessClose1                       NA         NA      NA       NA    
## RestaurantRestrict1                    NA         NA      NA       NA    
## StayAtHome1                            NA         NA      NA       NA    
## PublicMask1                    -24.977333   2.670815  -9.352  < 2e-16 ***
## OtherBusinessClose1            -28.442941   2.386109 -11.920  < 2e-16 ***
## BusinessMask1                   10.107554   2.283925   4.426 1.34e-05 ***
## SchoolMask1                      5.727425   1.719644   3.331 0.000973 ***
## Quarantine1                     -3.846803   4.213446  -0.913 0.361970    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.284 on 306 degrees of freedom
## Multiple R-squared:  0.893,	Adjusted R-squared:  0.8889 
## F-statistic: 212.9 on 12 and 306 DF,  p-value: < 2.2e-16
## 
## [1] "Trinity County"
## 
## Call:
## lm(formula = full_time_work_prop ~ confirmed_7dav_cumulative_prop + 
##     confirmed_7dav_incidence_prop + deaths_7dav_cumulative_prop + 
##     deaths_7dav_incidence_prop + EmergDec + GathRestrict + BarRestrict + 
##     NEBusinessClose + RestaurantRestrict + StayAtHome + PublicMask + 
##     OtherBusinessClose + BusinessMask + SchoolMask + Quarantine, 
##     data = .)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -22.4324  -5.0000   0.1602   5.5035  18.0000 
## 
## Coefficients: (3 not defined because of singularities)
##                                  Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    117.500000   2.197674  53.466  < 2e-16 ***
## confirmed_7dav_cumulative_prop  -0.006387   0.003842  -1.662  0.09745 .  
## confirmed_7dav_incidence_prop    0.073049   0.081146   0.900  0.36872    
## deaths_7dav_cumulative_prop     -0.937385   0.302113  -3.103  0.00210 ** 
## deaths_7dav_incidence_prop       0.502285   2.032639   0.247  0.80499    
## EmergDec1                      -10.500000   3.806484  -2.758  0.00616 ** 
## GathRestrict1                   -7.500000   4.255778  -1.762  0.07902 .  
## BarRestrict1                   -10.500000   3.695050  -2.842  0.00479 ** 
## NEBusinessClose1                       NA         NA      NA       NA    
## RestaurantRestrict1                    NA         NA      NA       NA    
## StayAtHome1                            NA         NA      NA       NA    
## PublicMask1                    -25.000000   2.651184  -9.430  < 2e-16 ***
## OtherBusinessClose1            -27.507038   2.363326 -11.639  < 2e-16 ***
## BusinessMask1                    8.991437   2.249308   3.997 8.03e-05 ***
## SchoolMask1                     -0.335892   1.445152  -0.232  0.81636    
## Quarantine1                     -5.050737   4.603061  -1.097  0.27339    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.223 on 306 degrees of freedom
## Multiple R-squared:  0.8946,	Adjusted R-squared:  0.8905 
## F-statistic: 216.5 on 12 and 306 DF,  p-value: < 2.2e-16
## 
## [1] "Tulare County"
## 
## Call:
## lm(formula = full_time_work_prop ~ confirmed_7dav_cumulative_prop + 
##     confirmed_7dav_incidence_prop + deaths_7dav_cumulative_prop + 
##     deaths_7dav_incidence_prop + EmergDec + GathRestrict + BarRestrict + 
##     NEBusinessClose + RestaurantRestrict + StayAtHome + PublicMask + 
##     OtherBusinessClose + BusinessMask + SchoolMask + Quarantine, 
##     data = .)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -24.316  -5.468   0.500   5.403  16.649 
## 
## Coefficients: (3 not defined because of singularities)
##                                  Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    117.500000   2.271380  51.731  < 2e-16 ***
## confirmed_7dav_cumulative_prop  -0.005767   0.002727  -2.115  0.03522 *  
## confirmed_7dav_incidence_prop   -0.102634   0.044507  -2.306  0.02178 *  
## deaths_7dav_cumulative_prop      0.273492   0.250481   1.092  0.27575    
## deaths_7dav_incidence_prop      -9.855396   3.596015  -2.741  0.00649 ** 
## EmergDec1                      -10.500000   3.934146  -2.669  0.00801 ** 
## GathRestrict1                   -7.488776   4.398511  -1.703  0.08966 .  
## BarRestrict1                   -10.305293   3.819260  -2.698  0.00736 ** 
## NEBusinessClose1                       NA         NA      NA       NA    
## RestaurantRestrict1                    NA         NA      NA       NA    
## StayAtHome1                            NA         NA      NA       NA    
## PublicMask1                    -23.093737   3.023189  -7.639 2.83e-13 ***
## OtherBusinessClose1            -26.501850   3.095211  -8.562 5.48e-16 ***
## BusinessMask1                    8.549389   3.024925   2.826  0.00502 ** 
## SchoolMask1                      4.761190   2.603022   1.829  0.06836 .  
## Quarantine1                     -5.736332   2.582803  -2.221  0.02708 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.499 on 306 degrees of freedom
## Multiple R-squared:  0.8874,	Adjusted R-squared:  0.883 
## F-statistic:   201 on 12 and 306 DF,  p-value: < 2.2e-16
## 
## [1] "Tuolumne County"
## 
## Call:
## lm(formula = full_time_work_prop ~ confirmed_7dav_cumulative_prop + 
##     confirmed_7dav_incidence_prop + deaths_7dav_cumulative_prop + 
##     deaths_7dav_incidence_prop + EmergDec + GathRestrict + BarRestrict + 
##     NEBusinessClose + RestaurantRestrict + StayAtHome + PublicMask + 
##     OtherBusinessClose + BusinessMask + SchoolMask + Quarantine, 
##     data = .)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -22.9665  -4.5015  -0.0042   5.4590  17.9826 
## 
## Coefficients: (3 not defined because of singularities)
##                                  Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    117.500000   2.163676  54.306  < 2e-16 ***
## confirmed_7dav_cumulative_prop  -0.013731   0.001958  -7.014 1.49e-11 ***
## confirmed_7dav_incidence_prop   -0.042527   0.039180  -1.085 0.278591    
## deaths_7dav_cumulative_prop      0.898083   0.267959   3.352 0.000904 ***
## deaths_7dav_incidence_prop      -0.954375   2.073842  -0.460 0.645702    
## EmergDec1                      -10.500000   3.747597  -2.802 0.005406 ** 
## GathRestrict1                   -7.500000   4.189941  -1.790 0.074442 .  
## BarRestrict1                   -10.495765   3.637888  -2.885 0.004190 ** 
## NEBusinessClose1                       NA         NA      NA       NA    
## RestaurantRestrict1                    NA         NA      NA       NA    
## StayAtHome1                            NA         NA      NA       NA    
## PublicMask1                    -24.961312   2.610175  -9.563  < 2e-16 ***
## OtherBusinessClose1            -27.489454   2.326487 -11.816  < 2e-16 ***
## BusinessMask1                    9.517777   2.217411   4.292 2.38e-05 ***
## SchoolMask1                     -1.106815   1.647824  -0.672 0.502292    
## Quarantine1                     -0.119663   2.910575  -0.041 0.967232    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.096 on 306 degrees of freedom
## Multiple R-squared:  0.8978,	Adjusted R-squared:  0.8938 
## F-statistic: 224.1 on 12 and 306 DF,  p-value: < 2.2e-16
## 
## [1] "Ventura County"
## 
## Call:
## lm(formula = full_time_work_prop ~ confirmed_7dav_cumulative_prop + 
##     confirmed_7dav_incidence_prop + deaths_7dav_cumulative_prop + 
##     deaths_7dav_incidence_prop + EmergDec + GathRestrict + BarRestrict + 
##     NEBusinessClose + RestaurantRestrict + StayAtHome + PublicMask + 
##     OtherBusinessClose + BusinessMask + SchoolMask + Quarantine, 
##     data = .)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -23.5740  -5.2252   0.1398   5.9020  18.7934 
## 
## Coefficients: (3 not defined because of singularities)
##                                  Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    117.500000   2.231491  52.655  < 2e-16 ***
## confirmed_7dav_cumulative_prop  -0.005739   0.005701  -1.007  0.31494    
## confirmed_7dav_incidence_prop   -0.101628   0.100135  -1.015  0.31095    
## deaths_7dav_cumulative_prop      0.220346   0.572467   0.385  0.70057    
## deaths_7dav_incidence_prop     -20.791563   8.992041  -2.312  0.02143 *  
## EmergDec1                      -10.498922   3.865055  -2.716  0.00698 ** 
## GathRestrict1                   -7.487848   4.321270  -1.733  0.08414 .  
## BarRestrict1                    -9.795666   3.763740  -2.603  0.00970 ** 
## NEBusinessClose1                       NA         NA      NA       NA    
## RestaurantRestrict1                    NA         NA      NA       NA    
## StayAtHome1                            NA         NA      NA       NA    
## PublicMask1                    -24.575397   2.772324  -8.865  < 2e-16 ***
## OtherBusinessClose1            -27.043126   2.494545 -10.841  < 2e-16 ***
## BusinessMask1                    9.679839   2.426115   3.990 8.28e-05 ***
## SchoolMask1                      4.576221   2.221932   2.060  0.04029 *  
## Quarantine1                     -4.986158   2.586278  -1.928  0.05479 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.349 on 306 degrees of freedom
## Multiple R-squared:  0.8913,	Adjusted R-squared:  0.8871 
## F-statistic: 209.2 on 12 and 306 DF,  p-value: < 2.2e-16
## 
## [1] "Yolo County"
## 
## Call:
## lm(formula = full_time_work_prop ~ confirmed_7dav_cumulative_prop + 
##     confirmed_7dav_incidence_prop + deaths_7dav_cumulative_prop + 
##     deaths_7dav_incidence_prop + EmergDec + GathRestrict + BarRestrict + 
##     NEBusinessClose + RestaurantRestrict + StayAtHome + PublicMask + 
##     OtherBusinessClose + BusinessMask + SchoolMask + Quarantine, 
##     data = .)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -23.8469  -5.4241   0.9866   5.0866  16.8766 
## 
## Coefficients: (3 not defined because of singularities)
##                                  Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    117.500000   2.227539  52.749  < 2e-16 ***
## confirmed_7dav_cumulative_prop   0.016388   0.005693   2.879  0.00427 ** 
## confirmed_7dav_incidence_prop   -0.285997   0.096492  -2.964  0.00328 ** 
## deaths_7dav_cumulative_prop     -2.265222   0.475274  -4.766 2.91e-06 ***
## deaths_7dav_incidence_prop      -4.269671   4.010191  -1.065  0.28785    
## EmergDec1                      -10.487303   3.858215  -2.718  0.00694 ** 
## GathRestrict1                   -7.488778   4.313616  -1.736  0.08356 .  
## BarRestrict1                    -9.652082   3.748716  -2.575  0.01050 *  
## NEBusinessClose1                       NA         NA      NA       NA    
## RestaurantRestrict1                    NA         NA      NA       NA    
## StayAtHome1                            NA         NA      NA       NA    
## PublicMask1                    -16.711314   3.065671  -5.451 1.03e-07 ***
## OtherBusinessClose1            -15.652558   3.634954  -4.306 2.24e-05 ***
## BusinessMask1                   12.305327   2.371567   5.189 3.86e-07 ***
## SchoolMask1                     11.950728   2.140040   5.584 5.18e-08 ***
## Quarantine1                      5.539233   3.102331   1.786  0.07517 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.335 on 306 degrees of freedom
## Multiple R-squared:  0.8917,	Adjusted R-squared:  0.8875 
## F-statistic:   210 on 12 and 306 DF,  p-value: < 2.2e-16
## 
## [1] "Yuba County"
## 
## Call:
## lm(formula = full_time_work_prop ~ confirmed_7dav_cumulative_prop + 
##     confirmed_7dav_incidence_prop + deaths_7dav_cumulative_prop + 
##     deaths_7dav_incidence_prop + EmergDec + GathRestrict + BarRestrict + 
##     NEBusinessClose + RestaurantRestrict + StayAtHome + PublicMask + 
##     OtherBusinessClose + BusinessMask + SchoolMask + Quarantine, 
##     data = .)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -21.6370  -5.4534   0.3504   5.5149  19.8509 
## 
## Coefficients: (3 not defined because of singularities)
##                                  Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    117.500000   2.267140  51.827  < 2e-16 ***
## confirmed_7dav_cumulative_prop  -0.019160   0.002804  -6.833 4.49e-11 ***
## confirmed_7dav_incidence_prop    0.155331   0.073446   2.115 0.035247 *  
## deaths_7dav_cumulative_prop      2.264587   0.489219   4.629 5.44e-06 ***
## deaths_7dav_incidence_prop       3.174110   4.023877   0.789 0.430829    
## EmergDec1                      -10.500000   3.926802  -2.674 0.007899 ** 
## GathRestrict1                   -7.500000   4.390298  -1.708 0.088593 .  
## BarRestrict1                   -10.519976   3.811973  -2.760 0.006134 ** 
## NEBusinessClose1                       NA         NA      NA       NA    
## RestaurantRestrict1                    NA         NA      NA       NA    
## StayAtHome1                            NA         NA      NA       NA    
## PublicMask1                    -26.836275   2.769712  -9.689  < 2e-16 ***
## OtherBusinessClose1            -28.028634   2.446060 -11.459  < 2e-16 ***
## BusinessMask1                    8.289600   2.347389   3.531 0.000477 ***
## SchoolMask1                      3.860554   2.150621   1.795 0.073626 .  
## Quarantine1                     -2.120618   3.433324  -0.618 0.537261    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.483 on 306 degrees of freedom
## Multiple R-squared:  0.8878,	Adjusted R-squared:  0.8834 
## F-statistic: 201.9 on 12 and 306 DF,  p-value: < 2.2e-16
```

* We can also examine the effect when the weekends are droppped.


