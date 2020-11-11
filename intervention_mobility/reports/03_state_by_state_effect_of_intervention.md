---
title: "03_state_by_state_causal_effect_of_intervention"
author: "Kenneth Lee"
date: "10/11/2020"
output:
  html_document:
    code_folding: hide
    keep_md: yes
    toc: yes
    toc_float:
      collapsed: yes
      smooth_scroll: yes
---

# Introduction

In this notebook, our goal is to estimate the causal effect of different state policies on mobility signal in a state-level. 

We will look at a number of states that have enforced the same type of policies, both mandatory and non-mandatory, and see if the effect of the same intervention may vary from state to state. The mobility signals are from [Delphi Epidata API](https://cmu-delphi.github.io/delphi-epidata/api/covidcast-signals/safegraph.html), which include ``full_time_work_prop``, ``part_time_work_prop``, ``completely_home_prop``, and ``median_home_dwell_time``.

The policy data is from University of Washington's [State-level social distancing policies](https://github.com/COVID19StatePolicy/SocialDistancing) as we will use it in model building. For the definition of the policy, please refer to the [codebooks](https://github.com/COVID19StatePolicy/SocialDistancing/blob/master/codebooks/State%20COVID-19%20policy%20documentation%2C%20Fall%202020.pdf).

Based on exploratory data analysis, there is a significant decrase in mobility signal during weekends, we will drop all the weekends in the data throughout this analysis. 


```r
library(ggplot2)
library (readr)
library(tidyverse)
library(dplyr)
library(covidcast)
library(lubridate)
library(ggpubr)
library(reshape2)
library(tidyr)
library(viridis)
library(gridExtra)
library(zoo)
library(cowplot)
library(gplots)
library(car)
library(nortest)
#library(MASS)

source("code/painter.r")
source("code/loader.r")
source("code/parser.r")
```


```r
STARTDATE <- "2019-01-01"
ENDDATE <- lubridate::today()
GEO_TYPE = "state" # state-level
GEO_VALUE = "*" # all states
EXCLUDED_AREAS = c("as","gu", "mp","vi", "pr") # excluded areas due to small sample size
```


```r
# Full time away home mobility
ftime <- covidcast_signal(data_source = "safegraph", 
                            signal ="full_time_work_prop",
                            start_day = STARTDATE, 
                            end_day = ENDDATE,
                            geo_type = GEO_TYPE, 
                            geo_values = GEO_VALUE)
# filter out a few states
ftime <- ftime %>%  
    filter(!(geo_value %in% EXCLUDED_AREAS))
  

# The fraction of mobile devices that did not leave the immediate area of their home (SafeGraph’s completely_home_device_count / device_count)
chome <- covidcast_signal(data_source = "safegraph", 
                            signal ="completely_home_prop",
                            start_day = STARTDATE, 
                            end_day = ENDDATE,
                            geo_type = GEO_TYPE, 
                            geo_values = GEO_VALUE)
# filter out a few states
chome <- chome %>%  
    filter(!(geo_value %in% EXCLUDED_AREAS))


# The median time spent at home for all devices at this location for this time period, in minutes
mhome<- covidcast_signal(data_source = "safegraph", 
                            signal ="median_home_dwell_time",
                            start_day = STARTDATE, 
                            end_day = ENDDATE,
                            geo_type = GEO_TYPE, 
                            geo_values = GEO_VALUE)

# filter out a few states
mhome <- mhome %>%  
    filter(!(geo_value %in% EXCLUDED_AREAS))

# Read government intervention data
policy <- load_policy()
```


# Analysis

We would like to look at policies that have been implemented by all the states: school closure, restaurant restriction, emergency declaration, and bar restriction. The full list of policies are listed below (please refer to the [codebooks](https://github.com/COVID19StatePolicy/SocialDistancing/blob/master/codebooks/State%20COVID-19%20policy%20documentation%2C%20Fall%202020.pdf) for detailed definitions):

* ``EmergDec``: Emergency declaration; currently includes State of Emergency, Public Health Emergency, Public Health Disaster declarations, Civil Emergency declarations, and other permutations of state‐level declarations of emergency in response to COVID‐19.

* ``SchoolClose``: Formal closing of (at minimum) public K‐12 schools. 

* ``BarRestrict``: Restriction or limitation of bars, breweries,wineries, tasting rooms, and/or other venues where alcoholic beverages are consumed on‐premises and sales of on‐site alcohol consumption is the primary function of the venue (i.e., bars/bar areas contained within restaurants are coded within RestaurantRestrict, as are venues that may be called bars, pubs, etc. but have food licenses)

* ``GathRestrict``: gathering restriction       

* ``OtherBusinessClose``: Mandate to close or substantially reduce operations of any category of business that are not classified under restaurants or bars.

* ``RestaurantRestrict`` : Restriction or limitation of restaurants and other venues where food is consumed on‐premises. Establishments where alcohol is served and may be called a bar or like venue but have a food license are coded within the RestaurantRestrict policy category as they are viewed as operating more like a restaurant than a bar.

* ``CaseIsolation``: Policy that requires individuals with confirmed coronavirus infection (via testing) or suspected infection to self‐isolate for a specified period of time, or when they no longer test positive for infection.  

* ``StayAtHome``: Policy instructing individuals to stay at home for all non‐essential activities. Coding a case as a stay‐at‐home order mandate requires the executive order to using phrasing indicative of a mandate (e.g., "must stay at home"); otherwise it is coded as 0 for the "Mandate" variable if it uses advisory phrasing.        

* ``PublicMask``: Policy that recommends or requires individuals to wear masks or other mouth and nose coverings when they are outside their places of residence in the public.

* ``Quarantine``: Quarantines mandated for people entering the state, requiring a period of self‐isolation. Quarantines may be imposed on all people entering the state, out‐of‐ state residents, or travelers from a particular state or city.

* ``NEBusinessClose``: Mandate to close all non‐essential businesses. Coding a case as a closure order requires the executive order to use phrasing indicative of a mandate (e.g., "non‐essential businesses are required to close", "non‐essential businesses must cease
operations by date"). 

* ``TravelRestrictIntra``: Restrictions on travel within the state.

* ``TravelRestrictEntry``: Travel restriction mandates that limit non‐residents from entering a given state.

* ``SchoolMask``: Policy that involves requiring students to wear masks or other mouth and nose coverings while at school.   

* ``TravelRestrictExit``: Policies which prohibit residents of a state from leaving the state.

* ``BusinessMask``: Policy that involves requiring employees to wear masks or other mouth and nose coverings as part of business operations. 

Then, we focus on a number of states, in which some of them enforce one of the policies as mandatory, and some of them implement the policy as a recommendation. 


```r
# We filter down to only state wide policy
policy <- policy %>% 
  filter(StateWide ==  1)
```

## Regression Discontinuty Design

First, we look at the simplest regression discontinuty (RD) design by regressing mobility on time in different states.


### School Closure


```r
# Look at where has enacted school close as a mandate
mandate.states <- policy %>%
  filter(StatePolicy=='SchoolClose' & Mandate==1) %>%
  dplyr::select(StateName) %>%
  arrange(StateName) %>%
  unique %>%
  as.list

print("States that enact school closure policy as a mandate:") 
```

```
## [1] "States that enact school closure policy as a mandate:"
```

```r
print(mandate.states$StateName)
```

```
##  [1] "Alabama"              "Alaska"               "Arizona"             
##  [4] "Arkansas"             "Colorado"             "Connecticut"         
##  [7] "Delaware"             "District of Columbia" "Florida"             
## [10] "Georgia"              "Hawaii"               "Idaho"               
## [13] "Illinois"             "Indiana"              "Iowa"                
## [16] "Kansas"               "Louisiana"            "Maine"               
## [19] "Maryland"             "Massachusetts"        "Michigan"            
## [22] "Minnesota"            "Mississippi"          "Missouri"            
## [25] "Montana"              "Nebraska"             "Nevada"              
## [28] "New Hampshire"        "New Jersey"           "New Mexico"          
## [31] "New York"             "North Carolina"       "North Dakota"        
## [34] "Ohio"                 "Oklahoma"             "Oregon"              
## [37] "Pennsylvania"         "Rhode Island"         "South Carolina"      
## [40] "Texas"                "Utah"                 "Vermont"             
## [43] "Virginia"             "Washington"           "West Virginia"       
## [46] "Wisconsin"            "Wyoming"
```

```r
# Look at where school close is enacted as a recommendation
nonmandate.states <- policy %>%
  filter(StatePolicy=='SchoolClose' & Mandate==0) %>%
  dplyr::select(StateName) %>%
  arrange(StateName) %>%
  unique() %>%
  as.list %>%
  as.vector 

print("States that enact school closure policy as a recommendation:") 
```

```
## [1] "States that enact school closure policy as a recommendation:"
```

```r
print(nonmandate.states$StateName)
```

```
## [1] "California"   "Iowa"         "Kentucky"     "South Dakota" "Tennessee"
```

```r
# Check if any state has changed their school close from mandate to recommendation, or the other way around
print("States that have changed the mandatory status of school closure")
```

```
## [1] "States that have changed the mandatory status of school closure"
```

```r
dplyr::intersect(nonmandate.states$StateName, 
                 mandate.states$StateName)
```

```
## [1] "Iowa"
```

#### California


```r
plotRD(ftime,
         policy,
         "SchoolClose", 
         "ca",
         STARTDATE,
         ENDDATE)
```

![](03_state_by_state_effect_of_intervention_files/figure-html/ca-school-1.png)<!-- -->

#### Tennessee

```r
plotRD(ftime,
         policy,
         "SchoolClose", 
         "tn",
         STARTDATE,
         ENDDATE)
```

![](03_state_by_state_effect_of_intervention_files/figure-html/tn-school-1.png)<!-- -->

#### New York


```r
plotRD(ftime,
         policy,
         "SchoolClose", 
         "ny",
         STARTDATE,
         ENDDATE)
```

![](03_state_by_state_effect_of_intervention_files/figure-html/ny-school-1.png)<!-- -->

#### Alabama


```r
plotRD(ftime,
         policy,
         "SchoolClose", 
         "al",
         STARTDATE,
         ENDDATE)
```

![](03_state_by_state_effect_of_intervention_files/figure-html/al-school-1.png)<!-- -->

#### All-in-one


```r
states <- unique(ftime$geo_value)

plist <- list()
count <- 1
for(state in states){
  p <- plotRD(ftime,
         policy,
         "SchoolClose", 
         state,
         STARTDATE,
         ENDDATE,
         plotMultiple = T)

  plist[[count]] <- p
  count = count + 1
}

n <- length(plist)
nCol <- floor(sqrt(n))
do.call("grid.arrange", c(plist, ncol=nCol))
```

![](03_state_by_state_effect_of_intervention_files/figure-html/allinone-schoolclose-1.png)<!-- -->

### Emergency Declaration


```r
# Look at where has enacted school close as a mandate
mandate.states <- policy %>%
  filter(StatePolicy=='EmergDec' & Mandate==1) %>%
  dplyr::select(StateName) %>%
  arrange(StateName) %>%
  unique %>%
  as.list

print("States that enact emergency declaration policy as a mandate:") 
```

```
## [1] "States that enact emergency declaration policy as a mandate:"
```

```r
print(mandate.states$StateName)
```

```
##  [1] "Alabama"              "Alaska"               "Arizona"             
##  [4] "Arkansas"             "California"           "Colorado"            
##  [7] "Connecticut"          "Delaware"             "District of Columbia"
## [10] "Florida"              "Georgia"              "Hawaii"              
## [13] "Idaho"                "Illinois"             "Indiana"             
## [16] "Iowa"                 "Kansas"               "Kentucky"            
## [19] "Louisiana"            "Maine"                "Maryland"            
## [22] "Massachusetts"        "Michigan"             "Minnesota"           
## [25] "Mississippi"          "Missouri"             "Montana"             
## [28] "Nebraska"             "Nevada"               "New Hampshire"       
## [31] "New Jersey"           "New Mexico"           "New York"            
## [34] "North Carolina"       "North Dakota"         "Ohio"                
## [37] "Oklahoma"             "Oregon"               "Pennsylvania"        
## [40] "Rhode Island"         "South Carolina"       "South Dakota"        
## [43] "Tennessee"            "Texas"                "Utah"                
## [46] "Vermont"              "Virginia"             "Washington"          
## [49] "West Virginia"        "Wisconsin"            "Wyoming"
```

```r
# Look at where school close is enacted as a recommendation
nonmandate.states <- policy %>%
  filter(StatePolicy=='EmergDec' & Mandate==0) %>%
  dplyr::select(StateName) %>%
  arrange(StateName) %>%
  unique() %>%
  as.list %>%
  as.vector 

print("States that enact emergency declaration policy as a recommendation:") 
```

```
## [1] "States that enact emergency declaration policy as a recommendation:"
```

```r
print(nonmandate.states$StateName)
```

```
## character(0)
```

```r
# Check if any state has changed their school close from mandate to recommendation, or the other way around
print("States that have changed the mandatory status of emergency declaration")
```

```
## [1] "States that have changed the mandatory status of emergency declaration"
```

```r
dplyr::intersect(nonmandate.states$StateName, 
                 mandate.states$StateName)
```

```
## character(0)
```

#### All-in-one


```r
states <- unique(ftime$geo_value)

plist <- list()
count <- 1
for(state in states){
  p <- plotRD(ftime,
         policy,
         "EmergDec", 
         state,
         STARTDATE,
         ENDDATE,
         plotMultiple = T)

  plist[[count]] <- p
  count = count + 1
}

n <- length(plist)
nCol <- floor(sqrt(n))
do.call("grid.arrange", c(plist, ncol=nCol))
```

![](03_state_by_state_effect_of_intervention_files/figure-html/all-in-one-emergDec-1.png)<!-- -->

### Bar Restriction


```r
# Look at where has enacted school close as a mandate
mandate.states <- policy %>%
  filter(StatePolicy=='BarRestrict' & Mandate==1) %>%
  dplyr::select(StateName) %>%
  arrange(StateName) %>%
  unique %>%
  as.list

print("States that enact bar restriction policy as a mandate:") 
```

```
## [1] "States that enact bar restriction policy as a mandate:"
```

```r
print(mandate.states$StateName)
```

```
##  [1] "Alabama"              "Alaska"               "Arizona"             
##  [4] "Arkansas"             "California"           "Colorado"            
##  [7] "Connecticut"          "Delaware"             "District of Columbia"
## [10] "Florida"              "Georgia"              "Hawaii"              
## [13] "Idaho"                "Illinois"             "Indiana"             
## [16] "Iowa"                 "Kansas"               "Kentucky"            
## [19] "Louisiana"            "Maine"                "Maryland"            
## [22] "Massachusetts"        "Michigan"             "Minnesota"           
## [25] "Mississippi"          "Missouri"             "Montana"             
## [28] "Nebraska"             "Nevada"               "New Hampshire"       
## [31] "New Jersey"           "New Mexico"           "New York"            
## [34] "North Carolina"       "North Dakota"         "Ohio"                
## [37] "Oklahoma"             "Oregon"               "Pennsylvania"        
## [40] "Rhode Island"         "South Carolina"       "South Dakota"        
## [43] "Tennessee"            "Texas"                "Utah"                
## [46] "Vermont"              "Virginia"             "Washington"          
## [49] "West Virginia"        "Wisconsin"            "Wyoming"
```

```r
# Look at where school close is enacted as a recommendation
nonmandate.states <- policy %>%
  filter(StatePolicy=='BarRestrict' & Mandate==0) %>%
  dplyr::select(StateName) %>%
  arrange(StateName) %>%
  unique() %>%
  as.list %>%
  as.vector 

print("States that enact bar restriction policy as a recommendation:") 
```

```
## [1] "States that enact bar restriction policy as a recommendation:"
```

```r
print(nonmandate.states$StateName)
```

```
## [1] "California"
```

```r
# Check if any state has changed their school close from mandate to recommendation, or the other way around
print("States that have changed the mandatory status of bar restriction")
```

```
## [1] "States that have changed the mandatory status of bar restriction"
```

```r
dplyr::intersect(nonmandate.states$StateName, 
                 mandate.states$StateName)
```

```
## [1] "California"
```


```r
states <- unique(ftime$geo_value)

plist <- list()
count <- 1
for(state in states){
  p <- plotRD(ftime,
         policy,
         "BarRestrict", 
         state,
         STARTDATE,
         ENDDATE,
         plotMultiple = T)

  plist[[count]] <- p
  count = count + 1
}

n <- length(plist)
nCol <- floor(sqrt(n))
do.call("grid.arrange", c(plist, ncol=nCol))
```

![](03_state_by_state_effect_of_intervention_files/figure-html/all-in-one bar restriction-1.png)<!-- -->

### Restaurant Restriction


```r
# Look at where has enacted school close as a mandate
mandate.states <- policy %>%
  filter(StatePolicy=='RestaurantRestrict' & Mandate==1) %>%
  dplyr::select(StateName) %>%
  arrange(StateName) %>%
  unique %>%
  as.list

print("States that enact restaurant restriction policy as a mandate:") 
```

```
## [1] "States that enact restaurant restriction policy as a mandate:"
```

```r
print(mandate.states$StateName)
```

```
##  [1] "Alabama"              "Alaska"               "Arizona"             
##  [4] "Arkansas"             "California"           "Colorado"            
##  [7] "Connecticut"          "Delaware"             "District of Columbia"
## [10] "Florida"              "Georgia"              "Hawaii"              
## [13] "Idaho"                "Illinois"             "Indiana"             
## [16] "Iowa"                 "Kansas"               "Kentucky"            
## [19] "Louisiana"            "Maine"                "Maryland"            
## [22] "Massachusetts"        "Michigan"             "Minnesota"           
## [25] "Mississippi"          "Missouri"             "Montana"             
## [28] "Nebraska"             "Nevada"               "New Hampshire"       
## [31] "New Jersey"           "New Mexico"           "New York"            
## [34] "North Carolina"       "North Dakota"         "Ohio"                
## [37] "Oklahoma"             "Oregon"               "Pennsylvania"        
## [40] "Rhode Island"         "South Carolina"       "South Dakota"        
## [43] "Tennessee"            "Texas"                "Utah"                
## [46] "Vermont"              "Virginia"             "Washington"          
## [49] "West Virginia"        "Wisconsin"            "Wyoming"
```

```r
# Look at where school close is enacted as a recommendation
nonmandate.states <- policy %>%
  filter(StatePolicy=='RestaurantRestrict' & Mandate==0) %>%
  dplyr::select(StateName) %>%
  arrange(StateName) %>%
  unique() %>%
  as.list %>%
  as.vector 

print("States that enact restaurant restriction policy as a recommendation:") 
```

```
## [1] "States that enact restaurant restriction policy as a recommendation:"
```

```r
print(nonmandate.states$StateName)
```

```
## [1] "California"
```

```r
# Check if any state has changed their school close from mandate to recommendation, or the other way around
print("States that have changed the mandatory status of restaurant restriction")
```

```
## [1] "States that have changed the mandatory status of restaurant restriction"
```

```r
dplyr::intersect(nonmandate.states$StateName, 
                 mandate.states$StateName)
```

```
## [1] "California"
```



```r
states <- unique(ftime$geo_value)

plist <- list()
count <- 1
for(state in states){
  p <- plotRD(ftime,
         policy,
         "RestaurantRestrict", 
         state,
         STARTDATE,
         ENDDATE,
         plotMultiple = T)

  plist[[count]] <- p
  count = count + 1
}

n <- length(plist)
nCol <- floor(sqrt(n))
do.call("grid.arrange", c(plist, ncol=nCol))
```

![](03_state_by_state_effect_of_intervention_files/figure-html/all-in-one-restaurant, -1.png)<!-- -->

## Linear Regression 

$$y = \beta_{0} + \beta_{1}t + \beta_{2}I_t + \beta_{3}tI_t$$

### Full-time-work-prop


#### Emergency Declaration


```r
# compute the number of policies and rolling mean of the number
# for each day between start and end dates
for(state in states){
  print(state)
  print("---------------------")
  ftime.state <- ftime %>% filter(geo_value == state)
  policy.state <- policy %>% filter(StatePostal == state)
  policy_signal.state <- getSumOfPolicy(policy.state, STARTDATE, ENDDATE)
  # left join mobility with policy signal by time 
  ftime.policy.df <- left_join(ftime.state , policy_signal.state, by = "time_value")
  lm.fit <- lm(value~time_value*EmergDec,data=ftime.policy.df)
  print(summary(lm.fit))
}
```

```
## [1] "ak"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * EmergDec, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.026417 -0.009086 -0.000152  0.008103  0.047791 
## 
## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)         -4.276e-01  9.067e-02  -4.716 2.93e-06 ***
## time_value           2.632e-05  5.005e-06   5.259 1.95e-07 ***
## EmergDec            -6.447e-01  2.424e-01  -2.660  0.00800 ** 
## time_value:EmergDec  3.451e-05  1.317e-05   2.621  0.00897 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.01315 on 673 degrees of freedom
## Multiple R-squared:  0.07389,	Adjusted R-squared:  0.06976 
## F-statistic:  17.9 on 3 and 673 DF,  p-value: 3.474e-11
## 
## [1] "al"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * EmergDec, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.049487 -0.020126  0.003867  0.018500  0.058335 
## 
## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)         -1.012e+00  1.660e-01  -6.100 1.79e-09 ***
## time_value           5.917e-05  9.162e-06   6.458 2.03e-10 ***
## EmergDec            -1.461e+00  4.511e-01  -3.238  0.00126 ** 
## time_value:EmergDec  7.734e-05  2.451e-05   3.156  0.00167 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.02424 on 673 degrees of freedom
## Multiple R-squared:  0.1554,	Adjusted R-squared:  0.1516 
## F-statistic: 41.27 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "ar"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * EmergDec, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.045534 -0.020209  0.004031  0.017795  0.045962 
## 
## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)         -8.460e-01  1.546e-01  -5.472 6.28e-08 ***
## time_value           4.981e-05  8.534e-06   5.836 8.29e-09 ***
## EmergDec            -1.078e+00  4.132e-01  -2.610  0.00926 ** 
## time_value:EmergDec  5.693e-05  2.245e-05   2.536  0.01145 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.02243 on 673 degrees of freedom
## Multiple R-squared:  0.1272,	Adjusted R-squared:  0.1233 
## F-statistic: 32.69 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "az"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * EmergDec, data = ftime.policy.df)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.03639 -0.01109  0.00309  0.01114  0.04208 
## 
## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)         -5.390e-01  1.068e-01  -5.047 5.77e-07 ***
## time_value           3.255e-05  5.895e-06   5.521 4.82e-08 ***
## EmergDec            -4.009e-01  2.854e-01  -1.405    0.161    
## time_value:EmergDec  2.059e-05  1.551e-05   1.328    0.185    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.01549 on 673 degrees of freedom
## Multiple R-squared:  0.1387,	Adjusted R-squared:  0.1348 
## F-statistic: 36.12 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "ca"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * EmergDec, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.034347 -0.012780  0.003605  0.011645  0.038186 
## 
## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)   
## (Intercept)         -2.911e-01  1.093e-01  -2.664  0.00791 **
## time_value           1.889e-05  6.033e-06   3.131  0.00182 **
## EmergDec            -1.737e-01  2.758e-01  -0.630  0.52911   
## time_value:EmergDec  8.734e-06  1.499e-05   0.583  0.56042   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.01548 on 673 degrees of freedom
## Multiple R-squared:  0.05431,	Adjusted R-squared:  0.05009 
## F-statistic: 12.88 on 3 and 673 DF,  p-value: 3.424e-08
## 
## [1] "co"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * EmergDec, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.042039 -0.014170  0.003516  0.014331  0.048124 
## 
## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)         -6.070e-01  1.329e-01  -4.567 5.90e-06 ***
## time_value           3.672e-05  7.338e-06   5.005 7.16e-07 ***
## EmergDec            -5.309e-01  3.553e-01  -1.494    0.136    
## time_value:EmergDec  2.741e-05  1.930e-05   1.420    0.156    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.01928 on 673 degrees of freedom
## Multiple R-squared:  0.1342,	Adjusted R-squared:  0.1304 
## F-statistic: 34.78 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "ct"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * EmergDec, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.050564 -0.016670  0.002116  0.019567  0.072735 
## 
## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)         -7.913e-01  1.736e-01  -4.558 6.13e-06 ***
## time_value           4.720e-05  9.584e-06   4.925 1.06e-06 ***
## EmergDec            -3.054e-01  4.602e-01  -0.664    0.507    
## time_value:EmergDec  1.452e-05  2.500e-05   0.581    0.562    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.0251 on 673 degrees of freedom
## Multiple R-squared:  0.1778,	Adjusted R-squared:  0.1741 
## F-statistic: 48.51 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "dc"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * EmergDec, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.035570 -0.007016  0.001160  0.010169  0.046302 
## 
## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)         -7.779e-01  1.147e-01  -6.780 2.63e-11 ***
## time_value           4.583e-05  6.333e-06   7.236 1.26e-12 ***
## EmergDec             1.985e-01  3.066e-01   0.647    0.518    
## time_value:EmergDec -1.209e-05  1.666e-05  -0.726    0.468    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.01664 on 673 degrees of freedom
## Multiple R-squared:  0.1335,	Adjusted R-squared:  0.1297 
## F-statistic: 34.58 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "de"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * EmergDec, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.043534 -0.014009  0.003783  0.014864  0.041470 
## 
## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)         -7.889e-01  1.389e-01  -5.678 2.03e-08 ***
## time_value           4.667e-05  7.669e-06   6.086 1.95e-09 ***
## EmergDec            -1.236e-01  3.776e-01  -0.327    0.744    
## time_value:EmergDec  4.915e-06  2.051e-05   0.240    0.811    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.02029 on 673 degrees of freedom
## Multiple R-squared:  0.1857,	Adjusted R-squared:  0.182 
## F-statistic: 51.15 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "fl"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * EmergDec, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.037730 -0.017856  0.005213  0.013438  0.041434 
## 
## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)         -6.608e-01  1.265e-01  -5.223 2.35e-07 ***
## time_value           3.923e-05  6.984e-06   5.617 2.84e-08 ***
## EmergDec            -5.962e-01  3.326e-01  -1.792   0.0735 .  
## time_value:EmergDec  3.111e-05  1.807e-05   1.721   0.0856 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.01823 on 673 degrees of freedom
## Multiple R-squared:  0.1128,	Adjusted R-squared:  0.1089 
## F-statistic: 28.53 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "ga"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * EmergDec, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.042416 -0.020517  0.004818  0.016910  0.039188 
## 
## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)         -6.327e-01  1.458e-01  -4.339 1.65e-05 ***
## time_value           3.781e-05  8.048e-06   4.698 3.19e-06 ***
## EmergDec            -1.224e+00  3.996e-01  -3.062  0.00228 ** 
## time_value:EmergDec  6.498e-05  2.171e-05   2.993  0.00286 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.02137 on 673 degrees of freedom
## Multiple R-squared:  0.1238,	Adjusted R-squared:  0.1199 
## F-statistic: 31.69 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "hi"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * EmergDec, data = ftime.policy.df)
## 
## Residuals:
##        Min         1Q     Median         3Q        Max 
## -0.0122597 -0.0039008  0.0003853  0.0037220  0.0158008 
## 
## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)         -2.565e-01  3.653e-02  -7.020 5.43e-12 ***
## time_value           1.567e-05  2.017e-06   7.769 2.96e-14 ***
## EmergDec            -9.552e-03  9.221e-02  -0.104    0.918    
## time_value:EmergDec  3.047e-07  5.013e-06   0.061    0.952    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.005174 on 673 degrees of freedom
## Multiple R-squared:  0.1104,	Adjusted R-squared:  0.1065 
## F-statistic: 27.85 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "ia"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * EmergDec, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.060198 -0.024632  0.001803  0.024630  0.080625 
## 
## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)         -1.365e+00  2.096e-01  -6.509 1.48e-10 ***
## time_value           7.913e-05  1.157e-05   6.837 1.81e-11 ***
## EmergDec            -1.029e+00  5.511e-01  -1.867   0.0624 .  
## time_value:EmergDec  5.317e-05  2.995e-05   1.775   0.0763 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03021 on 673 degrees of freedom
## Multiple R-squared:  0.1756,	Adjusted R-squared:  0.1719 
## F-statistic: 47.79 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "id"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * EmergDec, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.040569 -0.016174  0.004098  0.013824  0.038512 
## 
## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)         -5.760e-01  1.213e-01  -4.748 2.52e-06 ***
## time_value           3.475e-05  6.697e-06   5.188 2.81e-07 ***
## EmergDec            -7.248e-01  3.298e-01  -2.198   0.0283 *  
## time_value:EmergDec  3.828e-05  1.792e-05   2.137   0.0330 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.01772 on 673 degrees of freedom
## Multiple R-squared:  0.0922,	Adjusted R-squared:  0.08815 
## F-statistic: 22.78 on 3 and 673 DF,  p-value: 4.665e-14
## 
## [1] "il"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * EmergDec, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.051453 -0.016370  0.000582  0.016973  0.071834 
## 
## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)         -1.180e+00  1.761e-01  -6.699 4.42e-11 ***
## time_value           6.868e-05  9.723e-06   7.064 4.05e-12 ***
## EmergDec             3.137e-01  4.630e-01   0.678    0.498    
## time_value:EmergDec -1.954e-05  2.516e-05  -0.776    0.438    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.02538 on 673 degrees of freedom
## Multiple R-squared:  0.2187,	Adjusted R-squared:  0.2152 
## F-statistic:  62.8 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "in"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * EmergDec, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.043499 -0.017842  0.003002  0.016872  0.054311 
## 
## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)         -8.224e-01  1.443e-01  -5.697 1.82e-08 ***
## time_value           4.837e-05  7.970e-06   6.069 2.15e-09 ***
## EmergDec            -5.349e-01  3.703e-01  -1.445    0.149    
## time_value:EmergDec  2.750e-05  2.013e-05   1.366    0.172    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.02059 on 673 degrees of freedom
## Multiple R-squared:  0.1294,	Adjusted R-squared:  0.1256 
## F-statistic: 33.35 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "ks"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * EmergDec, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.050910 -0.019817  0.002065  0.019446  0.051069 
## 
## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)         -1.112e+00  1.704e-01  -6.528 1.31e-10 ***
## time_value           6.478e-05  9.405e-06   6.888 1.30e-11 ***
## EmergDec            -1.148e+00  4.592e-01  -2.500   0.0126 *  
## time_value:EmergDec  6.009e-05  2.495e-05   2.409   0.0163 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.0248 on 673 degrees of freedom
## Multiple R-squared:  0.1846,	Adjusted R-squared:  0.1809 
## F-statistic: 50.78 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "ky"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * EmergDec, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.048569 -0.018830  0.002522  0.018900  0.061169 
## 
## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)         -9.898e-01  1.630e-01  -6.074 2.09e-09 ***
## time_value           5.782e-05  8.998e-06   6.426 2.48e-10 ***
## EmergDec             4.220e-02  4.181e-01   0.101    0.920    
## time_value:EmergDec -4.176e-06  2.272e-05  -0.184    0.854    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.02324 on 673 degrees of freedom
## Multiple R-squared:  0.1466,	Adjusted R-squared:  0.1428 
## F-statistic: 38.53 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "la"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * EmergDec, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.040618 -0.017629  0.003188  0.016161  0.045429 
## 
## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)         -9.344e-01  1.402e-01  -6.666 5.47e-11 ***
## time_value           5.473e-05  7.737e-06   7.074 3.79e-12 ***
## EmergDec            -8.596e-01  3.746e-01  -2.294   0.0221 *  
## time_value:EmergDec  4.503e-05  2.036e-05   2.212   0.0273 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.02033 on 673 degrees of freedom
## Multiple R-squared:  0.1473,	Adjusted R-squared:  0.1435 
## F-statistic: 38.74 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "ma"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * EmergDec, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.045636 -0.013099  0.001137  0.017257  0.060335 
## 
## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)         -7.225e-01  1.569e-01  -4.605 4.92e-06 ***
## time_value           4.335e-05  8.661e-06   5.005 7.15e-07 ***
## EmergDec            -1.461e-01  4.159e-01  -0.351    0.725    
## time_value:EmergDec  6.071e-06  2.260e-05   0.269    0.788    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.02268 on 673 degrees of freedom
## Multiple R-squared:  0.1768,	Adjusted R-squared:  0.1731 
## F-statistic: 48.17 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "md"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * EmergDec, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.046822 -0.013950  0.002482  0.015651  0.059148 
## 
## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)         -8.287e-01  1.583e-01  -5.235 2.20e-07 ***
## time_value           4.903e-05  8.739e-06   5.610 2.95e-08 ***
## EmergDec             4.521e-01  4.028e-01   1.122    0.262    
## time_value:EmergDec -2.647e-05  2.189e-05  -1.209    0.227    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.0225 on 673 degrees of freedom
## Multiple R-squared:  0.186,	Adjusted R-squared:  0.1824 
## F-statistic: 51.26 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "me"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * EmergDec, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.038186 -0.015932  0.001911  0.014898  0.046994 
## 
## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)         -6.336e-01  1.297e-01  -4.886 1.29e-06 ***
## time_value           3.817e-05  7.158e-06   5.332 1.32e-07 ***
## EmergDec            -3.280e-01  3.584e-01  -0.915    0.360    
## time_value:EmergDec  1.642e-05  1.947e-05   0.843    0.399    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.01907 on 673 degrees of freedom
## Multiple R-squared:  0.1281,	Adjusted R-squared:  0.1242 
## F-statistic: 32.96 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "mi"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * EmergDec, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.038241 -0.014689  0.000265  0.014968  0.059921 
## 
## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)         -3.022e-01  1.025e-01  -2.948 0.003305 ** 
## time_value           1.970e-05  5.648e-06   3.488 0.000518 ***
## EmergDec            -6.970e-01  4.535e-01  -1.537 0.124845    
## time_value:EmergDec  3.672e-05  2.462e-05   1.491 0.136360    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.02061 on 673 degrees of freedom
## Multiple R-squared:  0.1155,	Adjusted R-squared:  0.1116 
## F-statistic:  29.3 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "mn"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * EmergDec, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.053030 -0.016667  0.001861  0.018253  0.054131 
## 
## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)         -1.213e+00  1.749e-01  -6.937 9.39e-12 ***
## time_value           7.058e-05  9.652e-06   7.312 7.49e-13 ***
## EmergDec             2.547e-02  4.752e-01   0.054    0.957    
## time_value:EmergDec -4.052e-06  2.582e-05  -0.157    0.875    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.02554 on 673 degrees of freedom
## Multiple R-squared:  0.2414,	Adjusted R-squared:  0.238 
## F-statistic: 71.39 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "mo"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * EmergDec, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.051692 -0.020794  0.003311  0.020207  0.053376 
## 
## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)         -1.131e+00  1.764e-01  -6.408 2.77e-10 ***
## time_value           6.595e-05  9.739e-06   6.771 2.79e-11 ***
## EmergDec            -1.118e+00  4.795e-01  -2.331   0.0200 *  
## time_value:EmergDec  5.837e-05  2.605e-05   2.241   0.0254 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.02577 on 673 degrees of freedom
## Multiple R-squared:  0.1825,	Adjusted R-squared:  0.1789 
## F-statistic: 50.09 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "ms"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * EmergDec, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.040804 -0.020633  0.004182  0.018109  0.043497 
## 
## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)         -7.494e-01  1.509e-01  -4.966 8.68e-07 ***
## time_value           4.446e-05  8.330e-06   5.337 1.29e-07 ***
## EmergDec            -1.467e+00  4.136e-01  -3.548 0.000416 ***
## time_value:EmergDec  7.818e-05  2.247e-05   3.480 0.000535 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.02212 on 673 degrees of freedom
## Multiple R-squared:  0.1195,	Adjusted R-squared:  0.1156 
## F-statistic: 30.46 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "mt"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * EmergDec, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.037483 -0.014949  0.002592  0.013345  0.038015 
## 
## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)         -6.156e-01  1.169e-01  -5.265 1.89e-07 ***
## time_value           3.719e-05  6.454e-06   5.762 1.26e-08 ***
## EmergDec            -4.518e-01  3.151e-01  -1.434    0.152    
## time_value:EmergDec  2.348e-05  1.712e-05   1.371    0.171    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.01702 on 673 degrees of freedom
## Multiple R-squared:  0.09047,	Adjusted R-squared:  0.08642 
## F-statistic: 22.31 on 3 and 673 DF,  p-value: 8.761e-14
## 
## [1] "nc"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * EmergDec, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.039868 -0.017725  0.004292  0.015260  0.052927 
## 
## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)         -5.778e-01  1.351e-01  -4.277 2.17e-05 ***
## time_value           3.468e-05  7.458e-06   4.651 3.98e-06 ***
## EmergDec            -3.723e-01  3.581e-01  -1.040    0.299    
## time_value:EmergDec  1.900e-05  1.946e-05   0.976    0.329    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.01953 on 673 degrees of freedom
## Multiple R-squared:  0.09645,	Adjusted R-squared:  0.09242 
## F-statistic: 23.95 on 3 and 673 DF,  p-value: 9.829e-15
## 
## [1] "nd"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * EmergDec, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.051649 -0.020051  0.000671  0.020410  0.060354 
## 
## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)         -1.195e+00  1.806e-01  -6.618 7.45e-11 ***
## time_value           6.952e-05  9.970e-06   6.974 7.40e-12 ***
## EmergDec            -8.364e-01  4.909e-01  -1.704   0.0889 .  
## time_value:EmergDec  4.303e-05  2.667e-05   1.614   0.1071    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.02638 on 673 degrees of freedom
## Multiple R-squared:  0.1798,	Adjusted R-squared:  0.1761 
## F-statistic: 49.18 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "ne"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * EmergDec, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.056863 -0.023122  0.002713  0.022197  0.058287 
## 
## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)         -1.306e+00  1.947e-01  -6.706 4.25e-11 ***
## time_value           7.571e-05  1.075e-05   7.044 4.62e-12 ***
## EmergDec            -1.690e+00  5.292e-01  -3.193  0.00147 ** 
## time_value:EmergDec  8.925e-05  2.875e-05   3.104  0.00199 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.02844 on 673 degrees of freedom
## Multiple R-squared:  0.1764,	Adjusted R-squared:  0.1728 
## F-statistic: 48.06 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "nh"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * EmergDec, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.050288 -0.018031  0.003059  0.019714  0.051967 
## 
## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)         -6.807e-01  1.710e-01  -3.982 7.59e-05 ***
## time_value           4.110e-05  9.437e-06   4.355 1.54e-05 ***
## EmergDec            -7.464e-01  4.647e-01  -1.606    0.109    
## time_value:EmergDec  3.854e-05  2.524e-05   1.527    0.127    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.02497 on 673 degrees of freedom
## Multiple R-squared:  0.1754,	Adjusted R-squared:  0.1717 
## F-statistic: 47.71 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "nj"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * EmergDec, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.053725 -0.015531  0.000963  0.021926  0.080801 
## 
## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)         -7.820e-01  1.911e-01  -4.092 4.80e-05 ***
## time_value           4.683e-05  1.055e-05   4.439 1.06e-05 ***
## EmergDec            -7.158e-02  5.024e-01  -0.142    0.887    
## time_value:EmergDec  1.582e-06  2.730e-05   0.058    0.954    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.02754 on 673 degrees of freedom
## Multiple R-squared:    0.2,	Adjusted R-squared:  0.1964 
## F-statistic: 56.07 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "nm"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * EmergDec, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.028441 -0.009280  0.001983  0.008393  0.029671 
## 
## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)         -4.843e-01  8.295e-02  -5.839 8.16e-09 ***
## time_value           2.946e-05  4.579e-06   6.433 2.37e-10 ***
## EmergDec            -5.228e-02  2.217e-01  -0.236    0.814    
## time_value:EmergDec  2.047e-06  1.205e-05   0.170    0.865    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.01203 on 673 degrees of freedom
## Multiple R-squared:  0.096,	Adjusted R-squared:  0.09197 
## F-statistic: 23.82 on 3 and 673 DF,  p-value: 1.159e-14
## 
## [1] "nv"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * EmergDec, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.030037 -0.011140  0.003470  0.009298  0.035078 
## 
## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)         -4.842e-01  9.037e-02  -5.358 1.16e-07 ***
## time_value           2.925e-05  4.989e-06   5.864 7.10e-09 ***
## EmergDec            -4.526e-01  2.436e-01  -1.858   0.0636 .  
## time_value:EmergDec  2.367e-05  1.323e-05   1.789   0.0741 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.01316 on 673 degrees of freedom
## Multiple R-squared:  0.1106,	Adjusted R-squared:  0.1067 
## F-statistic: 27.91 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "ny"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * EmergDec, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.043116 -0.013234  0.000096  0.016801  0.067217 
## 
## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)         -7.929e-01  1.516e-01  -5.231 2.25e-07 ***
## time_value           4.710e-05  8.368e-06   5.628 2.67e-08 ***
## EmergDec            -1.566e-01  3.920e-01  -0.400    0.690    
## time_value:EmergDec  6.768e-06  2.131e-05   0.318    0.751    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.02169 on 673 degrees of freedom
## Multiple R-squared:  0.1537,	Adjusted R-squared:  0.1499 
## F-statistic: 40.75 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "oh"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * EmergDec, data = ftime.policy.df)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.04764 -0.01787  0.00142  0.01794  0.07458 
## 
## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)         -8.822e-01  1.593e-01  -5.537 4.41e-08 ***
## time_value           5.192e-05  8.795e-06   5.903 5.66e-09 ***
## EmergDec            -5.092e-01  4.189e-01  -1.216    0.224    
## time_value:EmergDec  2.586e-05  2.276e-05   1.136    0.256    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.02296 on 673 degrees of freedom
## Multiple R-squared:  0.1407,	Adjusted R-squared:  0.1369 
## F-statistic: 36.74 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "ok"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * EmergDec, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.044599 -0.018007  0.003906  0.016681  0.047811 
## 
## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)         -9.106e-01  1.472e-01  -6.187 1.06e-09 ***
## time_value           5.332e-05  8.123e-06   6.564 1.05e-10 ***
## EmergDec            -9.792e-01  4.068e-01  -2.407   0.0163 *  
## time_value:EmergDec  5.141e-05  2.210e-05   2.327   0.0203 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.02164 on 673 degrees of freedom
## Multiple R-squared:  0.1504,	Adjusted R-squared:  0.1466 
## F-statistic: 39.71 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "or"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * EmergDec, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.033651 -0.012569  0.002483  0.010863  0.044479 
## 
## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)         -3.484e-01  1.125e-01  -3.096 0.002045 ** 
## time_value           2.201e-05  6.213e-06   3.542 0.000425 ***
## EmergDec             7.073e-02  2.935e-01   0.241 0.809616    
## time_value:EmergDec -4.725e-06  1.595e-05  -0.296 0.767147    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.01616 on 673 degrees of freedom
## Multiple R-squared:  0.08468,	Adjusted R-squared:  0.0806 
## F-statistic: 20.75 on 3 and 673 DF,  p-value: 7.195e-13
## 
## [1] "pa"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * EmergDec, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.046586 -0.015434  0.001733  0.016900  0.066284 
## 
## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)         -9.280e-01  1.526e-01  -6.081 2.00e-09 ***
## time_value           5.447e-05  8.425e-06   6.466 1.94e-10 ***
## EmergDec            -1.178e-01  3.915e-01  -0.301    0.764    
## time_value:EmergDec  4.532e-06  2.128e-05   0.213    0.831    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.02176 on 673 degrees of freedom
## Multiple R-squared:  0.1644,	Adjusted R-squared:  0.1607 
## F-statistic: 44.14 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "ri"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * EmergDec, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.042665 -0.014121  0.001247  0.016700  0.057840 
## 
## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)         -8.051e-01  1.463e-01  -5.503 5.32e-08 ***
## time_value           4.771e-05  8.077e-06   5.906 5.55e-09 ***
## EmergDec             2.136e-01  3.846e-01   0.555    0.579    
## time_value:EmergDec -1.324e-05  2.090e-05  -0.633    0.527    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.02108 on 673 degrees of freedom
## Multiple R-squared:  0.1419,	Adjusted R-squared:  0.1381 
## F-statistic:  37.1 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "sc"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * EmergDec, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.038268 -0.018426  0.004922  0.014957  0.032305 
## 
## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)         -6.073e-01  1.272e-01  -4.773 2.23e-06 ***
## time_value           3.627e-05  7.023e-06   5.164 3.18e-07 ***
## EmergDec            -9.046e-01  3.458e-01  -2.616   0.0091 ** 
## time_value:EmergDec  4.794e-05  1.879e-05   2.552   0.0109 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.01858 on 673 degrees of freedom
## Multiple R-squared:  0.1012,	Adjusted R-squared:  0.09724 
## F-statistic: 25.27 on 3 and 673 DF,  p-value: 1.678e-15
## 
## [1] "sd"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * EmergDec, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.049642 -0.018993  0.001042  0.019406  0.059793 
## 
## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)         -1.211e+00  1.617e-01  -7.488 2.20e-13 ***
## time_value           7.019e-05  8.926e-06   7.864 1.48e-14 ***
## EmergDec            -1.159e+00  4.395e-01  -2.637  0.00856 ** 
## time_value:EmergDec  6.084e-05  2.388e-05   2.548  0.01105 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.02362 on 673 degrees of freedom
## Multiple R-squared:  0.1715,	Adjusted R-squared:  0.1678 
## F-statistic: 46.43 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "tn"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * EmergDec, data = ftime.policy.df)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.04347 -0.01906  0.00437  0.01691  0.04173 
## 
## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)         -7.813e-01  1.477e-01  -5.291 1.65e-07 ***
## time_value           4.615e-05  8.151e-06   5.662 2.21e-08 ***
## EmergDec            -9.189e-01  3.980e-01  -2.309   0.0213 *  
## time_value:EmergDec  4.826e-05  2.162e-05   2.232   0.0259 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.0215 on 673 degrees of freedom
## Multiple R-squared:  0.1382,	Adjusted R-squared:  0.1343 
## F-statistic: 35.96 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "tx"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * EmergDec, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.043244 -0.017279  0.002979  0.014712  0.042935 
## 
## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)         -9.086e-01  1.446e-01  -6.285 5.90e-10 ***
## time_value           5.321e-05  7.981e-06   6.667 5.45e-11 ***
## EmergDec            -8.336e-01  3.929e-01  -2.121   0.0342 *  
## time_value:EmergDec  4.333e-05  2.135e-05   2.030   0.0428 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.02112 on 673 degrees of freedom
## Multiple R-squared:  0.1891,	Adjusted R-squared:  0.1855 
## F-statistic: 52.32 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "ut"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * EmergDec, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.044813 -0.019163  0.005105  0.015792  0.051903 
## 
## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)         -4.653e-01  1.472e-01  -3.162 0.001637 ** 
## time_value           2.890e-05  8.125e-06   3.557 0.000401 ***
## EmergDec            -4.114e-01  3.775e-01  -1.090 0.276249    
## time_value:EmergDec  2.121e-05  2.052e-05   1.034 0.301732    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.02099 on 673 degrees of freedom
## Multiple R-squared:  0.07866,	Adjusted R-squared:  0.07455 
## F-statistic: 19.15 on 3 and 673 DF,  p-value: 6.3e-12
## 
## [1] "va"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * EmergDec, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.045308 -0.015766  0.004225  0.016106  0.042491 
## 
## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)         -7.442e-01  1.456e-01  -5.111 4.19e-07 ***
## time_value           4.422e-05  8.039e-06   5.502 5.35e-08 ***
## EmergDec            -2.862e-01  3.925e-01  -0.729    0.466    
## time_value:EmergDec  1.377e-05  2.132e-05   0.646    0.519    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.0212 on 673 degrees of freedom
## Multiple R-squared:  0.1718,	Adjusted R-squared:  0.1681 
## F-statistic: 46.54 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "vt"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * EmergDec, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.041627 -0.013583  0.001789  0.013311  0.049354 
## 
## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)         -9.197e-01  1.285e-01  -7.158 2.15e-12 ***
## time_value           5.406e-05  7.092e-06   7.622 8.53e-14 ***
## EmergDec            -2.043e-01  3.492e-01  -0.585    0.559    
## time_value:EmergDec  9.454e-06  1.897e-05   0.498    0.618    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.01877 on 673 degrees of freedom
## Multiple R-squared:  0.1614,	Adjusted R-squared:  0.1576 
## F-statistic: 43.16 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "wa"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * EmergDec, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.037853 -0.013000  0.002802  0.012482  0.037309 
## 
## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)         -3.924e-01  1.265e-01  -3.101 0.002006 ** 
## time_value           2.455e-05  6.986e-06   3.514 0.000471 ***
## EmergDec             3.543e-02  3.093e-01   0.115 0.908836    
## time_value:EmergDec -3.033e-06  1.682e-05  -0.180 0.856955    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.01767 on 673 degrees of freedom
## Multiple R-squared:  0.1178,	Adjusted R-squared:  0.1139 
## F-statistic: 29.95 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "wi"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * EmergDec, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.055791 -0.019139  0.002927  0.019673  0.055683 
## 
## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)         -1.381e+00  1.830e-01  -7.544 1.49e-13 ***
## time_value           7.990e-05  1.010e-05   7.909 1.07e-14 ***
## EmergDec            -2.920e-02  4.933e-01  -0.059    0.953    
## time_value:EmergDec -1.144e-06  2.680e-05  -0.043    0.966    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.02664 on 673 degrees of freedom
## Multiple R-squared:  0.2208,	Adjusted R-squared:  0.2174 
## F-statistic: 63.58 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "wv"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * EmergDec, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.044567 -0.016782  0.002939  0.014983  0.042522 
## 
## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)         -9.634e-01  1.358e-01  -7.092 3.34e-12 ***
## time_value           5.627e-05  7.498e-06   7.506 1.94e-13 ***
## EmergDec            -4.389e-01  3.787e-01  -1.159    0.247    
## time_value:EmergDec  2.214e-05  2.057e-05   1.077    0.282    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.02004 on 673 degrees of freedom
## Multiple R-squared:  0.1531,	Adjusted R-squared:  0.1493 
## F-statistic: 40.55 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "wy"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * EmergDec, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.037799 -0.014816  0.001688  0.013655  0.038296 
## 
## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)         -5.732e-01  1.121e-01  -5.113 4.14e-07 ***
## time_value           3.447e-05  6.189e-06   5.570 3.68e-08 ***
## EmergDec            -1.034e+00  3.047e-01  -3.392 0.000735 ***
## time_value:EmergDec  5.508e-05  1.655e-05   3.327 0.000925 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.01638 on 673 degrees of freedom
## Multiple R-squared:  0.1091,	Adjusted R-squared:  0.1051 
## F-statistic: 27.47 on 3 and 673 DF,  p-value: < 2.2e-16
```

#### School Closure


```r
# compute the number of policies and rolling mean of the number
# for each day between start and end dates
for(state in states){
  print(state)
  print("---------------------")
  ftime.state <- ftime %>% filter(geo_value == state)
  policy.state <- policy %>% filter(StatePostal == state)
  policy_signal.state <- getSumOfPolicy(policy.state, STARTDATE, ENDDATE)
  # left join mobility with policy signal by time 
  ftime.policy.df <- left_join(ftime.state , policy_signal.state, by = "time_value")
  lm.fit <- lm(value~time_value*SchoolClose,data=ftime.policy.df)
  print(summary(lm.fit))
}
```

```
## [1] "ak"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * SchoolClose, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.025117 -0.008892 -0.000403  0.008317  0.047289 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)            -2.931e-01  5.238e-02  -5.595 3.21e-08 ***
## time_value              1.888e-05  2.879e-06   6.557 1.09e-10 ***
## SchoolClose             8.031e-02  5.437e-01   0.148    0.883    
## time_value:SchoolClose -4.788e-06  2.954e-05  -0.162    0.871    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.01317 on 673 degrees of freedom
## Multiple R-squared:  0.07224,	Adjusted R-squared:  0.06811 
## F-statistic: 17.47 on 3 and 673 DF,  p-value: 6.242e-11
## 
## [1] "al"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * SchoolClose, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.042844 -0.021308  0.002003  0.018371  0.068852 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)
## (Intercept)            -8.996e-02  9.608e-02  -0.936    0.349
## time_value              8.118e-06  5.276e-06   1.539    0.124
## SchoolClose            -1.695e+00  1.590e+00  -1.066    0.287
## time_value:SchoolClose  9.098e-05  8.644e-05   1.052    0.293
## 
## Residual standard error: 0.02528 on 673 degrees of freedom
## Multiple R-squared:  0.08155,	Adjusted R-squared:  0.07745 
## F-statistic: 19.92 on 3 and 673 DF,  p-value: 2.228e-12
## 
## [1] "ar"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * SchoolClose, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.045456 -0.019334  0.004116  0.015847  0.051221 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)            -3.124e-01  9.573e-02  -3.263 0.001157 ** 
## time_value              2.029e-05  5.266e-06   3.853 0.000128 ***
## SchoolClose            -4.857e-01  7.126e-01  -0.682 0.495716    
## time_value:SchoolClose  2.519e-05  3.870e-05   0.651 0.515427    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.02261 on 673 degrees of freedom
## Multiple R-squared:  0.113,	Adjusted R-squared:  0.109 
## F-statistic: 28.58 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "az"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * SchoolClose, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.031563 -0.013238  0.001502  0.011487  0.048300 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)
## (Intercept)            -1.517e-02  6.630e-02  -0.229    0.819
## time_value              3.565e-06  3.646e-06   0.978    0.329
## SchoolClose            -7.634e-01  5.310e-01  -1.438    0.151
## time_value:SchoolClose  4.079e-05  2.884e-05   1.414    0.158
## 
## Residual standard error: 0.01594 on 673 degrees of freedom
## Multiple R-squared:  0.08849,	Adjusted R-squared:  0.08443 
## F-statistic: 21.78 on 3 and 673 DF,  p-value: 1.802e-13
## 
## [1] "ca"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * SchoolClose, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.034792 -0.012221  0.003735  0.011346  0.028839 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)            -3.287e-01  1.046e-01  -3.144  0.00174 ** 
## time_value              2.098e-05  5.772e-06   3.634  0.00030 ***
## SchoolClose            -5.146e-01  2.842e-01  -1.811  0.07064 .  
## time_value:SchoolClose  2.712e-05  1.544e-05   1.756  0.07948 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.01527 on 673 degrees of freedom
## Multiple R-squared:  0.07866,	Adjusted R-squared:  0.07456 
## F-statistic: 19.15 on 3 and 673 DF,  p-value: 6.289e-12
## 
## [1] "co"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * SchoolClose, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.037560 -0.016524  0.002225  0.013083  0.049252 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)
## (Intercept)             8.332e-02  7.766e-02   1.073    0.284
## time_value             -1.476e-06  4.267e-06  -0.346    0.730
## SchoolClose            -1.515e+00  9.643e-01  -1.571    0.117
## time_value:SchoolClose  8.149e-05  5.240e-05   1.555    0.120
## 
## Residual standard error: 0.01982 on 673 degrees of freedom
## Multiple R-squared:  0.08569,	Adjusted R-squared:  0.08162 
## F-statistic: 21.03 on 3 and 673 DF,  p-value: 4.981e-13
## 
## [1] "ct"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * SchoolClose, data = ftime.policy.df)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.04448 -0.02326  0.00045  0.01705  0.06199 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)  
## (Intercept)             2.457e-01  1.033e-01   2.379   0.0177 *
## time_value             -1.017e-05  5.676e-06  -1.791   0.0737 .
## SchoolClose            -8.954e-01  1.084e+00  -0.826   0.4093  
## time_value:SchoolClose  4.749e-05  5.893e-05   0.806   0.4206  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.02597 on 673 degrees of freedom
## Multiple R-squared:  0.1198,	Adjusted R-squared:  0.1158 
## F-statistic: 30.52 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "dc"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * SchoolClose, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.035311 -0.007202  0.001442  0.010093  0.041163 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)            -7.560e-01  1.125e-01  -6.720 3.86e-11 ***
## time_value              4.462e-05  6.209e-06   7.185 1.78e-12 ***
## SchoolClose             1.878e-02  3.136e-01   0.060    0.952    
## time_value:SchoolClose -2.345e-06  1.703e-05  -0.138    0.891    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.0166 on 673 degrees of freedom
## Multiple R-squared:  0.1381,	Adjusted R-squared:  0.1343 
## F-statistic: 35.96 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "de"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * SchoolClose, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.043534 -0.014009  0.003783  0.014864  0.041470 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)            -7.889e-01  1.389e-01  -5.678 2.03e-08 ***
## time_value              4.667e-05  7.669e-06   6.086 1.95e-09 ***
## SchoolClose            -1.236e-01  3.776e-01  -0.327    0.744    
## time_value:SchoolClose  4.915e-06  2.051e-05   0.240    0.811    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.02029 on 673 degrees of freedom
## Multiple R-squared:  0.1857,	Adjusted R-squared:  0.182 
## F-statistic: 51.15 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "fl"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * SchoolClose, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.034182 -0.017634  0.004396  0.012359  0.037956 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)  
## (Intercept)            -1.189e-01  7.430e-02  -1.600   0.1101  
## time_value              9.250e-06  4.084e-06   2.265   0.0238 *
## SchoolClose            -7.623e-01  7.318e-01  -1.042   0.2979  
## time_value:SchoolClose  4.059e-05  3.976e-05   1.021   0.3077  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.01851 on 673 degrees of freedom
## Multiple R-squared:  0.08542,	Adjusted R-squared:  0.08134 
## F-statistic: 20.95 on 3 and 673 DF,  p-value: 5.495e-13
## 
## [1] "ga"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * SchoolClose, data = ftime.policy.df)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.03610 -0.02429  0.00384  0.01544  0.04623 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)
## (Intercept)            -2.591e-02  8.511e-02  -0.304    0.761
## time_value              4.233e-06  4.676e-06   0.905    0.366
## SchoolClose            -1.049e+00  1.062e+00  -0.987    0.324
## time_value:SchoolClose  5.602e-05  5.774e-05   0.970    0.332
## 
## Residual standard error: 0.02184 on 673 degrees of freedom
## Multiple R-squared:  0.08474,	Adjusted R-squared:  0.08066 
## F-statistic: 20.77 on 3 and 673 DF,  p-value: 7.024e-13
## 
## [1] "hi"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * SchoolClose, data = ftime.policy.df)
## 
## Residuals:
##        Min         1Q     Median         3Q        Max 
## -0.0113905 -0.0040139  0.0005136  0.0035754  0.0163294 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)            -1.802e-01  2.379e-02  -7.573 1.21e-13 ***
## time_value              1.145e-05  1.310e-06   8.742  < 2e-16 ***
## SchoolClose            -1.119e-01  1.324e-01  -0.845    0.398    
## time_value:SchoolClose  5.919e-06  7.190e-06   0.823    0.411    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.005177 on 673 degrees of freedom
## Multiple R-squared:  0.1095,	Adjusted R-squared:  0.1055 
## F-statistic: 27.58 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "ia"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * SchoolClose, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.061225 -0.024878  0.003637  0.024381  0.061308 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)            -1.314e+00  2.027e-01  -6.485 1.72e-10 ***
## time_value              7.635e-05  1.119e-05   6.825 1.96e-11 ***
## SchoolClose            -1.674e+00  5.649e-01  -2.963  0.00315 ** 
## time_value:SchoolClose  8.813e-05  3.069e-05   2.872  0.00420 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.0299 on 673 degrees of freedom
## Multiple R-squared:  0.192,	Adjusted R-squared:  0.1884 
## F-statistic:  53.3 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "id"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * SchoolClose, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.034295 -0.016697  0.002978  0.012885  0.042115 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)  
## (Intercept)            -7.324e-02  6.997e-02  -1.047   0.2956  
## time_value              6.920e-06  3.843e-06   1.800   0.0722 .
## SchoolClose            -8.305e-01  1.019e+00  -0.815   0.4152  
## time_value:SchoolClose  4.451e-05  5.538e-05   0.804   0.4219  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.01815 on 673 degrees of freedom
## Multiple R-squared:  0.04808,	Adjusted R-squared:  0.04383 
## F-statistic: 11.33 on 3 and 673 DF,  p-value: 2.946e-07
## 
## [1] "il"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * SchoolClose, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.044303 -0.022061 -0.001736  0.014212  0.074542 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)   
## (Intercept)             3.195e-01  1.032e-01   3.094  0.00206 **
## time_value             -1.428e-05  5.670e-06  -2.518  0.01202 * 
## SchoolClose            -8.430e-01  1.683e+00  -0.501  0.61650   
## time_value:SchoolClose  4.463e-05  9.150e-05   0.488  0.62587   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.02716 on 673 degrees of freedom
## Multiple R-squared:  0.1053,	Adjusted R-squared:  0.1013 
## F-statistic: 26.41 on 3 and 673 DF,  p-value: 3.705e-16
## 
## [1] "in"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * SchoolClose, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.034158 -0.021714  0.002862  0.014886  0.048719 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)
## (Intercept)            -2.734e-02  8.079e-02  -0.338    0.735
## time_value              4.368e-06  4.437e-06   0.984    0.325
## SchoolClose            -1.110e+00  1.254e+00  -0.885    0.377
## time_value:SchoolClose  5.936e-05  6.821e-05   0.870    0.384
## 
## Residual standard error: 0.02114 on 673 degrees of freedom
## Multiple R-squared:  0.08196,	Adjusted R-squared:  0.07787 
## F-statistic: 20.03 on 3 and 673 DF,  p-value: 1.919e-12
## 
## [1] "ks"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * SchoolClose, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.042916 -0.023513 -0.002194  0.018450  0.066477 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)
## (Intercept)             1.281e-01  9.820e-02   1.304    0.193
## time_value             -3.878e-06  5.390e-06  -0.720    0.472
## SchoolClose             5.113e-01  2.829e+00   0.181    0.857
## time_value:SchoolClose -2.901e-05  1.540e-04  -0.188    0.851
## 
## Residual standard error: 0.02658 on 673 degrees of freedom
## Multiple R-squared:  0.06364,	Adjusted R-squared:  0.05947 
## F-statistic: 15.25 on 3 and 673 DF,  p-value: 1.31e-09
## 
## [1] "ky"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * SchoolClose, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.048063 -0.018784  0.003916  0.018701  0.045893 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)            -9.321e-01  1.563e-01  -5.965 3.96e-09 ***
## time_value              5.462e-05  8.626e-06   6.333 4.41e-10 ***
## SchoolClose            -5.102e-01  4.356e-01  -1.171    0.242    
## time_value:SchoolClose  2.577e-05  2.366e-05   1.089    0.277    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.02306 on 673 degrees of freedom
## Multiple R-squared:  0.1599,	Adjusted R-squared:  0.1561 
## F-statistic: 42.68 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "la"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * SchoolClose, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.034816 -0.019483  0.000854  0.015404  0.053231 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)  
## (Intercept)            -1.061e-01  8.067e-02  -1.316   0.1887  
## time_value              8.900e-06  4.430e-06   2.009   0.0449 *
## SchoolClose            -1.071e+00  1.258e+00  -0.851   0.3950  
## time_value:SchoolClose  5.732e-05  6.844e-05   0.838   0.4026  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.02121 on 673 degrees of freedom
## Multiple R-squared:  0.07196,	Adjusted R-squared:  0.06782 
## F-statistic: 17.39 on 3 and 673 DF,  p-value: 6.909e-11
## 
## [1] "ma"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * SchoolClose, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.040437 -0.017954 -0.000471  0.015374  0.056786 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)  
## (Intercept)             1.604e-01  9.721e-02   1.650   0.0995 .
## time_value             -5.498e-06  5.346e-06  -1.028   0.3042  
## SchoolClose            -9.668e-01  7.866e-01  -1.229   0.2194  
## time_value:SchoolClose  5.144e-05  4.272e-05   1.204   0.2290  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.02338 on 673 degrees of freedom
## Multiple R-squared:  0.1252,	Adjusted R-squared:  0.1213 
## F-statistic: 32.11 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "md"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * SchoolClose, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.041668 -0.019645  0.000774  0.015749  0.055280 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)   
## (Intercept)             2.793e-01  9.258e-02   3.017  0.00265 **
## time_value             -1.227e-05  5.087e-06  -2.411  0.01617 * 
## SchoolClose            -8.268e-01  1.025e+00  -0.806  0.42035   
## time_value:SchoolClose  4.395e-05  5.573e-05   0.789  0.43060   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.02347 on 673 degrees of freedom
## Multiple R-squared:  0.1137,	Adjusted R-squared:  0.1098 
## F-statistic: 28.79 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "me"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * SchoolClose, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.033823 -0.018097  0.000302  0.012867  0.051591 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)
## (Intercept)             6.569e-02  7.815e-02   0.841    0.401
## time_value             -5.267e-07  4.294e-06  -0.123    0.902
## SchoolClose            -7.497e-01  9.608e-01  -0.780    0.435
## time_value:SchoolClose  3.998e-05  5.218e-05   0.766    0.444
## 
## Residual standard error: 0.01973 on 673 degrees of freedom
## Multiple R-squared:  0.06627,	Adjusted R-squared:  0.06211 
## F-statistic: 15.92 on 3 and 673 DF,  p-value: 5.193e-10
## 
## [1] "mi"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * SchoolClose, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.034263 -0.020186  0.000178  0.013701  0.050538 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)
## (Intercept)             5.642e-02  8.004e-02   0.705    0.481
## time_value             -1.495e-07  4.396e-06  -0.034    0.973
## SchoolClose            -7.999e-01  1.208e+00  -0.662    0.508
## time_value:SchoolClose  4.254e-05  6.571e-05   0.647    0.518
## 
## Residual standard error: 0.02095 on 673 degrees of freedom
## Multiple R-squared:  0.0859,	Adjusted R-squared:  0.08183 
## F-statistic: 21.08 on 3 and 673 DF,  p-value: 4.612e-13
## 
## [1] "mn"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * SchoolClose, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.043036 -0.020898 -0.001645  0.014876  0.070027 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)  
## (Intercept)             2.135e-01  1.097e-01   1.947   0.0519 .
## time_value             -8.351e-06  6.027e-06  -1.386   0.1663  
## SchoolClose            -4.846e-01  1.121e+00  -0.432   0.6656  
## time_value:SchoolClose  2.504e-05  6.089e-05   0.411   0.6811  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.02744 on 673 degrees of freedom
## Multiple R-squared:  0.1247,	Adjusted R-squared:  0.1208 
## F-statistic: 31.95 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "mo"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * SchoolClose, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.043329 -0.023873  0.002436  0.016271  0.066971 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)
## (Intercept)            -7.206e-03  1.051e-01  -0.069    0.945
## time_value              3.780e-06  5.774e-06   0.655    0.513
## SchoolClose            -1.116e+00  1.422e+00  -0.785    0.433
## time_value:SchoolClose  5.935e-05  7.726e-05   0.768    0.443
## 
## Residual standard error: 0.02707 on 673 degrees of freedom
## Multiple R-squared:  0.09811,	Adjusted R-squared:  0.09409 
## F-statistic:  24.4 on 3 and 673 DF,  p-value: 5.339e-15
## 
## [1] "ms"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * SchoolClose, data = ftime.policy.df)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.03692 -0.02305  0.00120  0.01783  0.05186 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)
## (Intercept)            -5.481e-02  8.486e-02  -0.646    0.519
## time_value              6.006e-06  4.658e-06   1.290    0.198
## SchoolClose             2.595e-01  2.244e+00   0.116    0.908
## time_value:SchoolClose -1.513e-05  1.221e-04  -0.124    0.901
## 
## Residual standard error: 0.02288 on 673 degrees of freedom
## Multiple R-squared:  0.05823,	Adjusted R-squared:  0.05403 
## F-statistic: 13.87 on 3 and 673 DF,  p-value: 8.741e-09
## 
## [1] "mt"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * SchoolClose, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.032636 -0.016939  0.000639  0.012870  0.042601 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)
## (Intercept)             3.509e-03  6.464e-02   0.054    0.957
## time_value              2.918e-06  3.547e-06   0.823    0.411
## SchoolClose             7.281e-01  2.838e+00   0.257    0.798
## time_value:SchoolClose -4.013e-05  1.545e-04  -0.260    0.795
## 
## Residual standard error: 0.01769 on 673 degrees of freedom
## Multiple R-squared:  0.017,	Adjusted R-squared:  0.01262 
## F-statistic:  3.88 on 3 and 673 DF,  p-value: 0.009103
## 
## [1] "nc"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * SchoolClose, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.039627 -0.017474  0.004601  0.015004  0.039468 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)            -5.609e-01  1.318e-01  -4.256 2.37e-05 ***
## time_value              3.375e-05  7.274e-06   4.640 4.20e-06 ***
## SchoolClose            -6.259e-01  3.674e-01  -1.704   0.0889 .  
## time_value:SchoolClose  3.273e-05  1.995e-05   1.640   0.1014    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.01945 on 673 degrees of freedom
## Multiple R-squared:  0.1045,	Adjusted R-squared:  0.1005 
## F-statistic: 26.17 on 3 and 673 DF,  p-value: 5.088e-16
## 
## [1] "nd"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * SchoolClose, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.041606 -0.023394  0.000055  0.018009  0.073114 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)
## (Intercept)            -1.726e-02  1.082e-01  -0.160    0.873
## time_value              4.341e-06  5.943e-06   0.730    0.465
## SchoolClose             7.478e-01  1.316e+00   0.568    0.570
## time_value:SchoolClose -4.194e-05  7.154e-05  -0.586    0.558
## 
## Residual standard error: 0.02773 on 673 degrees of freedom
## Multiple R-squared:  0.09371,	Adjusted R-squared:  0.08967 
## F-statistic: 23.19 on 3 and 673 DF,  p-value: 2.688e-14
## 
## [1] "ne"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * SchoolClose, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.048286 -0.027128  0.002659  0.020617  0.070552 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)  
## (Intercept)            -2.057e-01  1.156e-01  -1.779   0.0757 .
## time_value              1.483e-05  6.353e-06   2.334   0.0199 *
## SchoolClose            -4.115e-01  1.620e+00  -0.254   0.7996  
## time_value:SchoolClose  2.075e-05  8.804e-05   0.236   0.8137  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.02964 on 673 degrees of freedom
## Multiple R-squared:  0.1054,	Adjusted R-squared:  0.1014 
## F-statistic: 26.43 on 3 and 673 DF,  p-value: 3.588e-16
## 
## [1] "nh"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * SchoolClose, data = ftime.policy.df)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.04499 -0.02511  0.00061  0.01657  0.06291 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)   
## (Intercept)             3.234e-01  1.008e-01   3.209   0.0014 **
## time_value             -1.446e-05  5.537e-06  -2.612   0.0092 **
## SchoolClose            -3.933e-01  1.315e+00  -0.299   0.7649   
## time_value:SchoolClose  2.032e-05  7.147e-05   0.284   0.7763   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.02603 on 673 degrees of freedom
## Multiple R-squared:  0.1039,	Adjusted R-squared:  0.09995 
## F-statistic: 26.02 on 3 and 673 DF,  p-value: 6.173e-16
## 
## [1] "nj"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * SchoolClose, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.049624 -0.021335  0.000465  0.019175  0.066406 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)   
## (Intercept)             3.377e-01  1.166e-01   2.896   0.0039 **
## time_value             -1.511e-05  6.413e-06  -2.356   0.0187 * 
## SchoolClose            -1.470e+00  1.001e+00  -1.469   0.1423   
## time_value:SchoolClose  7.849e-05  5.436e-05   1.444   0.1492   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.02834 on 673 degrees of freedom
## Multiple R-squared:  0.1526,	Adjusted R-squared:  0.1489 
## F-statistic: 40.41 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "nm"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * SchoolClose, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.028203 -0.009554  0.002087  0.008331  0.029876 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)            -4.693e-01  8.141e-02  -5.765 1.25e-08 ***
## time_value              2.863e-05  4.494e-06   6.370 3.50e-10 ***
## SchoolClose            -1.629e-01  2.269e-01  -0.718    0.473    
## time_value:SchoolClose  8.047e-06  1.233e-05   0.653    0.514    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.01201 on 673 degrees of freedom
## Multiple R-squared:  0.09909,	Adjusted R-squared:  0.09508 
## F-statistic: 24.68 on 3 and 673 DF,  p-value: 3.714e-15
## 
## [1] "nv"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * SchoolClose, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.027625 -0.012250  0.001805  0.010129  0.034902 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)
## (Intercept)            -1.525e-02  5.072e-02  -0.301    0.764
## time_value              3.300e-06  2.785e-06   1.185    0.236
## SchoolClose            -6.560e-02  1.065e+00  -0.062    0.951
## time_value:SchoolClose  3.016e-06  5.795e-05   0.052    0.959
## 
## Residual standard error: 0.01356 on 673 degrees of freedom
## Multiple R-squared:  0.05553,	Adjusted R-squared:  0.05132 
## F-statistic: 13.19 on 3 and 673 DF,  p-value: 2.239e-08
## 
## [1] "ny"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * SchoolClose, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.037158 -0.016367  0.000829  0.014118  0.059762 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)
## (Intercept)             4.048e-02  9.002e-02   0.450    0.653
## time_value              9.949e-07  4.949e-06   0.201    0.841
## SchoolClose            -9.834e-01  8.316e-01  -1.182    0.237
## time_value:SchoolClose  5.235e-05  4.518e-05   1.159    0.247
## 
## Residual standard error: 0.02217 on 673 degrees of freedom
## Multiple R-squared:  0.116,	Adjusted R-squared:  0.1121 
## F-statistic: 29.45 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "oh"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * SchoolClose, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.038255 -0.024981  0.001425  0.015901  0.055934 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)
## (Intercept)             4.831e-02  9.078e-02   0.532    0.595
## time_value              4.324e-07  4.986e-06   0.087    0.931
## SchoolClose            -7.495e-01  1.349e+00  -0.556    0.579
## time_value:SchoolClose  3.969e-05  7.334e-05   0.541    0.589
## 
## Residual standard error: 0.02371 on 673 degrees of freedom
## Multiple R-squared:  0.08361,	Adjusted R-squared:  0.07953 
## F-statistic: 20.47 on 3 and 673 DF,  p-value: 1.058e-12
## 
## [1] "ok"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * SchoolClose, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.038585 -0.018001  0.002417  0.013497  0.056696 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)  
## (Intercept)            -1.592e-01  8.876e-02  -1.793   0.0734 .
## time_value              1.175e-05  4.879e-06   2.408   0.0163 *
## SchoolClose            -5.922e-01  9.201e-01  -0.644   0.5200  
## time_value:SchoolClose  3.108e-05  5.000e-05   0.622   0.5344  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.02228 on 673 degrees of freedom
## Multiple R-squared:  0.09965,	Adjusted R-squared:  0.09564 
## F-statistic: 24.83 on 3 and 673 DF,  p-value: 3.024e-15
## 
## [1] "or"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * SchoolClose, data = ftime.policy.df)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.03261 -0.01404  0.00160  0.01092  0.04154 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)  
## (Intercept)             1.568e-01  6.287e-02   2.495   0.0128 *
## time_value             -5.945e-06  3.453e-06  -1.722   0.0856 .
## SchoolClose            -3.376e-01  9.348e-01  -0.361   0.7181  
## time_value:SchoolClose  1.786e-05  5.083e-05   0.351   0.7254  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.01643 on 673 degrees of freedom
## Multiple R-squared:  0.05377,	Adjusted R-squared:  0.04956 
## F-statistic: 12.75 on 3 and 673 DF,  p-value: 4.119e-08
## 
## [1] "pa"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * SchoolClose, data = ftime.policy.df)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.03754 -0.02236  0.00126  0.01411  0.05219 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)
## (Intercept)             1.188e-01  8.696e-02   1.366    0.173
## time_value             -3.447e-06  4.776e-06  -0.722    0.471
## SchoolClose            -8.146e-01  1.293e+00  -0.630    0.529
## time_value:SchoolClose  4.329e-05  7.032e-05   0.616    0.538
## 
## Residual standard error: 0.02273 on 673 degrees of freedom
## Multiple R-squared:  0.0885,	Adjusted R-squared:  0.08444 
## F-statistic: 21.78 on 3 and 673 DF,  p-value: 1.798e-13
## 
## [1] "ri"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * SchoolClose, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.036380 -0.013715  0.001115  0.014429  0.050941 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)  
## (Intercept)            -1.318e-01  9.741e-02  -1.353   0.1766  
## time_value              1.047e-05  5.362e-06   1.952   0.0514 .
## SchoolClose            -5.204e-01  5.617e-01  -0.926   0.3546  
## time_value:SchoolClose  2.719e-05  3.049e-05   0.892   0.3729  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.02145 on 673 degrees of freedom
## Multiple R-squared:  0.1114,	Adjusted R-squared:  0.1074 
## F-statistic: 28.11 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "sc"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * SchoolClose, data = ftime.policy.df)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.03142 -0.02089  0.00342  0.01351  0.03789 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)
## (Intercept)            -5.916e-02  7.264e-02  -0.814    0.416
## time_value              5.934e-06  3.989e-06   1.488    0.137
## SchoolClose            -4.859e-01  1.166e+00  -0.417    0.677
## time_value:SchoolClose  2.572e-05  6.342e-05   0.406    0.685
## 
## Residual standard error: 0.0191 on 673 degrees of freedom
## Multiple R-squared:  0.05071,	Adjusted R-squared:  0.04648 
## F-statistic: 11.98 on 3 and 673 DF,  p-value: 1.189e-07
## 
## [1] "sd"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * SchoolClose, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.036370 -0.023816 -0.004372  0.019169  0.069533 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)
## (Intercept)            -2.101e-03  9.316e-02  -0.023    0.982
## time_value              3.267e-06  5.111e-06   0.639    0.523
## SchoolClose             4.345e+00  5.585e+00   0.778    0.437
## time_value:SchoolClose -2.376e-04  3.042e-04  -0.781    0.435
## 
## Residual standard error: 0.02562 on 673 degrees of freedom
## Multiple R-squared:  0.02498,	Adjusted R-squared:  0.02063 
## F-statistic: 5.747 on 3 and 673 DF,  p-value: 0.0006948
## 
## [1] "tn"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * SchoolClose, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.037606 -0.018078  0.003831  0.014805  0.048375 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)
## (Intercept)            -7.958e-02  8.789e-02  -0.905    0.366
## time_value              7.326e-06  4.831e-06   1.517    0.130
## SchoolClose            -5.781e-01  9.006e-01  -0.642    0.521
## time_value:SchoolClose  3.039e-05  4.894e-05   0.621    0.535
## 
## Residual standard error: 0.02205 on 673 degrees of freedom
## Multiple R-squared:  0.09311,	Adjusted R-squared:  0.08907 
## F-statistic: 23.03 on 3 and 673 DF,  p-value: 3.339e-14
## 
## [1] "tx"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * SchoolClose, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.038054 -0.017100  0.002101  0.012625  0.054744 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)
## (Intercept)             1.366e-02  8.707e-02   0.157    0.875
## time_value              2.171e-06  4.784e-06   0.454    0.650
## SchoolClose            -7.301e-01  1.039e+00  -0.702    0.483
## time_value:SchoolClose  3.858e-05  5.648e-05   0.683    0.495
## 
## Residual standard error: 0.02216 on 673 degrees of freedom
## Multiple R-squared:  0.1069,	Adjusted R-squared:  0.103 
## F-statistic: 26.86 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "ut"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * SchoolClose, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.044371 -0.018913  0.005882  0.015558  0.041097 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)            -4.337e-01  1.415e-01  -3.065 0.002262 ** 
## time_value              2.715e-05  7.810e-06   3.476 0.000541 ***
## SchoolClose            -7.910e-01  3.945e-01  -2.005 0.045328 *  
## time_value:SchoolClose  4.178e-05  2.142e-05   1.950 0.051590 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.02088 on 673 degrees of freedom
## Multiple R-squared:  0.08801,	Adjusted R-squared:  0.08395 
## F-statistic: 21.65 on 3 and 673 DF,  p-value: 2.146e-13
## 
## [1] "va"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * SchoolClose, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.040307 -0.022240  0.002455  0.013502  0.051315 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)  
## (Intercept)             2.152e-01  8.559e-02   2.515   0.0121 *
## time_value             -8.864e-06  4.701e-06  -1.885   0.0598 .
## SchoolClose            -1.301e-01  1.182e+00  -0.110   0.9124  
## time_value:SchoolClose  6.177e-06  6.427e-05   0.096   0.9235  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.02223 on 673 degrees of freedom
## Multiple R-squared:  0.08951,	Adjusted R-squared:  0.08545 
## F-statistic: 22.05 on 3 and 673 DF,  p-value: 1.244e-13
## 
## [1] "vt"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * SchoolClose, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.035144 -0.012312  0.000354  0.011654  0.054604 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)            -2.598e-01  8.570e-02  -3.031 0.002529 ** 
## time_value              1.755e-05  4.717e-06   3.721 0.000215 ***
## SchoolClose            -4.408e-01  5.399e-01  -0.816 0.414555    
## time_value:SchoolClose  2.292e-05  2.931e-05   0.782 0.434515    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.01933 on 673 degrees of freedom
## Multiple R-squared:   0.11,	Adjusted R-squared:  0.106 
## F-statistic: 27.73 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "wa"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * SchoolClose, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.034026 -0.013816  0.001899  0.012363  0.041098 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)  
## (Intercept)             1.621e-01  7.204e-02   2.250   0.0248 *
## time_value             -6.125e-06  3.960e-06  -1.547   0.1224  
## SchoolClose            -6.595e-01  6.833e-01  -0.965   0.3348  
## time_value:SchoolClose  3.512e-05  3.712e-05   0.946   0.3445  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.01784 on 673 degrees of freedom
## Multiple R-squared:  0.1008,	Adjusted R-squared:  0.09682 
## F-statistic: 25.15 on 3 and 673 DF,  p-value: 1.961e-15
## 
## [1] "wi"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * SchoolClose, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.045192 -0.024011 -0.000082  0.016040  0.070910 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)  
## (Intercept)             2.265e-01  1.092e-01   2.074   0.0384 *
## time_value             -9.027e-06  5.995e-06  -1.506   0.1326  
## SchoolClose            -2.170e-01  1.927e+00  -0.113   0.9104  
## time_value:SchoolClose  1.054e-05  1.048e-04   0.101   0.9199  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.02886 on 673 degrees of freedom
## Multiple R-squared:  0.08596,	Adjusted R-squared:  0.08188 
## F-statistic:  21.1 on 3 and 673 DF,  p-value: 4.524e-13
## 
## [1] "wv"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * SchoolClose, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.045018 -0.016787  0.002805  0.015025  0.042212 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)            -9.949e-01  1.364e-01  -7.296 8.38e-13 ***
## time_value              5.802e-05  7.527e-06   7.708 4.60e-14 ***
## SchoolClose            -3.882e-01  3.738e-01  -1.039    0.299    
## time_value:SchoolClose  1.936e-05  2.030e-05   0.953    0.341    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.01999 on 673 degrees of freedom
## Multiple R-squared:  0.1579,	Adjusted R-squared:  0.1542 
## F-statistic: 42.07 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "wy"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * SchoolClose, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.031832 -0.014904  0.000532  0.012412  0.041768 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)  
## (Intercept)            -1.037e-01  6.338e-02  -1.637    0.102  
## time_value              8.483e-06  3.480e-06   2.438    0.015 *
## SchoolClose            -1.322e-01  1.282e+00  -0.103    0.918  
## time_value:SchoolClose  6.521e-06  6.973e-05   0.094    0.926  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.01688 on 673 degrees of freedom
## Multiple R-squared:  0.05369,	Adjusted R-squared:  0.04948 
## F-statistic: 12.73 on 3 and 673 DF,  p-value: 4.237e-08
```

#### Bar Restriction


```r
# compute the number of policies and rolling mean of the number
# for each day between start and end dates
for(state in states){
  print(state)
  print("---------------------")
  ftime.state <- ftime %>% filter(geo_value == state)
  policy.state <- policy %>% filter(StatePostal == state)
  policy_signal.state <- getSumOfPolicy(policy.state, STARTDATE, ENDDATE)
  # left join mobility with policy signal by time 
  ftime.policy.df <- left_join(ftime.state , policy_signal.state, by = "time_value")
  lm.fit <- lm(value~time_value*BarRestrict,data=ftime.policy.df)
  print(summary(lm.fit))
}
```

```
## [1] "ak"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * BarRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.024157 -0.009866 -0.000590  0.008085  0.049688 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)            -2.094e-01  4.912e-02  -4.263 2.30e-05 ***
## time_value              1.424e-05  2.696e-06   5.283 1.72e-07 ***
## BarRestrict            -4.531e-01  1.584e+00  -0.286    0.775    
## time_value:BarRestrict  2.431e-05  8.620e-05   0.282    0.778    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.01333 on 673 degrees of freedom
## Multiple R-squared:  0.04844,	Adjusted R-squared:  0.04419 
## F-statistic: 11.42 on 3 and 673 DF,  p-value: 2.603e-07
## 
## [1] "al"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * BarRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.054702 -0.020943  0.003719  0.018625  0.059624 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)            -9.120e-01  1.636e-01  -5.575 3.59e-08 ***
## time_value              5.361e-05  9.029e-06   5.937 4.65e-09 ***
## BarRestrict            -1.701e+00  4.679e-01  -3.636 0.000299 ***
## time_value:BarRestrict  9.048e-05  2.541e-05   3.561 0.000395 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.02438 on 673 degrees of freedom
## Multiple R-squared:  0.1455,	Adjusted R-squared:  0.1417 
## F-statistic:  38.2 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "ar"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * BarRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.044473 -0.020696  0.004237  0.017885  0.046803 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)            -7.696e-01  1.504e-01  -5.117 4.06e-07 ***
## time_value              4.558e-05  8.301e-06   5.490 5.69e-08 ***
## BarRestrict            -1.442e+00  4.302e-01  -3.352 0.000848 ***
## time_value:BarRestrict  7.668e-05  2.336e-05   3.282 0.001082 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.02242 on 673 degrees of freedom
## Multiple R-squared:  0.128,	Adjusted R-squared:  0.1241 
## F-statistic: 32.92 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "az"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * BarRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.033596 -0.013210  0.002789  0.011723  0.044827 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)            -3.358e-01  1.019e-01  -3.295 0.001036 ** 
## time_value              2.129e-05  5.623e-06   3.786 0.000167 ***
## BarRestrict            -7.585e-01  3.242e-01  -2.340 0.019580 *  
## time_value:BarRestrict  4.020e-05  1.759e-05   2.285 0.022607 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.01581 on 673 degrees of freedom
## Multiple R-squared:  0.1034,	Adjusted R-squared:  0.0994 
## F-statistic: 25.87 on 3 and 673 DF,  p-value: 7.575e-16
## 
## [1] "ca"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * BarRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.034534 -0.012354  0.003733  0.011387  0.028814 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)            -3.068e-01  1.037e-01  -2.960 0.003187 ** 
## time_value              1.976e-05  5.722e-06   3.454 0.000587 ***
## BarRestrict            -5.610e-01  2.890e-01  -1.941 0.052654 .  
## time_value:BarRestrict  2.965e-05  1.570e-05   1.889 0.059277 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.0153 on 673 degrees of freedom
## Multiple R-squared:  0.07596,	Adjusted R-squared:  0.07184 
## F-statistic: 18.44 on 3 and 673 DF,  p-value: 1.657e-11
## 
## [1] "co"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * BarRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.041660 -0.014137  0.003602  0.014433  0.041623 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)            -5.753e-01  1.298e-01  -4.433 1.09e-05 ***
## time_value              3.497e-05  7.163e-06   4.881 1.32e-06 ***
## BarRestrict            -7.953e-01  3.649e-01  -2.180   0.0296 *  
## time_value:BarRestrict  4.175e-05  1.982e-05   2.107   0.0355 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.01922 on 673 degrees of freedom
## Multiple R-squared:  0.1405,	Adjusted R-squared:  0.1366 
## F-statistic: 36.66 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "ct"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * BarRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.050213 -0.017160  0.002498  0.019499  0.050938 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)            -7.618e-01  1.689e-01  -4.509 7.67e-06 ***
## time_value              4.557e-05  9.325e-06   4.887 1.28e-06 ***
## BarRestrict            -7.121e-01  4.710e-01  -1.512    0.131    
## time_value:BarRestrict  3.655e-05  2.558e-05   1.429    0.154    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.02493 on 673 degrees of freedom
## Multiple R-squared:  0.1889,	Adjusted R-squared:  0.1853 
## F-statistic: 52.24 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "dc"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * BarRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.035311 -0.007202  0.001442  0.010093  0.041163 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)            -7.560e-01  1.125e-01  -6.720 3.86e-11 ***
## time_value              4.462e-05  6.209e-06   7.185 1.78e-12 ***
## BarRestrict             1.878e-02  3.136e-01   0.060    0.952    
## time_value:BarRestrict -2.345e-06  1.703e-05  -0.138    0.891    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.0166 on 673 degrees of freedom
## Multiple R-squared:  0.1381,	Adjusted R-squared:  0.1343 
## F-statistic: 35.96 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "de"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * BarRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.042956 -0.014172  0.003767  0.014745  0.042128 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)            -7.406e-01  1.381e-01  -5.362 1.13e-07 ***
## time_value              4.400e-05  7.624e-06   5.771 1.20e-08 ***
## BarRestrict            -1.975e-01  3.851e-01  -0.513    0.608    
## time_value:BarRestrict  8.980e-06  2.091e-05   0.429    0.668    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.02038 on 673 degrees of freedom
## Multiple R-squared:  0.1786,	Adjusted R-squared:  0.1749 
## F-statistic: 48.77 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "fl"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * BarRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.037218 -0.017960  0.005048  0.012582  0.035991 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)            -2.844e-01  8.722e-02  -3.260 0.001168 ** 
## time_value              1.841e-05  4.804e-06   3.832 0.000139 ***
## BarRestrict            -1.140e+00  4.453e-01  -2.561 0.010650 *  
## time_value:BarRestrict  6.099e-05  2.417e-05   2.523 0.011861 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.01834 on 673 degrees of freedom
## Multiple R-squared:  0.1025,	Adjusted R-squared:  0.09853 
## F-statistic: 25.63 on 3 and 673 DF,  p-value: 1.044e-15
## 
## [1] "ga"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * BarRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.040910 -0.020509  0.004988  0.016781  0.040865 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)            -5.052e-01  1.421e-01  -3.556 0.000402 ***
## time_value              3.074e-05  7.839e-06   3.922 9.69e-05 ***
## BarRestrict            -1.525e+00  4.244e-01  -3.593 0.000351 ***
## time_value:BarRestrict  8.143e-05  2.304e-05   3.534 0.000437 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.02153 on 673 degrees of freedom
## Multiple R-squared:  0.1105,	Adjusted R-squared:  0.1065 
## F-statistic: 27.87 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "hi"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * BarRestrict, data = ftime.policy.df)
## 
## Residuals:
##        Min         1Q     Median         3Q        Max 
## -0.0122371 -0.0037473  0.0004605  0.0036079  0.0158151 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)            -2.548e-01  3.472e-02  -7.338 6.28e-13 ***
## time_value              1.558e-05  1.917e-06   8.129 2.09e-15 ***
## BarRestrict            -1.156e-01  9.762e-02  -1.184    0.237    
## time_value:BarRestrict  6.040e-06  5.302e-06   1.139    0.255    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.005141 on 673 degrees of freedom
## Multiple R-squared:  0.1216,	Adjusted R-squared:  0.1177 
## F-statistic: 31.07 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "ia"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * BarRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.060955 -0.025076  0.003689  0.024380  0.061472 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)            -1.298e+00  2.020e-01  -6.424 2.52e-10 ***
## time_value              7.542e-05  1.115e-05   6.765 2.91e-11 ***
## BarRestrict            -1.746e+00  5.679e-01  -3.075  0.00219 ** 
## time_value:BarRestrict  9.207e-05  3.085e-05   2.985  0.00294 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.02991 on 673 degrees of freedom
## Multiple R-squared:  0.1918,	Adjusted R-squared:  0.1882 
## F-statistic: 53.24 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "id"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * BarRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.039247 -0.016400  0.003993  0.013700  0.039325 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)            -4.805e-01  1.170e-01  -4.106 4.52e-05 ***
## time_value              2.945e-05  6.457e-06   4.561 6.04e-06 ***
## BarRestrict            -1.025e+00  3.527e-01  -2.905  0.00379 ** 
## time_value:BarRestrict  5.462e-05  1.914e-05   2.853  0.00446 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.01779 on 673 degrees of freedom
## Multiple R-squared:  0.08493,	Adjusted R-squared:  0.08085 
## F-statistic: 20.82 on 3 and 673 DF,  p-value: 6.555e-13
## 
## [1] "il"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * BarRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.053548 -0.016150  0.002711  0.016111  0.057910 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)            -1.152e+00  1.699e-01  -6.780 2.64e-11 ***
## time_value              6.711e-05  9.376e-06   7.158 2.15e-12 ***
## BarRestrict            -2.565e-01  4.735e-01  -0.542    0.588    
## time_value:BarRestrict  1.133e-05  2.572e-05   0.441    0.660    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.02506 on 673 degrees of freedom
## Multiple R-squared:  0.2379,	Adjusted R-squared:  0.2345 
## F-statistic: 70.02 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "in"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * BarRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.042840 -0.018423  0.004236  0.016388  0.039710 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)            -7.752e-01  1.382e-01  -5.608  3.0e-08 ***
## time_value              4.575e-05  7.631e-06   5.996  3.3e-09 ***
## BarRestrict            -1.043e+00  3.854e-01  -2.707  0.00696 ** 
## time_value:BarRestrict  5.505e-05  2.093e-05   2.630  0.00874 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.0204 on 673 degrees of freedom
## Multiple R-squared:  0.1451,	Adjusted R-squared:  0.1413 
## F-statistic: 38.07 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "ks"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * BarRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.042767 -0.024601 -0.003248  0.018805  0.066957 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)
## (Intercept)             1.501e-01  9.826e-02   1.528    0.127
## time_value             -5.110e-06  5.392e-06  -0.948    0.344
## BarRestrict            -8.969e-01  3.850e+00  -0.233    0.816
## time_value:BarRestrict  4.761e-05  2.095e-04   0.227    0.820
## 
## Residual standard error: 0.0267 on 673 degrees of freedom
## Multiple R-squared:  0.05514,	Adjusted R-squared:  0.05093 
## F-statistic: 13.09 on 3 and 673 DF,  p-value: 2.56e-08
## 
## [1] "ky"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * BarRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.048063 -0.018784  0.003916  0.018701  0.045893 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)            -9.321e-01  1.563e-01  -5.965 3.96e-09 ***
## time_value              5.462e-05  8.626e-06   6.333 4.41e-10 ***
## BarRestrict            -5.102e-01  4.356e-01  -1.171    0.242    
## time_value:BarRestrict  2.577e-05  2.366e-05   1.089    0.277    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.02306 on 673 degrees of freedom
## Multiple R-squared:  0.1599,	Adjusted R-squared:  0.1561 
## F-statistic: 42.68 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "la"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * BarRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.040005 -0.017859  0.003537  0.015898  0.043926 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)            -8.795e-01  1.372e-01  -6.409 2.76e-10 ***
## time_value              5.169e-05  7.575e-06   6.824 1.97e-11 ***
## BarRestrict            -1.129e+00  3.858e-01  -2.926  0.00354 ** 
## time_value:BarRestrict  5.968e-05  2.096e-05   2.848  0.00453 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.02032 on 673 degrees of freedom
## Multiple R-squared:  0.1485,	Adjusted R-squared:  0.1447 
## F-statistic: 39.13 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "ma"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * BarRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.045112 -0.014374  0.001635  0.017129  0.046951 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)            -6.756e-01  1.525e-01  -4.429 1.10e-05 ***
## time_value              4.074e-05  8.418e-06   4.840 1.61e-06 ***
## BarRestrict            -5.262e-01  4.288e-01  -1.227    0.220    
## time_value:BarRestrict  2.669e-05  2.329e-05   1.146    0.252    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.02258 on 673 degrees of freedom
## Multiple R-squared:  0.1841,	Adjusted R-squared:  0.1805 
## F-statistic: 50.63 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "md"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * BarRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.046350 -0.013070  0.003793  0.015348  0.042190 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)            -7.867e-01  1.505e-01  -5.227 2.30e-07 ***
## time_value              4.670e-05  8.307e-06   5.622 2.77e-08 ***
## BarRestrict            -1.949e-01  4.196e-01  -0.464    0.642    
## time_value:BarRestrict  8.567e-06  2.279e-05   0.376    0.707    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.02221 on 673 degrees of freedom
## Multiple R-squared:  0.2066,	Adjusted R-squared:  0.2031 
## F-statistic: 58.41 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "me"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * BarRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.038021 -0.016073  0.001985  0.014755  0.047114 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)            -6.187e-01  1.282e-01  -4.827 1.71e-06 ***
## time_value              3.734e-05  7.074e-06   5.279 1.75e-07 ***
## BarRestrict            -4.529e-01  3.634e-01  -1.246    0.213    
## time_value:BarRestrict  2.320e-05  1.974e-05   1.175    0.240    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.01904 on 673 degrees of freedom
## Multiple R-squared:  0.1308,	Adjusted R-squared:  0.127 
## F-statistic: 33.77 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "mi"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * BarRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.038587 -0.014453  0.001388  0.014769  0.045932 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)            -3.134e-01  1.006e-01  -3.116 0.001912 ** 
## time_value              2.032e-05  5.542e-06   3.668 0.000264 ***
## BarRestrict            -1.228e+00  4.676e-01  -2.627 0.008807 ** 
## time_value:BarRestrict  6.548e-05  2.538e-05   2.580 0.010091 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.02037 on 673 degrees of freedom
## Multiple R-squared:  0.1354,	Adjusted R-squared:  0.1315 
## F-statistic: 35.12 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "mn"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * BarRestrict, data = ftime.policy.df)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.05225 -0.01708  0.00211  0.01805  0.05508 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)            -1.140e+00  1.732e-01  -6.583 9.30e-11 ***
## time_value              6.652e-05  9.557e-06   6.960 8.09e-12 ***
## BarRestrict            -1.723e-01  4.868e-01  -0.354    0.723    
## time_value:BarRestrict  6.741e-06  2.644e-05   0.255    0.799    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.02564 on 673 degrees of freedom
## Multiple R-squared:  0.2357,	Adjusted R-squared:  0.2323 
## F-statistic: 69.18 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "mo"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * BarRestrict, data = ftime.policy.df)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.05447 -0.02198  0.00383  0.01987  0.05586 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)            -9.471e-01  1.723e-01  -5.495 5.54e-08 ***
## time_value              5.578e-05  9.511e-06   5.865 7.05e-09 ***
## BarRestrict            -1.580e+00  5.104e-01  -3.095  0.00205 ** 
## time_value:BarRestrict  8.359e-05  2.771e-05   3.017  0.00265 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.02603 on 673 degrees of freedom
## Multiple R-squared:  0.1658,	Adjusted R-squared:  0.1621 
## F-statistic: 44.58 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "ms"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * BarRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.043422 -0.021264  0.004372  0.017756  0.045205 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)            -6.196e-01  1.470e-01  -4.216 2.82e-05 ***
## time_value              3.727e-05  8.110e-06   4.595 5.17e-06 ***
## BarRestrict            -1.793e+00  4.391e-01  -4.084 4.97e-05 ***
## time_value:BarRestrict  9.596e-05  2.384e-05   4.026 6.31e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.02227 on 673 degrees of freedom
## Multiple R-squared:  0.1071,	Adjusted R-squared:  0.1031 
## F-statistic:  26.9 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "mt"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * BarRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.036836 -0.015069  0.002754  0.013072  0.038411 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)            -5.689e-01  1.138e-01  -5.001 7.29e-07 ***
## time_value              3.461e-05  6.279e-06   5.512 5.07e-08 ***
## BarRestrict            -6.767e-01  3.282e-01  -2.062   0.0396 *  
## time_value:BarRestrict  3.570e-05  1.782e-05   2.003   0.0456 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.01701 on 673 degrees of freedom
## Multiple R-squared:  0.09105,	Adjusted R-squared:  0.087 
## F-statistic: 22.47 on 3 and 673 DF,  p-value: 7.102e-14
## 
## [1] "nc"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * BarRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.039575 -0.017283  0.004611  0.014811  0.039518 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)            -5.573e-01  1.313e-01  -4.246 2.49e-05 ***
## time_value              3.354e-05  7.244e-06   4.630 4.38e-06 ***
## BarRestrict            -6.680e-01  3.690e-01  -1.810   0.0707 .  
## time_value:BarRestrict  3.501e-05  2.004e-05   1.747   0.0811 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.01943 on 673 degrees of freedom
## Multiple R-squared:  0.1056,	Adjusted R-squared:  0.1016 
## F-statistic: 26.49 on 3 and 673 DF,  p-value: 3.319e-16
## 
## [1] "nd"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * BarRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.040950 -0.025181 -0.005945  0.019199  0.076035 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)  
## (Intercept)             1.955e-01  1.047e-01   1.867   0.0624 .
## time_value             -7.459e-06  5.747e-06  -1.298   0.1947  
## BarRestrict             7.618e-01  3.538e+00   0.215   0.8296  
## time_value:BarRestrict -4.254e-05  1.926e-04  -0.221   0.8252  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.02845 on 673 degrees of freedom
## Multiple R-squared:  0.04647,	Adjusted R-squared:  0.04222 
## F-statistic: 10.93 on 3 and 673 DF,  p-value: 5.116e-07
## 
## [1] "ne"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * BarRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.055302 -0.025777  0.001748  0.021831  0.067621 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)            -4.638e-01  1.306e-01  -3.550 0.000412 ***
## time_value              2.911e-05  7.190e-06   4.049 5.75e-05 ***
## BarRestrict            -2.625e+00  8.918e-01  -2.943 0.003363 ** 
## time_value:BarRestrict  1.408e-04  4.839e-05   2.909 0.003747 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.02928 on 673 degrees of freedom
## Multiple R-squared:  0.1273,	Adjusted R-squared:  0.1234 
## F-statistic: 32.73 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "nh"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * BarRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.043611 -0.016199  0.001539  0.017494  0.060825 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)
## (Intercept)             1.458e-01  1.080e-01   1.351    0.177
## time_value             -4.627e-06  5.939e-06  -0.779    0.436
## BarRestrict            -6.511e-01  8.246e-01  -0.790    0.430
## time_value:BarRestrict  3.417e-05  4.479e-05   0.763    0.446
## 
## Residual standard error: 0.02569 on 673 degrees of freedom
## Multiple R-squared:  0.1272,	Adjusted R-squared:  0.1233 
## F-statistic: 32.69 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "nj"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * BarRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.053596 -0.015873  0.002651  0.021972  0.054609 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)            -7.736e-01  1.842e-01  -4.199 3.04e-05 ***
## time_value              4.637e-05  1.017e-05   4.560 6.08e-06 ***
## BarRestrict            -6.711e-01  5.135e-01  -1.307    0.192    
## time_value:BarRestrict  3.401e-05  2.789e-05   1.219    0.223    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.02718 on 673 degrees of freedom
## Multiple R-squared:  0.2204,	Adjusted R-squared:  0.2169 
## F-statistic: 63.41 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "nm"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * BarRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.028080 -0.009430  0.002061  0.008308  0.029982 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)            -4.615e-01  8.048e-02  -5.735 1.48e-08 ***
## time_value              2.819e-05  4.442e-06   6.347 4.03e-10 ***
## BarRestrict            -2.362e-01  2.302e-01  -1.026    0.305    
## time_value:BarRestrict  1.202e-05  1.250e-05   0.962    0.336    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.012 on 673 degrees of freedom
## Multiple R-squared:  0.1017,	Adjusted R-squared:  0.09773 
## F-statistic: 25.41 on 3 and 673 DF,  p-value: 1.402e-15
## 
## [1] "nv"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * BarRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.029430 -0.011550  0.003415  0.009343  0.034449 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)            -4.340e-01  8.775e-02  -4.946 9.59e-07 ***
## time_value              2.647e-05  4.843e-06   5.466 6.50e-08 ***
## BarRestrict            -6.631e-01  2.553e-01  -2.597  0.00962 ** 
## time_value:BarRestrict  3.512e-05  1.387e-05   2.533  0.01153 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.01317 on 673 degrees of freedom
## Multiple R-squared:  0.1091,	Adjusted R-squared:  0.1052 
## F-statistic: 27.49 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "ny"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * BarRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.042965 -0.013417  0.001414  0.016665  0.050083 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)            -7.822e-01  1.447e-01  -5.405 9.01e-08 ***
## time_value              4.651e-05  7.988e-06   5.822 8.99e-09 ***
## BarRestrict            -7.390e-01  4.034e-01  -1.832   0.0674 .  
## time_value:BarRestrict  3.827e-05  2.191e-05   1.747   0.0812 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.02135 on 673 degrees of freedom
## Multiple R-squared:  0.1797,	Adjusted R-squared:  0.176 
## F-statistic: 49.13 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "oh"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * BarRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.047705 -0.018116  0.002821  0.017686  0.045161 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)            -8.861e-01  1.541e-01  -5.749 1.36e-08 ***
## time_value              5.214e-05  8.508e-06   6.128 1.51e-09 ***
## BarRestrict            -9.543e-01  4.260e-01  -2.240   0.0254 *  
## time_value:BarRestrict  4.992e-05  2.314e-05   2.157   0.0313 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.02267 on 673 degrees of freedom
## Multiple R-squared:  0.1622,	Adjusted R-squared:  0.1584 
## F-statistic: 43.42 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "ok"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * BarRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.035792 -0.021598  0.000209  0.015340  0.059620 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)
## (Intercept)             3.952e-02  8.455e-02   0.467    0.640
## time_value              7.243e-07  4.640e-06   0.156    0.876
## BarRestrict            -4.032e-01  2.987e+00  -0.135    0.893
## time_value:BarRestrict  2.094e-05  1.625e-04   0.129    0.898
## 
## Residual standard error: 0.02289 on 673 degrees of freedom
## Multiple R-squared:  0.04988,	Adjusted R-squared:  0.04564 
## F-statistic: 11.78 on 3 and 673 DF,  p-value: 1.585e-07
## 
## [1] "or"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * BarRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.033939 -0.012434  0.003175  0.010746  0.039576 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)            -3.683e-01  1.079e-01  -3.414 0.000678 ***
## time_value              2.311e-05  5.954e-06   3.881 0.000114 ***
## BarRestrict            -2.937e-01  3.033e-01  -0.968 0.333251    
## time_value:BarRestrict  1.495e-05  1.647e-05   0.908 0.364318    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.01597 on 673 degrees of freedom
## Multiple R-squared:  0.106,	Adjusted R-squared:  0.1021 
## F-statistic: 26.61 on 3 and 673 DF,  p-value: 2.834e-16
## 
## [1] "pa"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * BarRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.045659 -0.015330  0.003066  0.016833  0.045006 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)            -8.633e-01  1.455e-01  -5.933 4.76e-09 ***
## time_value              5.089e-05  8.031e-06   6.336 4.31e-10 ***
## BarRestrict            -7.367e-01  4.091e-01  -1.801   0.0722 .  
## time_value:BarRestrict  3.809e-05  2.222e-05   1.714   0.0869 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.02154 on 673 degrees of freedom
## Multiple R-squared:  0.1811,	Adjusted R-squared:  0.1774 
## F-statistic: 49.61 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "ri"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * BarRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.042556 -0.014530  0.002094  0.016566  0.043991 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)            -7.972e-01  1.407e-01  -5.664 2.19e-08 ***
## time_value              4.727e-05  7.768e-06   6.085 1.95e-09 ***
## BarRestrict            -2.575e-01  3.957e-01  -0.651    0.515    
## time_value:BarRestrict  1.225e-05  2.149e-05   0.570    0.569    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.02084 on 673 degrees of freedom
## Multiple R-squared:  0.1617,	Adjusted R-squared:  0.1579 
## F-statistic: 43.26 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "sc"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * BarRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.037606 -0.018620  0.005056  0.014748  0.032723 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)            -5.610e-01  1.254e-01  -4.474 9.00e-06 ***
## time_value              3.370e-05  6.920e-06   4.871 1.39e-06 ***
## BarRestrict            -1.037e+00  3.555e-01  -2.916  0.00367 ** 
## time_value:BarRestrict  5.514e-05  1.931e-05   2.856  0.00442 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.01862 on 673 degrees of freedom
## Multiple R-squared:  0.09733,	Adjusted R-squared:  0.0933 
## F-statistic: 24.19 on 3 and 673 DF,  p-value: 7.122e-15
## 
## [1] "sd"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * BarRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.036192 -0.023699 -0.004966  0.019513  0.069966 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)
## (Intercept)             2.343e-02  9.310e-02   0.252    0.801
## time_value              1.841e-06  5.107e-06   0.360    0.719
## BarRestrict             2.793e+00  1.487e+01   0.188    0.851
## time_value:BarRestrict -1.530e-04  8.095e-04  -0.189    0.850
## 
## Residual standard error: 0.02575 on 673 degrees of freedom
## Multiple R-squared:  0.01512,	Adjusted R-squared:  0.01073 
## F-statistic: 3.443 on 3 and 673 DF,  p-value: 0.01649
## 
## [1] "tn"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * BarRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.041629 -0.020979  0.003643  0.016627  0.046567 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)            -2.354e-01  1.069e-01  -2.202 0.027982 *  
## time_value              1.595e-05  5.888e-06   2.708 0.006939 ** 
## BarRestrict            -1.896e+00  5.340e-01  -3.551 0.000411 ***
## time_value:BarRestrict  1.019e-04  2.898e-05   3.515 0.000469 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.02196 on 673 degrees of freedom
## Multiple R-squared:  0.1004,	Adjusted R-squared:  0.09637 
## F-statistic: 25.03 on 3 and 673 DF,  p-value: 2.314e-15
## 
## [1] "tx"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * BarRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.043979 -0.018127  0.003147  0.014394  0.044874 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)            -7.736e-01  1.423e-01  -5.435 7.67e-08 ***
## time_value              4.573e-05  7.856e-06   5.821 9.08e-09 ***
## BarRestrict            -1.096e+00  4.142e-01  -2.646  0.00834 ** 
## time_value:BarRestrict  5.770e-05  2.249e-05   2.565  0.01052 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.02136 on 673 degrees of freedom
## Multiple R-squared:  0.1704,	Adjusted R-squared:  0.1667 
## F-statistic: 46.09 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "ut"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * BarRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.044234 -0.018794  0.005849  0.015499  0.041181 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)            -4.238e-01  1.398e-01  -3.031 0.002534 ** 
## time_value              2.660e-05  7.718e-06   3.447 0.000603 ***
## BarRestrict            -9.180e-01  4.000e-01  -2.295 0.022031 *  
## time_value:BarRestrict  4.866e-05  2.172e-05   2.240 0.025402 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.02084 on 673 degrees of freedom
## Multiple R-squared:  0.09115,	Adjusted R-squared:  0.0871 
## F-statistic:  22.5 on 3 and 673 DF,  p-value: 6.836e-14
## 
## [1] "va"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * BarRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.042912 -0.017487  0.004461  0.015734  0.044042 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)            -5.710e-01  1.409e-01  -4.052 5.67e-05 ***
## time_value              3.463e-05  7.775e-06   4.453 9.91e-06 ***
## BarRestrict            -6.989e-01  4.247e-01  -1.646    0.100    
## time_value:BarRestrict  3.632e-05  2.305e-05   1.575    0.116    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.02143 on 673 degrees of freedom
## Multiple R-squared:  0.1539,	Adjusted R-squared:  0.1501 
## F-statistic: 40.81 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "vt"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * BarRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.041099 -0.013628  0.001977  0.013091  0.049719 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)            -8.828e-01  1.268e-01  -6.960 8.07e-12 ***
## time_value              5.201e-05  7.000e-06   7.430 3.31e-13 ***
## BarRestrict            -3.547e-01  3.566e-01  -0.995    0.320    
## time_value:BarRestrict  1.764e-05  1.937e-05   0.911    0.363    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.01878 on 673 degrees of freedom
## Multiple R-squared:  0.1604,	Adjusted R-squared:  0.1567 
## F-statistic: 42.87 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "wa"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * BarRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.037408 -0.012929  0.003457  0.012421  0.037585 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)            -3.605e-01  1.182e-01  -3.050 0.002375 ** 
## time_value              2.278e-05  6.523e-06   3.493 0.000509 ***
## BarRestrict            -5.621e-01  3.294e-01  -1.706 0.088436 .  
## time_value:BarRestrict  2.932e-05  1.789e-05   1.639 0.101747    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.01744 on 673 degrees of freedom
## Multiple R-squared:  0.1409,	Adjusted R-squared:  0.137 
## F-statistic: 36.78 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "wi"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * BarRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.045042 -0.026365 -0.002078  0.016772  0.072723 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)   
## (Intercept)             3.536e-01  1.075e-01   3.291  0.00105 **
## time_value             -1.608e-05  5.896e-06  -2.728  0.00654 **
## BarRestrict             1.837e+00  4.227e+00   0.435  0.66401   
## time_value:BarRestrict -1.011e-04  2.301e-04  -0.439  0.66050   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.02933 on 673 degrees of freedom
## Multiple R-squared:  0.05567,	Adjusted R-squared:  0.05146 
## F-statistic: 13.22 on 3 and 673 DF,  p-value: 2.136e-08
## 
## [1] "wv"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * BarRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.044452 -0.016753  0.003098  0.014758  0.042602 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)            -9.554e-01  1.346e-01  -7.097 3.24e-12 ***
## time_value              5.583e-05  7.430e-06   7.514 1.83e-13 ***
## BarRestrict            -5.516e-01  3.817e-01  -1.445    0.149    
## time_value:BarRestrict  2.825e-05  2.073e-05   1.362    0.174    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.02 on 673 degrees of freedom
## Multiple R-squared:  0.157,	Adjusted R-squared:  0.1532 
## F-statistic: 41.77 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "wy"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * BarRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.037044 -0.015010  0.001662  0.013544  0.038762 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)            -5.189e-01  1.103e-01  -4.705 3.08e-06 ***
## time_value              3.146e-05  6.086e-06   5.169 3.10e-07 ***
## BarRestrict            -1.173e+00  3.154e-01  -3.718 0.000217 ***
## time_value:BarRestrict  6.267e-05  1.713e-05   3.659 0.000273 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.01644 on 673 degrees of freedom
## Multiple R-squared:  0.1027,	Adjusted R-squared:  0.09869 
## F-statistic: 25.67 on 3 and 673 DF,  p-value: 9.849e-16
```

#### Restaurant Restriction


```r
# compute the number of policies and rolling mean of the number
# for each day between start and end dates
for(state in states){
  print(state)
  print("---------------------")
  ftime.state <- ftime %>% filter(geo_value == state)
  policy.state <- policy %>% filter(StatePostal == state)
  policy_signal.state <- getSumOfPolicy(policy.state, STARTDATE, ENDDATE)
  # left join mobility with policy signal by time 
  ftime.policy.df <- left_join(ftime.state , policy_signal.state, by = "time_value")
  lm.fit <- lm(value~time_value*RestaurantRestrict,data=ftime.policy.df)
  print(summary(lm.fit))
}
```

```
## [1] "ak"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * RestaurantRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.024157 -0.009866 -0.000590  0.008085  0.049688 
## 
## Coefficients:
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                   -2.094e-01  4.912e-02  -4.263 2.30e-05 ***
## time_value                     1.424e-05  2.696e-06   5.283 1.72e-07 ***
## RestaurantRestrict            -4.531e-01  1.584e+00  -0.286    0.775    
## time_value:RestaurantRestrict  2.431e-05  8.620e-05   0.282    0.778    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.01333 on 673 degrees of freedom
## Multiple R-squared:  0.04844,	Adjusted R-squared:  0.04419 
## F-statistic: 11.42 on 3 and 673 DF,  p-value: 2.603e-07
## 
## [1] "al"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * RestaurantRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.054702 -0.020943  0.003719  0.018625  0.059624 
## 
## Coefficients:
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                   -9.120e-01  1.636e-01  -5.575 3.59e-08 ***
## time_value                     5.361e-05  9.029e-06   5.937 4.65e-09 ***
## RestaurantRestrict            -1.701e+00  4.679e-01  -3.636 0.000299 ***
## time_value:RestaurantRestrict  9.048e-05  2.541e-05   3.561 0.000395 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.02438 on 673 degrees of freedom
## Multiple R-squared:  0.1455,	Adjusted R-squared:  0.1417 
## F-statistic:  38.2 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "ar"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * RestaurantRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.044473 -0.020696  0.004237  0.017885  0.046803 
## 
## Coefficients:
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                   -7.696e-01  1.504e-01  -5.117 4.06e-07 ***
## time_value                     4.558e-05  8.301e-06   5.490 5.69e-08 ***
## RestaurantRestrict            -1.442e+00  4.302e-01  -3.352 0.000848 ***
## time_value:RestaurantRestrict  7.668e-05  2.336e-05   3.282 0.001082 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.02242 on 673 degrees of freedom
## Multiple R-squared:  0.128,	Adjusted R-squared:  0.1241 
## F-statistic: 32.92 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "az"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * RestaurantRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.033596 -0.013210  0.002789  0.011723  0.044827 
## 
## Coefficients:
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                   -3.358e-01  1.019e-01  -3.295 0.001036 ** 
## time_value                     2.129e-05  5.623e-06   3.786 0.000167 ***
## RestaurantRestrict            -7.585e-01  3.242e-01  -2.340 0.019580 *  
## time_value:RestaurantRestrict  4.020e-05  1.759e-05   2.285 0.022607 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.01581 on 673 degrees of freedom
## Multiple R-squared:  0.1034,	Adjusted R-squared:  0.0994 
## F-statistic: 25.87 on 3 and 673 DF,  p-value: 7.575e-16
## 
## [1] "ca"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * RestaurantRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.034534 -0.012354  0.003733  0.011387  0.028814 
## 
## Coefficients:
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                   -3.068e-01  1.037e-01  -2.960 0.003187 ** 
## time_value                     1.976e-05  5.722e-06   3.454 0.000587 ***
## RestaurantRestrict            -5.610e-01  2.890e-01  -1.941 0.052654 .  
## time_value:RestaurantRestrict  2.965e-05  1.570e-05   1.889 0.059277 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.0153 on 673 degrees of freedom
## Multiple R-squared:  0.07596,	Adjusted R-squared:  0.07184 
## F-statistic: 18.44 on 3 and 673 DF,  p-value: 1.657e-11
## 
## [1] "co"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * RestaurantRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.041660 -0.014137  0.003602  0.014433  0.041623 
## 
## Coefficients:
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                   -5.753e-01  1.298e-01  -4.433 1.09e-05 ***
## time_value                     3.497e-05  7.163e-06   4.881 1.32e-06 ***
## RestaurantRestrict            -7.953e-01  3.649e-01  -2.180   0.0296 *  
## time_value:RestaurantRestrict  4.175e-05  1.982e-05   2.107   0.0355 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.01922 on 673 degrees of freedom
## Multiple R-squared:  0.1405,	Adjusted R-squared:  0.1366 
## F-statistic: 36.66 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "ct"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * RestaurantRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.050213 -0.017160  0.002498  0.019499  0.050938 
## 
## Coefficients:
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                   -7.618e-01  1.689e-01  -4.509 7.67e-06 ***
## time_value                     4.557e-05  9.325e-06   4.887 1.28e-06 ***
## RestaurantRestrict            -7.121e-01  4.710e-01  -1.512    0.131    
## time_value:RestaurantRestrict  3.655e-05  2.558e-05   1.429    0.154    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.02493 on 673 degrees of freedom
## Multiple R-squared:  0.1889,	Adjusted R-squared:  0.1853 
## F-statistic: 52.24 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "dc"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * RestaurantRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.035311 -0.007202  0.001442  0.010093  0.041163 
## 
## Coefficients:
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                   -7.560e-01  1.125e-01  -6.720 3.86e-11 ***
## time_value                     4.462e-05  6.209e-06   7.185 1.78e-12 ***
## RestaurantRestrict             1.878e-02  3.136e-01   0.060    0.952    
## time_value:RestaurantRestrict -2.345e-06  1.703e-05  -0.138    0.891    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.0166 on 673 degrees of freedom
## Multiple R-squared:  0.1381,	Adjusted R-squared:  0.1343 
## F-statistic: 35.96 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "de"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * RestaurantRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.042956 -0.014172  0.003767  0.014745  0.042128 
## 
## Coefficients:
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                   -7.406e-01  1.381e-01  -5.362 1.13e-07 ***
## time_value                     4.400e-05  7.624e-06   5.771 1.20e-08 ***
## RestaurantRestrict            -1.975e-01  3.851e-01  -0.513    0.608    
## time_value:RestaurantRestrict  8.980e-06  2.091e-05   0.429    0.668    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.02038 on 673 degrees of freedom
## Multiple R-squared:  0.1786,	Adjusted R-squared:  0.1749 
## F-statistic: 48.77 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "fl"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * RestaurantRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.037218 -0.017960  0.005048  0.012582  0.035991 
## 
## Coefficients:
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                   -2.844e-01  8.722e-02  -3.260 0.001168 ** 
## time_value                     1.841e-05  4.804e-06   3.832 0.000139 ***
## RestaurantRestrict            -1.140e+00  4.453e-01  -2.561 0.010650 *  
## time_value:RestaurantRestrict  6.099e-05  2.417e-05   2.523 0.011861 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.01834 on 673 degrees of freedom
## Multiple R-squared:  0.1025,	Adjusted R-squared:  0.09853 
## F-statistic: 25.63 on 3 and 673 DF,  p-value: 1.044e-15
## 
## [1] "ga"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * RestaurantRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.039321 -0.021466  0.005076  0.016554  0.042570 
## 
## Coefficients:
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                   -3.738e-01  1.388e-01  -2.693 0.007250 ** 
## time_value                     2.346e-05  7.657e-06   3.064 0.002269 ** 
## RestaurantRestrict            -1.765e+00  4.537e-01  -3.891 0.000110 ***
## time_value:RestaurantRestrict  9.461e-05  2.462e-05   3.844 0.000133 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.02174 on 673 degrees of freedom
## Multiple R-squared:  0.0934,	Adjusted R-squared:  0.08935 
## F-statistic: 23.11 on 3 and 673 DF,  p-value: 3.012e-14
## 
## [1] "hi"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * RestaurantRestrict, data = ftime.policy.df)
## 
## Residuals:
##        Min         1Q     Median         3Q        Max 
## -0.0122371 -0.0037473  0.0004605  0.0036079  0.0158151 
## 
## Coefficients:
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                   -2.548e-01  3.472e-02  -7.338 6.28e-13 ***
## time_value                     1.558e-05  1.917e-06   8.129 2.09e-15 ***
## RestaurantRestrict            -1.156e-01  9.762e-02  -1.184    0.237    
## time_value:RestaurantRestrict  6.040e-06  5.302e-06   1.139    0.255    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.005141 on 673 degrees of freedom
## Multiple R-squared:  0.1216,	Adjusted R-squared:  0.1177 
## F-statistic: 31.07 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "ia"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * RestaurantRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.060955 -0.025076  0.003689  0.024380  0.061472 
## 
## Coefficients:
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                   -1.298e+00  2.020e-01  -6.424 2.52e-10 ***
## time_value                     7.542e-05  1.115e-05   6.765 2.91e-11 ***
## RestaurantRestrict            -1.746e+00  5.679e-01  -3.075  0.00219 ** 
## time_value:RestaurantRestrict  9.207e-05  3.085e-05   2.985  0.00294 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.02991 on 673 degrees of freedom
## Multiple R-squared:  0.1918,	Adjusted R-squared:  0.1882 
## F-statistic: 53.24 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "id"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * RestaurantRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.039247 -0.016400  0.003993  0.013700  0.039325 
## 
## Coefficients:
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                   -4.805e-01  1.170e-01  -4.106 4.52e-05 ***
## time_value                     2.945e-05  6.457e-06   4.561 6.04e-06 ***
## RestaurantRestrict            -1.025e+00  3.527e-01  -2.905  0.00379 ** 
## time_value:RestaurantRestrict  5.462e-05  1.914e-05   2.853  0.00446 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.01779 on 673 degrees of freedom
## Multiple R-squared:  0.08493,	Adjusted R-squared:  0.08085 
## F-statistic: 20.82 on 3 and 673 DF,  p-value: 6.555e-13
## 
## [1] "il"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * RestaurantRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.053548 -0.016150  0.002711  0.016111  0.057910 
## 
## Coefficients:
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                   -1.152e+00  1.699e-01  -6.780 2.64e-11 ***
## time_value                     6.711e-05  9.376e-06   7.158 2.15e-12 ***
## RestaurantRestrict            -2.565e-01  4.735e-01  -0.542    0.588    
## time_value:RestaurantRestrict  1.133e-05  2.572e-05   0.441    0.660    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.02506 on 673 degrees of freedom
## Multiple R-squared:  0.2379,	Adjusted R-squared:  0.2345 
## F-statistic: 70.02 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "in"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * RestaurantRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.042840 -0.018423  0.004236  0.016388  0.039710 
## 
## Coefficients:
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                   -7.752e-01  1.382e-01  -5.608  3.0e-08 ***
## time_value                     4.575e-05  7.631e-06   5.996  3.3e-09 ***
## RestaurantRestrict            -1.043e+00  3.854e-01  -2.707  0.00696 ** 
## time_value:RestaurantRestrict  5.505e-05  2.093e-05   2.630  0.00874 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.0204 on 673 degrees of freedom
## Multiple R-squared:  0.1451,	Adjusted R-squared:  0.1413 
## F-statistic: 38.07 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "ks"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * RestaurantRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.042767 -0.024601 -0.003248  0.018805  0.066957 
## 
## Coefficients:
##                                 Estimate Std. Error t value Pr(>|t|)
## (Intercept)                    1.501e-01  9.826e-02   1.528    0.127
## time_value                    -5.110e-06  5.392e-06  -0.948    0.344
## RestaurantRestrict            -8.969e-01  3.850e+00  -0.233    0.816
## time_value:RestaurantRestrict  4.761e-05  2.095e-04   0.227    0.820
## 
## Residual standard error: 0.0267 on 673 degrees of freedom
## Multiple R-squared:  0.05514,	Adjusted R-squared:  0.05093 
## F-statistic: 13.09 on 3 and 673 DF,  p-value: 2.56e-08
## 
## [1] "ky"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * RestaurantRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.048063 -0.018784  0.003916  0.018701  0.045893 
## 
## Coefficients:
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                   -9.321e-01  1.563e-01  -5.965 3.96e-09 ***
## time_value                     5.462e-05  8.626e-06   6.333 4.41e-10 ***
## RestaurantRestrict            -5.102e-01  4.356e-01  -1.171    0.242    
## time_value:RestaurantRestrict  2.577e-05  2.366e-05   1.089    0.277    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.02306 on 673 degrees of freedom
## Multiple R-squared:  0.1599,	Adjusted R-squared:  0.1561 
## F-statistic: 42.68 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "la"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * RestaurantRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.040005 -0.017859  0.003537  0.015898  0.043926 
## 
## Coefficients:
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                   -8.795e-01  1.372e-01  -6.409 2.76e-10 ***
## time_value                     5.169e-05  7.575e-06   6.824 1.97e-11 ***
## RestaurantRestrict            -1.129e+00  3.858e-01  -2.926  0.00354 ** 
## time_value:RestaurantRestrict  5.968e-05  2.096e-05   2.848  0.00453 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.02032 on 673 degrees of freedom
## Multiple R-squared:  0.1485,	Adjusted R-squared:  0.1447 
## F-statistic: 39.13 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "ma"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * RestaurantRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.045112 -0.014374  0.001635  0.017129  0.046951 
## 
## Coefficients:
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                   -6.756e-01  1.525e-01  -4.429 1.10e-05 ***
## time_value                     4.074e-05  8.418e-06   4.840 1.61e-06 ***
## RestaurantRestrict            -5.262e-01  4.288e-01  -1.227    0.220    
## time_value:RestaurantRestrict  2.669e-05  2.329e-05   1.146    0.252    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.02258 on 673 degrees of freedom
## Multiple R-squared:  0.1841,	Adjusted R-squared:  0.1805 
## F-statistic: 50.63 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "md"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * RestaurantRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.046350 -0.013070  0.003793  0.015348  0.042190 
## 
## Coefficients:
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                   -7.867e-01  1.505e-01  -5.227 2.30e-07 ***
## time_value                     4.670e-05  8.307e-06   5.622 2.77e-08 ***
## RestaurantRestrict            -1.949e-01  4.196e-01  -0.464    0.642    
## time_value:RestaurantRestrict  8.567e-06  2.279e-05   0.376    0.707    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.02221 on 673 degrees of freedom
## Multiple R-squared:  0.2066,	Adjusted R-squared:  0.2031 
## F-statistic: 58.41 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "me"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * RestaurantRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.038021 -0.016073  0.001985  0.014755  0.047114 
## 
## Coefficients:
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                   -6.187e-01  1.282e-01  -4.827 1.71e-06 ***
## time_value                     3.734e-05  7.074e-06   5.279 1.75e-07 ***
## RestaurantRestrict            -4.529e-01  3.634e-01  -1.246    0.213    
## time_value:RestaurantRestrict  2.320e-05  1.974e-05   1.175    0.240    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.01904 on 673 degrees of freedom
## Multiple R-squared:  0.1308,	Adjusted R-squared:  0.127 
## F-statistic: 33.77 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "mi"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * RestaurantRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.038587 -0.014453  0.001388  0.014769  0.045932 
## 
## Coefficients:
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                   -3.134e-01  1.006e-01  -3.116 0.001912 ** 
## time_value                     2.032e-05  5.542e-06   3.668 0.000264 ***
## RestaurantRestrict            -1.228e+00  4.676e-01  -2.627 0.008807 ** 
## time_value:RestaurantRestrict  6.548e-05  2.538e-05   2.580 0.010091 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.02037 on 673 degrees of freedom
## Multiple R-squared:  0.1354,	Adjusted R-squared:  0.1315 
## F-statistic: 35.12 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "mn"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * RestaurantRestrict, data = ftime.policy.df)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.05225 -0.01708  0.00211  0.01805  0.05508 
## 
## Coefficients:
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                   -1.140e+00  1.732e-01  -6.583 9.30e-11 ***
## time_value                     6.652e-05  9.557e-06   6.960 8.09e-12 ***
## RestaurantRestrict            -1.723e-01  4.868e-01  -0.354    0.723    
## time_value:RestaurantRestrict  6.741e-06  2.644e-05   0.255    0.799    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.02564 on 673 degrees of freedom
## Multiple R-squared:  0.2357,	Adjusted R-squared:  0.2323 
## F-statistic: 69.18 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "mo"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * RestaurantRestrict, data = ftime.policy.df)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.05447 -0.02198  0.00383  0.01987  0.05586 
## 
## Coefficients:
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                   -9.471e-01  1.723e-01  -5.495 5.54e-08 ***
## time_value                     5.578e-05  9.511e-06   5.865 7.05e-09 ***
## RestaurantRestrict            -1.580e+00  5.104e-01  -3.095  0.00205 ** 
## time_value:RestaurantRestrict  8.359e-05  2.771e-05   3.017  0.00265 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.02603 on 673 degrees of freedom
## Multiple R-squared:  0.1658,	Adjusted R-squared:  0.1621 
## F-statistic: 44.58 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "ms"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * RestaurantRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.043422 -0.021264  0.004372  0.017756  0.045205 
## 
## Coefficients:
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                   -6.196e-01  1.470e-01  -4.216 2.82e-05 ***
## time_value                     3.727e-05  8.110e-06   4.595 5.17e-06 ***
## RestaurantRestrict            -1.793e+00  4.391e-01  -4.084 4.97e-05 ***
## time_value:RestaurantRestrict  9.596e-05  2.384e-05   4.026 6.31e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.02227 on 673 degrees of freedom
## Multiple R-squared:  0.1071,	Adjusted R-squared:  0.1031 
## F-statistic:  26.9 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "mt"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * RestaurantRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.036836 -0.015069  0.002754  0.013072  0.038411 
## 
## Coefficients:
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                   -5.689e-01  1.138e-01  -5.001 7.29e-07 ***
## time_value                     3.461e-05  6.279e-06   5.512 5.07e-08 ***
## RestaurantRestrict            -6.767e-01  3.282e-01  -2.062   0.0396 *  
## time_value:RestaurantRestrict  3.570e-05  1.782e-05   2.003   0.0456 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.01701 on 673 degrees of freedom
## Multiple R-squared:  0.09105,	Adjusted R-squared:  0.087 
## F-statistic: 22.47 on 3 and 673 DF,  p-value: 7.102e-14
## 
## [1] "nc"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * RestaurantRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.039575 -0.017283  0.004611  0.014811  0.039518 
## 
## Coefficients:
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                   -5.573e-01  1.313e-01  -4.246 2.49e-05 ***
## time_value                     3.354e-05  7.244e-06   4.630 4.38e-06 ***
## RestaurantRestrict            -6.680e-01  3.690e-01  -1.810   0.0707 .  
## time_value:RestaurantRestrict  3.501e-05  2.004e-05   1.747   0.0811 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.01943 on 673 degrees of freedom
## Multiple R-squared:  0.1056,	Adjusted R-squared:  0.1016 
## F-statistic: 26.49 on 3 and 673 DF,  p-value: 3.319e-16
## 
## [1] "nd"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * RestaurantRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.040950 -0.025181 -0.005945  0.019199  0.076035 
## 
## Coefficients:
##                                 Estimate Std. Error t value Pr(>|t|)  
## (Intercept)                    1.955e-01  1.047e-01   1.867   0.0624 .
## time_value                    -7.459e-06  5.747e-06  -1.298   0.1947  
## RestaurantRestrict             7.618e-01  3.538e+00   0.215   0.8296  
## time_value:RestaurantRestrict -4.254e-05  1.926e-04  -0.221   0.8252  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.02845 on 673 degrees of freedom
## Multiple R-squared:  0.04647,	Adjusted R-squared:  0.04222 
## F-statistic: 10.93 on 3 and 673 DF,  p-value: 5.116e-07
## 
## [1] "ne"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * RestaurantRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.055302 -0.025777  0.001748  0.021831  0.067621 
## 
## Coefficients:
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                   -4.638e-01  1.306e-01  -3.550 0.000412 ***
## time_value                     2.911e-05  7.190e-06   4.049 5.75e-05 ***
## RestaurantRestrict            -2.625e+00  8.918e-01  -2.943 0.003363 ** 
## time_value:RestaurantRestrict  1.408e-04  4.839e-05   2.909 0.003747 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.02928 on 673 degrees of freedom
## Multiple R-squared:  0.1273,	Adjusted R-squared:  0.1234 
## F-statistic: 32.73 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "nh"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * RestaurantRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.043611 -0.016199  0.001539  0.017494  0.060825 
## 
## Coefficients:
##                                 Estimate Std. Error t value Pr(>|t|)
## (Intercept)                    1.458e-01  1.080e-01   1.351    0.177
## time_value                    -4.627e-06  5.939e-06  -0.779    0.436
## RestaurantRestrict            -6.511e-01  8.246e-01  -0.790    0.430
## time_value:RestaurantRestrict  3.417e-05  4.479e-05   0.763    0.446
## 
## Residual standard error: 0.02569 on 673 degrees of freedom
## Multiple R-squared:  0.1272,	Adjusted R-squared:  0.1233 
## F-statistic: 32.69 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "nj"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * RestaurantRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.053596 -0.015873  0.002651  0.021972  0.054609 
## 
## Coefficients:
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                   -7.736e-01  1.842e-01  -4.199 3.04e-05 ***
## time_value                     4.637e-05  1.017e-05   4.560 6.08e-06 ***
## RestaurantRestrict            -6.711e-01  5.135e-01  -1.307    0.192    
## time_value:RestaurantRestrict  3.401e-05  2.789e-05   1.219    0.223    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.02718 on 673 degrees of freedom
## Multiple R-squared:  0.2204,	Adjusted R-squared:  0.2169 
## F-statistic: 63.41 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "nm"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * RestaurantRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.028080 -0.009430  0.002061  0.008308  0.029982 
## 
## Coefficients:
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                   -4.615e-01  8.048e-02  -5.735 1.48e-08 ***
## time_value                     2.819e-05  4.442e-06   6.347 4.03e-10 ***
## RestaurantRestrict            -2.362e-01  2.302e-01  -1.026    0.305    
## time_value:RestaurantRestrict  1.202e-05  1.250e-05   0.962    0.336    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.012 on 673 degrees of freedom
## Multiple R-squared:  0.1017,	Adjusted R-squared:  0.09773 
## F-statistic: 25.41 on 3 and 673 DF,  p-value: 1.402e-15
## 
## [1] "nv"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * RestaurantRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.029430 -0.011550  0.003415  0.009343  0.034449 
## 
## Coefficients:
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                   -4.340e-01  8.775e-02  -4.946 9.59e-07 ***
## time_value                     2.647e-05  4.843e-06   5.466 6.50e-08 ***
## RestaurantRestrict            -6.631e-01  2.553e-01  -2.597  0.00962 ** 
## time_value:RestaurantRestrict  3.512e-05  1.387e-05   2.533  0.01153 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.01317 on 673 degrees of freedom
## Multiple R-squared:  0.1091,	Adjusted R-squared:  0.1052 
## F-statistic: 27.49 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "ny"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * RestaurantRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.042965 -0.013417  0.001414  0.016665  0.050083 
## 
## Coefficients:
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                   -7.822e-01  1.447e-01  -5.405 9.01e-08 ***
## time_value                     4.651e-05  7.988e-06   5.822 8.99e-09 ***
## RestaurantRestrict            -7.390e-01  4.034e-01  -1.832   0.0674 .  
## time_value:RestaurantRestrict  3.827e-05  2.191e-05   1.747   0.0812 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.02135 on 673 degrees of freedom
## Multiple R-squared:  0.1797,	Adjusted R-squared:  0.176 
## F-statistic: 49.13 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "oh"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * RestaurantRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.047705 -0.018116  0.002821  0.017686  0.045161 
## 
## Coefficients:
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                   -8.861e-01  1.541e-01  -5.749 1.36e-08 ***
## time_value                     5.214e-05  8.508e-06   6.128 1.51e-09 ***
## RestaurantRestrict            -9.543e-01  4.260e-01  -2.240   0.0254 *  
## time_value:RestaurantRestrict  4.992e-05  2.314e-05   2.157   0.0313 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.02267 on 673 degrees of freedom
## Multiple R-squared:  0.1622,	Adjusted R-squared:  0.1584 
## F-statistic: 43.42 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "ok"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * RestaurantRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.035792 -0.021598  0.000209  0.015340  0.059620 
## 
## Coefficients:
##                                 Estimate Std. Error t value Pr(>|t|)
## (Intercept)                    3.952e-02  8.455e-02   0.467    0.640
## time_value                     7.243e-07  4.640e-06   0.156    0.876
## RestaurantRestrict            -4.032e-01  2.987e+00  -0.135    0.893
## time_value:RestaurantRestrict  2.094e-05  1.625e-04   0.129    0.898
## 
## Residual standard error: 0.02289 on 673 degrees of freedom
## Multiple R-squared:  0.04988,	Adjusted R-squared:  0.04564 
## F-statistic: 11.78 on 3 and 673 DF,  p-value: 1.585e-07
## 
## [1] "or"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * RestaurantRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.033939 -0.012434  0.003175  0.010746  0.039576 
## 
## Coefficients:
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                   -3.683e-01  1.079e-01  -3.414 0.000678 ***
## time_value                     2.311e-05  5.954e-06   3.881 0.000114 ***
## RestaurantRestrict            -2.937e-01  3.033e-01  -0.968 0.333251    
## time_value:RestaurantRestrict  1.495e-05  1.647e-05   0.908 0.364318    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.01597 on 673 degrees of freedom
## Multiple R-squared:  0.106,	Adjusted R-squared:  0.1021 
## F-statistic: 26.61 on 3 and 673 DF,  p-value: 2.834e-16
## 
## [1] "pa"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * RestaurantRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.045659 -0.015330  0.003066  0.016833  0.045006 
## 
## Coefficients:
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                   -8.633e-01  1.455e-01  -5.933 4.76e-09 ***
## time_value                     5.089e-05  8.031e-06   6.336 4.31e-10 ***
## RestaurantRestrict            -7.367e-01  4.091e-01  -1.801   0.0722 .  
## time_value:RestaurantRestrict  3.809e-05  2.222e-05   1.714   0.0869 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.02154 on 673 degrees of freedom
## Multiple R-squared:  0.1811,	Adjusted R-squared:  0.1774 
## F-statistic: 49.61 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "ri"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * RestaurantRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.042556 -0.014530  0.002094  0.016566  0.043991 
## 
## Coefficients:
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                   -7.972e-01  1.407e-01  -5.664 2.19e-08 ***
## time_value                     4.727e-05  7.768e-06   6.085 1.95e-09 ***
## RestaurantRestrict            -2.575e-01  3.957e-01  -0.651    0.515    
## time_value:RestaurantRestrict  1.225e-05  2.149e-05   0.570    0.569    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.02084 on 673 degrees of freedom
## Multiple R-squared:  0.1617,	Adjusted R-squared:  0.1579 
## F-statistic: 43.26 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "sc"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * RestaurantRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.037146 -0.018824  0.004524  0.014192  0.035077 
## 
## Coefficients:
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                   -2.805e-01  9.251e-02  -3.033 0.002518 ** 
## time_value                     1.819e-05  5.096e-06   3.570 0.000383 ***
## RestaurantRestrict            -1.400e+00  4.373e-01  -3.202 0.001428 ** 
## time_value:RestaurantRestrict  7.519e-05  2.373e-05   3.168 0.001605 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.01879 on 673 degrees of freedom
## Multiple R-squared:  0.08172,	Adjusted R-squared:  0.07762 
## F-statistic: 19.96 on 3 and 673 DF,  p-value: 2.097e-12
## 
## [1] "sd"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * RestaurantRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.036192 -0.023699 -0.004966  0.019513  0.069966 
## 
## Coefficients:
##                                 Estimate Std. Error t value Pr(>|t|)
## (Intercept)                    2.343e-02  9.310e-02   0.252    0.801
## time_value                     1.841e-06  5.107e-06   0.360    0.719
## RestaurantRestrict             2.793e+00  1.487e+01   0.188    0.851
## time_value:RestaurantRestrict -1.530e-04  8.095e-04  -0.189    0.850
## 
## Residual standard error: 0.02575 on 673 degrees of freedom
## Multiple R-squared:  0.01512,	Adjusted R-squared:  0.01073 
## F-statistic: 3.443 on 3 and 673 DF,  p-value: 0.01649
## 
## [1] "tn"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * RestaurantRestrict, data = ftime.policy.df)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.03749 -0.02423  0.00203  0.01603  0.05119 
## 
## Coefficients:
##                                 Estimate Std. Error t value Pr(>|t|)
## (Intercept)                    1.163e-01  8.313e-02   1.399    0.162
## time_value                    -3.538e-06  4.562e-06  -0.775    0.438
## RestaurantRestrict            -8.467e-01  3.021e+00  -0.280    0.779
## time_value:RestaurantRestrict  4.517e-05  1.644e-04   0.275    0.784
## 
## Residual standard error: 0.0226 on 673 degrees of freedom
## Multiple R-squared:  0.04737,	Adjusted R-squared:  0.04313 
## F-statistic: 11.16 on 3 and 673 DF,  p-value: 3.752e-07
## 
## [1] "tx"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * RestaurantRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.043979 -0.018127  0.003147  0.014394  0.044874 
## 
## Coefficients:
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                   -7.736e-01  1.423e-01  -5.435 7.67e-08 ***
## time_value                     4.573e-05  7.856e-06   5.821 9.08e-09 ***
## RestaurantRestrict            -1.096e+00  4.142e-01  -2.646  0.00834 ** 
## time_value:RestaurantRestrict  5.770e-05  2.249e-05   2.565  0.01052 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.02136 on 673 degrees of freedom
## Multiple R-squared:  0.1704,	Adjusted R-squared:  0.1667 
## F-statistic: 46.09 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "ut"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * RestaurantRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.044234 -0.018794  0.005849  0.015499  0.041181 
## 
## Coefficients:
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                   -4.238e-01  1.398e-01  -3.031 0.002534 ** 
## time_value                     2.660e-05  7.718e-06   3.447 0.000603 ***
## RestaurantRestrict            -9.180e-01  4.000e-01  -2.295 0.022031 *  
## time_value:RestaurantRestrict  4.866e-05  2.172e-05   2.240 0.025402 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.02084 on 673 degrees of freedom
## Multiple R-squared:  0.09115,	Adjusted R-squared:  0.0871 
## F-statistic:  22.5 on 3 and 673 DF,  p-value: 6.836e-14
## 
## [1] "va"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * RestaurantRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.044464 -0.015552  0.004332  0.015928  0.043039 
## 
## Coefficients:
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                   -6.835e-01  1.436e-01  -4.761 2.36e-06 ***
## time_value                     4.086e-05  7.924e-06   5.157 3.31e-07 ***
## RestaurantRestrict            -4.650e-01  4.036e-01  -1.152    0.250    
## time_value:RestaurantRestrict  2.352e-05  2.192e-05   1.073    0.284    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.02126 on 673 degrees of freedom
## Multiple R-squared:  0.1674,	Adjusted R-squared:  0.1637 
## F-statistic:  45.1 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "vt"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * RestaurantRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.041099 -0.013628  0.001977  0.013091  0.049719 
## 
## Coefficients:
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                   -8.828e-01  1.268e-01  -6.960 8.07e-12 ***
## time_value                     5.201e-05  7.000e-06   7.430 3.31e-13 ***
## RestaurantRestrict            -3.547e-01  3.566e-01  -0.995    0.320    
## time_value:RestaurantRestrict  1.764e-05  1.937e-05   0.911    0.363    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.01878 on 673 degrees of freedom
## Multiple R-squared:  0.1604,	Adjusted R-squared:  0.1567 
## F-statistic: 42.87 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "wa"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * RestaurantRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.037408 -0.012929  0.003457  0.012421  0.037585 
## 
## Coefficients:
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                   -3.605e-01  1.182e-01  -3.050 0.002375 ** 
## time_value                     2.278e-05  6.523e-06   3.493 0.000509 ***
## RestaurantRestrict            -5.621e-01  3.294e-01  -1.706 0.088436 .  
## time_value:RestaurantRestrict  2.932e-05  1.789e-05   1.639 0.101747    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.01744 on 673 degrees of freedom
## Multiple R-squared:  0.1409,	Adjusted R-squared:  0.137 
## F-statistic: 36.78 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "wi"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * RestaurantRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.045042 -0.026365 -0.002078  0.016772  0.072723 
## 
## Coefficients:
##                                 Estimate Std. Error t value Pr(>|t|)   
## (Intercept)                    3.536e-01  1.075e-01   3.291  0.00105 **
## time_value                    -1.608e-05  5.896e-06  -2.728  0.00654 **
## RestaurantRestrict             1.837e+00  4.227e+00   0.435  0.66401   
## time_value:RestaurantRestrict -1.011e-04  2.301e-04  -0.439  0.66050   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.02933 on 673 degrees of freedom
## Multiple R-squared:  0.05567,	Adjusted R-squared:  0.05146 
## F-statistic: 13.22 on 3 and 673 DF,  p-value: 2.136e-08
## 
## [1] "wv"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * RestaurantRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.044452 -0.016753  0.003098  0.014758  0.042602 
## 
## Coefficients:
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                   -9.554e-01  1.346e-01  -7.097 3.24e-12 ***
## time_value                     5.583e-05  7.430e-06   7.514 1.83e-13 ***
## RestaurantRestrict            -5.516e-01  3.817e-01  -1.445    0.149    
## time_value:RestaurantRestrict  2.825e-05  2.073e-05   1.362    0.174    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.02 on 673 degrees of freedom
## Multiple R-squared:  0.157,	Adjusted R-squared:  0.1532 
## F-statistic: 41.77 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "wy"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * RestaurantRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.037044 -0.015010  0.001662  0.013544  0.038762 
## 
## Coefficients:
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                   -5.189e-01  1.103e-01  -4.705 3.08e-06 ***
## time_value                     3.146e-05  6.086e-06   5.169 3.10e-07 ***
## RestaurantRestrict            -1.173e+00  3.154e-01  -3.718 0.000217 ***
## time_value:RestaurantRestrict  6.267e-05  1.713e-05   3.659 0.000273 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.01644 on 673 degrees of freedom
## Multiple R-squared:  0.1027,	Adjusted R-squared:  0.09869 
## F-statistic: 25.67 on 3 and 673 DF,  p-value: 9.849e-16
```

### Completely staying at home

#### Emergency Declaration


```r
# The fraction of mobile devices that did not leave the immediate area of their home (SafeGraph’s completely_home_device_count / device_count)
for(state in states){
  print(state)
  print("---------------------")
  chome.state <- chome %>% filter(geo_value == state)
  policy.state <- policy %>% filter(StatePostal == state)
  policy_signal.state <- getSumOfPolicy(policy.state, STARTDATE, ENDDATE)
  # left join mobility with policy signal by time 
  ftime.policy.df <- left_join(chome.state , policy_signal.state, by = "time_value")
  lm.fit <- lm(value~time_value*EmergDec,data=ftime.policy.df)
  print(summary(lm.fit))
  }
```

```
## [1] "ak"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * EmergDec, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.118181 -0.025234 -0.003872  0.021509  0.143624 
## 
## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)          5.333e+00  2.529e-01  21.084   <2e-16 ***
## time_value          -2.768e-04  1.396e-05 -19.823   <2e-16 ***
## EmergDec             8.722e-01  6.760e-01   1.290    0.197    
## time_value:EmergDec -4.329e-05  3.673e-05  -1.178    0.239    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03669 on 673 degrees of freedom
## Multiple R-squared:   0.44,	Adjusted R-squared:  0.4375 
## F-statistic: 176.3 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "al"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * EmergDec, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.072783 -0.022718 -0.006026  0.017032  0.157695 
## 
## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)          4.011e+00  2.225e-01  18.029  < 2e-16 ***
## time_value          -2.077e-04  1.228e-05 -16.911  < 2e-16 ***
## EmergDec             2.630e+00  6.046e-01   4.349 1.58e-05 ***
## time_value:EmergDec -1.389e-04  3.285e-05  -4.228 2.68e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03249 on 673 degrees of freedom
## Multiple R-squared:  0.3826,	Adjusted R-squared:  0.3799 
## F-statistic:   139 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "ar"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * EmergDec, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.094329 -0.022900 -0.006559  0.016189  0.135311 
## 
## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)          4.201e+00  2.246e-01  18.708  < 2e-16 ***
## time_value          -2.177e-04  1.240e-05 -17.557  < 2e-16 ***
## EmergDec             2.967e+00  6.003e-01   4.943 9.72e-07 ***
## time_value:EmergDec -1.572e-04  3.261e-05  -4.820 1.78e-06 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03258 on 673 degrees of freedom
## Multiple R-squared:  0.4112,	Adjusted R-squared:  0.4086 
## F-statistic: 156.7 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "az"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * EmergDec, data = ftime.policy.df)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.12424 -0.02130 -0.00306  0.01671  0.10489 
## 
## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)          4.796e+00  2.203e-01  21.773  < 2e-16 ***
## time_value          -2.472e-04  1.216e-05 -20.327  < 2e-16 ***
## EmergDec             1.901e+00  5.888e-01   3.228  0.00131 ** 
## time_value:EmergDec -9.746e-05  3.199e-05  -3.047  0.00240 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03196 on 673 degrees of freedom
## Multiple R-squared:  0.472,	Adjusted R-squared:  0.4697 
## F-statistic: 200.6 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "ca"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * EmergDec, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.209766 -0.019353 -0.004193  0.019150  0.137197 
## 
## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)          4.999e+00  2.607e-01  19.177  < 2e-16 ***
## time_value          -2.595e-04  1.439e-05 -18.034  < 2e-16 ***
## EmergDec             2.851e+00  6.580e-01   4.334 1.69e-05 ***
## time_value:EmergDec -1.463e-04  3.577e-05  -4.091 4.81e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03692 on 673 degrees of freedom
## Multiple R-squared:  0.5849,	Adjusted R-squared:  0.583 
## F-statistic: 316.1 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "co"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * EmergDec, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.140846 -0.024861 -0.007512  0.020798  0.170492 
## 
## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)          4.947e+00  2.580e-01  19.173  < 2e-16 ***
## time_value          -2.558e-04  1.424e-05 -17.955  < 2e-16 ***
## EmergDec             5.680e+00  6.897e-01   8.235 9.35e-16 ***
## time_value:EmergDec -3.023e-04  3.747e-05  -8.067 3.32e-15 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03744 on 673 degrees of freedom
## Multiple R-squared:  0.4738,	Adjusted R-squared:  0.4715 
## F-statistic:   202 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "ct"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * EmergDec, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.200220 -0.026513 -0.008043  0.020840  0.216985 
## 
## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)          4.862e+00  2.825e-01   17.21   <2e-16 ***
## time_value          -2.521e-04  1.559e-05  -16.17   <2e-16 ***
## EmergDec             8.068e+00  7.488e-01   10.77   <2e-16 ***
## time_value:EmergDec -4.304e-04  4.069e-05  -10.58   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.04084 on 673 degrees of freedom
## Multiple R-squared:  0.5273,	Adjusted R-squared:  0.5252 
## F-statistic: 250.2 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "dc"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * EmergDec, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.187824 -0.021491 -0.004255  0.019569  0.169197 
## 
## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)          2.736e+00  2.415e-01   11.33   <2e-16 ***
## time_value          -1.334e-04  1.333e-05  -10.00   <2e-16 ***
## EmergDec             7.657e+00  6.456e-01   11.86   <2e-16 ***
## time_value:EmergDec -4.081e-04  3.508e-05  -11.63   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03504 on 673 degrees of freedom
## Multiple R-squared:  0.6444,	Adjusted R-squared:  0.6428 
## F-statistic: 406.4 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "de"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * EmergDec, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.093040 -0.024825 -0.006167  0.016593  0.166592 
## 
## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)          4.943e+00  2.424e-01  20.387   <2e-16 ***
## time_value          -2.554e-04  1.338e-05 -19.086   <2e-16 ***
## EmergDec             5.941e+00  6.590e-01   9.017   <2e-16 ***
## time_value:EmergDec -3.158e-04  3.580e-05  -8.822   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03541 on 673 degrees of freedom
## Multiple R-squared:  0.5268,	Adjusted R-squared:  0.5247 
## F-statistic: 249.8 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "fl"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * EmergDec, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.162729 -0.020582 -0.003494  0.017246  0.129487 
## 
## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)          3.667e+00  2.476e-01  14.810  < 2e-16 ***
## time_value          -1.864e-04  1.367e-05 -13.636  < 2e-16 ***
## EmergDec             4.648e+00  6.509e-01   7.140 2.43e-12 ***
## time_value:EmergDec -2.468e-04  3.537e-05  -6.978 7.20e-12 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03568 on 673 degrees of freedom
## Multiple R-squared:  0.4131,	Adjusted R-squared:  0.4105 
## F-statistic: 157.9 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "ga"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * EmergDec, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.068552 -0.020153 -0.005537  0.016502  0.142500 
## 
## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)          4.045e+00  2.141e-01  18.891  < 2e-16 ***
## time_value          -2.084e-04  1.182e-05 -17.632  < 2e-16 ***
## EmergDec             4.967e+00  5.868e-01   8.464  < 2e-16 ***
## time_value:EmergDec -2.642e-04  3.188e-05  -8.290 6.15e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03138 on 673 degrees of freedom
## Multiple R-squared:  0.4814,	Adjusted R-squared:  0.479 
## F-statistic: 208.2 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "hi"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * EmergDec, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.167423 -0.019881 -0.002859  0.019213  0.155331 
## 
## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)          4.655e+00  2.506e-01  18.576   <2e-16 ***
## time_value          -2.421e-04  1.384e-05 -17.498   <2e-16 ***
## EmergDec            -1.479e+00  6.326e-01  -2.338   0.0197 *  
## time_value:EmergDec  8.757e-05  3.439e-05   2.547   0.0111 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03549 on 673 degrees of freedom
## Multiple R-squared:  0.5115,	Adjusted R-squared:  0.5093 
## F-statistic: 234.9 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "ia"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * EmergDec, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.160064 -0.026280 -0.006819  0.020351  0.176467 
## 
## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)          5.581e+00  2.799e-01  19.936  < 2e-16 ***
## time_value          -2.927e-04  1.545e-05 -18.939  < 2e-16 ***
## EmergDec             3.636e+00  7.360e-01   4.941 9.83e-07 ***
## time_value:EmergDec -1.923e-04  3.999e-05  -4.808 1.88e-06 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.04034 on 673 degrees of freedom
## Multiple R-squared:  0.4465,	Adjusted R-squared:  0.444 
## F-statistic: 180.9 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "id"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * EmergDec, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.074713 -0.024426 -0.006103  0.016123  0.124697 
## 
## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)          5.320e+00  2.331e-01  22.821  < 2e-16 ***
## time_value          -2.766e-04  1.287e-05 -21.492  < 2e-16 ***
## EmergDec             1.871e+00  6.336e-01   2.953  0.00326 ** 
## time_value:EmergDec -9.702e-05  3.442e-05  -2.819  0.00496 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03405 on 673 degrees of freedom
## Multiple R-squared:  0.4805,	Adjusted R-squared:  0.4782 
## F-statistic: 207.5 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "il"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * EmergDec, data = ftime.policy.df)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.18714 -0.02570 -0.00585  0.02313  0.19870 
## 
## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)          5.092e+00  2.648e-01  19.232   <2e-16 ***
## time_value          -2.649e-04  1.462e-05 -18.125   <2e-16 ***
## EmergDec             6.117e+00  6.961e-01   8.788   <2e-16 ***
## time_value:EmergDec -3.249e-04  3.783e-05  -8.589   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03815 on 673 degrees of freedom
## Multiple R-squared:  0.5199,	Adjusted R-squared:  0.5178 
## F-statistic: 242.9 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "in"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * EmergDec, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.140235 -0.024779 -0.006304  0.021323  0.150195 
## 
## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)          4.908e+00  2.601e-01  18.874  < 2e-16 ***
## time_value          -2.548e-04  1.436e-05 -17.750  < 2e-16 ***
## EmergDec             4.485e+00  6.671e-01   6.724 3.79e-11 ***
## time_value:EmergDec -2.387e-04  3.626e-05  -6.583 9.26e-11 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03709 on 673 degrees of freedom
## Multiple R-squared:  0.4442,	Adjusted R-squared:  0.4418 
## F-statistic: 179.3 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "ks"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * EmergDec, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.132875 -0.024683 -0.006244  0.019287  0.165602 
## 
## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)          5.216e+00  2.496e-01  20.895  < 2e-16 ***
## time_value          -2.722e-04  1.378e-05 -19.752  < 2e-16 ***
## EmergDec             4.626e+00  6.728e-01   6.876 1.41e-11 ***
## time_value:EmergDec -2.459e-04  3.655e-05  -6.727 3.71e-11 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03634 on 673 degrees of freedom
## Multiple R-squared:  0.4819,	Adjusted R-squared:  0.4796 
## F-statistic: 208.7 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "ky"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * EmergDec, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.132696 -0.023217 -0.004515  0.019575  0.157990 
## 
## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)          5.803e+00  2.410e-01  24.080   <2e-16 ***
## time_value          -3.043e-04  1.330e-05 -22.873   <2e-16 ***
## EmergDec             8.989e-01  6.182e-01   1.454    0.146    
## time_value:EmergDec -4.389e-05  3.360e-05  -1.306    0.192    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03436 on 673 degrees of freedom
## Multiple R-squared:  0.5012,	Adjusted R-squared:  0.499 
## F-statistic: 225.4 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "la"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * EmergDec, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.138433 -0.020795 -0.004425  0.018025  0.143471 
## 
## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)          4.117e+00  2.145e-01  19.189  < 2e-16 ***
## time_value          -2.125e-04  1.184e-05 -17.944  < 2e-16 ***
## EmergDec             4.354e+00  5.734e-01   7.593 1.05e-13 ***
## time_value:EmergDec -2.320e-04  3.116e-05  -7.446 2.96e-13 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03112 on 673 degrees of freedom
## Multiple R-squared:  0.4542,	Adjusted R-squared:  0.4518 
## F-statistic: 186.7 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "ma"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * EmergDec, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.222961 -0.025088 -0.005798  0.021354  0.190146 
## 
## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)          4.858e+00  2.718e-01  17.872   <2e-16 ***
## time_value          -2.515e-04  1.501e-05 -16.762   <2e-16 ***
## EmergDec             7.328e+00  7.206e-01  10.170   <2e-16 ***
## time_value:EmergDec -3.895e-04  3.916e-05  -9.948   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.0393 on 673 degrees of freedom
## Multiple R-squared:  0.5739,	Adjusted R-squared:  0.572 
## F-statistic: 302.1 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "md"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * EmergDec, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.195948 -0.025671 -0.006125  0.020821  0.224362 
## 
## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)          4.614e+00  2.996e-01  15.399  < 2e-16 ***
## time_value          -2.378e-04  1.654e-05 -14.374  < 2e-16 ***
## EmergDec             4.155e+00  7.624e-01   5.450 7.09e-08 ***
## time_value:EmergDec -2.177e-04  4.144e-05  -5.254 2.00e-07 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.04258 on 673 degrees of freedom
## Multiple R-squared:  0.4854,	Adjusted R-squared:  0.4831 
## F-statistic: 211.6 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "me"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * EmergDec, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.077522 -0.023275 -0.005773  0.017844  0.225133 
## 
## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)          5.542e+00  2.454e-01  22.577  < 2e-16 ***
## time_value          -2.880e-04  1.355e-05 -21.260  < 2e-16 ***
## EmergDec             3.441e+00  6.784e-01   5.072 5.09e-07 ***
## time_value:EmergDec -1.822e-04  3.685e-05  -4.944 9.66e-07 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.0361 on 673 degrees of freedom
## Multiple R-squared:  0.5034,	Adjusted R-squared:  0.5012 
## F-statistic: 227.4 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "mi"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * EmergDec, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.200850 -0.027417 -0.007042  0.026083  0.149015 
## 
## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)          3.480e+00  2.116e-01   16.45   <2e-16 ***
## time_value          -1.748e-04  1.166e-05  -14.99   <2e-16 ***
## EmergDec             1.209e+01  9.364e-01   12.91   <2e-16 ***
## time_value:EmergDec -6.522e-04  5.084e-05  -12.83   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.04254 on 673 degrees of freedom
## Multiple R-squared:  0.4429,	Adjusted R-squared:  0.4404 
## F-statistic: 178.3 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "mn"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * EmergDec, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.109147 -0.026878 -0.006463  0.020557  0.173275 
## 
## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)          4.949e+00  2.675e-01  18.499  < 2e-16 ***
## time_value          -2.566e-04  1.477e-05 -17.377  < 2e-16 ***
## EmergDec             5.849e+00  7.271e-01   8.044 3.94e-15 ***
## time_value:EmergDec -3.111e-04  3.950e-05  -7.876 1.36e-14 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03908 on 673 degrees of freedom
## Multiple R-squared:  0.4646,	Adjusted R-squared:  0.4622 
## F-statistic: 194.7 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "mo"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * EmergDec, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.079281 -0.024099 -0.007712  0.017227  0.166329 
## 
## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)          5.058e+00  2.412e-01  20.967  < 2e-16 ***
## time_value          -2.634e-04  1.332e-05 -19.782  < 2e-16 ***
## EmergDec             4.293e+00  6.557e-01   6.547 1.16e-10 ***
## time_value:EmergDec -2.279e-04  3.562e-05  -6.398 2.95e-10 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03524 on 673 degrees of freedom
## Multiple R-squared:  0.4766,	Adjusted R-squared:  0.4743 
## F-statistic: 204.3 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "ms"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * EmergDec, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.059928 -0.021247 -0.005491  0.015740  0.162425 
## 
## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)          3.660e+00  2.114e-01  17.315  < 2e-16 ***
## time_value          -1.884e-04  1.167e-05 -16.152  < 2e-16 ***
## EmergDec             3.441e+00  5.793e-01   5.939 4.60e-09 ***
## time_value:EmergDec -1.832e-04  3.147e-05  -5.822 9.01e-09 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03098 on 673 degrees of freedom
## Multiple R-squared:  0.3875,	Adjusted R-squared:  0.3847 
## F-statistic: 141.9 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "mt"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * EmergDec, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.102498 -0.025953 -0.006006  0.017715  0.126098 
## 
## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)          4.887e+00  2.500e-01  19.549   <2e-16 ***
## time_value          -2.525e-04  1.380e-05 -18.300   <2e-16 ***
## EmergDec             1.137e+00  6.738e-01   1.688   0.0919 .  
## time_value:EmergDec -5.796e-05  3.661e-05  -1.583   0.1138    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03639 on 673 degrees of freedom
## Multiple R-squared:  0.4034,	Adjusted R-squared:  0.4007 
## F-statistic: 151.7 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "nc"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * EmergDec, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.132201 -0.019995 -0.004334  0.018395  0.126754 
## 
## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)          3.948e+00  2.203e-01  17.923  < 2e-16 ***
## time_value          -2.024e-04  1.216e-05 -16.642  < 2e-16 ***
## EmergDec             2.998e+00  5.839e-01   5.134 3.71e-07 ***
## time_value:EmergDec -1.582e-04  3.173e-05  -4.985 7.89e-07 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03185 on 673 degrees of freedom
## Multiple R-squared:  0.399,	Adjusted R-squared:  0.3963 
## F-statistic: 148.9 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "nd"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * EmergDec, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.084465 -0.029972 -0.007477  0.022218  0.169081 
## 
## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)          4.956e+00  2.952e-01  16.787  < 2e-16 ***
## time_value          -2.570e-04  1.630e-05 -15.771  < 2e-16 ***
## EmergDec             3.122e+00  8.023e-01   3.891 0.000110 ***
## time_value:EmergDec -1.656e-04  4.359e-05  -3.798 0.000159 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.04312 on 673 degrees of freedom
## Multiple R-squared:  0.3678,	Adjusted R-squared:  0.365 
## F-statistic: 130.5 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "ne"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * EmergDec, data = ftime.policy.df)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.08008 -0.02513 -0.00650  0.01985  0.18124 
## 
## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)          5.900e+00  2.528e-01  23.333  < 2e-16 ***
## time_value          -3.097e-04  1.396e-05 -22.191  < 2e-16 ***
## EmergDec             4.977e+00  6.872e-01   7.242 1.21e-12 ***
## time_value:EmergDec -2.647e-04  3.733e-05  -7.090 3.39e-12 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03693 on 673 degrees of freedom
## Multiple R-squared:  0.5388,	Adjusted R-squared:  0.5367 
## F-statistic: 262.1 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "nh"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * EmergDec, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.115394 -0.025866 -0.007482  0.020830  0.244891 
## 
## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)          5.070e+00  2.651e-01  19.125  < 2e-16 ***
## time_value          -2.632e-04  1.463e-05 -17.987  < 2e-16 ***
## EmergDec             5.854e+00  7.204e-01   8.126 2.13e-15 ***
## time_value:EmergDec -3.114e-04  3.914e-05  -7.956 7.52e-15 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03872 on 673 degrees of freedom
## Multiple R-squared:  0.4748,	Adjusted R-squared:  0.4724 
## F-statistic: 202.8 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "nj"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * EmergDec, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.246597 -0.027118 -0.008894  0.025734  0.153268 
## 
## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)          5.091e+00  3.090e-01   16.47   <2e-16 ***
## time_value          -2.652e-04  1.706e-05  -15.55   <2e-16 ***
## EmergDec             1.061e+01  8.124e-01   13.06   <2e-16 ***
## time_value:EmergDec -5.662e-04  4.415e-05  -12.82   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.04453 on 673 degrees of freedom
## Multiple R-squared:  0.6181,	Adjusted R-squared:  0.6164 
## F-statistic:   363 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "nm"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * EmergDec, data = ftime.policy.df)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.10652 -0.02145 -0.00412  0.01877  0.12753 
## 
## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)          5.149e+00  2.183e-01  23.586   <2e-16 ***
## time_value          -2.668e-04  1.205e-05 -22.138   <2e-16 ***
## EmergDec             6.082e-02  5.835e-01   0.104    0.917    
## time_value:EmergDec  2.174e-06  3.171e-05   0.069    0.945    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03167 on 673 degrees of freedom
## Multiple R-squared:  0.4667,	Adjusted R-squared:  0.4643 
## F-statistic: 196.3 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "nv"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * EmergDec, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.162640 -0.020325 -0.003665  0.018208  0.113522 
## 
## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)          5.436e+00  2.114e-01  25.720  < 2e-16 ***
## time_value          -2.824e-04  1.167e-05 -24.204  < 2e-16 ***
## EmergDec             3.865e+00  5.697e-01   6.784 2.57e-11 ***
## time_value:EmergDec -2.028e-04  3.095e-05  -6.552 1.13e-10 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03077 on 673 degrees of freedom
## Multiple R-squared:  0.5953,	Adjusted R-squared:  0.5935 
## F-statistic: 329.9 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "ny"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * EmergDec, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.227829 -0.025142 -0.007099  0.024318  0.152839 
## 
## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)          5.154e+00  2.945e-01  17.501   <2e-16 ***
## time_value          -2.672e-04  1.626e-05 -16.431   <2e-16 ***
## EmergDec             7.682e+00  7.617e-01  10.085   <2e-16 ***
## time_value:EmergDec -4.086e-04  4.140e-05  -9.869   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.04215 on 673 degrees of freedom
## Multiple R-squared:  0.5529,	Adjusted R-squared:  0.5509 
## F-statistic: 277.4 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "oh"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * EmergDec, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.146683 -0.024999 -0.006934  0.021792  0.184164 
## 
## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)          5.328e+00  2.530e-01  21.056  < 2e-16 ***
## time_value          -2.773e-04  1.397e-05 -19.854  < 2e-16 ***
## EmergDec             5.014e+00  6.652e-01   7.537 1.55e-13 ***
## time_value:EmergDec -2.667e-04  3.615e-05  -7.378 4.76e-13 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03646 on 673 degrees of freedom
## Multiple R-squared:  0.4953,	Adjusted R-squared:  0.4931 
## F-statistic: 220.2 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "ok"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * EmergDec, data = ftime.policy.df)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.07365 -0.02343 -0.00721  0.01761  0.15659 
## 
## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)          4.486e+00  2.322e-01  19.317  < 2e-16 ***
## time_value          -2.331e-04  1.282e-05 -18.187  < 2e-16 ***
## EmergDec             4.162e+00  6.419e-01   6.484 1.73e-10 ***
## time_value:EmergDec -2.215e-04  3.486e-05  -6.353 3.89e-10 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03415 on 673 degrees of freedom
## Multiple R-squared:  0.4409,	Adjusted R-squared:  0.4384 
## F-statistic: 176.9 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "or"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * EmergDec, data = ftime.policy.df)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.14956 -0.02155 -0.00498  0.01945  0.12046 
## 
## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)          5.092e+00  2.375e-01  21.441  < 2e-16 ***
## time_value          -2.634e-04  1.311e-05 -20.090  < 2e-16 ***
## EmergDec             1.690e+00  6.192e-01   2.729  0.00652 ** 
## time_value:EmergDec -8.570e-05  3.365e-05  -2.547  0.01110 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.0341 on 673 degrees of freedom
## Multiple R-squared:  0.4633,	Adjusted R-squared:  0.4609 
## F-statistic: 193.6 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "pa"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * EmergDec, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.182181 -0.024204 -0.006774  0.021683  0.155600 
## 
## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)          5.503e+00  2.781e-01  19.790  < 2e-16 ***
## time_value          -2.863e-04  1.535e-05 -18.649  < 2e-16 ***
## EmergDec             4.867e+00  7.134e-01   6.822 2.00e-11 ***
## time_value:EmergDec -2.576e-04  3.877e-05  -6.643 6.34e-11 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03966 on 673 degrees of freedom
## Multiple R-squared:  0.4761,	Adjusted R-squared:  0.4738 
## F-statistic: 203.9 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "ri"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * EmergDec, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.180570 -0.023965 -0.006575  0.020807  0.173224 
## 
## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)          5.719e+00  2.670e-01  21.420  < 2e-16 ***
## time_value          -2.986e-04  1.474e-05 -20.259  < 2e-16 ***
## EmergDec             4.957e+00  7.019e-01   7.063 4.08e-12 ***
## time_value:EmergDec -2.618e-04  3.814e-05  -6.865 1.52e-11 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03847 on 673 degrees of freedom
## Multiple R-squared:  0.5202,	Adjusted R-squared:  0.518 
## F-statistic: 243.2 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "sc"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * EmergDec, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.073057 -0.019195 -0.004634  0.015034  0.135415 
## 
## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)          4.030e+00  2.042e-01  19.741  < 2e-16 ***
## time_value          -2.076e-04  1.127e-05 -18.424  < 2e-16 ***
## EmergDec             2.582e+00  5.549e-01   4.652 3.95e-06 ***
## time_value:EmergDec -1.361e-04  3.014e-05  -4.514 7.50e-06 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.02982 on 673 degrees of freedom
## Multiple R-squared:  0.4216,	Adjusted R-squared:  0.419 
## F-statistic: 163.5 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "sd"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * EmergDec, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.080225 -0.027216 -0.006661  0.020125  0.201660 
## 
## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)          5.085e+00  2.693e-01  18.881  < 2e-16 ***
## time_value          -2.644e-04  1.487e-05 -17.785  < 2e-16 ***
## EmergDec             3.289e+00  7.319e-01   4.493 8.25e-06 ***
## time_value:EmergDec -1.746e-04  3.976e-05  -4.392 1.30e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03934 on 673 degrees of freedom
## Multiple R-squared:  0.4326,	Adjusted R-squared:  0.4301 
## F-statistic: 171.1 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "tn"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * EmergDec, data = ftime.policy.df)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.11230 -0.02175 -0.00479  0.01886  0.16568 
## 
## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)          4.337e+00  2.167e-01  20.009  < 2e-16 ***
## time_value          -2.242e-04  1.196e-05 -18.738  < 2e-16 ***
## EmergDec             3.606e+00  5.842e-01   6.173 1.16e-09 ***
## time_value:EmergDec -1.913e-04  3.174e-05  -6.028 2.74e-09 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03155 on 673 degrees of freedom
## Multiple R-squared:  0.4501,	Adjusted R-squared:  0.4477 
## F-statistic: 183.6 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "tx"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * EmergDec, data = ftime.policy.df)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.09715 -0.02065 -0.00380  0.01673  0.11688 
## 
## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)          4.549e+00  2.132e-01  21.340   <2e-16 ***
## time_value          -2.358e-04  1.177e-05 -20.041   <2e-16 ***
## EmergDec             5.108e+00  5.794e-01   8.816   <2e-16 ***
## time_value:EmergDec -2.711e-04  3.148e-05  -8.613   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03114 on 673 degrees of freedom
## Multiple R-squared:  0.5441,	Adjusted R-squared:  0.5421 
## F-statistic: 267.8 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "ut"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * EmergDec, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.151401 -0.023459 -0.008082  0.015514  0.157776 
## 
## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)          5.154e+00  2.628e-01  19.610  < 2e-16 ***
## time_value          -2.686e-04  1.451e-05 -18.508  < 2e-16 ***
## EmergDec             2.062e+00  6.743e-01   3.057  0.00232 ** 
## time_value:EmergDec -1.062e-04  3.665e-05  -2.898  0.00388 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03748 on 673 degrees of freedom
## Multiple R-squared:  0.4164,	Adjusted R-squared:  0.4138 
## F-statistic:   160 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "va"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * EmergDec, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.161008 -0.021667 -0.005693  0.018346  0.198565 
## 
## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)          4.787e+00  2.275e-01  21.040  < 2e-16 ***
## time_value          -2.481e-04  1.256e-05 -19.754  < 2e-16 ***
## EmergDec             4.465e+00  6.132e-01   7.281 9.31e-13 ***
## time_value:EmergDec -2.354e-04  3.332e-05  -7.064 4.04e-12 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03312 on 673 degrees of freedom
## Multiple R-squared:  0.5533,	Adjusted R-squared:  0.5513 
## F-statistic: 277.9 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "vt"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * EmergDec, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.127597 -0.025716 -0.002503  0.023175  0.194156 
## 
## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)          6.766e+00  2.633e-01  25.696   <2e-16 ***
## time_value          -3.550e-04  1.453e-05 -24.427   <2e-16 ***
## EmergDec             1.528e+00  7.157e-01   2.135   0.0332 *  
## time_value:EmergDec -7.719e-05  3.888e-05  -1.985   0.0475 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03846 on 673 degrees of freedom
## Multiple R-squared:  0.5338,	Adjusted R-squared:  0.5318 
## F-statistic: 256.9 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "wa"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * EmergDec, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.156309 -0.024259 -0.005259  0.021670  0.181113 
## 
## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)          4.686e+00  2.894e-01  16.195   <2e-16 ***
## time_value          -2.415e-04  1.598e-05 -15.114   <2e-16 ***
## EmergDec             1.346e+00  7.075e-01   1.902   0.0576 .  
## time_value:EmergDec -6.643e-05  3.847e-05  -1.727   0.0847 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.04042 on 673 degrees of freedom
## Multiple R-squared:  0.4004,	Adjusted R-squared:  0.3977 
## F-statistic: 149.8 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "wi"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * EmergDec, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.166781 -0.026754 -0.007107  0.021540  0.158694 
## 
## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)          5.790e+00  2.667e-01  21.708  < 2e-16 ***
## time_value          -3.025e-04  1.472e-05 -20.547  < 2e-16 ***
## EmergDec             3.895e+00  7.188e-01   5.418 8.39e-08 ***
## time_value:EmergDec -2.054e-04  3.906e-05  -5.260 1.94e-07 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03883 on 673 degrees of freedom
## Multiple R-squared:  0.4794,	Adjusted R-squared:  0.4771 
## F-statistic: 206.6 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "wv"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * EmergDec, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.079529 -0.024137 -0.005228  0.018867  0.136093 
## 
## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)          6.384e+00  2.270e-01  28.129   <2e-16 ***
## time_value          -3.353e-04  1.253e-05 -26.762   <2e-16 ***
## EmergDec             1.632e+00  6.327e-01   2.579   0.0101 *  
## time_value:EmergDec -8.354e-05  3.436e-05  -2.431   0.0153 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03349 on 673 degrees of freedom
## Multiple R-squared:  0.5888,	Adjusted R-squared:  0.587 
## F-statistic: 321.3 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "wy"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * EmergDec, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.071525 -0.026586 -0.007897  0.017685  0.161698 
## 
## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)          5.035e+00  2.523e-01  19.957  < 2e-16 ***
## time_value          -2.604e-04  1.393e-05 -18.699  < 2e-16 ***
## EmergDec             2.876e+00  6.857e-01   4.194 3.11e-05 ***
## time_value:EmergDec -1.524e-04  3.725e-05  -4.093 4.78e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03685 on 673 degrees of freedom
## Multiple R-squared:  0.458,	Adjusted R-squared:  0.4556 
## F-statistic: 189.6 on 3 and 673 DF,  p-value: < 2.2e-16
```

#### School Closure


```r
# The fraction of mobile devices that did not leave the immediate area of their home (SafeGraph’s completely_home_device_count / device_count)
for(state in states){
  print(state)
  print("---------------------")
  chome.state <- chome %>% filter(geo_value == state)
  policy.state <- policy %>% filter(StatePostal == state)
  policy_signal.state <- getSumOfPolicy(policy.state, STARTDATE, ENDDATE)
  # left join mobility with policy signal by time 
  ftime.policy.df <- left_join(chome.state , policy_signal.state, by = "time_value")
  lm.fit <- lm(value~time_value*SchoolClose,data=ftime.policy.df)
  print(summary(lm.fit))
}
```

```
## [1] "ak"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * SchoolClose, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.113825 -0.025961 -0.002819  0.026542  0.128292 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             3.252e+00  1.530e-01  21.249  < 2e-16 ***
## time_value             -1.617e-04  8.411e-06 -19.222  < 2e-16 ***
## SchoolClose             7.983e+00  1.588e+00   5.026 6.43e-07 ***
## time_value:SchoolClose -4.316e-04  8.632e-05  -5.000 7.33e-07 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03847 on 673 degrees of freedom
## Multiple R-squared:  0.3846,	Adjusted R-squared:  0.3818 
## F-statistic: 140.2 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "al"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * SchoolClose, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.081719 -0.022119 -0.002799  0.019811  0.141825 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             2.091e+00  1.297e-01  16.121  < 2e-16 ***
## time_value             -1.015e-04  7.124e-06 -14.242  < 2e-16 ***
## SchoolClose             1.567e+01  2.146e+00   7.299 8.19e-13 ***
## time_value:SchoolClose -8.496e-04  1.167e-04  -7.279 9.42e-13 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03413 on 673 degrees of freedom
## Multiple R-squared:  0.3187,	Adjusted R-squared:  0.3157 
## F-statistic: 104.9 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "ar"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * SchoolClose, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.073620 -0.021604 -0.004841  0.017923  0.125186 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             2.647e+00  1.429e-01  18.522  < 2e-16 ***
## time_value             -1.316e-04  7.860e-06 -16.748  < 2e-16 ***
## SchoolClose             8.135e+00  1.064e+00   7.648 7.10e-14 ***
## time_value:SchoolClose -4.394e-04  5.777e-05  -7.606 9.54e-14 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03375 on 673 degrees of freedom
## Multiple R-squared:  0.3682,	Adjusted R-squared:  0.3654 
## F-statistic: 130.7 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "az"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * SchoolClose, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.091907 -0.023849 -0.003341  0.025266  0.107213 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             2.285e+00  1.493e-01  15.309  < 2e-16 ***
## time_value             -1.083e-04  8.210e-06 -13.189  < 2e-16 ***
## SchoolClose             4.651e+00  1.196e+00   3.890 0.000110 ***
## time_value:SchoolClose -2.492e-04  6.494e-05  -3.837 0.000136 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03589 on 673 degrees of freedom
## Multiple R-squared:  0.3342,	Adjusted R-squared:  0.3312 
## F-statistic: 112.6 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "ca"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * SchoolClose, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.110627 -0.019955 -0.004798  0.016761  0.115736 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             5.004e+00  2.069e-01   24.19   <2e-16 ***
## time_value             -2.598e-04  1.142e-05  -22.75   <2e-16 ***
## SchoolClose             5.938e+00  5.623e-01   10.56   <2e-16 ***
## time_value:SchoolClose -3.132e-04  3.055e-05  -10.26   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03022 on 673 degrees of freedom
## Multiple R-squared:  0.7219,	Adjusted R-squared:  0.7206 
## F-statistic: 582.3 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "co"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * SchoolClose, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.102593 -0.024419 -0.002579  0.022147  0.179653 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             2.404e+00  1.535e-01   15.66   <2e-16 ***
## time_value             -1.150e-04  8.432e-06  -13.64   <2e-16 ***
## SchoolClose             2.078e+01  1.906e+00   10.91   <2e-16 ***
## time_value:SchoolClose -1.125e-03  1.036e-04  -10.87   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03916 on 673 degrees of freedom
## Multiple R-squared:  0.4241,	Adjusted R-squared:  0.4215 
## F-statistic: 165.2 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "ct"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * SchoolClose, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.111449 -0.027086 -0.003499  0.024553  0.242033 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             2.030e+00  1.641e-01   12.37   <2e-16 ***
## time_value             -9.548e-05  9.021e-06  -10.58   <2e-16 ***
## SchoolClose             2.075e+01  1.724e+00   12.04   <2e-16 ***
## time_value:SchoolClose -1.122e-03  9.366e-05  -11.98   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.04128 on 673 degrees of freedom
## Multiple R-squared:  0.5172,	Adjusted R-squared:  0.5151 
## F-statistic: 240.3 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "dc"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * SchoolClose, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.081030 -0.021309 -0.004592  0.018198  0.169819 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             2.619e+00  2.246e-01   11.66   <2e-16 ***
## time_value             -1.269e-04  1.239e-05  -10.24   <2e-16 ***
## SchoolClose             9.128e+00  6.260e-01   14.58   <2e-16 ***
## time_value:SchoolClose -4.878e-04  3.400e-05  -14.35   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03314 on 673 degrees of freedom
## Multiple R-squared:  0.682,	Adjusted R-squared:  0.6806 
## F-statistic: 481.1 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "de"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * SchoolClose, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.093040 -0.024825 -0.006167  0.016593  0.166592 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             4.943e+00  2.424e-01  20.387   <2e-16 ***
## time_value             -2.554e-04  1.338e-05 -19.086   <2e-16 ***
## SchoolClose             5.941e+00  6.590e-01   9.017   <2e-16 ***
## time_value:SchoolClose -3.158e-04  3.580e-05  -8.822   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03541 on 673 degrees of freedom
## Multiple R-squared:  0.5268,	Adjusted R-squared:  0.5247 
## F-statistic: 249.8 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "fl"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * SchoolClose, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.105270 -0.022168 -0.001969  0.021579  0.130148 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             1.530e+00  1.463e-01  10.457  < 2e-16 ***
## time_value             -6.815e-05  8.040e-06  -8.476  < 2e-16 ***
## SchoolClose             1.181e+01  1.441e+00   8.197 1.25e-15 ***
## time_value:SchoolClose -6.377e-04  7.828e-05  -8.146 1.83e-15 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03644 on 673 degrees of freedom
## Multiple R-squared:  0.3877,	Adjusted R-squared:  0.385 
## F-statistic: 142.1 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "ga"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * SchoolClose, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.084242 -0.023890 -0.001934  0.020352  0.127183 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             1.746e+00  1.337e-01  13.057   <2e-16 ***
## time_value             -8.121e-05  7.347e-06 -11.052   <2e-16 ***
## SchoolClose             1.572e+01  1.669e+00   9.418   <2e-16 ***
## time_value:SchoolClose -8.511e-04  9.073e-05  -9.380   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03432 on 673 degrees of freedom
## Multiple R-squared:  0.3797,	Adjusted R-squared:  0.3769 
## F-statistic: 137.3 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "hi"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * SchoolClose, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.106666 -0.024007 -0.003525  0.024183  0.153436 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             1.494e+00  1.793e-01   8.335 4.36e-16 ***
## time_value             -6.729e-05  9.871e-06  -6.817 2.07e-11 ***
## SchoolClose             5.266e+00  9.981e-01   5.276 1.78e-07 ***
## time_value:SchoolClose -2.812e-04  5.418e-05  -5.190 2.78e-07 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03901 on 673 degrees of freedom
## Multiple R-squared:  0.4097,	Adjusted R-squared:  0.4071 
## F-statistic: 155.7 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "ia"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * SchoolClose, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.079593 -0.026832 -0.007375  0.019025  0.165356 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             5.576e+00  2.612e-01  21.349  < 2e-16 ***
## time_value             -2.924e-04  1.442e-05 -20.284  < 2e-16 ***
## SchoolClose             5.228e+00  7.281e-01   7.180 1.84e-12 ***
## time_value:SchoolClose -2.784e-04  3.955e-05  -7.039 4.79e-12 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03854 on 673 degrees of freedom
## Multiple R-squared:  0.4947,	Adjusted R-squared:  0.4924 
## F-statistic: 219.6 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "id"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * SchoolClose, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.084090 -0.025128 -0.003762  0.018820  0.125974 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             2.999e+00  1.413e-01  21.224  < 2e-16 ***
## time_value             -1.482e-04  7.762e-06 -19.088  < 2e-16 ***
## SchoolClose             1.277e+01  2.058e+00   6.206 9.48e-10 ***
## time_value:SchoolClose -6.916e-04  1.118e-04  -6.183 1.09e-09 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03665 on 673 degrees of freedom
## Multiple R-squared:  0.3981,	Adjusted R-squared:  0.3954 
## F-statistic: 148.4 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "il"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * SchoolClose, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.117641 -0.026846 -0.000537  0.023549  0.224757 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             1.922e+00  1.476e-01  13.027   <2e-16 ***
## time_value             -8.957e-05  8.105e-06 -11.052   <2e-16 ***
## SchoolClose             2.326e+01  2.405e+00   9.670   <2e-16 ***
## time_value:SchoolClose -1.259e-03  1.308e-04  -9.626   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03882 on 673 degrees of freedom
## Multiple R-squared:  0.5029,	Adjusted R-squared:  0.5007 
## F-statistic:   227 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "in"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * SchoolClose, data = ftime.policy.df)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.08958 -0.02434 -0.00152  0.01722  0.16802 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             2.863e+00  1.362e-01   21.02   <2e-16 ***
## time_value             -1.417e-04  7.480e-06  -18.95   <2e-16 ***
## SchoolClose             2.190e+01  2.115e+00   10.36   <2e-16 ***
## time_value:SchoolClose -1.187e-03  1.150e-04  -10.32   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03563 on 673 degrees of freedom
## Multiple R-squared:  0.4869,	Adjusted R-squared:  0.4846 
## F-statistic: 212.9 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "ks"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * SchoolClose, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.098547 -0.026056 -0.001322  0.021672  0.178042 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             2.747e+00  1.422e-01  19.317   <2e-16 ***
## time_value             -1.356e-04  7.805e-06 -17.367   <2e-16 ***
## SchoolClose             9.638e+00  4.098e+00   2.352    0.019 *  
## time_value:SchoolClose -5.200e-04  2.230e-04  -2.331    0.020 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03849 on 673 degrees of freedom
## Multiple R-squared:  0.4187,	Adjusted R-squared:  0.4161 
## F-statistic: 161.6 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "ky"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * SchoolClose, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.060263 -0.024298 -0.004796  0.021060  0.158447 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             5.738e+00  2.209e-01  25.982  < 2e-16 ***
## time_value             -3.008e-04  1.219e-05 -24.671  < 2e-16 ***
## SchoolClose             2.696e+00  6.157e-01   4.380 1.38e-05 ***
## time_value:SchoolClose -1.412e-04  3.344e-05  -4.222 2.76e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03259 on 673 degrees of freedom
## Multiple R-squared:  0.5514,	Adjusted R-squared:  0.5494 
## F-statistic: 275.8 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "la"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * SchoolClose, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.156787 -0.022669 -0.003016  0.019988  0.141535 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             2.196e+00  1.274e-01  17.234  < 2e-16 ***
## time_value             -1.062e-04  6.997e-06 -15.179  < 2e-16 ***
## SchoolClose             1.253e+01  1.988e+00   6.302 5.31e-10 ***
## time_value:SchoolClose -6.782e-04  1.081e-04  -6.274 6.32e-10 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03351 on 673 degrees of freedom
## Multiple R-squared:  0.3675,	Adjusted R-squared:  0.3647 
## F-statistic: 130.4 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "ma"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * SchoolClose, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.108904 -0.026022 -0.002082  0.024543  0.218297 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             1.696e+00  1.698e-01   9.987  < 2e-16 ***
## time_value             -7.663e-05  9.341e-06  -8.203 1.19e-15 ***
## SchoolClose             1.999e+01  1.374e+00  14.548  < 2e-16 ***
## time_value:SchoolClose -1.080e-03  7.464e-05 -14.472  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.04086 on 673 degrees of freedom
## Multiple R-squared:  0.5396,	Adjusted R-squared:  0.5375 
## F-statistic: 262.9 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "md"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * SchoolClose, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.133514 -0.028199 -0.001454  0.026827  0.257196 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             1.058e+00  1.722e-01   6.144 1.37e-09 ***
## time_value             -4.105e-05  9.461e-06  -4.339 1.65e-05 ***
## SchoolClose             1.803e+01  1.907e+00   9.453  < 2e-16 ***
## time_value:SchoolClose -9.742e-04  1.036e-04  -9.400  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.04365 on 673 degrees of freedom
## Multiple R-squared:  0.4593,	Adjusted R-squared:  0.4569 
## F-statistic: 190.6 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "me"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * SchoolClose, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.097390 -0.024363 -0.001864  0.020203  0.242797 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             3.489e+00  1.503e-01  23.203  < 2e-16 ***
## time_value             -1.744e-04  8.262e-06 -21.112  < 2e-16 ***
## SchoolClose             1.478e+01  1.848e+00   7.995 5.65e-15 ***
## time_value:SchoolClose -7.999e-04  1.004e-04  -7.967 6.95e-15 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03797 on 673 degrees of freedom
## Multiple R-squared:  0.4506,	Adjusted R-squared:  0.4482 
## F-statistic:   184 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "mi"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * SchoolClose, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.143952 -0.026483 -0.002328  0.025705  0.154182 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             2.826e+00  1.512e-01   18.70   <2e-16 ***
## time_value             -1.386e-04  8.304e-06  -16.69   <2e-16 ***
## SchoolClose             2.579e+01  2.282e+00   11.30   <2e-16 ***
## time_value:SchoolClose -1.397e-03  1.241e-04  -11.26   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03957 on 673 degrees of freedom
## Multiple R-squared:  0.5181,	Adjusted R-squared:  0.516 
## F-statistic: 241.2 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "mn"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * SchoolClose, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.096094 -0.026323 -0.002741  0.024078  0.153797 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             2.394e+00  1.633e-01   14.66   <2e-16 ***
## time_value             -1.153e-04  8.975e-06  -12.85   <2e-16 ***
## SchoolClose             1.691e+01  1.669e+00   10.13   <2e-16 ***
## time_value:SchoolClose -9.147e-04  9.067e-05  -10.09   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.04085 on 673 degrees of freedom
## Multiple R-squared:  0.4148,	Adjusted R-squared:  0.4122 
## F-statistic:   159 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "mo"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * SchoolClose, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.088162 -0.025406 -0.003396  0.019901  0.187614 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             2.716e+00  1.448e-01   18.76   <2e-16 ***
## time_value             -1.338e-04  7.952e-06  -16.83   <2e-16 ***
## SchoolClose             1.956e+01  1.958e+00    9.99   <2e-16 ***
## time_value:SchoolClose -1.060e-03  1.064e-04   -9.96   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03728 on 673 degrees of freedom
## Multiple R-squared:  0.4142,	Adjusted R-squared:  0.4116 
## F-statistic: 158.6 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "ms"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * SchoolClose, data = ftime.policy.df)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.07386 -0.02155 -0.00295  0.01654  0.14466 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             1.997e+00  1.186e-01  16.838  < 2e-16 ***
## time_value             -9.642e-05  6.509e-06 -14.812  < 2e-16 ***
## SchoolClose             2.073e+01  3.136e+00   6.611 7.77e-11 ***
## time_value:SchoolClose -1.125e-03  1.706e-04  -6.594 8.64e-11 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03197 on 673 degrees of freedom
## Multiple R-squared:  0.3477,	Adjusted R-squared:  0.3448 
## F-statistic: 119.6 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "mt"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * SchoolClose, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.091803 -0.025740 -0.003505  0.020946  0.136383 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             2.717e+00  1.399e-01  19.419   <2e-16 ***
## time_value             -1.325e-04  7.678e-06 -17.253   <2e-16 ***
## SchoolClose            -5.291e+00  6.143e+00  -0.861    0.389    
## time_value:SchoolClose  2.912e-04  3.345e-04   0.870    0.384    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.0383 on 673 degrees of freedom
## Multiple R-squared:  0.339,	Adjusted R-squared:  0.3361 
## F-statistic: 115.1 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "nc"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * SchoolClose, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.064816 -0.020474 -0.004477  0.016972  0.126229 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             3.913e+00  2.077e-01  18.843  < 2e-16 ***
## time_value             -2.004e-04  1.146e-05 -17.487  < 2e-16 ***
## SchoolClose             4.114e+00  5.789e-01   7.107 3.04e-12 ***
## time_value:SchoolClose -2.185e-04  3.144e-05  -6.951 8.60e-12 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03064 on 673 degrees of freedom
## Multiple R-squared:  0.4436,	Adjusted R-squared:  0.4411 
## F-statistic: 178.8 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "nd"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * SchoolClose, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.107092 -0.025993 -0.005294  0.022949  0.160820 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             3.220e+00  1.711e-01  18.812  < 2e-16 ***
## time_value             -1.610e-04  9.403e-06 -17.118  < 2e-16 ***
## SchoolClose             1.181e+01  2.082e+00   5.672 2.10e-08 ***
## time_value:SchoolClose -6.395e-04  1.132e-04  -5.649 2.38e-08 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.04388 on 673 degrees of freedom
## Multiple R-squared:  0.3453,	Adjusted R-squared:  0.3423 
## F-statistic: 118.3 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "ne"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * SchoolClose, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.092182 -0.026045 -0.003285  0.022049  0.154089 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             3.541e+00  1.569e-01  22.576  < 2e-16 ***
## time_value             -1.792e-04  8.618e-06 -20.792  < 2e-16 ***
## SchoolClose             1.662e+01  2.198e+00   7.559 1.33e-13 ***
## time_value:SchoolClose -8.991e-04  1.194e-04  -7.529 1.65e-13 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.04021 on 673 degrees of freedom
## Multiple R-squared:  0.4534,	Adjusted R-squared:  0.4509 
## F-statistic: 186.1 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "nh"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * SchoolClose, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.120127 -0.024783 -0.002111  0.021487  0.268532 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             2.385e+00  1.613e-01   14.79  < 2e-16 ***
## time_value             -1.147e-04  8.859e-06  -12.94  < 2e-16 ***
## SchoolClose             1.527e+01  2.103e+00    7.26 1.07e-12 ***
## time_value:SchoolClose -8.257e-04  1.144e-04   -7.22 1.41e-12 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.04165 on 673 degrees of freedom
## Multiple R-squared:  0.3922,	Adjusted R-squared:  0.3895 
## F-statistic: 144.8 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "nj"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * SchoolClose, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.130505 -0.028340 -0.002308  0.025049  0.171646 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             1.581e+00  1.779e-01   8.884  < 2e-16 ***
## time_value             -7.106e-05  9.785e-06  -7.262 1.06e-12 ***
## SchoolClose             2.847e+01  1.527e+00  18.647  < 2e-16 ***
## time_value:SchoolClose -1.539e-03  8.294e-05 -18.561  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.04324 on 673 degrees of freedom
## Multiple R-squared:  0.6398,	Adjusted R-squared:  0.6382 
## F-statistic: 398.5 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "nm"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * SchoolClose, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.070889 -0.021668 -0.004627  0.017887  0.128296 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             5.054e+00  2.117e-01  23.874   <2e-16 ***
## time_value             -2.615e-04  1.169e-05 -22.382   <2e-16 ***
## SchoolClose             8.328e-01  5.902e-01   1.411    0.159    
## time_value:SchoolClose -3.970e-05  3.205e-05  -1.238    0.216    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03124 on 673 degrees of freedom
## Multiple R-squared:  0.4812,	Adjusted R-squared:  0.4789 
## F-statistic: 208.1 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "nv"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * SchoolClose, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.107529 -0.024389  0.000064  0.024540  0.117622 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             1.831e+00  1.437e-01  12.738  < 2e-16 ***
## time_value             -8.290e-05  7.891e-06 -10.506  < 2e-16 ***
## SchoolClose             1.389e+01  3.018e+00   4.604 4.97e-06 ***
## time_value:SchoolClose -7.513e-04  1.642e-04  -4.575 5.66e-06 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03842 on 673 degrees of freedom
## Multiple R-squared:  0.369,	Adjusted R-squared:  0.3662 
## F-statistic: 131.2 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "ny"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * SchoolClose, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.136069 -0.025124 -0.001708  0.024883  0.181048 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             1.974e+00  1.637e-01   12.06   <2e-16 ***
## time_value             -9.122e-05  8.999e-06  -10.14   <2e-16 ***
## SchoolClose             2.292e+01  1.512e+00   15.16   <2e-16 ***
## time_value:SchoolClose -1.239e-03  8.215e-05  -15.08   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.04031 on 673 degrees of freedom
## Multiple R-squared:  0.5911,	Adjusted R-squared:  0.5892 
## F-statistic: 324.2 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "oh"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * SchoolClose, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.103869 -0.024419 -0.001981  0.021686  0.205269 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             2.917e+00  1.400e-01   20.84   <2e-16 ***
## time_value             -1.440e-04  7.687e-06  -18.73   <2e-16 ***
## SchoolClose             2.252e+01  2.079e+00   10.83   <2e-16 ***
## time_value:SchoolClose -1.220e-03  1.131e-04  -10.79   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03655 on 673 degrees of freedom
## Multiple R-squared:  0.4929,	Adjusted R-squared:  0.4906 
## F-statistic:   218 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "ok"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * SchoolClose, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.089668 -0.025750 -0.003575  0.018949  0.156860 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             2.680e+00  1.419e-01  18.884  < 2e-16 ***
## time_value             -1.332e-04  7.800e-06 -17.080  < 2e-16 ***
## SchoolClose             1.210e+01  1.471e+00   8.227 9.95e-16 ***
## time_value:SchoolClose -6.548e-04  7.994e-05  -8.191 1.30e-15 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03562 on 673 degrees of freedom
## Multiple R-squared:  0.3916,	Adjusted R-squared:  0.3889 
## F-statistic: 144.4 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "or"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * SchoolClose, data = ftime.policy.df)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.10545 -0.02468 -0.00068  0.02259  0.14463 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             1.954e+00  1.447e-01  13.504  < 2e-16 ***
## time_value             -8.978e-05  7.946e-06 -11.298  < 2e-16 ***
## SchoolClose             1.001e+01  2.151e+00   4.653 3.94e-06 ***
## time_value:SchoolClose -5.404e-04  1.170e-04  -4.619 4.61e-06 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03781 on 673 degrees of freedom
## Multiple R-squared:   0.34,	Adjusted R-squared:  0.3371 
## F-statistic: 115.6 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "pa"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * SchoolClose, data = ftime.policy.df)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.14551 -0.02541 -0.00123  0.02371  0.18187 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             2.520e+00  1.501e-01  16.782   <2e-16 ***
## time_value             -1.213e-04  8.247e-06 -14.703   <2e-16 ***
## SchoolClose             2.130e+01  2.233e+00   9.538   <2e-16 ***
## time_value:SchoolClose -1.153e-03  1.214e-04  -9.495   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03924 on 673 degrees of freedom
## Multiple R-squared:  0.4869,	Adjusted R-squared:  0.4846 
## F-statistic: 212.9 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "ri"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * SchoolClose, data = ftime.policy.df)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.11701 -0.02537 -0.00374  0.02388  0.19779 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             2.989e+00  1.803e-01   16.58   <2e-16 ***
## time_value             -1.476e-04  9.927e-06  -14.87   <2e-16 ***
## SchoolClose             1.449e+01  1.040e+00   13.93   <2e-16 ***
## time_value:SchoolClose -7.817e-04  5.646e-05  -13.85   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03972 on 673 degrees of freedom
## Multiple R-squared:  0.4885,	Adjusted R-squared:  0.4862 
## F-statistic: 214.2 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "sc"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * SchoolClose, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.077652 -0.021180 -0.001187  0.019570  0.125116 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             2.018e+00  1.231e-01  16.396  < 2e-16 ***
## time_value             -9.629e-05  6.760e-06 -14.244  < 2e-16 ***
## SchoolClose             1.195e+01  1.976e+00   6.045 2.47e-09 ***
## time_value:SchoolClose -6.472e-04  1.075e-04  -6.022 2.83e-09 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03237 on 673 degrees of freedom
## Multiple R-squared:  0.3187,	Adjusted R-squared:  0.3156 
## F-statistic: 104.9 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "sd"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * SchoolClose, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.097919 -0.027621 -0.003649  0.022156  0.209240 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             3.060e+00  1.514e-01  20.220   <2e-16 ***
## time_value             -1.523e-04  8.303e-06 -18.344   <2e-16 ***
## SchoolClose            -1.664e+01  9.074e+00  -1.833   0.0672 .  
## time_value:SchoolClose  9.097e-04  4.943e-04   1.841   0.0661 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.04163 on 673 degrees of freedom
## Multiple R-squared:  0.3646,	Adjusted R-squared:  0.3618 
## F-statistic: 128.7 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "tn"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * SchoolClose, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.089717 -0.023446 -0.001276  0.019976  0.152701 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             2.426e+00  1.324e-01  18.328   <2e-16 ***
## time_value             -1.185e-04  7.276e-06 -16.285   <2e-16 ***
## SchoolClose             1.153e+01  1.356e+00   8.504   <2e-16 ***
## time_value:SchoolClose -6.240e-04  7.371e-05  -8.466   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03321 on 673 degrees of freedom
## Multiple R-squared:  0.3908,	Adjusted R-squared:  0.3881 
## F-statistic: 143.9 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "tx"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * SchoolClose, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.097788 -0.024353 -0.002766  0.023800  0.121354 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             1.889e+00  1.385e-01  13.637   <2e-16 ***
## time_value             -8.862e-05  7.610e-06 -11.645   <2e-16 ***
## SchoolClose             1.514e+01  1.653e+00   9.158   <2e-16 ***
## time_value:SchoolClose -8.189e-04  8.985e-05  -9.114   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03525 on 673 degrees of freedom
## Multiple R-squared:  0.4157,	Adjusted R-squared:  0.4131 
## F-statistic: 159.6 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "ut"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * SchoolClose, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.064975 -0.024729 -0.009033  0.015149  0.141794 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             5.096e+00  2.363e-01  21.560  < 2e-16 ***
## time_value             -2.653e-04  1.304e-05 -20.339  < 2e-16 ***
## SchoolClose             4.293e+00  6.588e-01   6.517 1.41e-10 ***
## time_value:SchoolClose -2.270e-04  3.578e-05  -6.343 4.14e-10 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03487 on 673 degrees of freedom
## Multiple R-squared:  0.4948,	Adjusted R-squared:  0.4926 
## F-statistic: 219.7 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "va"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * SchoolClose, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.111727 -0.025646 -0.001484  0.025057  0.228770 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             1.486e+00  1.495e-01   9.939  < 2e-16 ***
## time_value             -6.547e-05  8.213e-06  -7.972 6.71e-15 ***
## SchoolClose             1.431e+01  2.065e+00   6.930 9.89e-12 ***
## time_value:SchoolClose -7.735e-04  1.123e-04  -6.890 1.29e-11 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03883 on 673 degrees of freedom
## Multiple R-squared:  0.3861,	Adjusted R-squared:  0.3834 
## F-statistic: 141.1 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "vt"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * SchoolClose, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.124791 -0.025083 -0.001614  0.027387  0.216716 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             4.265e+00  1.847e-01  23.093  < 2e-16 ***
## time_value             -2.167e-04  1.016e-05 -21.320  < 2e-16 ***
## SchoolClose             7.705e+00  1.163e+00   6.622 7.24e-11 ***
## time_value:SchoolClose -4.150e-04  6.317e-05  -6.571 1.00e-10 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.04166 on 673 degrees of freedom
## Multiple R-squared:  0.453,	Adjusted R-squared:  0.4506 
## F-statistic: 185.8 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "wa"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * SchoolClose, data = ftime.policy.df)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.10098 -0.02654 -0.00330  0.02421  0.20631 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             1.434e+00  1.640e-01   8.745  < 2e-16 ***
## time_value             -6.161e-05  9.018e-06  -6.832 1.88e-11 ***
## SchoolClose             1.538e+01  1.556e+00   9.883  < 2e-16 ***
## time_value:SchoolClose -8.311e-04  8.454e-05  -9.832  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.04062 on 673 degrees of freedom
## Multiple R-squared:  0.3942,	Adjusted R-squared:  0.3915 
## F-statistic:   146 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "wi"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * SchoolClose, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.106494 -0.027022 -0.002347  0.023470  0.182672 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             2.887e+00  1.541e-01  18.729  < 2e-16 ***
## time_value             -1.419e-04  8.464e-06 -16.766  < 2e-16 ***
## SchoolClose             2.112e+01  2.721e+00   7.762 3.11e-14 ***
## time_value:SchoolClose -1.144e-03  1.480e-04  -7.733 3.84e-14 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.04074 on 673 degrees of freedom
## Multiple R-squared:  0.4268,	Adjusted R-squared:  0.4242 
## F-statistic:   167 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "wv"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * SchoolClose, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.081412 -0.024107 -0.004942  0.018954  0.135753 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             6.433e+00  2.289e-01  28.105   <2e-16 ***
## time_value             -3.379e-04  1.263e-05 -26.749   <2e-16 ***
## SchoolClose             1.363e+00  6.273e-01   2.172   0.0302 *  
## time_value:SchoolClose -6.893e-05  3.408e-05  -2.023   0.0435 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03355 on 673 degrees of freedom
## Multiple R-squared:  0.5875,	Adjusted R-squared:  0.5856 
## F-statistic: 319.5 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "wy"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * SchoolClose, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.081084 -0.026943 -0.003561  0.018717  0.161433 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             3.313e+00  1.423e-01  23.279  < 2e-16 ***
## time_value             -1.651e-04  7.814e-06 -21.134  < 2e-16 ***
## SchoolClose             1.255e+01  2.878e+00   4.361 1.50e-05 ***
## time_value:SchoolClose -6.800e-04  1.566e-04  -4.344 1.62e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.0379 on 673 degrees of freedom
## Multiple R-squared:  0.4268,	Adjusted R-squared:  0.4242 
## F-statistic:   167 on 3 and 673 DF,  p-value: < 2.2e-16
```

#### Bar Restriction


```r
# The fraction of mobile devices that did not leave the immediate area of their home (SafeGraph’s completely_home_device_count / device_count)
for(state in states){
  print(state)
  print("---------------------")
  chome.state <- chome %>% filter(geo_value == state)
  policy.state <- policy %>% filter(StatePostal == state)
  policy_signal.state <- getSumOfPolicy(policy.state, STARTDATE, ENDDATE)
  # left join mobility with policy signal by time 
  ftime.policy.df <- left_join(chome.state , policy_signal.state, by = "time_value")
  lm.fit <- lm(value~time_value*BarRestrict,data=ftime.policy.df)
  print(summary(lm.fit))
  }
```

```
## [1] "ak"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * BarRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.117085 -0.024855 -0.001525  0.025576  0.120719 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             3.008e+00  1.422e-01  21.152   <2e-16 ***
## time_value             -1.481e-04  7.804e-06 -18.982   <2e-16 ***
## BarRestrict             1.117e+01  4.585e+00   2.436   0.0151 *  
## time_value:BarRestrict -6.049e-04  2.496e-04  -2.424   0.0156 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.0386 on 673 degrees of freedom
## Multiple R-squared:  0.3801,	Adjusted R-squared:  0.3774 
## F-statistic: 137.6 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "al"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * BarRestrict, data = ftime.policy.df)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.07190 -0.02256 -0.00567  0.01790  0.15351 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             3.893e+00  2.169e-01  17.949  < 2e-16 ***
## time_value             -2.011e-04  1.197e-05 -16.803  < 2e-16 ***
## BarRestrict             3.339e+00  6.204e-01   5.382 1.02e-07 ***
## time_value:BarRestrict -1.774e-04  3.369e-05  -5.265 1.89e-07 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03233 on 673 degrees of freedom
## Multiple R-squared:  0.3889,	Adjusted R-squared:  0.3861 
## F-statistic: 142.7 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "ar"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * BarRestrict, data = ftime.policy.df)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.06290 -0.02266 -0.00673  0.01728  0.13065 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             4.002e+00  2.189e-01  18.282  < 2e-16 ***
## time_value             -2.066e-04  1.208e-05 -17.101  < 2e-16 ***
## BarRestrict             3.829e+00  6.260e-01   6.117 1.62e-09 ***
## time_value:BarRestrict -2.041e-04  3.399e-05  -6.003 3.17e-09 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03262 on 673 degrees of freedom
## Multiple R-squared:  0.4096,	Adjusted R-squared:  0.407 
## F-statistic: 155.6 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "az"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * BarRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.072551 -0.022397 -0.003798  0.017718  0.139349 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             4.022e+00  2.148e-01  18.719  < 2e-16 ***
## time_value             -2.043e-04  1.185e-05 -17.232  < 2e-16 ***
## BarRestrict             4.550e+00  6.833e-01   6.659 5.73e-11 ***
## time_value:BarRestrict -2.417e-04  3.708e-05  -6.520 1.38e-10 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03332 on 673 degrees of freedom
## Multiple R-squared:  0.426,	Adjusted R-squared:  0.4235 
## F-statistic: 166.5 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "ca"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * BarRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.072793 -0.020070 -0.005262  0.016497  0.119157 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             4.832e+00  2.057e-01   23.50   <2e-16 ***
## time_value             -2.503e-04  1.135e-05  -22.05   <2e-16 ***
## BarRestrict             6.660e+00  5.733e-01   11.62   <2e-16 ***
## time_value:BarRestrict -3.525e-04  3.114e-05  -11.32   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03035 on 673 degrees of freedom
## Multiple R-squared:  0.7195,	Adjusted R-squared:  0.7182 
## F-statistic: 575.4 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "co"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * BarRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.073780 -0.025394 -0.008116  0.021152  0.168897 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             4.790e+00  2.474e-01  19.360   <2e-16 ***
## time_value             -2.470e-04  1.366e-05 -18.090   <2e-16 ***
## BarRestrict             6.945e+00  6.956e-01   9.985   <2e-16 ***
## time_value:BarRestrict -3.709e-04  3.778e-05  -9.819   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03663 on 673 degrees of freedom
## Multiple R-squared:  0.4962,	Adjusted R-squared:  0.494 
## F-statistic:   221 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "ct"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * BarRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.082806 -0.027542 -0.008767  0.019732  0.218045 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             4.711e+00  2.636e-01   17.88   <2e-16 ***
## time_value             -2.438e-04  1.455e-05  -16.76   <2e-16 ***
## BarRestrict             9.846e+00  7.348e-01   13.40   <2e-16 ***
## time_value:BarRestrict -5.267e-04  3.991e-05  -13.20   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03889 on 673 degrees of freedom
## Multiple R-squared:  0.5713,	Adjusted R-squared:  0.5694 
## F-statistic:   299 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "dc"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * BarRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.081030 -0.021309 -0.004592  0.018198  0.169819 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             2.619e+00  2.246e-01   11.66   <2e-16 ***
## time_value             -1.269e-04  1.239e-05  -10.24   <2e-16 ***
## BarRestrict             9.128e+00  6.260e-01   14.58   <2e-16 ***
## time_value:BarRestrict -4.878e-04  3.400e-05  -14.35   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03314 on 673 degrees of freedom
## Multiple R-squared:  0.682,	Adjusted R-squared:  0.6806 
## F-statistic: 481.1 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "de"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * BarRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.080551 -0.025135 -0.006188  0.016862  0.167481 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             4.823e+00  2.394e-01  20.147   <2e-16 ***
## time_value             -2.488e-04  1.321e-05 -18.830   <2e-16 ***
## BarRestrict             6.536e+00  6.673e-01   9.795   <2e-16 ***
## time_value:BarRestrict -3.481e-04  3.625e-05  -9.605   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03532 on 673 degrees of freedom
## Multiple R-squared:  0.5292,	Adjusted R-squared:  0.5271 
## F-statistic: 252.2 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "fl"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * BarRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.099256 -0.022331 -0.003544  0.019611  0.124963 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             2.175e+00  1.671e-01   13.02   <2e-16 ***
## time_value             -1.039e-04  9.204e-06  -11.29   <2e-16 ***
## BarRestrict             9.079e+00  8.532e-01   10.64   <2e-16 ***
## time_value:BarRestrict -4.886e-04  4.631e-05  -10.55   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03513 on 673 degrees of freedom
## Multiple R-squared:  0.4309,	Adjusted R-squared:  0.4284 
## F-statistic: 169.9 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "ga"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * BarRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.063657 -0.021225 -0.006285  0.016656  0.157396 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             3.525e+00  2.196e-01  16.053   <2e-16 ***
## time_value             -1.796e-04  1.212e-05 -14.820   <2e-16 ***
## BarRestrict             5.981e+00  6.560e-01   9.117   <2e-16 ***
## time_value:BarRestrict -3.198e-04  3.561e-05  -8.980   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03328 on 673 degrees of freedom
## Multiple R-squared:  0.4166,	Adjusted R-squared:  0.414 
## F-statistic: 160.2 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "hi"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * BarRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.098179 -0.020605 -0.002689  0.017212  0.150900 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             4.633e+00  2.018e-01  22.956  < 2e-16 ***
## time_value             -2.409e-04  1.114e-05 -21.625  < 2e-16 ***
## BarRestrict             1.976e+00  5.674e-01   3.482  0.00053 ***
## time_value:BarRestrict -9.926e-05  3.082e-05  -3.221  0.00134 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.02988 on 673 degrees of freedom
## Multiple R-squared:  0.6537,	Adjusted R-squared:  0.6522 
## F-statistic: 423.6 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "ia"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * BarRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.069766 -0.026906 -0.007326  0.019199  0.165327 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             5.552e+00  2.597e-01  21.376  < 2e-16 ***
## time_value             -2.911e-04  1.433e-05 -20.306  < 2e-16 ***
## BarRestrict             5.413e+00  7.302e-01   7.413 3.72e-13 ***
## time_value:BarRestrict -2.884e-04  3.966e-05  -7.272 9.87e-13 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03845 on 673 degrees of freedom
## Multiple R-squared:  0.497,	Adjusted R-squared:  0.4947 
## F-statistic: 221.6 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "id"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * BarRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.064263 -0.024514 -0.007189  0.017043  0.120460 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             4.956e+00  2.268e-01  21.858  < 2e-16 ***
## time_value             -2.564e-04  1.251e-05 -20.493  < 2e-16 ***
## BarRestrict             3.148e+00  6.834e-01   4.607 4.89e-06 ***
## time_value:BarRestrict -1.666e-04  3.710e-05  -4.491 8.35e-06 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03448 on 673 degrees of freedom
## Multiple R-squared:  0.4674,	Adjusted R-squared:  0.465 
## F-statistic: 196.8 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "il"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * BarRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.079078 -0.025880 -0.005612  0.020277  0.199295 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             5.001e+00  2.386e-01   20.96   <2e-16 ***
## time_value             -2.599e-04  1.317e-05  -19.73   <2e-16 ***
## BarRestrict             8.188e+00  6.652e-01   12.31   <2e-16 ***
## time_value:BarRestrict -4.370e-04  3.613e-05  -12.10   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03521 on 673 degrees of freedom
## Multiple R-squared:  0.591,	Adjusted R-squared:  0.5892 
## F-statistic: 324.2 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "in"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * BarRestrict, data = ftime.policy.df)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.06679 -0.02535 -0.00653  0.02123  0.15068 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             4.840e+00  2.359e-01  20.519   <2e-16 ***
## time_value             -2.511e-04  1.302e-05 -19.285   <2e-16 ***
## BarRestrict             6.580e+00  6.576e-01  10.006   <2e-16 ***
## time_value:BarRestrict -3.521e-04  3.572e-05  -9.857   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03481 on 673 degrees of freedom
## Multiple R-squared:  0.5104,	Adjusted R-squared:  0.5082 
## F-statistic: 233.9 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "ks"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * BarRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.100310 -0.026156 -0.002284  0.020869  0.177787 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             2.666e+00  1.437e-01  18.560  < 2e-16 ***
## time_value             -1.310e-04  7.884e-06 -16.622  < 2e-16 ***
## BarRestrict             2.726e+01  5.629e+00   4.843 1.58e-06 ***
## time_value:BarRestrict -1.479e-03  3.063e-04  -4.828 1.71e-06 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03904 on 673 degrees of freedom
## Multiple R-squared:  0.4021,	Adjusted R-squared:  0.3994 
## F-statistic: 150.9 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "ky"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * BarRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.060263 -0.024298 -0.004796  0.021060  0.158447 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             5.738e+00  2.209e-01  25.982  < 2e-16 ***
## time_value             -3.008e-04  1.219e-05 -24.671  < 2e-16 ***
## BarRestrict             2.696e+00  6.157e-01   4.380 1.38e-05 ***
## time_value:BarRestrict -1.412e-04  3.344e-05  -4.222 2.76e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03259 on 673 degrees of freedom
## Multiple R-squared:  0.5514,	Adjusted R-squared:  0.5494 
## F-statistic: 275.8 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "la"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * BarRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.068490 -0.021029 -0.003876  0.017537  0.143264 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             4.037e+00  2.044e-01  19.746   <2e-16 ***
## time_value             -2.081e-04  1.128e-05 -18.441   <2e-16 ***
## BarRestrict             5.381e+00  5.748e-01   9.362   <2e-16 ***
## time_value:BarRestrict -2.876e-04  3.122e-05  -9.214   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03027 on 673 degrees of freedom
## Multiple R-squared:  0.4838,	Adjusted R-squared:  0.4815 
## F-statistic: 210.3 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "ma"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * BarRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.079058 -0.026434 -0.007243  0.021665  0.191646 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             4.646e+00  2.501e-01   18.57   <2e-16 ***
## time_value             -2.398e-04  1.381e-05  -17.37   <2e-16 ***
## BarRestrict             9.457e+00  7.032e-01   13.45   <2e-16 ***
## time_value:BarRestrict -5.049e-04  3.819e-05  -13.22   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03703 on 673 degrees of freedom
## Multiple R-squared:  0.6217,	Adjusted R-squared:   0.62 
## F-statistic: 368.7 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "md"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * BarRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.095817 -0.026128 -0.007208  0.020799  0.225112 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             4.514e+00  2.527e-01   17.86   <2e-16 ***
## time_value             -2.322e-04  1.395e-05  -16.65   <2e-16 ***
## BarRestrict             7.653e+00  7.045e-01   10.86   <2e-16 ***
## time_value:BarRestrict -4.070e-04  3.827e-05  -10.64   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03729 on 673 degrees of freedom
## Multiple R-squared:  0.6054,	Adjusted R-squared:  0.6036 
## F-statistic: 344.1 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "me"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * BarRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.073189 -0.023283 -0.005715  0.017847  0.225571 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             5.480e+00  2.418e-01  22.658  < 2e-16 ***
## time_value             -2.846e-04  1.335e-05 -21.322  < 2e-16 ***
## BarRestrict             3.888e+00  6.858e-01   5.669 2.13e-08 ***
## time_value:BarRestrict -2.064e-04  3.724e-05  -5.543 4.28e-08 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03593 on 673 degrees of freedom
## Multiple R-squared:  0.508,	Adjusted R-squared:  0.5058 
## F-statistic: 231.7 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "mi"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * BarRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.114466 -0.027744 -0.007282  0.025274  0.148959 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             3.496e+00  1.972e-01   17.73   <2e-16 ***
## time_value             -1.756e-04  1.086e-05  -16.17   <2e-16 ***
## BarRestrict             1.457e+01  9.167e-01   15.89   <2e-16 ***
## time_value:BarRestrict -7.863e-04  4.976e-05  -15.80   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03994 on 673 degrees of freedom
## Multiple R-squared:  0.5089,	Adjusted R-squared:  0.5068 
## F-statistic: 232.5 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "mn"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * BarRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.073166 -0.027604 -0.007667  0.020993  0.167912 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             4.837e+00  2.604e-01  18.573   <2e-16 ***
## time_value             -2.504e-04  1.437e-05 -17.421   <2e-16 ***
## BarRestrict             6.720e+00  7.322e-01   9.178   <2e-16 ***
## time_value:BarRestrict -3.584e-04  3.977e-05  -9.012   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03856 on 673 degrees of freedom
## Multiple R-squared:  0.4787,	Adjusted R-squared:  0.4764 
## F-statistic:   206 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "mo"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * BarRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.065832 -0.025520 -0.007768  0.017189  0.169721 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             4.610e+00  2.408e-01  19.143  < 2e-16 ***
## time_value             -2.386e-04  1.329e-05 -17.954  < 2e-16 ***
## BarRestrict             5.397e+00  7.131e-01   7.568 1.25e-13 ***
## time_value:BarRestrict -2.882e-04  3.872e-05  -7.444 2.99e-13 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03638 on 673 degrees of freedom
## Multiple R-squared:  0.4423,	Adjusted R-squared:  0.4398 
## F-statistic: 177.9 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "ms"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * BarRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.061899 -0.022202 -0.005824  0.017238  0.161102 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             3.278e+00  2.117e-01  15.487  < 2e-16 ***
## time_value             -1.673e-04  1.168e-05 -14.322  < 2e-16 ***
## BarRestrict             4.011e+00  6.324e-01   6.343 4.13e-10 ***
## time_value:BarRestrict -2.146e-04  3.433e-05  -6.251 7.24e-10 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03208 on 673 degrees of freedom
## Multiple R-squared:  0.3431,	Adjusted R-squared:  0.3402 
## F-statistic: 117.2 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "mt"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * BarRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.067898 -0.026344 -0.005347  0.017755  0.121093 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             4.744e+00  2.423e-01  19.577  < 2e-16 ***
## time_value             -2.446e-04  1.337e-05 -18.290  < 2e-16 ***
## BarRestrict             1.989e+00  6.991e-01   2.845  0.00458 ** 
## time_value:BarRestrict -1.042e-04  3.796e-05  -2.745  0.00622 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03624 on 673 degrees of freedom
## Multiple R-squared:  0.4084,	Adjusted R-squared:  0.4057 
## F-statistic: 154.9 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "nc"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * BarRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.065145 -0.020460 -0.004463  0.016808  0.126407 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             3.889e+00  2.066e-01  18.824  < 2e-16 ***
## time_value             -1.991e-04  1.140e-05 -17.461  < 2e-16 ***
## BarRestrict             4.265e+00  5.809e-01   7.342 6.09e-13 ***
## time_value:BarRestrict -2.267e-04  3.155e-05  -7.187 1.76e-12 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03059 on 673 degrees of freedom
## Multiple R-squared:  0.4455,	Adjusted R-squared:  0.443 
## F-statistic: 180.2 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "nd"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * BarRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.109620 -0.025571 -0.004662  0.023084  0.161807 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             3.063e+00  1.591e-01  19.245  < 2e-16 ***
## time_value             -1.523e-04  8.734e-06 -17.434  < 2e-16 ***
## BarRestrict             1.532e+01  5.377e+00   2.849  0.00452 ** 
## time_value:BarRestrict -8.299e-04  2.926e-04  -2.836  0.00471 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.04323 on 673 degrees of freedom
## Multiple R-squared:  0.3646,	Adjusted R-squared:  0.3617 
## F-statistic: 128.7 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "ne"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * BarRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.089927 -0.027034 -0.005149  0.019949  0.161567 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             3.737e+00  1.811e-01   20.63   <2e-16 ***
## time_value             -1.900e-04  9.968e-06  -19.06   <2e-16 ***
## BarRestrict             1.257e+01  1.237e+00   10.17   <2e-16 ***
## time_value:BarRestrict -6.793e-04  6.709e-05  -10.13   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.04059 on 673 degrees of freedom
## Multiple R-squared:  0.4429,	Adjusted R-squared:  0.4404 
## F-statistic: 178.4 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "nh"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * BarRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.117117 -0.024278 -0.003638  0.021443  0.266197 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             2.688e+00  1.721e-01   15.62   <2e-16 ***
## time_value             -1.315e-04  9.468e-06  -13.88   <2e-16 ***
## BarRestrict             1.368e+01  1.315e+00   10.41   <2e-16 ***
## time_value:BarRestrict -7.388e-04  7.140e-05  -10.35   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.04096 on 673 degrees of freedom
## Multiple R-squared:  0.4122,	Adjusted R-squared:  0.4096 
## F-statistic: 157.3 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "nj"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * BarRestrict, data = ftime.policy.df)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.08900 -0.02680 -0.01030  0.02484  0.15150 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             4.921e+00  2.703e-01   18.20   <2e-16 ***
## time_value             -2.558e-04  1.492e-05  -17.15   <2e-16 ***
## BarRestrict             1.348e+01  7.536e-01   17.89   <2e-16 ***
## time_value:BarRestrict -7.214e-04  4.093e-05  -17.62   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03989 on 673 degrees of freedom
## Multiple R-squared:  0.6935,	Adjusted R-squared:  0.6921 
## F-statistic: 507.5 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "nm"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * BarRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.072158 -0.022042 -0.004917  0.018312  0.129049 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             4.962e+00  2.099e-01  23.641   <2e-16 ***
## time_value             -2.564e-04  1.158e-05 -22.136   <2e-16 ***
## BarRestrict             1.226e+00  6.003e-01   2.043   0.0415 *  
## time_value:BarRestrict -6.109e-05  3.260e-05  -1.874   0.0613 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03128 on 673 degrees of freedom
## Multiple R-squared:  0.4797,	Adjusted R-squared:  0.4774 
## F-statistic: 206.8 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "nv"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * BarRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.067508 -0.020781 -0.004949  0.015887  0.172162 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             5.025e+00  2.084e-01  24.111   <2e-16 ***
## time_value             -2.596e-04  1.150e-05 -22.571   <2e-16 ***
## BarRestrict             5.476e+00  6.064e-01   9.030   <2e-16 ***
## time_value:BarRestrict -2.905e-04  3.293e-05  -8.822   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03127 on 673 degrees of freedom
## Multiple R-squared:  0.5819,	Adjusted R-squared:  0.5801 
## F-statistic: 312.3 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "ny"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * BarRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.107803 -0.026297 -0.008376  0.023005  0.153619 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             5.044e+00  2.476e-01   20.37   <2e-16 ***
## time_value             -2.610e-04  1.366e-05  -19.10   <2e-16 ***
## BarRestrict             1.096e+01  6.901e-01   15.88   <2e-16 ***
## time_value:BarRestrict -5.861e-04  3.749e-05  -15.63   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03653 on 673 degrees of freedom
## Multiple R-squared:  0.6641,	Adjusted R-squared:  0.6626 
## F-statistic: 443.5 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "oh"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * BarRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.090854 -0.024991 -0.007051  0.021346  0.184549 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             5.273e+00  2.371e-01  22.245   <2e-16 ***
## time_value             -2.743e-04  1.308e-05 -20.966   <2e-16 ***
## BarRestrict             6.392e+00  6.552e-01   9.756   <2e-16 ***
## time_value:BarRestrict -3.413e-04  3.559e-05  -9.589   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03486 on 673 degrees of freedom
## Multiple R-squared:  0.5386,	Adjusted R-squared:  0.5365 
## F-statistic: 261.9 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "ok"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * BarRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.095004 -0.025269 -0.004117  0.017279  0.157717 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             2.346e+00  1.346e-01  17.437  < 2e-16 ***
## time_value             -1.147e-04  7.385e-06 -15.531  < 2e-16 ***
## BarRestrict             3.492e+01  4.754e+00   7.345 5.98e-13 ***
## time_value:BarRestrict -1.896e-03  2.586e-04  -7.331 6.55e-13 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03643 on 673 degrees of freedom
## Multiple R-squared:  0.3638,	Adjusted R-squared:  0.361 
## F-statistic: 128.3 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "or"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * BarRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.079254 -0.022240 -0.005864  0.018478  0.120983 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             5.005e+00  2.143e-01  23.353  < 2e-16 ***
## time_value             -2.586e-04  1.183e-05 -21.859  < 2e-16 ***
## BarRestrict             3.693e+00  6.025e-01   6.130 1.50e-09 ***
## time_value:BarRestrict -1.942e-04  3.272e-05  -5.933 4.76e-09 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03173 on 673 degrees of freedom
## Multiple R-squared:  0.5353,	Adjusted R-squared:  0.5332 
## F-statistic: 258.4 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "pa"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * BarRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.077789 -0.024703 -0.007806  0.021095  0.156303 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             5.404e+00  2.389e-01   22.62   <2e-16 ***
## time_value             -2.808e-04  1.318e-05  -21.30   <2e-16 ***
## BarRestrict             7.959e+00  6.716e-01   11.85   <2e-16 ***
## time_value:BarRestrict -4.249e-04  3.648e-05  -11.65   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03537 on 673 degrees of freedom
## Multiple R-squared:  0.5833,	Adjusted R-squared:  0.5814 
## F-statistic:   314 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "ri"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * BarRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.083890 -0.024966 -0.007786  0.020549  0.174227 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             5.577e+00  2.415e-01   23.09   <2e-16 ***
## time_value             -2.907e-04  1.333e-05  -21.81   <2e-16 ***
## BarRestrict             7.187e+00  6.791e-01   10.58   <2e-16 ***
## time_value:BarRestrict -3.826e-04  3.688e-05  -10.37   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03576 on 673 degrees of freedom
## Multiple R-squared:  0.5853,	Adjusted R-squared:  0.5835 
## F-statistic: 316.6 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "sc"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * BarRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.061029 -0.019118 -0.004197  0.015461  0.131638 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             3.935e+00  1.993e-01  19.749  < 2e-16 ***
## time_value             -2.024e-04  1.100e-05 -18.400  < 2e-16 ***
## BarRestrict             3.211e+00  5.651e-01   5.683 1.97e-08 ***
## time_value:BarRestrict -1.703e-04  3.069e-05  -5.548 4.15e-08 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.0296 on 673 degrees of freedom
## Multiple R-squared:   0.43,	Adjusted R-squared:  0.4275 
## F-statistic: 169.3 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "sd"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * BarRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.100199 -0.027587 -0.004691  0.021257  0.208758 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             2.963e+00  1.539e-01  19.253   <2e-16 ***
## time_value             -1.469e-04  8.441e-06 -17.400   <2e-16 ***
## BarRestrict             5.310e+01  2.458e+01   2.161   0.0311 *  
## time_value:BarRestrict -2.887e-03  1.338e-03  -2.158   0.0313 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.04256 on 673 degrees of freedom
## Multiple R-squared:  0.3357,	Adjusted R-squared:  0.3327 
## F-statistic: 113.4 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "tn"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * BarRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.075136 -0.023935 -0.003648  0.019423  0.154200 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             2.805e+00  1.614e-01   17.38   <2e-16 ***
## time_value             -1.394e-04  8.892e-06  -15.68   <2e-16 ***
## BarRestrict             8.186e+00  8.065e-01   10.15   <2e-16 ***
## time_value:BarRestrict -4.414e-04  4.376e-05  -10.09   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03317 on 673 degrees of freedom
## Multiple R-squared:  0.3924,	Adjusted R-squared:  0.3897 
## F-statistic: 144.9 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "tx"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * BarRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.071385 -0.021271 -0.004936  0.016119  0.174623 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             4.182e+00  2.123e-01   19.70   <2e-16 ***
## time_value             -2.155e-04  1.172e-05  -18.39   <2e-16 ***
## BarRestrict             6.359e+00  6.178e-01   10.29   <2e-16 ***
## time_value:BarRestrict -3.392e-04  3.355e-05  -10.11   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03186 on 673 degrees of freedom
## Multiple R-squared:  0.5228,	Adjusted R-squared:  0.5207 
## F-statistic: 245.8 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "ut"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * BarRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.064669 -0.025118 -0.009098  0.014791  0.139568 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             4.960e+00  2.358e-01  21.032  < 2e-16 ***
## time_value             -2.578e-04  1.301e-05 -19.808  < 2e-16 ***
## BarRestrict             4.727e+00  6.744e-01   7.009 5.85e-12 ***
## time_value:BarRestrict -2.506e-04  3.663e-05  -6.842 1.75e-11 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03515 on 673 degrees of freedom
## Multiple R-squared:  0.4868,	Adjusted R-squared:  0.4846 
## F-statistic: 212.8 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "va"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * BarRestrict, data = ftime.policy.df)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.07316 -0.02407 -0.00613  0.01715  0.20372 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             4.102e+00  2.293e-01  17.892   <2e-16 ***
## time_value             -2.102e-04  1.265e-05 -16.611   <2e-16 ***
## BarRestrict             6.587e+00  6.910e-01   9.533   <2e-16 ***
## time_value:BarRestrict -3.511e-04  3.751e-05  -9.359   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03486 on 673 degrees of freedom
## Multiple R-squared:  0.5051,	Adjusted R-squared:  0.5029 
## F-statistic: 228.9 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "vt"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * BarRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.095812 -0.025844 -0.003187  0.022885  0.194401 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             6.731e+00  2.547e-01  26.433  < 2e-16 ***
## time_value             -3.531e-04  1.406e-05 -25.123  < 2e-16 ***
## BarRestrict             2.351e+00  7.160e-01   3.284  0.00108 ** 
## time_value:BarRestrict -1.218e-04  3.889e-05  -3.132  0.00181 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.0377 on 673 degrees of freedom
## Multiple R-squared:  0.552,	Adjusted R-squared:   0.55 
## F-statistic: 276.4 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "wa"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * BarRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.074054 -0.024533 -0.005863  0.019432  0.182140 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             4.512e+00  2.424e-01   18.61  < 2e-16 ***
## time_value             -2.318e-04  1.338e-05  -17.33  < 2e-16 ***
## BarRestrict             5.223e+00  6.757e-01    7.73 3.92e-14 ***
## time_value:BarRestrict -2.763e-04  3.670e-05   -7.53 1.64e-13 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03577 on 673 degrees of freedom
## Multiple R-squared:  0.5304,	Adjusted R-squared:  0.5283 
## F-statistic: 253.4 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "wi"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * BarRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.109928 -0.027936 -0.001762  0.025551  0.183727 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             2.666e+00  1.534e-01  17.378   <2e-16 ***
## time_value             -1.296e-04  8.418e-06 -15.401   <2e-16 ***
## BarRestrict             4.488e+00  6.035e+00   0.744    0.457    
## time_value:BarRestrict -2.389e-04  3.286e-04  -0.727    0.467    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.04188 on 673 degrees of freedom
## Multiple R-squared:  0.3944,	Adjusted R-squared:  0.3917 
## F-statistic: 146.1 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "wv"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * BarRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.079456 -0.024201 -0.005322  0.019249  0.136427 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             6.337e+00  2.250e-01  28.167  < 2e-16 ***
## time_value             -3.326e-04  1.242e-05 -26.789  < 2e-16 ***
## BarRestrict             1.911e+00  6.380e-01   2.995  0.00285 ** 
## time_value:BarRestrict -9.869e-05  3.465e-05  -2.848  0.00453 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03342 on 673 degrees of freedom
## Multiple R-squared:  0.5905,	Adjusted R-squared:  0.5887 
## F-statistic: 323.5 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "wy"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * BarRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.067598 -0.027399 -0.007896  0.018540  0.163806 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             4.929e+00  2.460e-01  20.038  < 2e-16 ***
## time_value             -2.546e-04  1.358e-05 -18.750  < 2e-16 ***
## BarRestrict             3.580e+00  7.036e-01   5.088 4.71e-07 ***
## time_value:BarRestrict -1.906e-04  3.821e-05  -4.990 7.70e-07 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03667 on 673 degrees of freedom
## Multiple R-squared:  0.4634,	Adjusted R-squared:  0.461 
## F-statistic: 193.7 on 3 and 673 DF,  p-value: < 2.2e-16
```


#### Restaurant Restriction


```r
# The fraction of mobile devices that did not leave the immediate area of their home (SafeGraph’s completely_home_device_count / device_count)
for(state in states){
  print(state)
  print("---------------------")
  chome.state <- chome %>% filter(geo_value == state)
  policy.state <- policy %>% filter(StatePostal == state)
  policy_signal.state <- getSumOfPolicy(policy.state, STARTDATE, ENDDATE)
  # left join mobility with policy signal by time 
  ftime.policy.df <- left_join(chome.state , policy_signal.state, by = "time_value")
  lm.fit <- lm(value~time_value*RestaurantRestrict,data=ftime.policy.df)
  print(summary(lm.fit))
}
```

```
## [1] "ak"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * RestaurantRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.117085 -0.024855 -0.001525  0.025576  0.120719 
## 
## Coefficients:
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    3.008e+00  1.422e-01  21.152   <2e-16 ***
## time_value                    -1.481e-04  7.804e-06 -18.982   <2e-16 ***
## RestaurantRestrict             1.117e+01  4.585e+00   2.436   0.0151 *  
## time_value:RestaurantRestrict -6.049e-04  2.496e-04  -2.424   0.0156 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.0386 on 673 degrees of freedom
## Multiple R-squared:  0.3801,	Adjusted R-squared:  0.3774 
## F-statistic: 137.6 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "al"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * RestaurantRestrict, data = ftime.policy.df)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.07190 -0.02256 -0.00567  0.01790  0.15351 
## 
## Coefficients:
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    3.893e+00  2.169e-01  17.949  < 2e-16 ***
## time_value                    -2.011e-04  1.197e-05 -16.803  < 2e-16 ***
## RestaurantRestrict             3.339e+00  6.204e-01   5.382 1.02e-07 ***
## time_value:RestaurantRestrict -1.774e-04  3.369e-05  -5.265 1.89e-07 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03233 on 673 degrees of freedom
## Multiple R-squared:  0.3889,	Adjusted R-squared:  0.3861 
## F-statistic: 142.7 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "ar"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * RestaurantRestrict, data = ftime.policy.df)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.06290 -0.02266 -0.00673  0.01728  0.13065 
## 
## Coefficients:
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    4.002e+00  2.189e-01  18.282  < 2e-16 ***
## time_value                    -2.066e-04  1.208e-05 -17.101  < 2e-16 ***
## RestaurantRestrict             3.829e+00  6.260e-01   6.117 1.62e-09 ***
## time_value:RestaurantRestrict -2.041e-04  3.399e-05  -6.003 3.17e-09 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03262 on 673 degrees of freedom
## Multiple R-squared:  0.4096,	Adjusted R-squared:  0.407 
## F-statistic: 155.6 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "az"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * RestaurantRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.072551 -0.022397 -0.003798  0.017718  0.139349 
## 
## Coefficients:
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    4.022e+00  2.148e-01  18.719  < 2e-16 ***
## time_value                    -2.043e-04  1.185e-05 -17.232  < 2e-16 ***
## RestaurantRestrict             4.550e+00  6.833e-01   6.659 5.73e-11 ***
## time_value:RestaurantRestrict -2.417e-04  3.708e-05  -6.520 1.38e-10 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03332 on 673 degrees of freedom
## Multiple R-squared:  0.426,	Adjusted R-squared:  0.4235 
## F-statistic: 166.5 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "ca"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * RestaurantRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.072793 -0.020070 -0.005262  0.016497  0.119157 
## 
## Coefficients:
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    4.832e+00  2.057e-01   23.50   <2e-16 ***
## time_value                    -2.503e-04  1.135e-05  -22.05   <2e-16 ***
## RestaurantRestrict             6.660e+00  5.733e-01   11.62   <2e-16 ***
## time_value:RestaurantRestrict -3.525e-04  3.114e-05  -11.32   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03035 on 673 degrees of freedom
## Multiple R-squared:  0.7195,	Adjusted R-squared:  0.7182 
## F-statistic: 575.4 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "co"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * RestaurantRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.073780 -0.025394 -0.008116  0.021152  0.168897 
## 
## Coefficients:
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    4.790e+00  2.474e-01  19.360   <2e-16 ***
## time_value                    -2.470e-04  1.366e-05 -18.090   <2e-16 ***
## RestaurantRestrict             6.945e+00  6.956e-01   9.985   <2e-16 ***
## time_value:RestaurantRestrict -3.709e-04  3.778e-05  -9.819   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03663 on 673 degrees of freedom
## Multiple R-squared:  0.4962,	Adjusted R-squared:  0.494 
## F-statistic:   221 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "ct"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * RestaurantRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.082806 -0.027542 -0.008767  0.019732  0.218045 
## 
## Coefficients:
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    4.711e+00  2.636e-01   17.88   <2e-16 ***
## time_value                    -2.438e-04  1.455e-05  -16.76   <2e-16 ***
## RestaurantRestrict             9.846e+00  7.348e-01   13.40   <2e-16 ***
## time_value:RestaurantRestrict -5.267e-04  3.991e-05  -13.20   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03889 on 673 degrees of freedom
## Multiple R-squared:  0.5713,	Adjusted R-squared:  0.5694 
## F-statistic:   299 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "dc"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * RestaurantRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.081030 -0.021309 -0.004592  0.018198  0.169819 
## 
## Coefficients:
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    2.619e+00  2.246e-01   11.66   <2e-16 ***
## time_value                    -1.269e-04  1.239e-05  -10.24   <2e-16 ***
## RestaurantRestrict             9.128e+00  6.260e-01   14.58   <2e-16 ***
## time_value:RestaurantRestrict -4.878e-04  3.400e-05  -14.35   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03314 on 673 degrees of freedom
## Multiple R-squared:  0.682,	Adjusted R-squared:  0.6806 
## F-statistic: 481.1 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "de"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * RestaurantRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.080551 -0.025135 -0.006188  0.016862  0.167481 
## 
## Coefficients:
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    4.823e+00  2.394e-01  20.147   <2e-16 ***
## time_value                    -2.488e-04  1.321e-05 -18.830   <2e-16 ***
## RestaurantRestrict             6.536e+00  6.673e-01   9.795   <2e-16 ***
## time_value:RestaurantRestrict -3.481e-04  3.625e-05  -9.605   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03532 on 673 degrees of freedom
## Multiple R-squared:  0.5292,	Adjusted R-squared:  0.5271 
## F-statistic: 252.2 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "fl"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * RestaurantRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.099256 -0.022331 -0.003544  0.019611  0.124963 
## 
## Coefficients:
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    2.175e+00  1.671e-01   13.02   <2e-16 ***
## time_value                    -1.039e-04  9.204e-06  -11.29   <2e-16 ***
## RestaurantRestrict             9.079e+00  8.532e-01   10.64   <2e-16 ***
## time_value:RestaurantRestrict -4.886e-04  4.631e-05  -10.55   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03513 on 673 degrees of freedom
## Multiple R-squared:  0.4309,	Adjusted R-squared:  0.4284 
## F-statistic: 169.9 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "ga"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * RestaurantRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.071770 -0.023221 -0.007309  0.016708  0.147587 
## 
## Coefficients:
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    2.925e+00  2.302e-01  12.708   <2e-16 ***
## time_value                    -1.463e-04  1.270e-05 -11.525   <2e-16 ***
## RestaurantRestrict             6.429e+00  7.524e-01   8.545   <2e-16 ***
## time_value:RestaurantRestrict -3.448e-04  4.082e-05  -8.448   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03605 on 673 degrees of freedom
## Multiple R-squared:  0.3156,	Adjusted R-squared:  0.3125 
## F-statistic: 103.4 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "hi"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * RestaurantRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.098179 -0.020605 -0.002689  0.017212  0.150900 
## 
## Coefficients:
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    4.633e+00  2.018e-01  22.956  < 2e-16 ***
## time_value                    -2.409e-04  1.114e-05 -21.625  < 2e-16 ***
## RestaurantRestrict             1.976e+00  5.674e-01   3.482  0.00053 ***
## time_value:RestaurantRestrict -9.926e-05  3.082e-05  -3.221  0.00134 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.02988 on 673 degrees of freedom
## Multiple R-squared:  0.6537,	Adjusted R-squared:  0.6522 
## F-statistic: 423.6 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "ia"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * RestaurantRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.069766 -0.026906 -0.007326  0.019199  0.165327 
## 
## Coefficients:
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    5.552e+00  2.597e-01  21.376  < 2e-16 ***
## time_value                    -2.911e-04  1.433e-05 -20.306  < 2e-16 ***
## RestaurantRestrict             5.413e+00  7.302e-01   7.413 3.72e-13 ***
## time_value:RestaurantRestrict -2.884e-04  3.966e-05  -7.272 9.87e-13 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03845 on 673 degrees of freedom
## Multiple R-squared:  0.497,	Adjusted R-squared:  0.4947 
## F-statistic: 221.6 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "id"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * RestaurantRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.064263 -0.024514 -0.007189  0.017043  0.120460 
## 
## Coefficients:
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    4.956e+00  2.268e-01  21.858  < 2e-16 ***
## time_value                    -2.564e-04  1.251e-05 -20.493  < 2e-16 ***
## RestaurantRestrict             3.148e+00  6.834e-01   4.607 4.89e-06 ***
## time_value:RestaurantRestrict -1.666e-04  3.710e-05  -4.491 8.35e-06 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03448 on 673 degrees of freedom
## Multiple R-squared:  0.4674,	Adjusted R-squared:  0.465 
## F-statistic: 196.8 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "il"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * RestaurantRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.079078 -0.025880 -0.005612  0.020277  0.199295 
## 
## Coefficients:
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    5.001e+00  2.386e-01   20.96   <2e-16 ***
## time_value                    -2.599e-04  1.317e-05  -19.73   <2e-16 ***
## RestaurantRestrict             8.188e+00  6.652e-01   12.31   <2e-16 ***
## time_value:RestaurantRestrict -4.370e-04  3.613e-05  -12.10   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03521 on 673 degrees of freedom
## Multiple R-squared:  0.591,	Adjusted R-squared:  0.5892 
## F-statistic: 324.2 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "in"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * RestaurantRestrict, data = ftime.policy.df)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.06679 -0.02535 -0.00653  0.02123  0.15068 
## 
## Coefficients:
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    4.840e+00  2.359e-01  20.519   <2e-16 ***
## time_value                    -2.511e-04  1.302e-05 -19.285   <2e-16 ***
## RestaurantRestrict             6.580e+00  6.576e-01  10.006   <2e-16 ***
## time_value:RestaurantRestrict -3.521e-04  3.572e-05  -9.857   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03481 on 673 degrees of freedom
## Multiple R-squared:  0.5104,	Adjusted R-squared:  0.5082 
## F-statistic: 233.9 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "ks"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * RestaurantRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.100310 -0.026156 -0.002284  0.020869  0.177787 
## 
## Coefficients:
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    2.666e+00  1.437e-01  18.560  < 2e-16 ***
## time_value                    -1.310e-04  7.884e-06 -16.622  < 2e-16 ***
## RestaurantRestrict             2.726e+01  5.629e+00   4.843 1.58e-06 ***
## time_value:RestaurantRestrict -1.479e-03  3.063e-04  -4.828 1.71e-06 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03904 on 673 degrees of freedom
## Multiple R-squared:  0.4021,	Adjusted R-squared:  0.3994 
## F-statistic: 150.9 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "ky"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * RestaurantRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.060263 -0.024298 -0.004796  0.021060  0.158447 
## 
## Coefficients:
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    5.738e+00  2.209e-01  25.982  < 2e-16 ***
## time_value                    -3.008e-04  1.219e-05 -24.671  < 2e-16 ***
## RestaurantRestrict             2.696e+00  6.157e-01   4.380 1.38e-05 ***
## time_value:RestaurantRestrict -1.412e-04  3.344e-05  -4.222 2.76e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03259 on 673 degrees of freedom
## Multiple R-squared:  0.5514,	Adjusted R-squared:  0.5494 
## F-statistic: 275.8 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "la"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * RestaurantRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.068490 -0.021029 -0.003876  0.017537  0.143264 
## 
## Coefficients:
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    4.037e+00  2.044e-01  19.746   <2e-16 ***
## time_value                    -2.081e-04  1.128e-05 -18.441   <2e-16 ***
## RestaurantRestrict             5.381e+00  5.748e-01   9.362   <2e-16 ***
## time_value:RestaurantRestrict -2.876e-04  3.122e-05  -9.214   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03027 on 673 degrees of freedom
## Multiple R-squared:  0.4838,	Adjusted R-squared:  0.4815 
## F-statistic: 210.3 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "ma"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * RestaurantRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.079058 -0.026434 -0.007243  0.021665  0.191646 
## 
## Coefficients:
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    4.646e+00  2.501e-01   18.57   <2e-16 ***
## time_value                    -2.398e-04  1.381e-05  -17.37   <2e-16 ***
## RestaurantRestrict             9.457e+00  7.032e-01   13.45   <2e-16 ***
## time_value:RestaurantRestrict -5.049e-04  3.819e-05  -13.22   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03703 on 673 degrees of freedom
## Multiple R-squared:  0.6217,	Adjusted R-squared:   0.62 
## F-statistic: 368.7 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "md"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * RestaurantRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.095817 -0.026128 -0.007208  0.020799  0.225112 
## 
## Coefficients:
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    4.514e+00  2.527e-01   17.86   <2e-16 ***
## time_value                    -2.322e-04  1.395e-05  -16.65   <2e-16 ***
## RestaurantRestrict             7.653e+00  7.045e-01   10.86   <2e-16 ***
## time_value:RestaurantRestrict -4.070e-04  3.827e-05  -10.64   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03729 on 673 degrees of freedom
## Multiple R-squared:  0.6054,	Adjusted R-squared:  0.6036 
## F-statistic: 344.1 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "me"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * RestaurantRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.073189 -0.023283 -0.005715  0.017847  0.225571 
## 
## Coefficients:
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    5.480e+00  2.418e-01  22.658  < 2e-16 ***
## time_value                    -2.846e-04  1.335e-05 -21.322  < 2e-16 ***
## RestaurantRestrict             3.888e+00  6.858e-01   5.669 2.13e-08 ***
## time_value:RestaurantRestrict -2.064e-04  3.724e-05  -5.543 4.28e-08 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03593 on 673 degrees of freedom
## Multiple R-squared:  0.508,	Adjusted R-squared:  0.5058 
## F-statistic: 231.7 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "mi"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * RestaurantRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.114466 -0.027744 -0.007282  0.025274  0.148959 
## 
## Coefficients:
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    3.496e+00  1.972e-01   17.73   <2e-16 ***
## time_value                    -1.756e-04  1.086e-05  -16.17   <2e-16 ***
## RestaurantRestrict             1.457e+01  9.167e-01   15.89   <2e-16 ***
## time_value:RestaurantRestrict -7.863e-04  4.976e-05  -15.80   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03994 on 673 degrees of freedom
## Multiple R-squared:  0.5089,	Adjusted R-squared:  0.5068 
## F-statistic: 232.5 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "mn"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * RestaurantRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.073166 -0.027604 -0.007667  0.020993  0.167912 
## 
## Coefficients:
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    4.837e+00  2.604e-01  18.573   <2e-16 ***
## time_value                    -2.504e-04  1.437e-05 -17.421   <2e-16 ***
## RestaurantRestrict             6.720e+00  7.322e-01   9.178   <2e-16 ***
## time_value:RestaurantRestrict -3.584e-04  3.977e-05  -9.012   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03856 on 673 degrees of freedom
## Multiple R-squared:  0.4787,	Adjusted R-squared:  0.4764 
## F-statistic:   206 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "mo"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * RestaurantRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.065832 -0.025520 -0.007768  0.017189  0.169721 
## 
## Coefficients:
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    4.610e+00  2.408e-01  19.143  < 2e-16 ***
## time_value                    -2.386e-04  1.329e-05 -17.954  < 2e-16 ***
## RestaurantRestrict             5.397e+00  7.131e-01   7.568 1.25e-13 ***
## time_value:RestaurantRestrict -2.882e-04  3.872e-05  -7.444 2.99e-13 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03638 on 673 degrees of freedom
## Multiple R-squared:  0.4423,	Adjusted R-squared:  0.4398 
## F-statistic: 177.9 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "ms"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * RestaurantRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.061899 -0.022202 -0.005824  0.017238  0.161102 
## 
## Coefficients:
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    3.278e+00  2.117e-01  15.487  < 2e-16 ***
## time_value                    -1.673e-04  1.168e-05 -14.322  < 2e-16 ***
## RestaurantRestrict             4.011e+00  6.324e-01   6.343 4.13e-10 ***
## time_value:RestaurantRestrict -2.146e-04  3.433e-05  -6.251 7.24e-10 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03208 on 673 degrees of freedom
## Multiple R-squared:  0.3431,	Adjusted R-squared:  0.3402 
## F-statistic: 117.2 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "mt"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * RestaurantRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.067898 -0.026344 -0.005347  0.017755  0.121093 
## 
## Coefficients:
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    4.744e+00  2.423e-01  19.577  < 2e-16 ***
## time_value                    -2.446e-04  1.337e-05 -18.290  < 2e-16 ***
## RestaurantRestrict             1.989e+00  6.991e-01   2.845  0.00458 ** 
## time_value:RestaurantRestrict -1.042e-04  3.796e-05  -2.745  0.00622 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03624 on 673 degrees of freedom
## Multiple R-squared:  0.4084,	Adjusted R-squared:  0.4057 
## F-statistic: 154.9 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "nc"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * RestaurantRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.065145 -0.020460 -0.004463  0.016808  0.126407 
## 
## Coefficients:
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    3.889e+00  2.066e-01  18.824  < 2e-16 ***
## time_value                    -1.991e-04  1.140e-05 -17.461  < 2e-16 ***
## RestaurantRestrict             4.265e+00  5.809e-01   7.342 6.09e-13 ***
## time_value:RestaurantRestrict -2.267e-04  3.155e-05  -7.187 1.76e-12 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03059 on 673 degrees of freedom
## Multiple R-squared:  0.4455,	Adjusted R-squared:  0.443 
## F-statistic: 180.2 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "nd"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * RestaurantRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.109620 -0.025571 -0.004662  0.023084  0.161807 
## 
## Coefficients:
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    3.063e+00  1.591e-01  19.245  < 2e-16 ***
## time_value                    -1.523e-04  8.734e-06 -17.434  < 2e-16 ***
## RestaurantRestrict             1.532e+01  5.377e+00   2.849  0.00452 ** 
## time_value:RestaurantRestrict -8.299e-04  2.926e-04  -2.836  0.00471 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.04323 on 673 degrees of freedom
## Multiple R-squared:  0.3646,	Adjusted R-squared:  0.3617 
## F-statistic: 128.7 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "ne"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * RestaurantRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.089927 -0.027034 -0.005149  0.019949  0.161567 
## 
## Coefficients:
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    3.737e+00  1.811e-01   20.63   <2e-16 ***
## time_value                    -1.900e-04  9.968e-06  -19.06   <2e-16 ***
## RestaurantRestrict             1.257e+01  1.237e+00   10.17   <2e-16 ***
## time_value:RestaurantRestrict -6.793e-04  6.709e-05  -10.13   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.04059 on 673 degrees of freedom
## Multiple R-squared:  0.4429,	Adjusted R-squared:  0.4404 
## F-statistic: 178.4 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "nh"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * RestaurantRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.117117 -0.024278 -0.003638  0.021443  0.266197 
## 
## Coefficients:
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    2.688e+00  1.721e-01   15.62   <2e-16 ***
## time_value                    -1.315e-04  9.468e-06  -13.88   <2e-16 ***
## RestaurantRestrict             1.368e+01  1.315e+00   10.41   <2e-16 ***
## time_value:RestaurantRestrict -7.388e-04  7.140e-05  -10.35   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.04096 on 673 degrees of freedom
## Multiple R-squared:  0.4122,	Adjusted R-squared:  0.4096 
## F-statistic: 157.3 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "nj"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * RestaurantRestrict, data = ftime.policy.df)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.08900 -0.02680 -0.01030  0.02484  0.15150 
## 
## Coefficients:
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    4.921e+00  2.703e-01   18.20   <2e-16 ***
## time_value                    -2.558e-04  1.492e-05  -17.15   <2e-16 ***
## RestaurantRestrict             1.348e+01  7.536e-01   17.89   <2e-16 ***
## time_value:RestaurantRestrict -7.214e-04  4.093e-05  -17.62   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03989 on 673 degrees of freedom
## Multiple R-squared:  0.6935,	Adjusted R-squared:  0.6921 
## F-statistic: 507.5 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "nm"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * RestaurantRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.072158 -0.022042 -0.004917  0.018312  0.129049 
## 
## Coefficients:
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    4.962e+00  2.099e-01  23.641   <2e-16 ***
## time_value                    -2.564e-04  1.158e-05 -22.136   <2e-16 ***
## RestaurantRestrict             1.226e+00  6.003e-01   2.043   0.0415 *  
## time_value:RestaurantRestrict -6.109e-05  3.260e-05  -1.874   0.0613 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03128 on 673 degrees of freedom
## Multiple R-squared:  0.4797,	Adjusted R-squared:  0.4774 
## F-statistic: 206.8 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "nv"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * RestaurantRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.067508 -0.020781 -0.004949  0.015887  0.172162 
## 
## Coefficients:
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    5.025e+00  2.084e-01  24.111   <2e-16 ***
## time_value                    -2.596e-04  1.150e-05 -22.571   <2e-16 ***
## RestaurantRestrict             5.476e+00  6.064e-01   9.030   <2e-16 ***
## time_value:RestaurantRestrict -2.905e-04  3.293e-05  -8.822   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03127 on 673 degrees of freedom
## Multiple R-squared:  0.5819,	Adjusted R-squared:  0.5801 
## F-statistic: 312.3 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "ny"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * RestaurantRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.107803 -0.026297 -0.008376  0.023005  0.153619 
## 
## Coefficients:
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    5.044e+00  2.476e-01   20.37   <2e-16 ***
## time_value                    -2.610e-04  1.366e-05  -19.10   <2e-16 ***
## RestaurantRestrict             1.096e+01  6.901e-01   15.88   <2e-16 ***
## time_value:RestaurantRestrict -5.861e-04  3.749e-05  -15.63   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03653 on 673 degrees of freedom
## Multiple R-squared:  0.6641,	Adjusted R-squared:  0.6626 
## F-statistic: 443.5 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "oh"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * RestaurantRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.090854 -0.024991 -0.007051  0.021346  0.184549 
## 
## Coefficients:
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    5.273e+00  2.371e-01  22.245   <2e-16 ***
## time_value                    -2.743e-04  1.308e-05 -20.966   <2e-16 ***
## RestaurantRestrict             6.392e+00  6.552e-01   9.756   <2e-16 ***
## time_value:RestaurantRestrict -3.413e-04  3.559e-05  -9.589   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03486 on 673 degrees of freedom
## Multiple R-squared:  0.5386,	Adjusted R-squared:  0.5365 
## F-statistic: 261.9 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "ok"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * RestaurantRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.095004 -0.025269 -0.004117  0.017279  0.157717 
## 
## Coefficients:
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    2.346e+00  1.346e-01  17.437  < 2e-16 ***
## time_value                    -1.147e-04  7.385e-06 -15.531  < 2e-16 ***
## RestaurantRestrict             3.492e+01  4.754e+00   7.345 5.98e-13 ***
## time_value:RestaurantRestrict -1.896e-03  2.586e-04  -7.331 6.55e-13 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03643 on 673 degrees of freedom
## Multiple R-squared:  0.3638,	Adjusted R-squared:  0.361 
## F-statistic: 128.3 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "or"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * RestaurantRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.079254 -0.022240 -0.005864  0.018478  0.120983 
## 
## Coefficients:
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    5.005e+00  2.143e-01  23.353  < 2e-16 ***
## time_value                    -2.586e-04  1.183e-05 -21.859  < 2e-16 ***
## RestaurantRestrict             3.693e+00  6.025e-01   6.130 1.50e-09 ***
## time_value:RestaurantRestrict -1.942e-04  3.272e-05  -5.933 4.76e-09 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03173 on 673 degrees of freedom
## Multiple R-squared:  0.5353,	Adjusted R-squared:  0.5332 
## F-statistic: 258.4 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "pa"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * RestaurantRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.077789 -0.024703 -0.007806  0.021095  0.156303 
## 
## Coefficients:
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    5.404e+00  2.389e-01   22.62   <2e-16 ***
## time_value                    -2.808e-04  1.318e-05  -21.30   <2e-16 ***
## RestaurantRestrict             7.959e+00  6.716e-01   11.85   <2e-16 ***
## time_value:RestaurantRestrict -4.249e-04  3.648e-05  -11.65   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03537 on 673 degrees of freedom
## Multiple R-squared:  0.5833,	Adjusted R-squared:  0.5814 
## F-statistic:   314 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "ri"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * RestaurantRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.083890 -0.024966 -0.007786  0.020549  0.174227 
## 
## Coefficients:
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    5.577e+00  2.415e-01   23.09   <2e-16 ***
## time_value                    -2.907e-04  1.333e-05  -21.81   <2e-16 ***
## RestaurantRestrict             7.187e+00  6.791e-01   10.58   <2e-16 ***
## time_value:RestaurantRestrict -3.826e-04  3.688e-05  -10.37   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03576 on 673 degrees of freedom
## Multiple R-squared:  0.5853,	Adjusted R-squared:  0.5835 
## F-statistic: 316.6 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "sc"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * RestaurantRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.068183 -0.020108 -0.003325  0.017632  0.128826 
## 
## Coefficients:
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    2.696e+00  1.537e-01  17.540  < 2e-16 ***
## time_value                    -1.338e-04  8.468e-06 -15.802  < 2e-16 ***
## RestaurantRestrict             5.767e+00  7.266e-01   7.936 8.72e-15 ***
## time_value:RestaurantRestrict -3.103e-04  3.943e-05  -7.869 1.43e-14 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03121 on 673 degrees of freedom
## Multiple R-squared:  0.3664,	Adjusted R-squared:  0.3635 
## F-statistic: 129.7 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "sd"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * RestaurantRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.100199 -0.027587 -0.004691  0.021257  0.208758 
## 
## Coefficients:
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    2.963e+00  1.539e-01  19.253   <2e-16 ***
## time_value                    -1.469e-04  8.441e-06 -17.400   <2e-16 ***
## RestaurantRestrict             5.310e+01  2.458e+01   2.161   0.0311 *  
## time_value:RestaurantRestrict -2.887e-03  1.338e-03  -2.158   0.0313 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.04256 on 673 degrees of freedom
## Multiple R-squared:  0.3357,	Adjusted R-squared:  0.3327 
## F-statistic: 113.4 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "tn"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * RestaurantRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.083882 -0.022883 -0.000495  0.018308  0.137664 
## 
## Coefficients:
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    2.128e+00  1.227e-01  17.349  < 2e-16 ***
## time_value                    -1.019e-04  6.731e-06 -15.146  < 2e-16 ***
## RestaurantRestrict             2.172e+01  4.457e+00   4.873 1.37e-06 ***
## time_value:RestaurantRestrict -1.178e-03  2.426e-04  -4.856 1.49e-06 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03335 on 673 degrees of freedom
## Multiple R-squared:  0.3858,	Adjusted R-squared:  0.3831 
## F-statistic: 140.9 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "tx"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * RestaurantRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.071385 -0.021271 -0.004936  0.016119  0.174623 
## 
## Coefficients:
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    4.182e+00  2.123e-01   19.70   <2e-16 ***
## time_value                    -2.155e-04  1.172e-05  -18.39   <2e-16 ***
## RestaurantRestrict             6.359e+00  6.178e-01   10.29   <2e-16 ***
## time_value:RestaurantRestrict -3.392e-04  3.355e-05  -10.11   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03186 on 673 degrees of freedom
## Multiple R-squared:  0.5228,	Adjusted R-squared:  0.5207 
## F-statistic: 245.8 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "ut"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * RestaurantRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.064669 -0.025118 -0.009098  0.014791  0.139568 
## 
## Coefficients:
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    4.960e+00  2.358e-01  21.032  < 2e-16 ***
## time_value                    -2.578e-04  1.301e-05 -19.808  < 2e-16 ***
## RestaurantRestrict             4.727e+00  6.744e-01   7.009 5.85e-12 ***
## time_value:RestaurantRestrict -2.506e-04  3.663e-05  -6.842 1.75e-11 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03515 on 673 degrees of freedom
## Multiple R-squared:  0.4868,	Adjusted R-squared:  0.4846 
## F-statistic: 212.8 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "va"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * RestaurantRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.075308 -0.022470 -0.005361  0.017810  0.199756 
## 
## Coefficients:
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    4.627e+00  2.193e-01  21.098   <2e-16 ***
## time_value                    -2.392e-04  1.210e-05 -19.764   <2e-16 ***
## RestaurantRestrict             5.560e+00  6.166e-01   9.018   <2e-16 ***
## time_value:RestaurantRestrict -2.948e-04  3.349e-05  -8.804   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03247 on 673 degrees of freedom
## Multiple R-squared:  0.5707,	Adjusted R-squared:  0.5688 
## F-statistic: 298.2 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "vt"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * RestaurantRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.095812 -0.025844 -0.003187  0.022885  0.194401 
## 
## Coefficients:
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    6.731e+00  2.547e-01  26.433  < 2e-16 ***
## time_value                    -3.531e-04  1.406e-05 -25.123  < 2e-16 ***
## RestaurantRestrict             2.351e+00  7.160e-01   3.284  0.00108 ** 
## time_value:RestaurantRestrict -1.218e-04  3.889e-05  -3.132  0.00181 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.0377 on 673 degrees of freedom
## Multiple R-squared:  0.552,	Adjusted R-squared:   0.55 
## F-statistic: 276.4 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "wa"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * RestaurantRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.074054 -0.024533 -0.005863  0.019432  0.182140 
## 
## Coefficients:
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    4.512e+00  2.424e-01   18.61  < 2e-16 ***
## time_value                    -2.318e-04  1.338e-05  -17.33  < 2e-16 ***
## RestaurantRestrict             5.223e+00  6.757e-01    7.73 3.92e-14 ***
## time_value:RestaurantRestrict -2.763e-04  3.670e-05   -7.53 1.64e-13 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03577 on 673 degrees of freedom
## Multiple R-squared:  0.5304,	Adjusted R-squared:  0.5283 
## F-statistic: 253.4 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "wi"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * RestaurantRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.109928 -0.027936 -0.001762  0.025551  0.183727 
## 
## Coefficients:
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    2.666e+00  1.534e-01  17.378   <2e-16 ***
## time_value                    -1.296e-04  8.418e-06 -15.401   <2e-16 ***
## RestaurantRestrict             4.488e+00  6.035e+00   0.744    0.457    
## time_value:RestaurantRestrict -2.389e-04  3.286e-04  -0.727    0.467    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.04188 on 673 degrees of freedom
## Multiple R-squared:  0.3944,	Adjusted R-squared:  0.3917 
## F-statistic: 146.1 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "wv"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * RestaurantRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.079456 -0.024201 -0.005322  0.019249  0.136427 
## 
## Coefficients:
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    6.337e+00  2.250e-01  28.167  < 2e-16 ***
## time_value                    -3.326e-04  1.242e-05 -26.789  < 2e-16 ***
## RestaurantRestrict             1.911e+00  6.380e-01   2.995  0.00285 ** 
## time_value:RestaurantRestrict -9.869e-05  3.465e-05  -2.848  0.00453 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03342 on 673 degrees of freedom
## Multiple R-squared:  0.5905,	Adjusted R-squared:  0.5887 
## F-statistic: 323.5 on 3 and 673 DF,  p-value: < 2.2e-16
## 
## [1] "wy"
## [1] "---------------------"
## 
## Call:
## lm(formula = value ~ time_value * RestaurantRestrict, data = ftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.067598 -0.027399 -0.007896  0.018540  0.163806 
## 
## Coefficients:
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    4.929e+00  2.460e-01  20.038  < 2e-16 ***
## time_value                    -2.546e-04  1.358e-05 -18.750  < 2e-16 ***
## RestaurantRestrict             3.580e+00  7.036e-01   5.088 4.71e-07 ***
## time_value:RestaurantRestrict -1.906e-04  3.821e-05  -4.990 7.70e-07 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03667 on 673 degrees of freedom
## Multiple R-squared:  0.4634,	Adjusted R-squared:  0.461 
## F-statistic: 193.7 on 3 and 673 DF,  p-value: < 2.2e-16
```


## Non-parametric Regression

# Conclusion
