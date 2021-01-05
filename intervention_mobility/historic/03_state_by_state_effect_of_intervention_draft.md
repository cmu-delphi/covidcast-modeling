---
title: "Estimating the causal effect of interventions in a state-level"
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
library(mgcv)
#library(MASS)

source("code/painter.r")
source("code/loader.r")
source("code/parser.r")
```


```r
STARTDATE <- "2019-01-01"
ENDDATE <- lubridate::today()
GEO_TYPE = "county" # county-level
# Select two counties from blue state, two counties from red state
GEO_VALUE = c("06003","06113","06075","06071","49035", "17089") # select some counties, use fips_to_name() to find the name of the county
#EXCLUDED_AREAS = c("as","gu", "mp","vi", "pr") # excluded areas due to small sample size
```


```r
# Full time away home mobility
ftime <- covidcast_signal(data_source = "safegraph", 
                            signal ="full_time_work_prop",
                            start_day = STARTDATE, 
                            end_day = ENDDATE,
                            geo_type = GEO_TYPE, 
                            geo_values = GEO_VALUE)

# Create a new column
ftime$county <- fips_to_name(ftime$geo_value)

# Turn the geo value into state postal for future data processing with policy
ftime[which(ftime$geo_value %in% c("06003","06113","06075","06071")), "geo_value"] <- "ca"
ftime[which(ftime$geo_value %in% c("49035", "17089")), "geo_value"] <- "ut"


# The fraction of mobile devices that did not leave the immediate area of their home (SafeGraph’s completely_home_device_count / device_count)
chome <- covidcast_signal(data_source = "safegraph", 
                            signal ="completely_home_prop",
                            start_day = STARTDATE, 
                            end_day = ENDDATE,
                            geo_type = GEO_TYPE, 
                            geo_values = GEO_VALUE)

# The median time spent at home for all devices at this location for this time period, in minutes
mhome<- covidcast_signal(data_source = "safegraph", 
                            signal ="median_home_dwell_time",
                            start_day = STARTDATE, 
                            end_day = ENDDATE,
                            geo_type = GEO_TYPE, 
                            geo_values = GEO_VALUE)

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
# We filter down to only state wide mandate policy 
policy <- policy %>% 
  filter(StateWide ==  1 & Mandate == 1)
# Filter policy
ca.policy <- policy %>% filter(StatePostal == "ca")
ut.policy <- policy %>% filter(StatePostal == "ut")
```

## Regression Discontinuity Design

First, we look at the simplest regression discontinuty (RD) design by regressing mobility on time in different states.


```r
policy_with_sum_of_policy <- getSumOfPolicy(ca.policy, STARTDATE, ENDDATE)


for (p in unique(policy$StatePolicy)){
  print(p)
  out<- tryCatch({
    getFirstDayOfIntervention(policy_with_sum_of_policy, "ca", p)
    
  },error=function(cond) {
            message(paste("Policy does not seem to exist"))
            # Choose a return value in case of error
            return(NA)
      }
  )
  print(out)
  out<- tryCatch({
    getLastDayOfIntervention(policy_with_sum_of_policy, "ca", p)
    
  },error=function(cond) {
            message(paste("Policy does not seem to exist"))
            # Choose a return value in case of error
            return(NA)
      }
  )
  print(out)
}
```

```
## [1] "EmergDec"
## [1] "2020-03-05"
## # A tibble: 0 x 1
## # … with 1 variable: time_value <date>
## [1] "SchoolClose"
## [1] NA
## # A tibble: 0 x 1
## # … with 1 variable: time_value <date>
## [1] "BarRestrict"
## [1] "2020-03-20"
## # A tibble: 0 x 1
## # … with 1 variable: time_value <date>
## [1] "GathRestrict"
## [1] "2020-03-20"
## # A tibble: 0 x 1
## # … with 1 variable: time_value <date>
## [1] "OtherBusinessClose"
## [1] "2020-03-20"
## # A tibble: 0 x 1
## # … with 1 variable: time_value <date>
## [1] "RestaurantRestrict"
## [1] "2020-03-20"
## # A tibble: 0 x 1
## # … with 1 variable: time_value <date>
## [1] "CaseIsolation"
## [1] NA
## # A tibble: 0 x 1
## # … with 1 variable: time_value <date>
## [1] "StayAtHome"
## [1] "2020-03-20"
## # A tibble: 0 x 1
## # … with 1 variable: time_value <date>
## [1] "PublicMask"
## [1] "2020-06-19"
## # A tibble: 0 x 1
## # … with 1 variable: time_value <date>
## [1] "Quarantine"
## [1] NA
## # A tibble: 0 x 1
## # … with 1 variable: time_value <date>
## [1] "NEBusinessClose"
## [1] "2020-03-20"
## # A tibble: 0 x 1
## # … with 1 variable: time_value <date>
## [1] "TravelRestrictIntra"
## [1] NA
## # A tibble: 0 x 1
## # … with 1 variable: time_value <date>
## [1] "TravelRestrictEntry"
## [1] NA
## # A tibble: 0 x 1
## # … with 1 variable: time_value <date>
## [1] "BusinessMask"
## [1] NA
## # A tibble: 0 x 1
## # … with 1 variable: time_value <date>
## [1] "SchoolMask"
## [1] NA
## # A tibble: 0 x 1
## # … with 1 variable: time_value <date>
## [1] "TravelRestrictExit"
## [1] NA
## # A tibble: 0 x 1
## # … with 1 variable: time_value <date>
```

### EmergDec

#### CA - San Bernardino County (most populous in CA)

San Bernardino County is the largest county in the world and one of the nation's most populous.

Area: 20,105 mi-squared
Population: 2.18 million (2019)


```r
policyName <- "EmergDec"
countyName <- "San Bernardino County"

cafirstEDday <- getFirstDayOfIntervention(policy_with_sum_of_policy, 
                          "ca", 
                          policyName)



start <- cafirstEDday- 45
end <- cafirstEDday + 45
# get the time period we want to plot the RD
period <- seq(as.Date(start), 
                   as.Date(end), by="days")

ftime.sb <- ftime %>% filter(geo_value == "ca" & 
                               time_value %in% period &
                                 county == countyName )

plotRD(ftime.sb,
       ca.policy,
       policyName, 
       "ca",
       countyName,
       start,
       end)
```

![](03_state_by_state_effect_of_intervention_files/figure-html/ca-ed-sb-1.png)<!-- -->

```r
# How about 3 months before and after?
start <- cafirstEDday- 150
end <- cafirstEDday + 150
# get the time period we want to plot the RD
period <- seq(as.Date(start), 
                   as.Date(end), by="days")

ftime.sb <- ftime %>% filter(geo_value == "ca" & 
                               time_value %in% period &
                                 county == countyName )
plotRD(ftime.sb,
       ca.policy,
       policyName, 
       "ca",
       countyName,
       start,
       end)
```

![](03_state_by_state_effect_of_intervention_files/figure-html/ca-ed-sb-2.png)<!-- -->

```r
# Linear regression
preperiod <- seq(as.Date(start), 
                   as.Date(cafirstEDday-1), by="days")

postpreiod <- seq(as.Date(cafirstEDday), 
                   as.Date(end), by="days")
  
preftime <- ftime %>% filter(geo_value == "ca" & 
                               time_value %in% preperiod &
                                 county == countyName)

postftime <- ftime %>% filter(geo_value == "ca" & 
                               time_value %in% postpreiod  &
                                 county == countyName)


# left join mobility with policy signal by time 
preftime.policy.df <- left_join(preftime, policy_with_sum_of_policy, by = "time_value")

postftime.policy.df <- left_join(postftime, policy_with_sum_of_policy, by = "time_value")

# Filter weekend effects
preftime.policy.df<- preftime.policy.df %>% 
  mutate(weekday= weekdays(as.Date(time_value)))%>% 
  filter(!weekday %in% c("Saturday", "Sunday")) 
  
postftime.policy.df<- postftime.policy.df %>% 
  mutate(weekday= weekdays(as.Date(time_value)))%>% 
  filter(!weekday %in% c("Saturday", "Sunday")) 

# Ensure the post and pre time period have the same number of data points 
if(nrow(preftime.policy.df) - nrow(postftime.policy.df) < 0){
  # Drop the last rows 
  postftime.policy.df <- head(postftime.policy.df, nrow(preftime.policy.df))
}else if(nrow(preftime.policy.df) - nrow(postftime.policy.df) > 0){
  preftime.policy.df <- tail(preftime.policy.df, nrow(postftime.policy.df))
}

# Line 1: Fit a linear regression 
prelm.fit <- lm(value~time_value*EmergDec,data=preftime.policy.df)

summary(prelm.fit)
```

```
## 
## Call:
## lm(formula = value ~ time_value * EmergDec, data = preftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.035300 -0.005744  0.003619  0.008396  0.018902 
## 
## Coefficients: (2 not defined because of singularities)
##                       Estimate Std. Error t value Pr(>|t|)
## (Intercept)          5.533e-01  5.085e-01   1.088    0.279
## time_value          -2.747e-05  2.786e-05  -0.986    0.326
## EmergDec                    NA         NA      NA       NA
## time_value:EmergDec         NA         NA      NA       NA
## 
## Residual standard error: 0.01246 on 105 degrees of freedom
## Multiple R-squared:  0.009172,	Adjusted R-squared:  -0.0002644 
## F-statistic: 0.972 on 1 and 105 DF,  p-value: 0.3265
```

```r
# Create a new data set
newdata <- data.frame(time_value = cafirstEDday, EmergDec=c(1))

# predict on the newdata
pred.before <- predict(prelm.fit, newdata, se.fit=TRUE)

# 95% confidence interval
predict(prelm.fit, newdata , interval='confidence', level=0.95)
```

```
##          fit        lwr        upr
## 1 0.04992418 0.04513215 0.05471621
```

```r
# 95% prediction interval
predict(prelm.fit, newdata , interval='prediction', level=0.95)
```

```
##          fit        lwr        upr
## 1 0.04992418 0.02474921 0.07509916
```

```r
# Computer var(A)
vec_t <- matrix(1,2,1)
vec_t[2,1] <- as.numeric(newdata$time_value)
varA <- t(vec_t)%*%vcov(prelm.fit)[1:2,1:2]%*%vec_t

# Line 2: Fit a linear regression
postlm.fit <- lm(value~time_value*EmergDec,data=postftime.policy.df)

summary(postlm.fit)
```

```
## 
## Call:
## lm(formula = value ~ time_value * EmergDec, data = postftime.policy.df)
## 
## Residuals:
##        Min         1Q     Median         3Q        Max 
## -0.0178673 -0.0051335 -0.0007919  0.0039512  0.0266310 
## 
## Coefficients: (2 not defined because of singularities)
##                       Estimate Std. Error t value Pr(>|t|)  
## (Intercept)          5.606e-01  3.199e-01   1.752   0.0826 .
## time_value          -2.822e-05  1.738e-05  -1.623   0.1075  
## EmergDec                    NA         NA      NA       NA  
## time_value:EmergDec         NA         NA      NA       NA  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.007776 on 105 degrees of freedom
## Multiple R-squared:  0.02448,	Adjusted R-squared:  0.01519 
## F-statistic: 2.635 on 1 and 105 DF,  p-value: 0.1075
```

```r
# Computer var(B)
varB <- t(vec_t)%*%vcov(postlm.fit)[1:2,1:2]%*%vec_t

# 95% confidence interval
predict(postlm.fit, newdata , interval='confidence', level=0.95)
```

```
##          fit        lwr       upr
## 1 0.04336661 0.04039472 0.0463385
```

```r
# 95% prediction interval
predict(postlm.fit, newdata , interval='prediction', level=0.95)
```

```
##          fit        lwr        upr
## 1 0.04336661 0.02766527 0.05906795
```

```r
# Predict on the new data
pred.after <- predict(postlm.fit, newdata, se.fit=TRUE)


# Get the difference of the fit
pointestimate <- pred.before$fit - pred.after$fit

# Get the t-score
score <- qt(0.975, nrow(postftime.policy.df) - 2)
se <- sqrt(varA + varB)
# variance of the difference
lower <- pointestimate - score*se
upper <- pointestimate + score*se

list(fit=pointestimate, lower = lower, upper = upper)
```

```
## $fit
##           1 
## 0.006557573 
## 
## $lower
##              [,1]
## [1,] 0.0009188069
## 
## $upper
##            [,1]
## [1,] 0.01219634
```

#### CA - Alpine County (least populous in CA)


```r
policyName <- "EmergDec"
countyName <- "Alpine County"

cafirstEDday <- getFirstDayOfIntervention(policy_with_sum_of_policy, 
                          "ca", 
                          policyName)



start <- cafirstEDday- 45
end <- cafirstEDday + 45
# get the time period we want to plot the RD
period <- seq(as.Date(start), 
                   as.Date(end), by="days")

ftime.sb <- ftime %>% filter(geo_value == "ca" & 
                               time_value %in% period &
                                 county == countyName )

plotRD(ftime.sb,
       ca.policy,
       policyName, 
       "ca",
       countyName,
       start,
       end)
```

![](03_state_by_state_effect_of_intervention_files/figure-html/ca-ed-apline-1.png)<!-- -->

```r
# How about 3 months before and after?
start <- cafirstEDday- 150
end <- cafirstEDday + 150
# get the time period we want to plot the RD
period <- seq(as.Date(start), 
                   as.Date(end), by="days")

ftime.sb <- ftime %>% filter(geo_value == "ca" & 
                               time_value %in% period &
                                 county == countyName )
plotRD(ftime.sb,
       ca.policy,
       policyName, 
       "ca",
       countyName,
       start,
       end)
```

![](03_state_by_state_effect_of_intervention_files/figure-html/ca-ed-apline-2.png)<!-- -->

```r
# Linear regression
preperiod <- seq(as.Date(start), 
                   as.Date(cafirstEDday-1), by="days")

postpreiod <- seq(as.Date(cafirstEDday), 
                   as.Date(end), by="days")
  
preftime <- ftime %>% filter(geo_value == "ca" & 
                               time_value %in% preperiod &
                                 county == countyName)

postftime <- ftime %>% filter(geo_value == "ca" & 
                               time_value %in% postpreiod  &
                                 county == countyName)


# left join mobility with policy signal by time 
preftime.policy.df <- left_join(preftime, policy_with_sum_of_policy, by = "time_value")

postftime.policy.df <- left_join(postftime, policy_with_sum_of_policy, by = "time_value")

# Filter weekend effects
preftime.policy.df<- preftime.policy.df %>% 
  mutate(weekday= weekdays(as.Date(time_value)))%>% 
  filter(!weekday %in% c("Saturday", "Sunday")) 
  
postftime.policy.df<- postftime.policy.df %>% 
  mutate(weekday= weekdays(as.Date(time_value)))%>% 
  filter(!weekday %in% c("Saturday", "Sunday")) 

# Ensure the post and pre time period have the same number of data points 
if(nrow(preftime.policy.df) - nrow(postftime.policy.df) < 0){
  # Drop the last rows 
  postftime.policy.df <- head(postftime.policy.df, nrow(preftime.policy.df))
}else if(nrow(preftime.policy.df) - nrow(postftime.policy.df) > 0){
  preftime.policy.df <- tail(preftime.policy.df, nrow(postftime.policy.df))
}

# Line 1: Fit a linear regression 
prelm.fit <- lm(value~time_value*EmergDec,data=preftime.policy.df)

summary(prelm.fit)
```

```
## 
## Call:
## lm(formula = value ~ time_value * EmergDec, data = preftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.036060 -0.023936 -0.008736  0.013521  0.156694 
## 
## Coefficients: (2 not defined because of singularities)
##                       Estimate Std. Error t value Pr(>|t|)  
## (Intercept)          3.147e+00  1.306e+00   2.411   0.0177 *
## time_value          -1.691e-04  7.154e-05  -2.364   0.0199 *
## EmergDec                    NA         NA      NA       NA  
## time_value:EmergDec         NA         NA      NA       NA  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03201 on 105 degrees of freedom
## Multiple R-squared:  0.05053,	Adjusted R-squared:  0.04149 
## F-statistic: 5.588 on 1 and 105 DF,  p-value: 0.01992
```

```r
# Create a new data set
newdata <- data.frame(time_value = cafirstEDday, EmergDec=c(1))

# predict on the newdata
pred.before <- predict(prelm.fit, newdata, se.fit=TRUE)

# 95% confidence interval
predict(prelm.fit, newdata , interval='confidence', level=0.95)
```

```
##          fit        lwr        upr
## 1 0.04828254 0.03597757 0.06058751
```

```r
# 95% prediction interval
predict(prelm.fit, newdata , interval='prediction', level=0.95)
```

```
##          fit         lwr       upr
## 1 0.04828254 -0.01636175 0.1129268
```

```r
# Computer var(A)
vec_t <- matrix(1,2,1)
vec_t[2,1] <- as.numeric(newdata$time_value)
varA <- t(vec_t)%*%vcov(prelm.fit)[1:2,1:2]%*%vec_t

# Line 2: Fit a linear regression
postlm.fit <- lm(value~time_value*EmergDec,data=postftime.policy.df)

summary(postlm.fit)
```

```
## 
## Call:
## lm(formula = value ~ time_value * EmergDec, data = postftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.022604 -0.012977 -0.008008  0.010257  0.064480 
## 
## Coefficients: (2 not defined because of singularities)
##                       Estimate Std. Error t value Pr(>|t|)
## (Intercept)         -6.095e-01  7.695e-01  -0.792    0.430
## time_value           3.598e-05  4.182e-05   0.860    0.392
## EmergDec                    NA         NA      NA       NA
## time_value:EmergDec         NA         NA      NA       NA
## 
## Residual standard error: 0.0187 on 105 degrees of freedom
## Multiple R-squared:  0.007002,	Adjusted R-squared:  -0.002455 
## F-statistic: 0.7404 on 1 and 105 DF,  p-value: 0.3915
```

```r
# Computer var(B)
varB <- t(vec_t)%*%vcov(postlm.fit)[1:2,1:2]%*%vec_t

# 95% confidence interval
predict(postlm.fit, newdata , interval='confidence', level=0.95)
```

```
##          fit        lwr        upr
## 1 0.04994051 0.04279144 0.05708958
```

```r
# 95% prediction interval
predict(postlm.fit, newdata , interval='prediction', level=0.95)
```

```
##          fit        lwr       upr
## 1 0.04994051 0.01216992 0.0877111
```

```r
# Predict on the new data
pred.after <- predict(postlm.fit, newdata, se.fit=TRUE)


# Get the difference of the fit
pointestimate <- pred.before$fit - pred.after$fit

# Get the t-score
score <- qt(0.975, nrow(postftime.policy.df) - 2)
se <- sqrt(varA + varB)
# variance of the difference
lower <- pointestimate - score*se
upper <- pointestimate + score*se

list(fit=pointestimate, lower = lower, upper = upper)
```

```
## $fit
##            1 
## -0.001657971 
## 
## $lower
##             [,1]
## [1,] -0.01588898
## 
## $upper
##            [,1]
## [1,] 0.01257303
```

#### CA - San Francisco County (smallest in CA in terms of area)


```r
policyName <- "EmergDec"
countyName <- "San Francisco County"

cafirstEDday <- getFirstDayOfIntervention(policy_with_sum_of_policy, 
                          "ca", 
                          policyName)


start <- cafirstEDday- 45
end <- cafirstEDday + 45
# get the time period we want to plot the RD
period <- seq(as.Date(start), 
                   as.Date(end), by="days")

ftime.sf <- ftime %>% filter(geo_value == "ca" & 
                               time_value %in% period &
                                 county == countyName)

plotRD(ftime.sf,
       ca.policy,
       policyName, 
       "ca",
       countyName,
       start,
       end)
```

![](03_state_by_state_effect_of_intervention_files/figure-html/ca-ed-sf-1.png)<!-- -->

```r
# How about 3 months before and after?
start <- cafirstEDday- 150
end <- cafirstEDday + 150
# get the time period we want to plot the RD
period <- seq(as.Date(start), 
                   as.Date(end), by="days")

ftime.sf  <- ftime %>% filter(geo_value == "ca" & 
                               time_value %in% period &
                                 county == countyName)
plotRD(ftime.sf,
       ca.policy,
       policyName, 
       "ca",
       countyName,
       start,
       end)
```

![](03_state_by_state_effect_of_intervention_files/figure-html/ca-ed-sf-2.png)<!-- -->

```r
# Linear regression
preperiod <- seq(as.Date(start), 
                   as.Date(cafirstEDday-1), by="days")

postpreiod <- seq(as.Date(cafirstEDday), 
                   as.Date(end), by="days")
  
preftime <- ftime %>% filter(geo_value == "ca" & 
                               time_value %in% preperiod &
                                 county == countyName)

postftime <- ftime %>% filter(geo_value == "ca" & 
                               time_value %in% postpreiod  &
                                 county == countyName)


# left join mobility with policy signal by time 
preftime.policy.df <- left_join(preftime, policy_with_sum_of_policy, by = "time_value")

postftime.policy.df <- left_join(postftime, policy_with_sum_of_policy, by = "time_value")

# Filter weekend effects
preftime.policy.df<- preftime.policy.df %>% 
  mutate(weekday= weekdays(as.Date(time_value)))%>% 
  filter(!weekday %in% c("Saturday", "Sunday")) 
  
postftime.policy.df<- postftime.policy.df %>% 
  mutate(weekday= weekdays(as.Date(time_value)))%>% 
  filter(!weekday %in% c("Saturday", "Sunday")) 

# Ensure the post and pre time period have the same number of data points 
if(nrow(preftime.policy.df) - nrow(postftime.policy.df) < 0){
  # Drop the last rows 
  postftime.policy.df <- head(postftime.policy.df, nrow(preftime.policy.df))
}else if(nrow(preftime.policy.df) - nrow(postftime.policy.df) > 0){
  preftime.policy.df <- tail(preftime.policy.df, nrow(postftime.policy.df))
}

# Line 1: Fit a linear regression 
prelm.fit <- lm(value~time_value*EmergDec,data=preftime.policy.df)

summary(prelm.fit)
```

```
## 
## Call:
## lm(formula = value ~ time_value * EmergDec, data = preftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.044331 -0.008972  0.004071  0.011791  0.030771 
## 
## Coefficients: (2 not defined because of singularities)
##                       Estimate Std. Error t value Pr(>|t|)
## (Intercept)         -7.904e-01  7.101e-01  -1.113    0.268
## time_value           4.727e-05  3.891e-05   1.215    0.227
## EmergDec                    NA         NA      NA       NA
## time_value:EmergDec         NA         NA      NA       NA
## 
## Residual standard error: 0.01741 on 105 degrees of freedom
## Multiple R-squared:  0.01386,	Adjusted R-squared:  0.004469 
## F-statistic: 1.476 on 1 and 105 DF,  p-value: 0.2271
```

```r
# Create a new data set
newdata <- data.frame(time_value = cafirstEDday, EmergDec=c(1))

# predict on the newdata
pred.before <- predict(prelm.fit, newdata, se.fit=TRUE)

# 95% confidence interval
predict(prelm.fit, newdata , interval='confidence', level=0.95)
```

```
##          fit       lwr        upr
## 1 0.07586556 0.0691732 0.08255792
```

```r
# We are 95% confident that the average FT mobility signal is between
# [0.04515871, 0.05483044]

# 95% prediction interval
predict(prelm.fit, newdata , interval='prediction', level=0.95)
```

```
##          fit        lwr       upr
## 1 0.07586556 0.04070719 0.1110239
```

```r
# We are 95% confident that the full-time mobility signal
# will be in [0.0247027, 0.07528644]

# Computer var(A)
vec_t <- matrix(1,2,1)
vec_t[2,1] <- as.numeric(newdata$time_value)
varA <- t(vec_t)%*%vcov(prelm.fit)[1:2,1:2]%*%vec_t

# Line 2: Fit a linear regression
postlm.fit <- lm(value~time_value*EmergDec,data=postftime.policy.df)

summary(postlm.fit)
```

```
## 
## Call:
## lm(formula = value ~ time_value * EmergDec, data = postftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.012945 -0.006460 -0.002148  0.004373  0.036643 
## 
## Coefficients: (2 not defined because of singularities)
##                       Estimate Std. Error t value Pr(>|t|)
## (Intercept)          5.017e-01  3.681e-01   1.363    0.176
## time_value          -2.434e-05  2.001e-05  -1.217    0.226
## EmergDec                    NA         NA      NA       NA
## time_value:EmergDec         NA         NA      NA       NA
## 
## Residual standard error: 0.008948 on 105 degrees of freedom
## Multiple R-squared:  0.0139,	Adjusted R-squared:  0.004508 
## F-statistic:  1.48 on 1 and 105 DF,  p-value: 0.2265
```

```r
# Computer var(B)
varB <- t(vec_t)%*%vcov(postlm.fit)[1:2,1:2]%*%vec_t

# 95% confidence interval
predict(postlm.fit, newdata , interval='confidence', level=0.95)
```

```
##          fit        lwr      upr
## 1 0.05567708 0.05225716 0.059097
```

```r
# We are 95% confident that the average FT mobility signal is between
# [0.04003865, 0.04596795]

# 95% prediction interval
predict(postlm.fit, newdata , interval='prediction', level=0.95)
```

```
##          fit        lwr        upr
## 1 0.05567708 0.03760865 0.07374552
```

```r
# We are 95% confident that the full-time mobility signal
# will be in [0.02719726, 0.05880934]

# Predict on the new data
pred.after <- predict(postlm.fit, newdata, se.fit=TRUE)


# Get the difference of the fit
pointestimate <- pred.before$fit - pred.after$fit

# Get the t-score
score <- qt(0.975, nrow(postftime.policy.df) - 2)
se <- sqrt(varA + varB)
# variance of the difference
lower <- pointestimate - score*se
upper <- pointestimate + score*se

list(fit=pointestimate, lower = lower, upper = upper)
```

```
## $fit
##          1 
## 0.02018847 
## 
## $lower
##            [,1]
## [1,] 0.01267292
## 
## $upper
##            [,1]
## [1,] 0.02770403
```


#### CA - Yolo County County (School Area)


```r
policyName <- "EmergDec"
countyName <- "Yolo County"

cafirstEDday <- getFirstDayOfIntervention(policy_with_sum_of_policy, 
                          "ca", 
                          policyName)


start <- cafirstEDday- 45
end <- cafirstEDday + 45
# get the time period we want to plot the RD
period <- seq(as.Date(start), 
                   as.Date(end), by="days")

ftime.sf <- ftime %>% filter(geo_value == "ca" & 
                               time_value %in% period &
                                 county == countyName)

plotRD(ftime.sf,
       ca.policy,
       policyName, 
       "ca",
       countyName,
       start,
       end)
```

![](03_state_by_state_effect_of_intervention_files/figure-html/ca-ed-yolo-1.png)<!-- -->

```r
# How about 3 months before and after?
start <- cafirstEDday- 150
end <- cafirstEDday + 150
# get the time period we want to plot the RD
period <- seq(as.Date(start), 
                   as.Date(end), by="days")

ftime.sf  <- ftime %>% filter(geo_value == "ca" & 
                               time_value %in% period &
                                 county == countyName)
plotRD(ftime.sf,
       ca.policy,
       policyName, 
       "ca",
       countyName,
       start,
       end)
```

![](03_state_by_state_effect_of_intervention_files/figure-html/ca-ed-yolo-2.png)<!-- -->

```r
# Linear regression
preperiod <- seq(as.Date(start), 
                   as.Date(cafirstEDday-1), by="days")

postpreiod <- seq(as.Date(cafirstEDday), 
                   as.Date(end), by="days")
  
preftime <- ftime %>% filter(geo_value == "ca" & 
                               time_value %in% preperiod &
                                 county == countyName)

postftime <- ftime %>% filter(geo_value == "ca" & 
                               time_value %in% postpreiod  &
                                 county == countyName)


# left join mobility with policy signal by time 
preftime.policy.df <- left_join(preftime, policy_with_sum_of_policy, by = "time_value")

postftime.policy.df <- left_join(postftime, policy_with_sum_of_policy, by = "time_value")

# Filter weekend effects
preftime.policy.df<- preftime.policy.df %>% 
  mutate(weekday= weekdays(as.Date(time_value)))%>% 
  filter(!weekday %in% c("Saturday", "Sunday")) 
  
postftime.policy.df<- postftime.policy.df %>% 
  mutate(weekday= weekdays(as.Date(time_value)))%>% 
  filter(!weekday %in% c("Saturday", "Sunday")) 

# Ensure the post and pre time period have the same number of data points 
if(nrow(preftime.policy.df) - nrow(postftime.policy.df) < 0){
  # Drop the last rows 
  postftime.policy.df <- head(postftime.policy.df, nrow(preftime.policy.df))
}else if(nrow(preftime.policy.df) - nrow(postftime.policy.df) > 0){
  preftime.policy.df <- tail(preftime.policy.df, nrow(postftime.policy.df))
}

# Line 1: Fit a linear regression 
prelm.fit <- lm(value~time_value*EmergDec,data=preftime.policy.df)

summary(prelm.fit)
```

```
## 
## Call:
## lm(formula = value ~ time_value * EmergDec, data = preftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.038183 -0.003445  0.002544  0.008745  0.024176 
## 
## Coefficients: (2 not defined because of singularities)
##                       Estimate Std. Error t value Pr(>|t|)
## (Intercept)          3.896e-01  5.500e-01   0.708    0.480
## time_value          -1.824e-05  3.014e-05  -0.605    0.546
## EmergDec                    NA         NA      NA       NA
## time_value:EmergDec         NA         NA      NA       NA
## 
## Residual standard error: 0.01348 on 105 degrees of freedom
## Multiple R-squared:  0.003477,	Adjusted R-squared:  -0.006013 
## F-statistic: 0.3664 on 1 and 105 DF,  p-value: 0.5463
```

```r
# Create a new data set
newdata <- data.frame(time_value = cafirstEDday, EmergDec=c(1))

# predict on the newdata
pred.before <- predict(prelm.fit, newdata, se.fit=TRUE)

# 95% confidence interval
predict(prelm.fit, newdata , interval='confidence', level=0.95)
```

```
##         fit        lwr        upr
## 1 0.0553053 0.05012139 0.06048921
```

```r
# We are 95% confident that the average FT mobility signal is between
# [0.04515871, 0.05483044]

# 95% prediction interval
predict(prelm.fit, newdata , interval='prediction', level=0.95)
```

```
##         fit        lwr        upr
## 1 0.0553053 0.02807158 0.08253902
```

```r
# We are 95% confident that the full-time mobility signal
# will be in [0.0247027, 0.07528644]

# Computer var(A)
vec_t <- matrix(1,2,1)
vec_t[2,1] <- as.numeric(newdata$time_value)
varA <- t(vec_t)%*%vcov(prelm.fit)[1:2,1:2]%*%vec_t

# Line 2: Fit a linear regression
postlm.fit <- lm(value~time_value*EmergDec,data=postftime.policy.df)

summary(postlm.fit)
```

```
## 
## Call:
## lm(formula = value ~ time_value * EmergDec, data = postftime.policy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.013045 -0.005471 -0.001414  0.005139  0.028953 
## 
## Coefficients: (2 not defined because of singularities)
##                       Estimate Std. Error t value Pr(>|t|)
## (Intercept)          3.214e-01  3.343e-01   0.961    0.339
## time_value          -1.524e-05  1.817e-05  -0.839    0.403
## EmergDec                    NA         NA      NA       NA
## time_value:EmergDec         NA         NA      NA       NA
## 
## Residual standard error: 0.008125 on 105 degrees of freedom
## Multiple R-squared:  0.006657,	Adjusted R-squared:  -0.002803 
## F-statistic: 0.7037 on 1 and 105 DF,  p-value: 0.4034
```

```r
# Computer var(B)
varB <- t(vec_t)%*%vcov(postlm.fit)[1:2,1:2]%*%vec_t

# 95% confidence interval
predict(postlm.fit, newdata , interval='confidence', level=0.95)
```

```
##          fit        lwr        upr
## 1 0.04209119 0.03898584 0.04519654
```

```r
# We are 95% confident that the average FT mobility signal is between
# [0.04003865, 0.04596795]

# 95% prediction interval
predict(postlm.fit, newdata , interval='prediction', level=0.95)
```

```
##          fit        lwr        upr
## 1 0.04209119 0.02568472 0.05849765
```

```r
# We are 95% confident that the full-time mobility signal
# will be in [0.02719726, 0.05880934]

# Predict on the new data
pred.after <- predict(postlm.fit, newdata, se.fit=TRUE)


# Get the difference of the fit
pointestimate <- pred.before$fit - pred.after$fit

# Get the t-score
score <- qt(0.975, nrow(postftime.policy.df) - 2)
se <- sqrt(varA + varB)
# variance of the difference
lower <- pointestimate - score*se
upper <- pointestimate + score*se

list(fit=pointestimate, lower = lower, upper = upper)
```

```
## $fit
##          1 
## 0.01321411 
## 
## $lower
##             [,1]
## [1,] 0.007171257
## 
## $upper
##            [,1]
## [1,] 0.01925697
```




```r
policy_with_sum_of_policy <- getSumOfPolicy(ut.policy, STARTDATE, ENDDATE)


for (p in unique(policy$StatePolicy)){
  print(p)
  out<- tryCatch({
    getFirstDayOfIntervention(policy_with_sum_of_policy, "ut", p)
    
  },error=function(cond) {
            message(paste("Policy does not seem to exist"))
            # Choose a return value in case of error
            return(NA)
      }
  )
  print(out)
  out<- tryCatch({
    getLastDayOfIntervention(policy_with_sum_of_policy, "ut", p)
    
  },error=function(cond) {
            message(paste("Policy does not seem to exist"))
            # Choose a return value in case of error
            return(NA)
      }
  )
  print(out)
}
```

```
## [1] "EmergDec"
## [1] "2020-03-07"
## # A tibble: 0 x 1
## # … with 1 variable: time_value <date>
## [1] "SchoolClose"
## [1] "2020-03-17"
## # A tibble: 0 x 1
## # … with 1 variable: time_value <date>
## [1] "BarRestrict"
## [1] "2020-03-20"
## # A tibble: 0 x 1
## # … with 1 variable: time_value <date>
## [1] "GathRestrict"
## [1] "2020-03-18"
## # A tibble: 0 x 1
## # … with 1 variable: time_value <date>
## [1] "OtherBusinessClose"
## [1] "2020-03-20"
## # A tibble: 0 x 1
## # … with 1 variable: time_value <date>
## [1] "RestaurantRestrict"
## [1] "2020-03-20"
## # A tibble: 0 x 1
## # … with 1 variable: time_value <date>
## [1] "CaseIsolation"
## [1] "2020-04-01"
## # A tibble: 0 x 1
## # … with 1 variable: time_value <date>
## [1] "StayAtHome"
## [1] NA
## # A tibble: 0 x 1
## # … with 1 variable: time_value <date>
## [1] "PublicMask"
## [1] "2020-06-30"
## # A tibble: 0 x 1
## # … with 1 variable: time_value <date>
## [1] "Quarantine"
## [1] NA
## # A tibble: 0 x 1
## # … with 1 variable: time_value <date>
## [1] "NEBusinessClose"
## [1] NA
## # A tibble: 0 x 1
## # … with 1 variable: time_value <date>
## [1] "TravelRestrictIntra"
## [1] NA
## # A tibble: 0 x 1
## # … with 1 variable: time_value <date>
## [1] "TravelRestrictEntry"
## [1] NA
## # A tibble: 0 x 1
## # … with 1 variable: time_value <date>
## [1] "BusinessMask"
## [1] "2020-05-02"
## # A tibble: 0 x 1
## # … with 1 variable: time_value <date>
## [1] "SchoolMask"
## [1] "2020-07-18"
## # A tibble: 0 x 1
## # … with 1 variable: time_value <date>
## [1] "TravelRestrictExit"
## [1] NA
## # A tibble: 0 x 1
## # … with 1 variable: time_value <date>
```

