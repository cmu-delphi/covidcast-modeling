---
title: "Estimating Causal Effect of Government Intervention to Mobility via Regression Discontinuity"
author: "Kenneth Lee"
date: "23/09/2020"
output: 
  html_document:
    code_folding: hide
    keep_md: yes
    toc: yes
    toc_float:
      collapsed: yes
      smooth_scroll: yes
---




```r
STARTDATE <- "2019-01-01"
ENDDATE <- lubridate::today()
GEO_TYPE = "state" # state-level
GEO_VALUE = "ca" # all states
EXCLUDED_AREAS = c("as","gu", "mp","vi") # excluded areas due to small sample size

# Pick one of the following to specify mobility signal from delphi API
# "full_time_work_prop"       "part_time_work_prop"      
# "completely_home_prop"     "median_home_dwell_time"  

MOBILITY <- "full_time_work_prop"


# Pick one of the following to specify policy
# Please refer to data source code book for details

# "EmergDec"            "SchoolClose"        
# "GathRestrict"        "BarRestrict"        
# "OtherBusinessClose"  "RestaurantRestrict" 
# "CaseIsolation"       "StayAtHome"         
# "PublicMask"          "Quarantine"         
# "NEBusinessClose"     "TravelRestrictIntra"
# "TravelRestrictEntry" "SchoolMask"         
# "TravelRestrictExit"  "BusinessMask"   

POLICY <- "EmergDec"
```



# Full-time away home signal


```r
# Filter to only specified one state
all.policies <- intervention %>%
  filter(geo_value == GEO_VALUE)

# We fist get all the signals
mobility <- data[[MOBILITY]][c("time_value","value")] #mobility

case.count <- data$Avg.Confirmed.Case.Count[c("time_value",
                                              "value")]# case count

cumulative.case.count <-data$Cum.Avg.Case.Count[c("time_value",
                                                  "value")]# cumulative case count

cumulative.case.count.prop <-data$Cum.Avg.Case.Count.Prop[c("time_value",
                                                            "value")] # cumulative case count per 100,000

avg.death.count <- data$Avg.Death.Case.Count[c("time_value",
                                               "value")] #death count

cum.avg.death.count <- data$Cum.Avg.Death.Count[c("time_value",
                                                  "value")] #cumulative death count

doc.visit <- data$smoothed_cli[c("time_value",
                                 "value")] # doctor visit

adj.doc.visit <- data$smoothed_adj_cli[c("time_value",
                                         "value")] # adjusted doctor visit

# Instead of calling "value", we distingush the value by its original name
colnames(mobility)[2]<- MOBILITY
colnames(case.count)[2] <- "confirmed_7dav_incidence_num"
colnames(cumulative.case.count)[2] <- "confirmed_7dav_cumulative"
colnames(cumulative.case.count.prop)[2] <- "confirmed_7dav_cumulative_prop"
colnames(avg.death.count)[2]<-"deaths_7dav_incidence_num"
colnames(cum.avg.death.count)[2] <- "deaths_7dav_cumulative_num"
colnames(doc.visit)[2] <- "smoothed_cli"
colnames(adj.doc.visit)[2] <- "smoothed_adj_cli"

# Create a list of confounders for left join with mobility
confounders <- list(case.count, 
                    cumulative.case.count, 
                    cumulative.case.count.prop,
                    avg.death.count, 
                    cum.avg.death.count, 
                    doc.visit, 
                    adj.doc.visit)

# Left join mobility dataframe with policy dataframe in CA
intervention_mobility_case <- joinMobilityAndPolicy(mobility,
                                                    all.policies,
                                                    confounders)

# Get the first time stamp of the intervention in the specified state
intervention.first.day <- getFirstDayOfIntervention(intervention, GEO_VALUE, POLICY)

# Plot the time-series: mobility signal as y, time as x 
plotInterventionTime(intervention_mobility_case, 
                                mobility.name=MOBILITY,
                                intervention.first.day,
                                ylab= MOBILITY,
                                xlab="time-value")
```

![](03_regression_discontinuity_files/figure-html/preprocess-1.png)<!-- -->

## Lag analysis at a state-level

![](03_regression_discontinuity_files/figure-html/state-lag-analysis-1.png)<!-- -->

## Use most correlated shift time to build linear models

$$y = \beta_{0} + \beta_{1}(\sum I(A_i = 1)) + \beta_{2}x_1 + \beta_{3}x_2$$



```r
# select best lag number based on intial exploration
case.shifted.days.spearman <- 37 # based spearman correlation
doc.visit.shifted.days.spearman <- 37 # based spearman correlation for doctor visit
cum.death.shifted.days.spearman <- 55
death.shifted.days.spearman <- 42

case.vec <- c("confirmed_7dav_incidence_num", "confirmed_7dav_cumulative", "confirmed_7dav_cumulative_prop")
doc.vec <- c("smoothed_cli", "smoothed_adj_cli")
cum.death.vec <- c("deaths_7dav_cumulative_num")
death.vec <- c("deaths_7dav_incidence_num")

# Make two copies for shifting the data
selected.df <- intervention_mobility_case
factored_data <- intervention_mobility_case
# Change the data by shifting the covariates
# Shift the case count column vector by the specified shift time
factored_data <- shiftDays(selected.df, factored_data, case.shifted.days.spearman, case.vec)

# Shift the  doctor column vector by the specified shift time
factored_data <-shiftDays(selected.df, factored_data, doc.visit.shifted.days.spearman, doc.vec)
  
# Shift the death count column vector by the specified shift time
factored_data <-shiftDays(selected.df, factored_data, cum.death.shifted.days.spearman, cum.death.vec)

# Shift the cum death count column vector by the specified shift time
factored_data <-shiftDays(selected.df, factored_data, death.shifted.days.spearman, death.vec)
  
variables <- c("intervention.duration", "smoothed_cli","smoothed_adj_cli")
formula <- as.formula(
  paste(MOBILITY, 
        paste(variables, collapse = " + "), 
        sep = " ~ "))

# We specifically look at emergency declaration
dummy.df <- factored_data%>% 
  mutate(intervention.duration = cumsum(eval(parse(text=POLICY))))

lm(formula=formula,dummy.df) %>% summary()
```

```
## 
## Call:
## lm(formula = formula, data = dummy.df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.016182 -0.006830 -0.000092  0.004789  0.038178 
## 
## Coefficients:
##                         Estimate Std. Error t value Pr(>|t|)    
## (Intercept)            4.150e-02  1.243e-03  33.399  < 2e-16 ***
## intervention.duration  2.908e-05  1.436e-05   2.025    0.044 *  
## smoothed_cli           4.654e-03  9.260e-04   5.026 9.82e-07 ***
## smoothed_adj_cli      -5.276e-03  1.152e-03  -4.580 7.48e-06 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.009336 on 239 degrees of freedom
##   (433 observations deleted due to missingness)
## Multiple R-squared:  0.139,	Adjusted R-squared:  0.1282 
## F-statistic: 12.86 on 3 and 239 DF,  p-value: 8.13e-08
```


```r
# We specifically look at emergency declaration

variables <- c("intervention.duration",
               "smoothed_cli",
               "smoothed_adj_cli", 
               "confirmed_7dav_cumulative_prop",
               "deaths_7dav_cumulative_num")

formula <- as.formula(
  paste(MOBILITY, 
        paste(variables, collapse = " + "), 
        sep = " ~ "))

dummy.df <- factored_data%>% 
  mutate(intervention.duration = cumsum(eval(parse(text=POLICY)))) 

lm(formula, dummy.df) %>% 
  summary()
```

```
## 
## Call:
## lm(formula = formula, data = dummy.df)
## 
## Residuals:
##        Min         1Q     Median         3Q        Max 
## -0.0149837 -0.0061097  0.0008416  0.0050389  0.0221589 
## 
## Coefficients:
##                                  Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                     4.120e-02  3.890e-03  10.593  < 2e-16 ***
## intervention.duration          -1.347e-05  6.040e-05  -0.223    0.824    
## smoothed_cli                    4.495e-03  7.783e-04   5.776 2.89e-08 ***
## smoothed_adj_cli               -4.559e-03  1.030e-03  -4.424 1.59e-05 ***
## confirmed_7dav_cumulative_prop -3.801e-06  4.600e-06  -0.826    0.410    
## deaths_7dav_cumulative_num      1.372e-06  1.116e-06   1.230    0.220    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.007803 on 200 degrees of freedom
##   (470 observations deleted due to missingness)
## Multiple R-squared:  0.2642,	Adjusted R-squared:  0.2458 
## F-statistic: 14.36 on 5 and 200 DF,  p-value: 5.152e-12
```

## Model with time only (weekends included)

![](03_regression_discontinuity_files/figure-html/time-only regression-weekends-included-1.png)<!-- -->

## Model with shifting the covariates (weekends included)

![](03_regression_discontinuity_files/figure-html/plot the regression-1.png)<!-- -->

## Model without shifting the covariates (weekends included)


```r
variables <- c("intervention.duration",
               "smoothed_cli",
               "smoothed_adj_cli")

formula <- as.formula(
  paste(MOBILITY, 
        paste(variables, collapse = " + "), 
        sep = " ~ "))

intervention.lm <- intervention_mobility_case %>% 
  mutate(intervention.duration = cumsum(eval(parse(text=POLICY)))) 

lm.fit.no.lag <- lm(formula, data =intervention.lm) 

intervention.lm$predlm <- c(rep(NA, nrow(intervention.lm) - length(predict(lm.fit.no.lag))), predict(lm.fit.no.lag))

intervention.lm%>% 
  mutate(policy.duration = cumsum(eval(parse(text=POLICY))), 
         Intervention = as.factor(eval(parse(text=POLICY)))) %>% 
  ggplot(aes_string(x = "time_value", y = MOBILITY, color ="Intervention")) +
  geom_point() + 
  geom_line(aes(x = time_value, y = predlm, colour="fitted value"), size = 1)+
  labs(title = "Covariates selected WITHOUT most correlated number of shift")
```

![](03_regression_discontinuity_files/figure-html/LR model without selecting most correlated lag covariates-1.png)<!-- -->

## Weekend effects

We suspect that the mobility signal is lower than usual during the weekend.


```r
intervention_mobility_case$weekday <- weekdays(as.Date(intervention_mobility_case$time_value)) 

p <- ggplot(intervention_mobility_case, 
            aes_string(x="weekday", y=MOBILITY)) + 
  geom_boxplot()
p
```

![](03_regression_discontinuity_files/figure-html/examine weekend effects from the data-1.png)<!-- -->


## Re-plot the regression line after dropping weekends

## Regressing on time only

![](03_regression_discontinuity_files/figure-html/time-only-regression-weekends-excluded-1.png)<!-- -->

### Covariates shifted


```
## 
## Call:
## lm(formula = formula, data = factored_data.without.weekend)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.017671 -0.004308 -0.001070  0.003144  0.034373 
## 
## Coefficients:
##                     Estimate Std. Error t value Pr(>|t|)    
## (Intercept)        4.527e-02  1.177e-03  38.459   <2e-16 ***
## EmergDec.duration  2.718e-05  1.341e-05   2.027   0.0442 *  
## smoothed_cli       6.743e-04  8.620e-04   0.782   0.4351    
## smoothed_adj_cli  -6.465e-04  1.104e-03  -0.585   0.5591    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.007543 on 171 degrees of freedom
##   (309 observations deleted due to missingness)
## Multiple R-squared:  0.08388,	Adjusted R-squared:  0.06781 
## F-statistic: 5.219 on 3 and 171 DF,  p-value: 0.001796
```

![](03_regression_discontinuity_files/figure-html/plot the regression line for the data WITH shift after dropping weekends-1.png)<!-- -->


### Covariates not shifted



```
## 
## Call:
## lm(formula = formula, data = noshift.without.weekends)
## 
## Residuals:
##        Min         1Q     Median         3Q        Max 
## -0.0304400 -0.0058428 -0.0004182  0.0056865  0.0243903 
## 
## Coefficients:
##                         Estimate Std. Error t value Pr(>|t|)    
## (Intercept)            5.561e-02  1.235e-03  45.017  < 2e-16 ***
## intervention.duration  5.054e-05  1.333e-05   3.791 0.000199 ***
## smoothed_cli           1.716e-03  9.105e-04   1.884 0.061006 .  
## smoothed_adj_cli      -4.766e-03  1.187e-03  -4.016 8.44e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.008814 on 196 degrees of freedom
##   (284 observations deleted due to missingness)
## Multiple R-squared:  0.1597,	Adjusted R-squared:  0.1468 
## F-statistic: 12.42 on 3 and 196 DF,  p-value: 1.81e-07
```

![](03_regression_discontinuity_files/figure-html/plot the regression line for the data WITHOUT shift after dropping weekends-1.png)<!-- -->

### What will happen if we add more covidcast signals as covariates?

#### Adding covidcast signals


```
## 
## Call:
## lm(formula = full_time_work_prop ~ EmergDec.duration + smoothed_cli + 
##     smoothed_adj_cli + confirmed_7dav_incidence_num + confirmed_7dav_cumulative + 
##     confirmed_7dav_cumulative_prop + deaths_7dav_incidence_num + 
##     deaths_7dav_cumulative_num)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.018989 -0.003057 -0.000010  0.003173  0.013876 
## 
## Coefficients: (1 not defined because of singularities)
##                                  Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                     4.871e-02  6.308e-03   7.722 1.98e-12 ***
## EmergDec.duration              -1.179e-04  1.150e-04  -1.025   0.3072    
## smoothed_cli                    1.871e-04  5.742e-04   0.326   0.7451    
## smoothed_adj_cli               -2.886e-05  8.177e-04  -0.035   0.9719    
## confirmed_7dav_incidence_num    6.414e-07  3.628e-07   1.768   0.0793 .  
## confirmed_7dav_cumulative      -2.426e-08  1.775e-08  -1.367   0.1738    
## confirmed_7dav_cumulative_prop         NA         NA      NA       NA    
## deaths_7dav_incidence_num       8.145e-05  4.858e-05   1.677   0.0958 .  
## deaths_7dav_cumulative_num      3.121e-06  2.158e-06   1.446   0.1503    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.004958 on 140 degrees of freedom
##   (336 observations deleted due to missingness)
## Multiple R-squared:  0.4041,	Adjusted R-squared:  0.3743 
## F-statistic: 13.56 on 7 and 140 DF,  p-value: 2.634e-13
```

![](03_regression_discontinuity_files/figure-html/more covariates to without weekend models-1.png)<!-- -->

#### Adding other policy duration


```r
# Try to add other intervention covariates
formula.head <- paste(MOBILITY, 
                 "EmergDec.duration", sep=" ~ ")

formula <- as.formula(paste(formula.head,
      "StayAtHome.duration",
      "PublicMask.duration",
      "SchoolClose.duration",
      "GathRestrict.duration",
      "BarRestrict.duration",
      "NEBusinessClose.duration",
      "RestaurantRestrict.duration",
      "smoothed_cli",
      "smoothed_adj_cli",
      "confirmed_7dav_incidence_num",
      "confirmed_7dav_cumulative",
      "confirmed_7dav_cumulative_prop",
      "deaths_7dav_incidence_num",
      "deaths_7dav_cumulative_num",
      "SchoolClose.duration", sep="+"))


lm(formula, factored_data.without.weekend) %>% 
  summary()
```

```
## 
## Call:
## lm(formula = formula, data = factored_data.without.weekend)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.018989 -0.003057 -0.000010  0.003173  0.013876 
## 
## Coefficients: (8 not defined because of singularities)
##                                  Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                     4.871e-02  6.308e-03   7.722 1.98e-12 ***
## EmergDec.duration              -1.179e-04  1.150e-04  -1.025   0.3072    
## StayAtHome.duration                    NA         NA      NA       NA    
## PublicMask.duration                    NA         NA      NA       NA    
## SchoolClose.duration                   NA         NA      NA       NA    
## GathRestrict.duration                  NA         NA      NA       NA    
## BarRestrict.duration                   NA         NA      NA       NA    
## NEBusinessClose.duration               NA         NA      NA       NA    
## RestaurantRestrict.duration            NA         NA      NA       NA    
## smoothed_cli                    1.871e-04  5.742e-04   0.326   0.7451    
## smoothed_adj_cli               -2.886e-05  8.177e-04  -0.035   0.9719    
## confirmed_7dav_incidence_num    6.414e-07  3.628e-07   1.768   0.0793 .  
## confirmed_7dav_cumulative      -2.426e-08  1.775e-08  -1.367   0.1738    
## confirmed_7dav_cumulative_prop         NA         NA      NA       NA    
## deaths_7dav_incidence_num       8.145e-05  4.858e-05   1.677   0.0958 .  
## deaths_7dav_cumulative_num      3.121e-06  2.158e-06   1.446   0.1503    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.004958 on 140 degrees of freedom
##   (336 observations deleted due to missingness)
## Multiple R-squared:  0.4041,	Adjusted R-squared:  0.3743 
## F-statistic: 13.56 on 7 and 140 DF,  p-value: 2.634e-13
```

```r
# Predict the mobility
new.pred <- lm(formula, factored_data.without.weekend) %>% predict()

# Pad the fitted values with NA 
factored_data.without.weekend$predlm <- c(rep(NA, nrow(factored_data.without.weekend) - length(new.pred)), new.pred)

# Plot the graph
factored_data.without.weekend %>%
  ggplot(aes_string(x = "time_value", 
                    y = MOBILITY,
                    color = "Intervention")) +
  geom_point() + 
  geom_line(aes(y = predlm, 
                colour="fitted value"), size = 1) +
   labs(title = "All covariates selected WITH most correlated number of shift (weekends dropped)")
```

![](03_regression_discontinuity_files/figure-html/add more other policy duration-1.png)<!-- -->
# Completely Staying at home

# 
