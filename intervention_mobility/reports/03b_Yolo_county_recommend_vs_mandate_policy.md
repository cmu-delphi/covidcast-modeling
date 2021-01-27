---
title: "Evaluating recommend and mandatory policy for Yolo county"
author: "Kenneth Lee"
date: "25/01/2021"
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

This report is to compare the impacts of recommended policy and mandatory policies on restaurant visit and staying at home in Yolo county. We will use the following model:

$$Y_t = \beta_{0} + \sum_{i}\beta_{i}C_{i,t-1} + \sum_{k}\beta_{k}I_{k,t} \text{    for  } i = \{1,\dots,6\}, k = \{1,\dots,19\}$$


```r
library(covidcast)
library(dplyr)
library(lubridate)
library(mgcv)
library(ggplot2)

source("code/loader.r")
source("code/parser.r")
```



```r
STARTDATE <- "2019-01-01"  # Forward the mobility one day in time
ENDDATE <- lubridate::today()
state <- "ca"
GEO_VALUE <- "06113" # Allegheny_County fip 
GEO_TYPE <- "county"

restvisit <-  covidcast_signal(data_source = "safegraph",
                                    signal ="restaurants_visit_prop",
                                    start_day = STARTDATE, 
                                    end_day = ENDDATE,
                                    geo_type = GEO_TYPE, 
                                    geo_values = GEO_VALUE)

p <- ggplot(restvisit, aes(x=time_value, y=value)) +
  geom_line() + 
  labs(title = "Number of daily restaurant visit by time in Yolo county", y= "# of resturant visit per 100,000 population")
p
```

![](03b_Yolo_county_recommend_vs_mandate_policy_files/figure-html/plotting-for-fun-1.png)<!-- -->


```r
state <- "ca"
GEO_VALUE <- "06113" # Allegheny_County fip 
GEO_TYPE <- "county"
STARTDATE <- "2020-02-20" # Start from where we have case count signals
ENDDATE <- "2021-01-17" # fix a recent end date

# Read in county-level intervention data 
# from stanford crowdsourced data
allcountypolicy <- read.csv("data/stanford-county-level-intervention-data.csv")
# filter down to county of intersts
countypolicy <- allcountypolicy %>% filter(county == "Yolo County")

# Read in state-wide policy data from UW
policy <- load_policy()
statepolicy <- policy %>% 
  filter(StatePostal==state, StateWide==1)


# Fetch case signals
dailyconfirmed <- covidcast_signal(data_source = "indicator-combination",
                                    signal ="confirmed_7dav_incidence_prop",
                                    start_day = STARTDATE, 
                                    end_day = ENDDATE,
                                    geo_type = GEO_TYPE, 
                                    geo_values = GEO_VALUE)

dailydeath <- covidcast_signal(data_source = "indicator-combination",
                                    signal ="deaths_7dav_incidence_prop",
                                    start_day = STARTDATE, 
                                    end_day = ENDDATE,
                                    geo_type = GEO_TYPE, 
                                    geo_values = GEO_VALUE)



# Fetch mobility signals of interests 

STARTDATE <- as.Date(STARTDATE) + 1  # Forward the mobility one day in time
ENDDATE <- as.Date(ENDDATE) + 1

restvisit <-  covidcast_signal(data_source = "safegraph",
                                    signal ="restaurants_visit_prop",
                                    start_day = STARTDATE, 
                                    end_day = ENDDATE,
                                    geo_type = GEO_TYPE, 
                                    geo_values = GEO_VALUE)
```


```r
plot_bar <- function(df,
                     x,
                     y,
                     lower.bound,
                     upper.bound,
                     gp,
                     name){
  p<- ggplot(df, aes_string(x=x, y=y, group=gp, color=gp)) + 
  #geom_line() +
  geom_point()+
  geom_errorbar(aes_string(xmin=lower.bound, xmax=upper.bound), width=.2,
                 position=position_dodge(0.05))+
    ggtitle(name)
      
  return(p)
}

# process each policy as follows
# 1. look for the policy enact time, call it start date
# 2. then look for the policy ended time
# 3. if ended time is NA, look for expiry date, if not, set it as end date
# 4. if expiry date is NA, then it means the policy hasn't ended, if not, set it as enddate
# 5. Form a sequence of start date and end dates, assign 1 to any dates fall into this sequence.
concatCaseNPolicy <- function(case.df){
  # Loop through each row in the column called policy
  for(row in (1:nrow(statepolicy))){
    # for each policy, we do
    policy.name <- statepolicy[row,]$StatePolicy
    modified.policyName <- statepolicy[row,]$policy
    # Set the start date of the policy
    start <- statepolicy[row,]$DateEnacted
    
    # Set the end date of the policy
    if(is.na(statepolicy[row,]$DateEnded)){
      # if DateEnded is NA, then look for DateExpiry
      if(is.na(statepolicy[row,]$DateExpiry)){
        # if DateExpiry is NA, then the policy is in effect until the next policy with the same name happens
        
        # check to see if the same policy exist
        if(row == nrow(statepolicy)){
          # If this is the last policy in the data,
          # take the last day of the case signal
          end <- lubridate::today()
        }else{
          # if it is not the last policy, then look for the next one 
          
          # check if we still have the same policy
          if(policy.name %in% statepolicy[row+1:nrow(statepolicy),]$StatePolicy){
            # if so, we look for the first one
            for(k in (row+1:nrow(statepolicy))){
              if(statepolicy[k,]$StatePolicy==policy.name){
                if(statepolicy[k,]$DateEnacted == start){
                   end <- lubridate::today()
                }else{
                  end <- statepolicy[k,]$DateEnacted-1
                }
                break
              }
            }
          }else{
            # if not, then we will assign 
            end <- lubridate::today()
          }
        }
      }else{
        end <- statepolicy[row,]$DateExpiry - 1
      }
    }else{
      end <- statepolicy[row,]$DateEnded - 1
    }
    
    
    policy.period <- seq(start, end, 1)
    
    assigned <- ifelse(case.df$time_value %in% policy.period, 1, 0)
    
    # check if there are any duplicate names
    if(modified.policyName %in% colnames(case.df)){
      # if so, we assign 1's to that created column

      case.df[, modified.policyName] <- case.df[, modified.policyName] + assigned
    }else{
      # if not, create a new column and assign one to that column within the time
      case.df[, modified.policyName] <- assigned
    }
  }
  return(case.df)
}
```


Let's only look at the data from February 2020 onward because we only have case count signals available starting from February 2020. 


```r
# Filter the dates that we don't have mobility signals
missingdates <- c(seq(as.Date("2020-12-13"),as.Date("2020-12-19"), 1),
                  as.Date("2021-01-17"))

# Filter the case signals
dailyconfirmed.f <- dailyconfirmed %>% 
  filter(!(time_value %in% missingdates)) %>%
  mutate(dailyconfirmed_prop = value)

dailydeath.f <- dailydeath %>% 
  filter(!(time_value %in% missingdates)) %>%
  mutate(dailydeath_prop = value) %>%
  select(dailydeath_prop)

# Concatnate case counts and 
restvisit$dailyconfirmed_prop <- dailyconfirmed.f$dailyconfirmed_prop
restvisit$dailydeath_prop <- dailydeath.f$dailydeath_prop
colnames(restvisit)[7] <- "restaurants_visit_prop"



# Plot the signal
p <- ggplot(restvisit, aes(x=time_value, y=restaurants_visit_prop)) +
  geom_line() + 
  labs(title = "Number of daily restaurant visit by time in Yolo county (weekends included)", y= "# of resturant visit per 100,000 population")
p
```

![](03b_Yolo_county_recommend_vs_mandate_policy_files/figure-html/preprocess-case-signals-1.png)<!-- -->




```r
# Combine some information for gathering restriction

# get all indicies that are gathering restriction
gathering.idx <- which(statepolicy$StatePolicy == "GathRestrict")

statepolicy$policy <- paste(statepolicy$StatePolicy,statepolicy$Mandate, sep="")

limit.idx <- paste(statepolicy$InGathLim,
      statepolicy$OutGathLim,
      statepolicy$InGathLimReligious,
      statepolicy$OutGathLimReligious,
      sep="-")

# Get all the indicies wrt gathering restriction
statepolicy[gathering.idx,]$policy <- paste(limit.idx[gathering.idx],statepolicy[gathering.idx,]$policy , sep="")


lmdf <- concatCaseNPolicy(restvisit)

  
# convert to factor
lmdf[,c(13:ncol(lmdf))] <- sapply(lmdf[,c(13:ncol(lmdf))], function(x) as.factor(x)) 
```

## Restaurant visit prop


```r
# Covaraites: all case signals t-1, all interventions, ease indicator, expand indicator
# reponse: restaurant visit/ bar visits
df <- lmdf[,c(7, c(11:ncol(lmdf)))]
names <- colnames(df)[-1]

lm.fit <- lm(restaurants_visit_prop ~ ., df)
sum.lm.fit <- summary(lm.fit)

plt.df <- data.frame(fit= sum.lm.fit$coefficients[c(-1:-3),1], interventions=names(sum.lm.fit$coefficients[c(-1:-3),1]))

revised_df <- cbind(plt.df, confint(lm.fit)[which(names(lm.fit$coefficients) %in% names(sum.lm.fit$coefficients[c(-1:-3),1])),])

colnames(revised_df)[3] <- "LCI"
colnames(revised_df)[4] <- "UCI"
  
sum.lm.fit 
```

```
## 
## Call:
## lm(formula = restaurants_visit_prop ~ ., data = df)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -39.671  -6.212  -0.230   5.874  46.371 
## 
## Coefficients: (4 not defined because of singularities)
##                                  Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                     107.25909    3.05388  35.122  < 2e-16 ***
## dailyconfirmed_prop              -0.31702    0.09718  -3.262  0.00123 ** 
## dailydeath_prop                  -3.75209    4.69539  -0.799  0.42484    
## EmergDec11                        6.27940    5.03129   1.248  0.21295    
## `250-250-250-250GathRestrict0`1 -20.65680    8.48202  -2.435  0.01544 *  
## SchoolClose01                     2.10086    9.65721   0.218  0.82793    
## `NA-NA-NA-NAGathRestrict0`1     -63.12363   12.10595  -5.214 3.38e-07 ***
## BarRestrict11                   -88.46158   15.37959  -5.752 2.12e-08 ***
## `NA-NA-0-NAGathRestrict1`1        8.47604   10.90024   0.778  0.43740    
## NEBusinessClose11                      NA         NA      NA       NA    
## RestaurantRestrict11                   NA         NA      NA       NA    
## StayAtHome11                           NA         NA      NA       NA    
## PublicMask01                      1.71508    3.49068   0.491  0.62354    
## OtherBusinessClose13              5.79653    3.12093   1.857  0.06422 .  
## `NA-NA-100-NAGathRestrict1`1           NA         NA      NA       NA    
## BusinessMask11                   26.83506   10.80781   2.483  0.01356 *  
## PublicMask11                      3.52340    4.63638   0.760  0.44786    
## SchoolMask11                      3.60393    2.21446   1.627  0.10466    
## Quarantine01                      0.91174    3.36188   0.271  0.78642    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 10.58 on 310 degrees of freedom
## Multiple R-squared:  0.7293,	Adjusted R-squared:  0.7171 
## F-statistic: 59.67 on 14 and 310 DF,  p-value: < 2.2e-16
```

```r
plot_bar(revised_df,
              "fit",
              "interventions",
              "LCI",
              "UCI",
              "fit",
               "Yolo County") 
```

![](03b_Yolo_county_recommend_vs_mandate_policy_files/figure-html/analysis-parametric-restvisit-1.png)<!-- -->

