---
title: "Regression models: mobility, intervention, and potential confounders"
author: "Kenneth Lee"
date: "15/09/2020"
output: 
  html_document:
    code_folding: hide
    keep_md: yes
    toc: yes
    toc_float:
      collapsed: yes
      smooth_scroll: yes
---

## Data Preprocessing


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
library(MASS)

source("code/painter.r")
source("code/loader.r")
source("code/parser.r")
```


```r
STARTDATE <- "2020-02-20"
ENDDATE <- lubridate::today()
GEO_TYPE = "state" # state-level
GEO_VALUE = "*" # all states
EXCLUDED_AREAS = c("as","gu", "mp","vi") # excluded areas due to small sample size
```


```r
# Load the covidcast signal data
data <- load_covidcast_data(STARTDATE, 
                            ENDDATE, 
                            GEO_TYPE, 
                            GEO_VALUE, 
                            EXCLUDED_AREAS)

# Load the policy data
policy <- load_policy()

policy_signal <- getSumOfPolicy(policy, STARTDATE, ENDDATE)
```



```r
# We fist get all the signals
ftime.mobility <- data$full_time_work_prop[c("time_value","geo_value","value")] #mobility

case.count <- data$Avg.Confirmed.Case.Count[c("time_value","geo_value","value")]# case count

cumulative.case.count <-data$Cum.Avg.Case.Count[c("time_value","geo_value","value")]# cumulative case count

cumulative.case.count.prop <-data$Cum.Avg.Case.Count.Prop[c("time_value","geo_value","value")] # cumulative case count per 100,000

avg.death.count <- data$Avg.Death.Case.Count[c("time_value","geo_value","value")] #death count

cum.avg.death.count <- data$Cum.Avg.Death.Count[c("time_value","geo_value","value")] #cumulative death count

doc.visit <- data$smoothed_cli[c("time_value","geo_value","value")] # doctor visit

adj.doc.visit <- data$smoothed_adj_cli[c("time_value","geo_value","value")] # adjusted doctor visit

# Change the column name
colnames(ftime.mobility)[3]<-"full_time_work_prop"
colnames(case.count)[3] <- "confirmed_7dav_incidence_num"
colnames(cumulative.case.count)[3] <- "confirmed_7dav_cumulative"
colnames(cumulative.case.count.prop)[3] <- "confirmed_7dav_cumulative_prop"
colnames(avg.death.count)[3]<-"deaths_7dav_incidence_num"
colnames(cum.avg.death.count)[3] <- "deaths_7dav_cumulative_num"
colnames(doc.visit)[3] <- "smoothed_cli"
colnames(adj.doc.visit)[3] <- "smoothed_adj_cli"

# Create a list of confounders for left join with mobility
confounders <- list(case.count, 
                    cumulative.case.count, 
                    cumulative.case.count.prop,
                    avg.death.count, 
                    cum.avg.death.count, 
                    doc.visit, 
                    adj.doc.visit)

# Turn the polical signal to be factors
factored.policy.signal <- cbind(policy_signal[1:2], 
                                lapply(policy_signal[3:17],
                                       as.factor),
                                policy_signal[18:20])

# Intervention left join mobility with policy signal
intervention_mobility_case <- left_join(ftime.mobility,factored.policy.signal , by=c("time_value", "geo_value"))

# Left join again with all other potential confounders
for (confounder in confounders){
  intervention_mobility_case <- left_join(intervention_mobility_case, confounder, by=c("time_value", "geo_value"))
}

# Filter state "pr" as it is not available in the intervention data
factored_data <- intervention_mobility_case %>% filter(!(geo_value %in% c("pr")))
```

## Treatment effect of government interventions

Moreover, we may be interested to know the following:

* Is there a difference in the means of mobility signal across states?

* Is there a difference in the means of mobility signal in terms of stay-at-home order?

* Is there an interaction between the factor of states and the factor of the intervention?

To answer these questions, assuming that the data are normally distributed and the variance across groups are homogeneous, we will use ANOVA (Analysis of Variance). We will check these assumptions in the model diagnostics. 


### Main effect of States

We can see that some states have a particularly higher range of the mean mobility signal. For example, Montana (MT) clearly stands out from the rest, whereas Hawaii (HI) has a much lower mean mobility signal from Feb. to Sep. in 2020.


```r
# Plotting the main effect of geo_value
plotmeans(full_time_work_prop~ geo_value,data=intervention_mobility_case, xlab="Geo_value", ylab="Mobility", main="Main effect (States)") 
```

![](01_regression_ANOVA_files/figure-html/check main effect-1.png)<!-- -->

### Interaction plots

We can also look at the interaction plots to visually examine whether the effect that an intervention has on the mean mobility signal is independent of the state.

We notice the following interventions may have statistically siginficant interaction with the factor of states on the mean mobility signal:

* Gathering Reommendation (Recommendation of against gathering that stops short of a formal mandate or restriction of gatherings) , public mask, quarantine.


```r
# Get all the column indicies for policies 
policyIdx <- which(!(names(factored_data) %in%
        c("time_value",
          "geo_value",
          "full_time_work_prop",
          "part_time_work_prop",
          "median_home_dwell_time",
          "completely_home_prop",
          "total.num.policy",
          "num.policy.7avg",
          "confirmed_7dav_incidence_num",
          "confirmed_7dav_cumulative",
          "confirmed_7dav_cumulative_prop",
          "deaths_7dav_incidence_num",
          "deaths_7dav_cumulative_num",
          "smoothed_cli",
          "smoothed_adj_cli")))

for(i in policyIdx){
  interaction.plot(factored_data[,"geo_value"], factored_data[,names(factored_data)[i]], factored_data$full_time_work_prop, xlab="States", ylab= "Full time work prop", main=paste("Interaction plot between states and", names(factored_data)[i], "on mobility"), trace.label = names(factored_data)[i])  
}
```

![](01_regression_ANOVA_files/figure-html/interaction plots across states-1.png)<!-- -->![](01_regression_ANOVA_files/figure-html/interaction plots across states-2.png)<!-- -->![](01_regression_ANOVA_files/figure-html/interaction plots across states-3.png)<!-- -->![](01_regression_ANOVA_files/figure-html/interaction plots across states-4.png)<!-- -->![](01_regression_ANOVA_files/figure-html/interaction plots across states-5.png)<!-- -->![](01_regression_ANOVA_files/figure-html/interaction plots across states-6.png)<!-- -->![](01_regression_ANOVA_files/figure-html/interaction plots across states-7.png)<!-- -->![](01_regression_ANOVA_files/figure-html/interaction plots across states-8.png)<!-- -->![](01_regression_ANOVA_files/figure-html/interaction plots across states-9.png)<!-- -->![](01_regression_ANOVA_files/figure-html/interaction plots across states-10.png)<!-- -->![](01_regression_ANOVA_files/figure-html/interaction plots across states-11.png)<!-- -->![](01_regression_ANOVA_files/figure-html/interaction plots across states-12.png)<!-- -->![](01_regression_ANOVA_files/figure-html/interaction plots across states-13.png)<!-- -->![](01_regression_ANOVA_files/figure-html/interaction plots across states-14.png)<!-- -->![](01_regression_ANOVA_files/figure-html/interaction plots across states-15.png)<!-- -->![](01_regression_ANOVA_files/figure-html/interaction plots across states-16.png)<!-- -->

### Boxplots of mobility across different levels of intervention

From the boxplots below, we can see there are some obvious difference between having or not having certain interventions on the mobility signals. We will examine whether these differences are statistically significant.

The noteworthy interventions are:

* Emergency declaration, gathering restriction, restaurant restriction, school closure, bar restriction, other buseinss closure. 


```r
# plot the boplots for categoical variables columns
p <- list()
counter <- 1
# loop through all categorical variables for intervention
for (i in policyIdx){
p[[counter]]<- ggplot(factored_data, aes_string(x=names(factored_data)[3], y=names(factored_data)[i])) + 
    geom_boxplot() + 
     stat_summary(fun = mean, geom = "errorbar", aes(xmax = ..x.., xmin = ..x..), color ="red", linetype = 2)+
  labs(title=paste("Mobility by", names(factored_data)[i]), x="Mobility", y=names(factored_data)[i]) + theme(plot.title = element_text(size=9))

counter <- counter + 1
}
# Plot all the ggplot
do.call(grid.arrange,p)
```

```
## Warning: Continuous x aesthetic -- did you forget aes(group=...)?
```

![](01_regression_ANOVA_files/figure-html/boxplots for categorical variables-1.png)<!-- -->

### Distribution of mobility by various intervention across states

We may also be interested to know if there are some states siginficantly contribute to the difference between having and not having certain interventions on the mobility.

For emergency declaration, all the states tend to have have lower mobility signal when it is implemented. Some states such as New Jersey, lowa, New York, have a clear large difference in mobility between having and not having emergency declaration. 

Also, for the school closure policy, the mobility in Mississippi remains quite strong as compared to not having the school closure policy.

Regarding gathering restriction, restaurant restriction, and bar restriction, there isn't any particular state standing out in terms of the difference of mobility between not having and having the intervention.


```r
# grouped boxplot
for (i in policyIdx){
  
p <- ggplot(factored_data, aes_string(x=names(factored_data)[3], y=names(factored_data)[2], fill=names(factored_data)[i])) + 
    geom_boxplot() +
  labs(title=paste("Mobility by State and", names(factored_data)[i]),x="mobility", y="State")

print(p)
}
```

![](01_regression_ANOVA_files/figure-html/mobility signal by intervention-1.png)<!-- -->![](01_regression_ANOVA_files/figure-html/mobility signal by intervention-2.png)<!-- -->![](01_regression_ANOVA_files/figure-html/mobility signal by intervention-3.png)<!-- -->![](01_regression_ANOVA_files/figure-html/mobility signal by intervention-4.png)<!-- -->![](01_regression_ANOVA_files/figure-html/mobility signal by intervention-5.png)<!-- -->![](01_regression_ANOVA_files/figure-html/mobility signal by intervention-6.png)<!-- -->![](01_regression_ANOVA_files/figure-html/mobility signal by intervention-7.png)<!-- -->![](01_regression_ANOVA_files/figure-html/mobility signal by intervention-8.png)<!-- -->![](01_regression_ANOVA_files/figure-html/mobility signal by intervention-9.png)<!-- -->![](01_regression_ANOVA_files/figure-html/mobility signal by intervention-10.png)<!-- -->![](01_regression_ANOVA_files/figure-html/mobility signal by intervention-11.png)<!-- -->![](01_regression_ANOVA_files/figure-html/mobility signal by intervention-12.png)<!-- -->![](01_regression_ANOVA_files/figure-html/mobility signal by intervention-13.png)<!-- -->![](01_regression_ANOVA_files/figure-html/mobility signal by intervention-14.png)<!-- -->![](01_regression_ANOVA_files/figure-html/mobility signal by intervention-15.png)<!-- -->![](01_regression_ANOVA_files/figure-html/mobility signal by intervention-16.png)<!-- -->

#### Transformations of the Response Variable

Distribution of mobility signal is right-skewed, so we may want to
consider a transformation to make it more normal like. The square-root transformation seems to work best in this case.


```r
par(mfrow=c(2,2))
hist(factored_data$full_time_work_prop, main="Histogram of unchanged response", xlab="full_time_work_prop")
hist(log(factored_data$full_time_work_prop), main="Histogram of log response", xlab="full_time_work_prop")
hist(1/(factored_data$full_time_work_prop), main="Histogram of 1/response", xlab="full_time_work_prop")
hist(sqrt(factored_data$full_time_work_prop), main="Histogram of squared root of response", xlab="full_time_work_prop")
```

![](01_regression_ANOVA_files/figure-html/visualize all histogram of reponse-1.png)<!-- -->

#### Histograms of all continuous variables

We also would like to explore some predictor variables.


```r
con.var.idx <- which(names(factored_data) %in%
                       c("total.num.policy",
                         "num.policy.7avg",
                         "confirmed_7dav_incidence_num",
                         "confirmed_7dav_cumulative",
                         "confirmed_7dav_cumulative_prop",
                         "deaths_7dav_incidence_num",
                         "deaths_7dav_cumulative_num",
                         "smoothed_cli",
                         "smoothed_adj_cli"))
par(mfrow=c(3,3))
for (i in con.var.idx) 
{
  hist(factored_data[,names(factored_data)[i]], xlab=names(factored_data)[i], main=paste("Histogram of", names(factored_data)[i]))
}
```

![](01_regression_ANOVA_files/figure-html/visualize the distributions of the continuous variables-1.png)<!-- -->

```r
par(mfrow=c(1,1))
```



```r
# select best lag number based on intial exploration
case.shifted.days.spearman <- 37 # based spearman correlation
doc.visit.shifted.days.spearman <- 50 # based spearman correlation for doctor visit
cum.death.shifted.days.spearman <- 37
death.shifted.days.spearman <- 34

case.vec <- c("confirmed_7dav_incidence_num", "confirmed_7dav_cumulative", "confirmed_7dav_cumulative_prop")
doc.vec <- c("smoothed_cli", "smoothed_adj_cli")
cum.death.vec <- c("deaths_7dav_cumulative_num")
death.vec <- c("deaths_7dav_incidence_num")

for (state in unique(factored_data$geo_value)){
  
  # Filter dataframe by every state
  selected.df <- factored_data %>% filter(geo_value==state)
  
  # Shift the case count column vector by the specified shift time
  factored_data <- shiftDays(selected.df, factored_data, case.shifted.days.spearman, case.vec)
  # Shift the  doctor column vector by the specified shift time
  factored_data <-shiftDays(selected.df, factored_data, doc.visit.shifted.days.spearman, doc.vec)
  
  # Shift the death count column vector by the specified shift time
  factored_data <-shiftDays(selected.df, factored_data, cum.death.shifted.days.spearman, cum.death.vec)
  # Shift the cum death count column vector by the specified shift time
  factored_data <-shiftDays(selected.df, factored_data, death.shifted.days.spearman, death.vec)
}
factored_data$geo_value <- as.factor(factored_data$geo_value)
```

### Linear regression models comparsion (Mobility, Intervention, Confounders)

We modified the data so that the confounders are shifted n days forwarded in time based on rank correlation in our initial exploratary data analysis.

We shifted mobility 25 days ahead for confirmed case count signal, 40 days ahead for doctor visit, and 50 days ahead for death case count.

After shifting the covariates, we also drop some data. The number of samples goes from 12597 to 10914. 

We also transform the reponse by taking a square-root of it to satisfy the normality assumption. 


```r
#Problematic: EmergDec, deaths_7dav_incidence_num

# model 1 : y is mobility, x's are the confounders and intervention
# Note: we dropped the geo and time value

lm.fit.1 <- lm(full_time_work_prop ~., data=factored_data[,-c(1)], na.action = na.exclude)
# Plot boxcox to see which transformation we should use
boxcox(lm.fit.1)
```

![](01_regression_ANOVA_files/figure-html/preliminary investigation-1.png)<!-- -->

```r
transformed.lm.fit.1<- lm(full_time_work_prop^(1/2) ~., data=factored_data[,-c(1)], na.action = na.exclude)

par(mfrow=c(2,2))
plot(lm.fit.1, which=1, main="Residuals vs Fitted (before transformation)") # residuals vs. fitted shows nonlinearity and nonconstant variance
plot(lm.fit.1, which=2, main="Residuals vs Fitted (before transformation)") # heavy tail

plot(transformed.lm.fit.1, which=1, main="Residuals vs Fitted (after transformation)") # residuals vs. fitted shows nonlinearity and nonconstant variance
plot(transformed.lm.fit.1, which=2, main="Normal (after transformation)") # residuals vs. fitted shows nonlinearity and nonconstant variance
```

![](01_regression_ANOVA_files/figure-html/preliminary investigation-2.png)<!-- -->

```r
par(mfrow=c(1,1))

# Plot the resulting mobility signal over time
plot(factored_data$time_value, factored_data$full_time_work_prop^(1/2), main="Square-root mobility by time", ylab="Square-root mobility", xlab="time")
```

![](01_regression_ANOVA_files/figure-html/preliminary investigation-3.png)<!-- -->


#### Model Selection in multiple regression

When we have a lot of predictor variables, instead of using them all, we would prefer choosing a few subsets of them that give us the best models based on a certain criterion.We can also try to find the "best model" sequentially using stepwise regression procedures.

**Observations**

First, when we focus on finding the "best" additive model, we find the model with the following covariates based on BIC:

* ``confirmed_7dav_cumulative_prop``
* ``geo_value`` 
* ``smoothed_cli``
* ``smoothed_adj_cli``
* ``confirmed_7dav_incidence_num``
* ``confirmed_7dav_cumulative`` 
* ``SchoolClose``
* ``BarRestrict`` 
* ``Quarantine``
* ``StayAtHome``

Note that from the summary, besides ``smoothed_cli ``, all other covidcast signals, ``smoothed_adj_cli``, ``deaths_7dav_incidence_num ``, , and ``confirmed_7dav_cumulative`` have negative coefficient on mobility, whereas most of the selected interventions (i.e. ``SchoolClose``, ``BarRestrict``, ``Quarantine``) have negative coefficients except for ``StayAtHome``.  

The reason why we are particularly interested in negative coefficients because we want to compare the effects of interventions and potential confounders on reducing mobility.  

Later, we can also look at the ANOVA model to see if these variables account for the variation of mobility significantly. 





```r
# We look at the summary of the selected model
fin_mod <- lm(formula = full_time_work_prop^(1/2) ~ confirmed_7dav_cumulative_prop + geo_value + smoothed_cli + smoothed_adj_cli + confirmed_7dav_cumulative + BarRestrict + deaths_7dav_incidence_num + Quarantine + SchoolClose + StayAtHome + confirmed_7dav_incidence_num, data = na.omit(factored_data[, -c(1)]))

# Show the summary
summary(fin_mod)
```

```
## 
## Call:
## lm(formula = full_time_work_prop^(1/2) ~ confirmed_7dav_cumulative_prop + 
##     geo_value + smoothed_cli + smoothed_adj_cli + confirmed_7dav_cumulative + 
##     BarRestrict + deaths_7dav_incidence_num + Quarantine + SchoolClose + 
##     StayAtHome + confirmed_7dav_incidence_num, data = na.omit(factored_data[, 
##     -c(1)]))
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.101389 -0.017740  0.003359  0.017547  0.105107 
## 
## Coefficients:
##                                  Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                     2.338e-01  2.544e-03  91.918  < 2e-16 ***
## confirmed_7dav_cumulative_prop  1.459e-05  6.433e-07  22.677  < 2e-16 ***
## geo_valueal                    -3.887e-02  3.117e-03 -12.469  < 2e-16 ***
## geo_valuear                    -2.800e-02  3.211e-03  -8.721  < 2e-16 ***
## geo_valueaz                    -4.323e-02  3.151e-03 -13.720  < 2e-16 ***
## geo_valueca                    -2.270e-02  3.351e-03  -6.773 1.33e-11 ***
## geo_valueco                    -3.625e-02  3.108e-03 -11.664  < 2e-16 ***
## geo_valuect                    -4.888e-02  3.179e-03 -15.377  < 2e-16 ***
## geo_valuedc                    -1.657e-02  3.229e-03  -5.132 2.91e-07 ***
## geo_valuede                    -2.468e-02  3.041e-03  -8.118 5.26e-16 ***
## geo_valuefl                    -3.977e-02  2.933e-03 -13.562  < 2e-16 ***
## geo_valuega                    -4.882e-02  3.123e-03 -15.632  < 2e-16 ***
## geo_valuehi                    -5.253e-02  2.858e-03 -18.380  < 2e-16 ***
## geo_valueia                    -3.087e-02  3.159e-03  -9.771  < 2e-16 ***
## geo_valueid                    -1.965e-02  3.005e-03  -6.539 6.49e-11 ***
## geo_valueil                    -5.299e-02  3.136e-03 -16.896  < 2e-16 ***
## geo_valuein                    -4.600e-02  3.109e-03 -14.794  < 2e-16 ***
## geo_valueks                    -3.566e-02  2.877e-03 -12.395  < 2e-16 ***
## geo_valueky                    -4.170e-02  2.879e-03 -14.481  < 2e-16 ***
## geo_valuela                    -4.062e-02  3.147e-03 -12.906  < 2e-16 ***
## geo_valuema                    -2.782e-02  2.897e-03  -9.601  < 2e-16 ***
## geo_valuemd                    -4.795e-02  3.216e-03 -14.907  < 2e-16 ***
## geo_valueme                    -7.146e-03  2.816e-03  -2.537 0.011180 *  
## geo_valuemi                    -3.846e-02  3.222e-03 -11.935  < 2e-16 ***
## geo_valuemn                    -4.820e-02  3.099e-03 -15.555  < 2e-16 ***
## geo_valuemo                    -3.660e-02  3.417e-03 -10.712  < 2e-16 ***
## geo_valuems                    -3.981e-02  3.129e-03 -12.724  < 2e-16 ***
## geo_valuemt                     1.752e-04  3.037e-03   0.058 0.953998    
## geo_valuenc                    -4.424e-02  3.110e-03 -14.223  < 2e-16 ***
## geo_valuend                    -1.727e-02  2.848e-03  -6.065 1.37e-09 ***
## geo_valuene                    -2.448e-02  3.280e-03  -7.462 9.19e-14 ***
## geo_valuenh                    -3.553e-02  2.863e-03 -12.413  < 2e-16 ***
## geo_valuenj                    -5.020e-02  3.187e-03 -15.751  < 2e-16 ***
## geo_valuenm                    -3.035e-02  2.845e-03 -10.666  < 2e-16 ***
## geo_valuenv                    -5.012e-02  3.123e-03 -16.051  < 2e-16 ***
## geo_valueny                    -3.302e-02  3.422e-03  -9.650  < 2e-16 ***
## geo_valueoh                    -2.379e-02  2.856e-03  -8.329  < 2e-16 ***
## geo_valueok                    -3.336e-02  2.834e-03 -11.772  < 2e-16 ***
## geo_valueor                    -3.690e-02  3.271e-03 -11.280  < 2e-16 ***
## geo_valuepa                    -3.076e-02  2.920e-03 -10.533  < 2e-16 ***
## geo_valueri                    -3.494e-02  2.885e-03 -12.111  < 2e-16 ***
## geo_valuesc                    -4.384e-02  3.076e-03 -14.251  < 2e-16 ***
## geo_valuesd                    -2.400e-02  2.732e-03  -8.786  < 2e-16 ***
## geo_valuetn                    -4.998e-02  2.992e-03 -16.708  < 2e-16 ***
## geo_valuetx                    -3.689e-02  3.181e-03 -11.598  < 2e-16 ***
## geo_valueut                    -2.723e-02  3.063e-03  -8.889  < 2e-16 ***
## geo_valueva                    -4.457e-02  3.346e-03 -13.319  < 2e-16 ***
## geo_valuevt                    -2.005e-03  2.815e-03  -0.712 0.476341    
## geo_valuewa                    -3.890e-02  3.094e-03 -12.570  < 2e-16 ***
## geo_valuewi                    -4.124e-02  2.924e-03 -14.104  < 2e-16 ***
## geo_valuewv                    -1.381e-02  2.995e-03  -4.611 4.06e-06 ***
## geo_valuewy                    -3.472e-03  3.235e-03  -1.073 0.283230    
## smoothed_cli                    2.426e-02  3.683e-04  65.849  < 2e-16 ***
## smoothed_adj_cli               -2.384e-02  4.126e-04 -57.777  < 2e-16 ***
## confirmed_7dav_cumulative      -3.020e-08  4.936e-09  -6.120 9.70e-10 ***
## BarRestrict1                   -9.277e-03  1.638e-03  -5.663 1.53e-08 ***
## deaths_7dav_incidence_num      -2.698e-05  7.807e-06  -3.456 0.000551 ***
## Quarantine1                    -5.450e-03  1.049e-03  -5.196 2.07e-07 ***
## SchoolClose1                   -3.061e-03  7.669e-04  -3.991 6.62e-05 ***
## StayAtHome1                     4.948e-03  1.283e-03   3.857 0.000115 ***
## confirmed_7dav_incidence_num   -1.335e-06  4.086e-07  -3.267 0.001092 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.02574 on 10700 degrees of freedom
## Multiple R-squared:  0.4469,	Adjusted R-squared:  0.4438 
## F-statistic: 144.1 on 60 and 10700 DF,  p-value: < 2.2e-16
```

#### Weekend effects

We suspect that the mobility signal is lower than usual during the weekend.


```r
factored_data$weekday <- weekdays(as.Date(factored_data$time_value)) 

p <- ggplot(factored_data, aes(x=weekday, y=full_time_work_prop)) + 
  geom_boxplot()
p
```

![](01_regression_ANOVA_files/figure-html/examine weekend effects from the data-1.png)<!-- -->

```r
# Drop the weekend
factored_data_without_wd <- factored_data%>% 
  mutate(weekday= weekdays(as.Date(time_value)))%>% 
  filter(!weekday %in% c("Saturday", "Sunday")) 
```

After we dropped the weekends in the data, we repeat the same process like we did before. By the box cox procedure, we should take the log transformation. We now have selected the following covariates to be included in the new model:

* ``confirmed_7dav_cumulative_prop``
* ``geo_value``
* ``weekday``
* ``SchoolClose``
* `` smoothed_cli``
* ``confirmed_7dav_cumulative``
* ``confirmed_7dav_incidence_num``
* ``deaths_7dav_incidence_num``
* ``PublicMask``
* ``OtherBusinessClose``
* ``StayAtHome `` 
* ``TravelRestrictExit`` 
* ``Quarantine``


```r
mod_without_end <- lm(full_time_work_prop~., data=na.omit(factored_data_without_wd[,-c(1)]))
boxcox(mod_without_end)
```

![](01_regression_ANOVA_files/figure-html/boxcox after dropping weekend-1.png)<!-- -->

```r
log.mod_without_end <- lm(log(full_time_work_prop)~., data=na.omit(factored_data_without_wd[,-c(1)]))

# plot the response
# Plot the resulting mobility signal over time

plot(factored_data$time_value, factored_data$full_time_work_prop^(1/2), main="Square-root mobility by time (with weekends)", ylab="Square-root mobility", xlab="time")
```

![](01_regression_ANOVA_files/figure-html/boxcox after dropping weekend-2.png)<!-- -->

```r
# Plot the resulting mobility signal over time
plot(factored_data_without_wd$time_value, log(factored_data_without_wd$full_time_work_prop), xlab="Time", ylab = "Full-time away home signal", main="Log mobility by time (without weekends)")
```

![](01_regression_ANOVA_files/figure-html/boxcox after dropping weekend-3.png)<!-- -->



We now fit the data without weekend with another "best model" selected by stepwise regression to see if we can get a better fit.

**Observations**

Indeed, we get a higher adj. R-squared of 0.6504, whereas the previous model has adj. R-squared of 0.4438. Interestingly, ``StayAtHome`` is now even more significant compared to previous model. Also, note that once we remove weekends, ``BarRestrict`` is no longer selected by the stepwise regression procedure. Also, now we have ``TravelRestrictExit`` in the model, but not significant at 0.05 significance level. ``Quarantine`` has also become less significant. Moreover, ``OtherBusinessClose`` has become a significant variable in explaining the variation of mobility when we remove the weekends. 



```r
# We look at the summary of the selected model
fin_mod_without_end <- lm(formula = log(full_time_work_prop) ~ confirmed_7dav_cumulative_prop + geo_value + weekday + SchoolClose + smoothed_cli + confirmed_7dav_cumulative + deaths_7dav_incidence_num + OtherBusinessClose + StayAtHome + confirmed_7dav_incidence_num + TravelRestrictExit + Quarantine, data = na.omit(factored_data_without_wd[, -c(1)]))

# Show the summary
summary(fin_mod_without_end)
```

```
## 
## Call:
## lm(formula = log(full_time_work_prop) ~ confirmed_7dav_cumulative_prop + 
##     geo_value + weekday + SchoolClose + smoothed_cli + confirmed_7dav_cumulative + 
##     deaths_7dav_incidence_num + OtherBusinessClose + StayAtHome + 
##     confirmed_7dav_incidence_num + TravelRestrictExit + Quarantine, 
##     data = na.omit(factored_data_without_wd[, -c(1)]))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -1.14821 -0.07273  0.00313  0.08411  0.49215 
## 
## Coefficients:
##                                  Estimate Std. Error  t value Pr(>|t|)    
## (Intercept)                    -3.042e+00  1.621e-02 -187.625  < 2e-16 ***
## confirmed_7dav_cumulative_prop  1.730e-04  4.059e-06   42.613  < 2e-16 ***
## geo_valueal                    -9.763e-02  1.953e-02   -5.000 5.86e-07 ***
## geo_valuear                    -2.539e-02  2.022e-02   -1.255 0.209344    
## geo_valueaz                    -2.425e-01  1.978e-02  -12.258  < 2e-16 ***
## geo_valueca                    -5.580e-02  2.096e-02   -2.662 0.007781 ** 
## geo_valueco                    -9.018e-02  1.948e-02   -4.630 3.71e-06 ***
## geo_valuect                    -2.091e-01  1.998e-02  -10.467  < 2e-16 ***
## geo_valuedc                    -2.578e-01  2.037e-02  -12.659  < 2e-16 ***
## geo_valuede                    -2.473e-01  1.910e-02  -12.948  < 2e-16 ***
## geo_valuefl                    -1.784e-01  1.849e-02   -9.651  < 2e-16 ***
## geo_valuega                    -2.071e-01  1.953e-02  -10.605  < 2e-16 ***
## geo_valuehi                    -4.653e-01  1.792e-02  -25.963  < 2e-16 ***
## geo_valueia                    -7.980e-03  1.970e-02   -0.405 0.685394    
## geo_valueid                    -1.041e-01  1.672e-02   -6.229 4.94e-10 ***
## geo_valueil                    -2.847e-01  1.966e-02  -14.484  < 2e-16 ***
## geo_valuein                    -1.508e-01  1.944e-02   -7.756 9.88e-15 ***
## geo_valueks                    -1.165e-01  1.817e-02   -6.409 1.55e-10 ***
## geo_valueky                    -5.953e-02  1.874e-02   -3.176 0.001499 ** 
## geo_valuela                    -1.872e-01  1.975e-02   -9.483  < 2e-16 ***
## geo_valuema                    -2.769e-01  1.640e-02  -16.886  < 2e-16 ***
## geo_valuemd                    -2.859e-01  2.022e-02  -14.136  < 2e-16 ***
## geo_valueme                     6.011e-02  1.770e-02    3.395 0.000689 ***
## geo_valuemi                    -1.679e-01  2.028e-02   -8.283  < 2e-16 ***
## geo_valuemn                    -2.175e-01  1.941e-02  -11.201  < 2e-16 ***
## geo_valuemo                    -6.483e-02  1.916e-02   -3.384 0.000717 ***
## geo_valuems                    -1.039e-01  1.972e-02   -5.268 1.41e-07 ***
## geo_valuemt                     9.767e-02  1.897e-02    5.148 2.70e-07 ***
## geo_valuenc                    -1.325e-01  1.943e-02   -6.819 9.84e-12 ***
## geo_valuend                    -7.270e-02  1.810e-02   -4.015 5.99e-05 ***
## geo_valuene                     9.208e-03  1.942e-02    0.474 0.635381    
## geo_valuenh                    -9.179e-02  1.941e-02   -4.730 2.29e-06 ***
## geo_valuenj                    -3.389e-01  2.015e-02  -16.822  < 2e-16 ***
## geo_valuenm                    -8.987e-02  1.794e-02   -5.008 5.62e-07 ***
## geo_valuenv                    -2.953e-01  1.951e-02  -15.138  < 2e-16 ***
## geo_valueny                    -2.113e-01  2.159e-02   -9.786  < 2e-16 ***
## geo_valueoh                    -5.615e-02  1.789e-02   -3.139 0.001704 ** 
## geo_valueok                    -6.844e-02  1.780e-02   -3.844 0.000122 ***
## geo_valueor                    -1.486e-01  2.039e-02   -7.286 3.50e-13 ***
## geo_valuepa                    -1.278e-01  1.830e-02   -6.980 3.20e-12 ***
## geo_valueri                    -2.194e-01  1.820e-02  -12.052  < 2e-16 ***
## geo_valuesc                    -1.243e-01  1.928e-02   -6.447 1.21e-10 ***
## geo_valuesd                    -1.117e-01  1.736e-02   -6.439 1.28e-10 ***
## geo_valuetn                    -1.732e-01  1.875e-02   -9.238  < 2e-16 ***
## geo_valuetx                    -1.928e-01  1.995e-02   -9.663  < 2e-16 ***
## geo_valueut                    -6.053e-03  1.921e-02   -0.315 0.752683    
## geo_valueva                    -1.921e-01  2.091e-02   -9.189  < 2e-16 ***
## geo_valuevt                     7.275e-02  1.769e-02    4.114 3.94e-05 ***
## geo_valuewa                    -2.064e-01  1.931e-02  -10.690  < 2e-16 ***
## geo_valuewi                    -1.379e-01  1.839e-02   -7.497 7.26e-14 ***
## geo_valuewv                     3.506e-02  1.877e-02    1.867 0.061898 .  
## geo_valuewy                     3.385e-02  2.035e-02    1.663 0.096265 .  
## weekdayMonday                   6.724e-02  4.970e-03   13.528  < 2e-16 ***
## weekdayThursday                 1.198e-01  4.975e-03   24.086  < 2e-16 ***
## weekdayTuesday                  1.331e-01  5.047e-03   26.369  < 2e-16 ***
## weekdayWednesday                1.099e-01  5.006e-03   21.965  < 2e-16 ***
## SchoolClose1                   -8.485e-02  4.867e-03  -17.433  < 2e-16 ***
## smoothed_cli                    1.009e-02  8.960e-04   11.256  < 2e-16 ***
## confirmed_7dav_cumulative      -2.897e-07  3.106e-08   -9.328  < 2e-16 ***
## deaths_7dav_incidence_num      -4.188e-04  4.964e-05   -8.435  < 2e-16 ***
## OtherBusinessClose1            -6.460e-02  9.860e-03   -6.552 6.05e-11 ***
## StayAtHome1                     3.903e-02  8.171e-03    4.777 1.81e-06 ***
## confirmed_7dav_incidence_num    1.029e-05  2.617e-06    3.933 8.45e-05 ***
## TravelRestrictExit1            -1.094e-01  3.426e-02   -3.193 0.001415 ** 
## Quarantine1                    -2.119e-02  6.651e-03   -3.186 0.001450 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.1384 on 7636 degrees of freedom
## Multiple R-squared:  0.6533,	Adjusted R-squared:  0.6504 
## F-statistic: 224.8 on 64 and 7636 DF,  p-value: < 2.2e-16
```

### ANOVA

**Observations**

Without dropping the weekends, from the selected additive model, we see that there are multiple covidcast signals that account for most of the variation of the mean mobility signal(squared-root) at 0.05 significance level. These are: 

* ``confirmed_7dav_cumulative_prop``
* ``smoothed_cli``
* ``smoothed_adj_cli`` 

Also, among all the interventions, ``BarRestrict`` is the most significant in explaining the variation of mobility and ``Quarantine `` comes the second. These variables account for less variation of the mean mobility signal when we compare them with the covidcast signals.

On the other hand, after we drop the weekends in the data, we notice that ``confirmed_7dav_cumulative_prop`` has become even more significant. All the covidcast signals have explained the mean mobility variation more fully than the interventions. While ``BarRestrict`` is not included anymore, we see that ``SchoolClose`` has become much more significant when we compare it between two models (with and without weekends). 



```r
# Show the summary and compare the models
anova(fin_mod) # with weeekends
```

```
## Analysis of Variance Table
## 
## Response: full_time_work_prop^(1/2)
##                                   Df Sum Sq Mean Sq  F value    Pr(>F)    
## confirmed_7dav_cumulative_prop     1 1.3177 1.31773 1988.836 < 2.2e-16 ***
## geo_value                         50 1.3597 0.02719   41.044 < 2.2e-16 ***
## smoothed_cli                       1 0.6305 0.63055  951.675 < 2.2e-16 ***
## smoothed_adj_cli                   1 2.2780 2.27803 3438.209 < 2.2e-16 ***
## confirmed_7dav_cumulative          1 0.0515 0.05153   77.774 < 2.2e-16 ***
## BarRestrict                        1 0.0304 0.03036   45.816 1.367e-11 ***
## deaths_7dav_incidence_num          1 0.0199 0.01986   29.974 4.478e-08 ***
## Quarantine                         1 0.0192 0.01923   29.017 7.327e-08 ***
## SchoolClose                        1 0.0075 0.00750   11.323 0.0007682 ***
## StayAtHome                         1 0.0071 0.00707   10.674 0.0010899 ** 
## confirmed_7dav_incidence_num       1 0.0071 0.00707   10.671 0.0010919 ** 
## Residuals                      10700 7.0894 0.00066                       
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
anova(fin_mod_without_end) # without weekends
```

```
## Analysis of Variance Table
## 
## Response: log(full_time_work_prop)
##                                  Df  Sum Sq Mean Sq  F value    Pr(>F)    
## confirmed_7dav_cumulative_prop    1 122.937 122.937 6417.066 < 2.2e-16 ***
## geo_value                        50 115.098   2.302  120.157 < 2.2e-16 ***
## weekday                           4  20.731   5.183  270.526 < 2.2e-16 ***
## SchoolClose                       1   9.595   9.595  500.824 < 2.2e-16 ***
## smoothed_cli                      1   2.655   2.655  138.606 < 2.2e-16 ***
## confirmed_7dav_cumulative         1   1.906   1.906   99.505 < 2.2e-16 ***
## deaths_7dav_incidence_num         1   0.589   0.589   30.742 3.045e-08 ***
## OtherBusinessClose                1   0.599   0.599   31.273 2.319e-08 ***
## StayAtHome                        1   0.771   0.771   40.248 2.364e-10 ***
## confirmed_7dav_incidence_num      1   0.329   0.329   17.159 3.474e-05 ***
## TravelRestrictExit                1   0.222   0.222   11.588 0.0006674 ***
## Quarantine                        1   0.194   0.194   10.148 0.0014503 ** 
## Residuals                      7636 146.289   0.019                       
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

## Conclusion

In conclusion, at 0.05 significance level, ``confirmed_7dav_cumulative_prop`` is highly significant in explaining the variation of mobility in both with or without weekends data. There are some differences in the results when we drop the weekends in the data. When we drop the weekends, ``SchoolClose`` is very sigificant and ``BarRestrict`` is not even included in the model. ``TravelRestrictExit`` and ``OtherBusinessClose`` have also appeared to be siginifcant. 

However, when we do not drop the weekends, ``BarRestrict`` has become the most significant intervention. ``smoothed_adj_cli `` is no longer selected to be in the model. Overall, ``confirmed_7dav_cumulative_prop``, ``confirmed_7dav_cumulative``,  ``deaths_7dav_incidence_num``,  ``smoothed_cli``, ``Quarantine``, ``StayAthome``, ``SchoolClose `` have consistently appear significantly in both models. 

