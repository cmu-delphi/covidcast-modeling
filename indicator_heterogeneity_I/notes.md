# Notes

## Background
* We have previously seen spatial heterogeneity in the DV signal.  Aaron 
  demonstrated that we could apply a simple fix to correct for this.
  For each location, we regress the case rate on DV and take the fitted
  values as the "DV sensor of case rate.  Generally, this regression
  is refitted at each time t, using data from times t' < t.
* However, in our current setting, the data at times t' < t may not be 
  fully available; they may also be subject to backfill:
  * Therefore, we may want to consider data at times t' \leq t-k, where 
    k is a sufficiently long period in time (e.g., k=7); or, even better,
    we want to use the imperfect data that was available at time 
    t (i.e., before it was backfill corrected).  To do this, we would
    rely on the **issue_date** feature of the COVIDcast API.

## Goals
* Repeat and extend the analysis that Aaron has performed, from DV to 
  all other sensors, including Facebook %CLI; Facebook %CLI-in-community; 
  Hospital Admissions; Google Health Trends; Google Symptoms
  * TODO: different ways we can approach this, e.g., starting k days back, 
    issue date, etc...
  * TODO: how many days back should be the max?  Ryan suggests 2-3 weeks 
    back.
  * Food for thought: Limiting the max days back makes sense if we 
    believe that there is both spatial and temporal heterogeneity.  
    If we believe that there is only spatial heterogeneity, then there
    is no reason to limit the number of days back.
* Correlation between sensorized and case rate versus original and case 
  rate
* Variability of the fitted coefficients over time, for each location.  
  Examining this variability will allow us to understand whether there is
  temporal heterogeneity.
* Forecasting ability of the original indicators versus the "corrected" / 
  "sensorized" indicators.  Runs parallel to comparing the correlations; 
  we simply want to understand whether sensorization allows us to better 
  correct for heterogeneity.
