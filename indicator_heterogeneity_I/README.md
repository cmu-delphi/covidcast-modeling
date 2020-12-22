# Indicator heterogeneity I

## Description

Our indicators may have changing relationships with corresponding targets of interest (e.g., COVID cases, COVID deaths), as time passes.  This is called nonstationarity.  Our indicators may even be on different scales, or have different relationships with targets of interest, as we look at this relationship across different locations.  I'll refer to this general problem as heterogeneity (both spatial and temporal).

We already saw that the doctor visits signal suffers rather badly from spatial heterogeneity (see [here](https://delphi-org.slack.com/archives/C011EU72MU2/p1599862022069500) and [here](https://delphi-org.slack.com/archives/C011EU72MU2/p1600127333081600), and ask Aaron).  Here Aaron showed that a simple fix can rescue us: at each time, rescale the signal using the coefficient from fitting a simple linear regression of case rates on doctors visits, over the last few weeks.  This is a process we call sensorization and we've been using it in flu in our sensor fusion systems, since [David's seminal thesis work](https://delphi.cmu.edu/~dfarrow/thesis.pdf) (see Chapter 4; see also [Maria's awesome recent paper](https://papers.nips.cc/paper/9475-kalman-filter-sensor-fusion-and-constrained-regression-equivalences-and-insights.pdf) on sensor fusion). Sensorization may not always be the answer: if there's a deterministic reason why the signals are on different scales in different locations, or have shifted over time, then we probably want to manually correct this (rather than trying to learn a relationship by linear regression).

Generally, we need to systematically investigate heterogeneity for all of our core indicators.  This is important for two reasons:

1. It could be important for guiding modeling decisions (e.g., if an indicator has different behavior in different locations, then we can't fit a global forecasting model).
2. It could lead to us showing things differently on the COVIDcast map (e.g., if a signal is truly on different scales in different locations, then we shouldn't show an "unadjusted" map, or at least, issue a warning and offer an "adjusted" option).

## Deliverables

1. Analysis should cover at least our "core" indicators: 
    a. Facebook % CLI, Facebook % CLI-in-community, Doctor Visits, Hospital Admissions, and Google Symptoms.  For the last one, should coordinate with Nat who is doing the Google Symptoms DAP.  (This DAP should itself produce a GS sensor, that is, the GS indicator will be defined using sensorization.  So the current DAP can just analyze it in tandem with the rest of the sensors you create here).
    b. Target for each signal should be new COVID case rates, except for Hospital Admissions, where the target should be new COVID death rates.  Note: the use of rates rather than counts here is extremely important (otherwise we'd need to put in population as a feature in the sensorization model).
2. Analysis should be done at the county level whenever possible, otherwise at the metro level.  (Some pre-screening of counties or metro levels may need to be done in order to get rid of counties with really low COVID case counts).
3. Analysis should investigate the variability of simple linear regression coefficients (intercept and slope, but slope is primarily of interest) of a given target on a given indicator, across locations and time.  
    a. For training use the last k weeks, where k is small (e.g., k = 3 or 4).  Sensitivity analysis around the choice of k would be a good idea.
    b. Note: you need to be careful how to measure "variability".  The signals are all on different scales, hence their coefficients will be.  We need a global way of judging whether a signal is more or less variable across space and time.	
4. Analysis should determine whether sensorization---rescaling an indicator based on linear regression over the recent past---helps correlations, or improves simple forecasting models.  You can use the [correlations notebook](https://cmu-delphi.github.io/covidcast/R-notebooks/signal_correlations.html) and the [forecasting demo](https://delphi.cmu.edu/blog/2020/09/21/can-symptoms-surveys-improve-covid-19-forecasts/) as a head start in terms of the code provided there.  Be sure to do this out-of-sample, i.e., the linear regression should only use target information available at the current time (strictly in the past, and even better, done on properly unstable data "as of" the current date).
5. Analysis should also consider whether there are deterministic scale/shifts that can be learned from domain-specific investigation.  Off the top of my head I don't know what this would be, but I wanted to leave it open that it could exist.

