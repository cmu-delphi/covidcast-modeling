
# Mobility, Government Intervention, and Potential Confounders

## Goals and Research Questions

Mobility is one of the potential factors in contributing to the spread of COVID-19. Our goal is to estimate the effect of state-wide policies on mobility signals at a county-level in this work to answer the following questions: 

  * Do mobility signals decrease more because of the governemnt interventions or public reaction to case counts?

  * Once we can measure the effects of governemnt interventions on mobility, can we rank the effectiveness of the interventions at a county level?

  * Can we characterize different counties based on the known effects of the governemnt interventions on mobility?

  * Do mandatory policies seem to be more effective than recommended policies in reducing mobility?


## Deliverables

* Lag analysis for case count signals, doctor visits on mobility across states [See 00_initial_exploration.md](reports/00_initial_exploration.md)

* Mobility signals across states under different interventions [See 00_initial_exploration.md](reports/00_initial_exploration.md)

* ANOVA and multiple comparisons [See 01_regression_ANOVA.md](reports/01_regression_ANOVA.md)

  * Analyze the difference in mean mobility among interventions

* Regression discontinuity design to measure the effects of interventions on mobility. [See 02_estimating_effect_of_intervention_via_RDD.md](reports/02_estimating_effect_of_intervention_via_RDD.md)
  

## File Directory Description

* `/data/`: this folder stores all data files.

* `/code/`: this folder contains all the codes to generate the files in `/reports/` and `/html/` folders.

* `/historic/`: this folder contains historic files, codes for implementing different ideas, but not officially used. 

* `/html/`: this folder contains the main reports in HTML produced by R Markdown.

* `/reports/`: this folder contains the main reports in Markdown format.

## Data sources

### Mobility 

  * [Delphi's COVIDcast](https://cmu-delphi.github.io/delphi-epidata/api/covidcast_signals.html)

### Interventions 

  * [State-level social distancing policies in response to COVID-19](https://github.com/COVID19StatePolicy/SocialDistancing)

    * [Data attributes at a glance](https://github.com/COVID19StatePolicy/SocialDistancing/tree/master/codebooks)

### Potential Confounders

  * [Delphi's COVIDcast](https://cmu-delphi.github.io/delphi-epidata/api/covidcast_signals.html)

### Other potential interventions data sources

* Country-level:

  * [Oxford COVID19 policy tracker](https://github.com/OxCGRT/covid-policy-tracker)
  
  * [ACAPS COVID-19: Government Measures Dataset](https://data.humdata.org/dataset/acaps-covid19-government-measures-dataset)
  
* State-level:
  
  * [US_interventions_from_Wikipedia](https://docs.google.com/spreadsheets/d/1k1ENKntZILmXGOTvjjoJMAEb3WQOYqp_pHJHzpZojGo/edit#gid=0)
  
  * [State's Phased Re-Opening Plans by Start Date (Governing)](https://www.governing.com/now/Reopening-the-Economy-Under-COVID-19-States-Plot-a-Way-Back.html)

* County-level:

  * [Coronavirus City and County Non-Pharmaceutical intervention roll date dataset (Keystone)](https://www.keystonestrategy.com/coronavirus-covid19-intervention-dataset-model/) 
  
  * [Help Predict COVID-19’s Spread in Your Community (Stanford)](https://socialdistancing.stanford.edu/)
  
  * [Standford Crowdsourced COVID-19 intervention data](https://docs.google.com/spreadsheets/d/133Lry-k80-BfdPXhlS0VHsLEUQh5_UutqAt7czZd7ek/edit#gid=0)
  
  * [US County-level COVID19 summaries](https://github.com/JieYingWu/COVID-19_US_County-level_Summaries/tree/master/raw_data/national)

### Reference

1. [Regression discontinuity designs: A guide to practice](https://www.sciencedirect.com/science/article/pii/S0304407607001091)

2. [Inferring causal impact using Bayesian structural time-series models](https://research.google/pubs/pub41854/)


