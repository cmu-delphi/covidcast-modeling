
# Mobility, Government Intervention, and Potential Confounders

## Goals and Research Questions

Mobility trends: Historically, did mobility patterns change as a result of government interventions? (Appears on mission statement)

## Deliverables

* Plot the SafeGraph mobility signals across time in a few locations that have had various restrictions. The COVIDcast map seems to suggest these signals don’t vary much; do we see anything interesting in the plots?

  * Lag analysis for case count signals, doctor visits on mobility across states

  * Mobility signals across states under different interventions

* ANOVA and multiple comparisons

  * Analyze the difference in mean mobility among interventions

* Potential causal models to compare the causal effects of interventions with other confounding variables on mobility.
  
  * Regression discontinuity design [See Reference #1](#reference)

  * Bayesian time-series structural model named [Causallmpact](http://google.github.io/CausalImpact/CausalImpact.html) from Google (synthetic control method) [See Reference #2](#reference)

* Blog post coupled with R Shiny interactive plots

## File Directory Description

* `/data/`: this folder stores all data files.

* `/code/`: this folder contains all the codes.

* `/notebooks/`: this folder contains the main report in HTML produced by R Markdown.

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


