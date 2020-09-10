## Plans
- Overview: Old GHT, new Google Symptoms.  Go through previous notebook (copy
  that notebook into this directory)
- Demo correlations notebook: very crude time analysis where we aggregate over 
  all counties and examine the correlation with respect to time
- Open problems; each one can be its own notebook:
    - Amount of missingness by county, symptom.  Should we just discard some 
      of the symptoms because it's available so rarely?  Many of the symptoms
      seem _a priori_ totally unrelated to COVID-19, e.g., excessive hoarding,
      should we toss those out to make our lives easier?
    - Overwhelming number of symptoms reported: 422.  How to efficiently 
      perform a correlations analysis for all of them at once?  (We would
      want to examine over time; across counties; at various time lags
      against cases)
    - Correlations analysis can give us a sense of what symptoms to include 
      in a new indicator; then we would need to weigh them appropriately 
      into the new indicator.  Unexpected correlations (that are not
      spurious) may provide new insight.
- My view on two approaches to a new indicator (we could do both, assuming 
  we have the people):
  - "Scientifically guided approach": consult medical / public health 
    literature & experts to obtain a subset of symptoms, weigh them
    appropriately, this is the new indicator.  Google considered such an
    approach, but decided to defer construction to users because of
    lack of uniform advice across different agencies with which they
    consulted.  PRO: May give us signal beyond (often unreliable) case
    reporting.
  - "Brute force approach": set up supervised problem(s) against cases 
    (at various lags), through in all of the symptoms, see what allows us
    to best mimic cases.  PRO: Should be well-correlated with cases; if
    trained against appropriately backfilled cases, it can give a "nowcast"
    of cases.

