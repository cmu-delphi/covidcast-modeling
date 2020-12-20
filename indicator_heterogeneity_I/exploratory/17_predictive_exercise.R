# Stolen from
#https://delphi.cmu.edu/blog/2020/09/21/can-symptoms-surveys-improve-covid-19-forecasts/
library(covidcast)
library(dplyr)
library(tidyr)

#### Functions #####

# Function to append shift values (lags or leads) to data frame
append_shifts = function(df, shifts) {
  # Make sure that we have a complete record of dates for each geo_value (fill
  # with NAs as necessary)
  df_all = df %>% group_by(geo_value) %>%
    summarize(time_value = seq.Date(as.Date(min(time_value)),
                                    as.Date(max(time_value)),
                                    by = "day")) %>% ungroup()
  df = full_join(df, df_all, by = c("geo_value", "time_value"))
  
  # Group by geo value, sort rows by increasing time
  df = df %>% group_by(geo_value) %>% arrange(time_value) 
  
  # Load over shifts, and add lag value or lead value
  for (shift in shifts) {
    fun = ifelse(shift < 0, dplyr::lag, dplyr::lead)
    varname = sprintf("value%+d", shift)
    df = mutate(df, !!varname := fun(value, n = abs(shift)))
  }
  
  # Ungroup and return
  return(ungroup(df))
}

# Some useful functions for transformations
Log = function(x, a = 0.01) log(x + a)
Exp = function(y, a = 0.01) exp(y) - a
Logit = function(x, a = 0.01) log((x + a) / (1 - x + a))
Sigmd = function(y, a = 0.01) (exp(y) * (1 + a) - a) / (1 + exp(y))
Id = function(x) x

#### Parameters #####

# Transforms to consider, in what follows
trans = Logit
inv_trans = Sigmd

# Rescale factors for our signals: bring them all down to proportions (between
# 0 and 1)

rescale_percent = 1e-2 # Originally a percentage
  # used for dv, fb
rescale_incidence = 1e-5 # Originally a count per 100,000 people
  # used for cases, hosp

n = 14 # Number of trailing days to use for training set
lp_solver = "gurobi" # LP solver to use in quantile_lasso()
verbose = TRUE # Print intermediate progress to console?

#### Data #####

source_names = c("doctor-visits", "fb-survey", "fb-survey",
                 "hospital-admissions", "hospital-admissions")
signal_names = c("smoothed_adj_cli", "smoothed_cli", "smoothed_hh_cmnty_cli", 
            "smoothed_adj_covid19_from_claims", "smoothed_adj_covid19_from_claims")
pretty_names = c("Doctor visits", "Facebook CLI", "Facebook CLI-in-community", 
          "Hospitalizations", "Hospitalizations")
target_names = c("Cases", "Cases", "Cases", "Cases", "Deaths")
rescale_ind = c(rescale_percent,
                rescale_percent,
                rescale_percent,
                rescale_percent,
                rescale_percent)
geo_level = 'county'

cache_fname = sprintf('cached_data/12_heterogeneity_core_indicators_%s.RDS',
                      geo_level)

cached_data = readRDS(cache_fname)
df_signals = cached_data[[1]]
df_cases = cached_data[[2]]
df_deaths = cached_data[[3]]

case_num = 500

if (geo_level == 'county') {
  geo_values = suppressWarnings(covidcast_signal("usa-facts", "confirmed_cumulative_num",
                                '2020-11-01', 
                                '2020-11-01')) %>%
    filter(value >= case_num) %>% pull(geo_value)
} else if (geo_level == 'state') {
  geo_values = unique(df_signals[[1]]$geo_value)
}

# Read in dataframe of sensorized values
sensorize_time_ranges = list(
      c(-42, -8),
      c(-49, -8),
      c(-56, -8),
      c(-63, -8),
      c(-70, -8)
)
splot_idx = 5



for (ind_idx in 1:length(source_names)) {
  print(pretty_names[ind_idx])
  predictive_fname = sprintf('results/17_predictive_%s_%s_%s_%s.RDS', geo_level,
                               source_names[ind_idx], signal_names[ind_idx],
                               target_names[ind_idx])
  if (file.exists(predictive_fname)) next
  if (target_names[ind_idx] == 'Cases') {
    df_target = df_cases
  } else if (target_names[ind_idx] == 'Deaths') {
    df_target = df_deaths
  } else {
    stop(sprintf("No matching dataframe for target %s.", target_names[ind_idx]))
  }

  ind_df = tibble(df_signals[[ind_idx]]) %>% filter(geo_value %in% geo_values)
  ind_target = inner_join(ind_df, tibble(df_target),
                          by=c('geo_value', 'time_value')) %>% select (
        geo_value=geo_value,
        time_value=time_value,
        indicator_value=value.x,
        target_value=value.y,
      )
  ind_global_sensorized =  ind_target %>% group_by (
        geo_value,
      ) %>% group_modify ( ~ {
        fit = lm(target_value ~ indicator_value, data =.x);
        tibble(time_value=.x$time_value,
               indicator_value=.x$indicator_value,
               target_value=.x$target_value,
               sensorized_value=fit$fitted.values)
      }) %>% ungroup


  sensorize_val_fname = sprintf('results/12_sensorize_vals_%s_%s_%s_%s.RDS',
                            geo_level,
                            source_names[ind_idx], signal_names[ind_idx],
                            target_names[ind_idx])
  ind_target_sensorized_list = readRDS(sensorize_val_fname)

  ind_target_sensorized = ind_target_sensorized_list[[splot_idx]]


  # TODO: replace these with the sensorized 
  # Fetch county-level Google and Facebook % CLI-in-community signals, and JHU
  # confirmed case incidence proportion
  start_day = "2020-04-11"
  end_day = "2020-11-21"

  # TODO: add raw here
  raw = ind_df %>% select (
      geo_value,
      time_value,
      value,
    )
  static = ind_global_sensorized %>% select (
      geo_value,
      time_value,
      value=sensorized_value,
    )
  dynamic = ind_target_sensorized %>% select (
      geo_value,
      time_value,
      value=sensorized_value,
    )
  target_temp = df_target %>% filter(
       geo_value %in% geo_values
     ) %>% select(
       geo_value, time_value, value
    )

  # Find "complete" counties, present in all three data signals at all times 
  geo_values_complete = intersect(intersect(intersect(raw$geo_value, static$geo_value),
                                  dynamic$geo_value),
                                  target_temp$geo_value)

  # Filter to complete counties, transform the signals, append 1-2 week lags to 
  # all three, and also 1-2 week leads to case rates
  lags = 1:2 * -7 
  leads = 1:2 * 7

  # TODO: add raw here
  raw = raw %>% filter(geo_value %in% geo_values_complete) %>% 
    mutate(value = trans(value * rescale_ind[ind_idx])) %>% 
    append_shifts(shifts = lags) 
  static = static %>% filter(geo_value %in% geo_values_complete) %>% 
    mutate(value = trans(value * rescale_incidence)) %>% 
    append_shifts(shifts = lags) 
  dynamic = dynamic %>% filter(geo_value %in% geo_values_complete) %>% 
    mutate(value = trans(value * rescale_incidence)) %>% 
    append_shifts(shifts = lags) 
  target_temp = target_temp %>% filter(geo_value %in% geo_values_complete) %>% 
    mutate(value = trans(value * rescale_incidence)) %>% 
    append_shifts(shifts = c(lags, leads))

  # Rename columns
  colnames(raw) = sub("^value", "raw", colnames(raw))
  colnames(static) = sub("^value", "static", colnames(static))
  colnames(dynamic) = sub("^value", "dynamic", colnames(dynamic))
  colnames(target_temp) = sub("^value", "target", colnames(target_temp))

  # Make one big matrix by joining these three data frames
  # TODO: Do I need data to be available in all three simultaneously?
  z = full_join(full_join(full_join(raw, static, by = c("geo_value", "time_value")),
                dynamic, by = c("geo_value", "time_value")),
                target_temp, by = c("geo_value", "time_value"))

  # TODO!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! make sure this is correct

  ##### Analysis #####

  # Use quantgen for LAD regression (this package supports quantile regression and
  # more; you can find it on GitHub here: https://github.com/ryantibs/quantgen)
  library(quantgen) 

  res_list = vector("list", length = length(leads))

  # Loop over lead, forecast dates, build models and record errors (warning: this
  # computation takes a while)
  for (i in 1:length(leads)) { 
    lead = leads[i]; if (verbose) cat("***", lead, "***\n")
    
    # Create a data frame to store our forecast results. Code below populates its
    # rows in a way that breaks from typical dplyr operations, done for efficiency 
    res_list[[i]] = z %>% 
      filter(between(time_value, as.Date(start_day) - min(lags) + lead, 
                     as.Date(end_day) - lead)) %>%
      select(geo_value, time_value) %>%
      mutate(err0 = as.double(NA), err1 = as.double(NA), err2 = as.double(NA), 
             err3 = as.double(NA), err4 = as.double(NA), lead = lead) 
    valid_dates = unique(res_list[[i]]$time_value)
    
    for (k in 1:length(valid_dates)) {
      date = valid_dates[k]; if (verbose) cat(format(date), "... ")
      
      # Filter down to training set and test set
      z_tr = z %>% filter(between(time_value, date - lead - n, date - lead))
      z_te = z %>% filter(time_value == date)
      inds = which(res_list[[i]]$time_value == date)
      
      # Create training and test responses
      y_tr = z_tr %>% pull(paste0("target+", lead))
      y_te = z_te %>% pull(paste0("target+", lead))
      
      # Strawman model
      if (verbose) cat("0")
      y_hat = z_te %>% pull(target)
      res_list[[i]][inds,]$err0 = abs(inv_trans(y_hat) - inv_trans(y_te))
      
      # Cases only model
      if (verbose) cat("1")
      x_tr_target = z_tr %>% select(starts_with("target") & !contains("+"))
      x_te_target = z_te %>% select(starts_with("target") & !contains("+"))
      x_tr = x_tr_target; x_te = x_te_target # For symmetry wrt what follows 
      ok = complete.cases(x_tr, y_tr)
      if (sum(ok) > 0) {
        obj = quantile_lasso(as.matrix(x_tr[ok,]), y_tr[ok], tau = 0.5,
                             lambda = 0, lp_solver = lp_solver)
        y_hat = as.numeric(predict(obj, newx = as.matrix(x_te)))
        res_list[[i]][inds,]$err1 = abs(inv_trans(y_hat) - inv_trans(y_te))
      }
      
      # Cases and Raw
      if (verbose) cat("2")
      x_tr_raw = z_tr %>% select(starts_with("raw"))
      x_te_raw = z_te %>% select(starts_with("raw"))
      x_tr = x_tr_raw; x_te = x_te_raw # For symmetry wrt what follows 
      ok = complete.cases(x_tr, y_tr)
      if (sum(ok) > 0) {
        obj = quantile_lasso(as.matrix(x_tr[ok,]), y_tr[ok], tau = 0.5,
                             lambda = 0, lp_solver = lp_solver)
        y_hat = as.numeric(predict(obj, newx = as.matrix(x_te)))
        res_list[[i]][inds,]$err2 = abs(inv_trans(y_hat) - inv_trans(y_te))
      }

      # Cases and Static
      if (verbose) cat("3")
      x_tr_static = z_tr %>% select(starts_with("static"))
      x_te_static = z_te %>% select(starts_with("static"))
      x_tr = x_tr_static; x_te = x_te_static # For symmetry wrt what follows 
      ok = complete.cases(x_tr, y_tr)
      if (sum(ok) > 0) {
        obj = quantile_lasso(as.matrix(x_tr[ok,]), y_tr[ok], tau = 0.5,
                             lambda = 0, lp_solver = lp_solver)
        y_hat = as.numeric(predict(obj, newx = as.matrix(x_te)))
        res_list[[i]][inds,]$err3 = abs(inv_trans(y_hat) - inv_trans(y_te))
      }
      
      # Cases and Dynamic
      if (verbose) cat("4\n")
      x_tr_dynamic = z_tr %>% select(starts_with("dynamic"))
      x_te_dynamic = z_te %>% select(starts_with("dynamic"))
      x_tr = x_tr_dynamic; x_te = x_te_dynamic # For symmetry wrt what follows 
      ok = complete.cases(x_tr, y_tr)
      if (sum(ok) > 0) {
        obj = quantile_lasso(as.matrix(x_tr[ok,]), y_tr[ok], tau = 0.5,
                             lambda = 0, lp_solver = lp_solver)
        y_hat = as.numeric(predict(obj, newx = as.matrix(x_te)))
        res_list[[i]][inds,]$err4 = abs(inv_trans(y_hat) - inv_trans(y_te))
      }
    }
  }

  # Bind results over different leads into one big data frame, and save 
  res = do.call(rbind, res_list)
  saveRDS(res, predictive_fname)
}




