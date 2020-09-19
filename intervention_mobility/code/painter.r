# A function to compute the corrlation by various number of shifts
getCorrByShift <- function(num_shift, signal_1, signal_2, corr_method, typename){
  dt_vec <- 0:num_shift
  df_list <- vector("list", length(dt_vec))
  for (i in 1:length(dt_vec)) {
    df_list[[i]] <- covidcast_cor(signal_1, signal_2, dt_x = dt_vec[i],by = "geo_value", method= corr_method)
    df_list[[i]]$dt <- dt_vec[i]
  }
  df <- do.call(rbind, df_list)
  return(df)
}

# A function to derive median of the signal from each shift 
getMedian <-function(df){
  df_med <- df %>%
    group_by(dt) %>%
    summarize(median = median(value, na.rm = TRUE), .groups = "drop_last")
  return(df_med)
}
