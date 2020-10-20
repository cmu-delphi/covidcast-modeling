shiftDays <- function(filtered.df, original.df, shifted.days, columns.vec){
  # Check how many states
  if (length(unique(original.df$geo_value)) > 1){
    if(length(columns.vec)==1){
      # Get the data starting from the speificed date for CASE COUNT
      confounders.shifted.by.day <- filtered.df[1: (nrow(filtered.df)-shifted.days), columns.vec]
      # Get the number of rows 
      num_row <- length(original.df[original.df$geo_value == state,columns.vec])
      # replace all values by NA first
      original.df[original.df$geo_value == state,columns.vec] <- NA
      # Starting from the shifted time, we fill in the data
      original.df[original.df$geo_value == state,columns.vec][(shifted.days+1):num_row] <- confounders.shifted.by.day
    }else{
      # Get the data starting from the speificed date for CASE COUNT
      confounders.shifted.by.day <- filtered.df[1: (nrow(filtered.df)-shifted.days), columns.vec]
      # Get the number of rows 
      num_row <- nrow(original.df[original.df$geo_value == state,columns.vec])
      # replace all values by NA first
      original.df[original.df$geo_value == state,columns.vec] <- NA
      # Starting from the shifted time, we fill in the data
      original.df[original.df$geo_value == state,columns.vec][(shifted.days+1):num_row,] <- confounders.shifted.by.day[,columns.vec]
    }
  }else{
    if(length(columns.vec)==1){
      # Get the data starting from the speificed date for CASE COUNT
      confounders.shifted.by.day <- filtered.df[1: (nrow(filtered.df)-shifted.days), columns.vec]
      # Get the number of rows 
      num_row <- length(original.df[,columns.vec])
      # replace all values by NA first
      original.df[,columns.vec] <- NA
      # Starting from the shifted time, we fill in the data
      original.df[,columns.vec][(shifted.days+1):num_row] <- confounders.shifted.by.day
    }else{
      # Get the data starting from the speificed date for CASE COUNT
      confounders.shifted.by.day <- filtered.df[1: (nrow(filtered.df)-shifted.days), columns.vec]
      # Get the number of rows 
      num_row <- nrow(original.df[,columns.vec])
      # replace all values by NA first
      original.df[,columns.vec] <- NA
      # Starting from the shifted time, we fill in the data
      original.df[,columns.vec][(shifted.days+1):num_row,] <- confounders.shifted.by.day[,columns.vec]
    }
  }
  return(original.df)
}

