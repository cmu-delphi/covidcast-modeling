# A function to compute the corrlation by various number of shifts
getCorrByShift <- function(num_shift, signal_1, signal_2, corr_method, by){
  # num_shift: specify how many days to forward signal_1
  # signal_1: the first signal
  # signal_2: the second signal
  # corr_method: correlation method
  # by: select either time_value or geo_value
  dt_vec <- 0:num_shift
  df_list <- vector("list", length(dt_vec))
  for (i in 1:length(dt_vec)) {
    df_list[[i]] <- covidcast_cor(signal_1, signal_2, dt_x = dt_vec[i], by = by, method= corr_method)
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

getMedianCorr <- function(shiftday, covidcast.like_signal.1, covidcast.like_signal.2, corr.method, name, by){
  # shiftday: specify the number of days you want to shift
  # covidcast.like_signal.1: a dataframe that has covidcast package ouput dataframe 
  # covidcast.like_signal.2: another dataframe that has covidcast package ouput dataframe 
  # corr.method: specify a correlation method
  # name: specify the name of the signal
  
  df <- getCorrByShift(shiftday, covidcast.like_signal.1, covidcast.like_signal.2, corr.method, by)
  med <- getMedian(df)
  med$Comparison <- name
  return(med)
}

plot.all.Corr.Median.by.shift <- function(other_signals, main_signal, shiftday, names, corr.method, title, by_method){
  # Compute correlation between other covidcast-like signals and mobility
  df.ls = list()
  for (i in 1:length(other_signals)){
    med.df <- getMedianCorr(shiftday, other_signals[[i]], main_signal, corr.method, names[[i]], by_method)
    df.ls[[i]] <- med.df
  }
  
  # Stack two dataframe row-wise
  df_all_signals <- bind_rows(df.ls)
  
  # plot the graph 
  ggplot(df_all_signals, aes(x = dt, y = median)) + geom_line(aes(color = Comparison)) + geom_point(aes(color = Comparison)) +
    labs(title = title,
         x = "Shift", y = "Correlation") +
    theme(legend.title = element_blank())
}


# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
