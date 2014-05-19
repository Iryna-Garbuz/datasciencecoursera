corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  
  source(file='complete.R')
  
  complete.df <- complete(directory)
  id.greater.threshold <- complete.df$id[complete.df$nobs > threshold]
  
  output <- vector(mode='numeric')
  
  for (i in id.greater.threshold) {
    data <- getmonitor(id=i, directory=directory)
    
    cor.one.monitor <- cor(x=data$sulfate, y=data$nitrate, use="complete.obs")
    
    output <- c(output, cor.one.monitor)
  }
  
  output
  
}