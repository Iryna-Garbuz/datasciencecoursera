complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  
  source(file='getmonitor.R')
  output <- data.frame()
  
  for ( i in id) {
    # 
    oneMonitorData <- getmonitor(id=i, directory=directory)
    # print(head(oneMonitorData))
    
    one.nobs <- nrow(oneMonitorData[complete.cases(oneMonitorData),])
    
    id.num <- as.integer(x=i)
    one.row <- c(id.num, one.nobs)
    
    # print(one.row)
    
    output <- rbind(output, one.row)
  }
  # print(one.row)
  # print(as.data.frame(one.row))
  
  colnames(output) <- c('id', 'nobs')
  output
}