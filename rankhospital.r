rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  data <- read.csv(file='outcome-of-care-measures.csv', colClasses = 'character')
  
  ## Check that state and outcome are valid
  if(!any(state == data$State)) {
    stop('invalid state')
  }
  
  if(outcome == 'heart attack') {
    i <- 11
  }
  else if(outcome == 'heart failure') {
    i <- 17
  }
  else if(outcome == 'pneumonia') {
    i <- 23
  }
  else {
    stop('invalid outcome')
  }
  
  
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  
  data.state <- data [data$State == state, ]
  
  data.state[,i] <- as.numeric(data.state[ ,i])

  suppressWarnings(data.state <- data.state[complete.cases(data.state),])
  
  if(num == "best")  num = 1
  
  else if (num == "worst") num = nrow(data.state)  
  
  else if(is.numeric(x=num)) {
    
    if (num < 1 || num > nrow(data.state)) return (NA)
    
  }
  else stop ('invalid num')

  
  ##num 
  data.state <- data.state[order(data.state[,i], data.state$Hospital.Name), ]
  
  suppressWarnings(hospital.name <- data.state [num, ]$Hospital.Name)
  
  sort(hospital.name, decreasing= FALSE)
 
  ##sort5.hsb2 <- hsb2[order(hsb2$science, na.last=FALSE) , ]
}
