rankall <- function(outcome, num = "best") {
  
  ## Read outcome data
  data <- read.csv(file='outcome-of-care-measures.csv', colClasses = 'character')
  
  ## Check that state and outcome are valid
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
  
  unique.states <- sort(unique(data$State))
  ##print(unique.states)
  
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  
  result <- list()
  
  for (state in unique.states)
  {
    data.state <- data[data$State == state,]
    data.state[,i] <- as.numeric(data.state[ ,i])
    
    data.state2 <- list()
    data.state2 <- which (!is.na(data.state[,i]))
  
    data.state <- data.state[order(data.state[,i], data.state$Hospital.Name), ]
    ## data.state <- data.state[complete.cases(data.state),]
    ##print(data.state)
    
    if(num == "best") num = 1
    
  ##  else if (num == "worst") num = which.max((data.state[,i]))
  
  else if (num == "worst") 
    {
    num = length(data.state2)
    }
  
    else if(is.numeric(x = num)) {
      
    if (num < 1 || num > nrow(data.state)) {
      result <- rbind(result, list(NA, state))
     ## print(state)
      next
    }
      
    }
    else stop ('invalid num')
  
  hospital.name <- data.state[num, ]$Hospital.Name
  
  result <- rbind(result, list(sort(hospital.name, decreasing= FALSE), state))

  }
  
  result <- as.data.frame(x=result)
  colnames(x=result) <- c('hospital', 'state')
  
  result
}
