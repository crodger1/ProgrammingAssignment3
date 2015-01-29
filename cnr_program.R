# setwd("~/Coursera/ProgrammingAssignment3")
# 
# outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
# head(outcome)
# 
# outcome[, 11] <- as.numeric(outcome[, 11])
# hist(outcome[, 11])


best <- function(state, outcome) {
  ## Read outcome data
  outcome.df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  # states<-c("AL","AK","AZ","AR","CA","CO","CT","DE","DC","FL","GA","HI","ID","IL","IN","IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA","PR","RI","SC","SD","TN","TX","UT","VT","VI","VA","WA","WV","WI","WY","GU")
  states<-unique(outcome.df$State)
  
  outcomes<-c("heart attack","heart failure","pneumonia")
  
  if(!(state %in% states)) {
    stop("invalid state")
  }
  
  if(!(outcome %in% outcomes)) {
    stop("invalid outcome")
  }

  outcome

  
  
  ## take a subset of the relevant data
  outcome_state <- outcome.df[outcome.df$State %in% state,c(2,7,11,17,23)]
  
  
  if(outcome=="heart attack") {
    ndx<-order(outcome_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,outcome_state$Hospital.Name, na.last=TRUE)
    subset<-outcome_state[ndx,]
    x<-subset$Hospital.Name[1]
  }
  
  else if(outcome=="heart failure") {
    subset<-outcome_state[order(outcome_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,outcome_state$Hospital.Name, na.last=TRUE),]
    x<-subset$Hospital.Name[1]  
  }
  
  else if(outcome=="pneumonia") {
    ndx<-order(outcome_state$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,outcome_state$Hospital.Name, na.last=TRUE)
    subset<-outcome_state[ndx,]
    x<-subset$Hospital.Name[1]  
  }
  
  x
  
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  
  #columns 11, 17, 23
  
  # sort(x, na.last=TRUE)
  
}



