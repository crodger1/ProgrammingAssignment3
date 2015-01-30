

rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    outcome.df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")


    ## Check that state and outcome are valid
    states<-unique(outcome.df$State)
    outcomes<-c("heart attack","heart failure","pneumonia")

    if(!(state %in% states)) {
      stop("invalid state")
    }

    if(!(outcome %in% outcomes)) {
      stop("invalid outcome")
    }


    if(num=="best") {
        num<-1
    }


    # if(!(num %in% c("best","worst",1:nobs(Hospital.Name)))) {
    #     stop("")
    # }

    ## Return hospital name in that state with the given rank

    ## take a subset of the relevant data
    outcome_state <- outcome.df[outcome.df$State %in% state,c(2,7,11,17,23)]
    outcome_state[-c(1,2)] <- lapply(outcome_state[-c(1,2)], as.numeric)


    if(outcome=="heart attack") {
        outcome_state_sub<-outcome_state[,1:3]
        outcome_state_sub<-outcome_state_sub[complete.cases(outcome_state_sub),]
        ndx<-order(outcome_state_sub$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, outcome_state_sub$Hospital.Name, na.last=TRUE)
        subset<-outcome_state_sub[ndx,]

        if(num=="worst") {
          num<-length(ndx)
        }

        if(num>length(ndx)) {
          x<-"NA"
        }

        else {
          x<-subset$Hospital.Name[num]
        }
    }



    else if(outcome=="heart failure") {
        outcome_state_sub<-outcome_state[,c(1,2,4)]
        outcome_state_sub<-outcome_state_sub[complete.cases(outcome_state_sub),]
        ndx<-order(outcome_state_sub$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, outcome_state_sub$Hospital.Name, na.last=TRUE)
        subset<-outcome_state_sub[ndx,]

        if(num=="worst") {
          num<-length(ndx)
        }

        if(num>length(ndx)) {
          x<-"NA"
        }

        else {
          x<-subset$Hospital.Name[num]
        }
    }

    else if(outcome=="pneumonia") {
        outcome_state_sub<-outcome_state[,c(1,2,5)]
        ndx<-order(outcome_state$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,outcome_state$Hospital.Name, na.last=TRUE)
        subset<-outcome_state[ndx,]

        if(num=="worst") {
          num<-length(ndx)
        }

        if(num>length(ndx)) {
          x<-"NA"
        }

        else {
          x<-subset$Hospital.Name[num]
        }
    }

    x

}
