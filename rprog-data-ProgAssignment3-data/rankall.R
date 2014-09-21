rankall <- function(outcome, num = "best")  { 
      
       ## Now validate the outcome input of the function call
       is.outcome.valid <- outcome %in% c("heart attack", "heart failure", "pneumonia")
            if (is.outcome.valid == "FALSE") stop("invalid outcome")
       
       tmp <- read.csv("outcome-of-care-measures.csv")
 
       all.states <- c(state.abb, "DC", "GU", "PR", "VI")
      sorted.all.states <- sort(all.states)
      
      if (num == "best") num <- 1
      temp <- lapply(sorted.all.states, function(state) { list(state, rankhospitalbystate(tmp, state, outcome, num)) } )
      data.frame(hospital=unlist(sapply(temp, "[", 2)), state=unlist(sapply(temp, "[", 1)))
      
 }