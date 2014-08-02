rankhospital <- function(state, outcome, num = "best") {
      
      if (num == "best") best(state, outcome)
      
      tmp <- read.csv("outcome-of-care-measures.csv")
      ## reads the csv file in current working directory
      
      ## Now validate the state input of the function call
      is.state.valid <- state %in% state.abb
      ## if value of state is one of the U.S. 50 states, this assigns "TRUE"
      ## otherwise it assigns "FALSE"
      if (is.state.valid == "FALSE") {
            morestates.abb <- c("GU", "PR", "VI", "DC")
            is.state.valid <- state %in% morestates.abb
            ## if value of state is a U.S. territory included in data, this assigns "TRUE"
            ## otherwise it remains "FALSE"
            if (is.state.valid == "FALSE") stop("invalid state")
      }
      
      ## Now validate the outcome input of the function call
      is.outcome.valid <- outcome %in% c("heart attack", "heart failure", "pneumonia")
      if (is.outcome.valid == "FALSE") stop("invalid outcome")
      
      ## Assigns the column number corresponding to the outcome stated in the function
      col <- switch(outcome, `heart attack` = 11, `heart failure` = 17, `pneumonia` = 23)
      
      ## The following creates a table in which the rows represent ONLY hospitals from the given state
      ## AND hospitals' mortality rate exists in the data (removes all "Not Available" data)
      ## The resulting table has 2 columns: Hospital Name and mortality rate for the given outcome
      state.hospitals <- tmp[tmp$State == state & tmp[,col] != "Not Available", c(2, col)]
      
      ## Now convert the data in column 2 to numeric values from labels
      state.hospitals[,2] <- as.numeric(as.character(state.hospitals[,2]))
      state.hospitals[,1] <- as.character(state.hospitals[,1])

      sorted.state.hospitals <- state.hospitals[order(state.hospitals[,2], state.hospitals[,1]),]
      
      if (num == "worst") return(sorted.state.hospitals[length(sorted.state.hospitals$Hospital.Name),1])
      
      if (num > length(sorted.state.hospitals[,1])) return(NA)
      
      Rank <- 1:num
      top.state.hospitals <- sorted.state.hospitals[1:num,]
      top.state.hospitals$Rank <- Rank
      
      return(sorted.state.hospitals[num,1])
      
}