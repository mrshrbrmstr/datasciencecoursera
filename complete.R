complete <- function(directory, id = 1:332) {
  
  cur <- getwd()                                           ## Store current working directory
  setwd(directory)                                         ## Set wd to correct file location
  
  nobs <- rep(0, length(id))                               ## Vector of same length as id for counting complete cases
  for (i in 1:length(id)) {                                ## Treats id as a vector for calculating mean
    Char.RecordNumber <- sprintf("%03d.csv", id[i])        ## Converts numeric id into a string of chr
    tmp <- read.csv(Char.RecordNumber)                     ## Reads the file and stores in tmp
    
    Is.complete <- complete.cases(tmp)                     ## Creates a vector of logical values for each
                                                           ##    adds to complete count if TRUE
    nobs[i] <- sum(Is.complete)                           ##     updates the value for this monitor id

  }
  
  setwd(cur)                                               ## Restores wd to previous one
  
  return(data.frame(cbind(id, nobs)))                      ## Returns data frame with id and nobs headers
  
}