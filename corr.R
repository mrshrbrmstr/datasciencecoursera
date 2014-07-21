corr <- function(directory, threshold = 0) {
  
  cur <- getwd()                                      ## Store current working directory
  
  size.corr <- 0
  corr.vector <- numeric(0)                           ## Creates an empty numeric vector of length 0
  
  cases <- complete(directory, 1:332)                 ## this gets all the complete cases
  ids <- cases[cases$nobs > threshold,]$id            ## cases[cases$nobs > threshold] is a sub-table with only the rows
          ## that satisfy the logical statement "cases$nobs > threshold" $id selects the id column vector
  for (i in ids) {                                    ## this iterates over that ids vector
    
    setwd(directory)                                  ## Set wd to correct file location
    Char.RecordNumber <- sprintf("%03d.csv", i)       ## Converts numeric id into a string of chr
    tmp <- read.csv(Char.RecordNumber)                ## Reads the file and stores in tmp
    setwd(cur)                                        ## Restores wd prior to other function
    
    size.corr <- size.corr + 1
    corr.vector[size.corr] <- round(cor(tmp$sulfate, tmp$nitrate, use = "pairwise.complete.obs"), digits = 4)
    
  }
  
  return(corr.vector)
}