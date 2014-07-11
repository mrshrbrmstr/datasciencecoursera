corr <- function(directory, threshold = 0) {
  
  cur <- getwd()                                           ## Store current working directory
  size.corr <- 0
  corr.vector <- 0
  for (i in 1:332) {
    setwd(directory)                                  ## Set wd to correct file location
    Char.RecordNumber <- sprintf("%03d.csv", i)       ## Converts numeric id into a string of chr
    tmp <- read.csv(Char.RecordNumber)                ## Reads the file and stores in tmp
    setwd(cur)                                        ## Restores wd prior to other function
    nobs <- complete(directory, i)
     if (nobs[1] > threshold) {
      
      size.corr <- size.corr + 1
      corr.vector[size.corr] <- round(cor(tmp$sulfate, tmp$nitrate, use = "na.or.complete", method = "pearson"), digits = 5)
      
    }

  }
  
  return(corr.vector)
}