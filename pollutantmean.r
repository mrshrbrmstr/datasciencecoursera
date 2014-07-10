pollutantmean <- function(directory, pollutant, id = 1:332) {
  
  if (pollutant != "nitrate" & pollutant != "sulfate") {   ## This happens if pollutant
    message("not a valid pollutant")                       ## is neither nitrate nor sulfate
    return(0)                                              ## It tests the pollutant value
  }                                                        ## Past this point, pollutant must be
                                                           ## "nitrate" or "sulfate".
  cur <- getwd()                                           ## Store current working directory
  setwd(directory)                                         ## Set wd to correct file location
  
  Count.pollutant <- 0                                     ## Number of data points
  Sum.pollutant <- 0                                       ## Sum of data for given pollutant, id
  
  for (i in 1:length(id)) {                                ## Treats id as a vector for calculating mean
    Char.RecordNumber <- sprintf("%03d.csv", id[i])        ## Converts numeric id into a string of chr
    tmp <- read.csv(Char.RecordNumber)                     ## Reads the file and stores in tmp
    if (pollutant == "nitrate") {                          ## Sum and count increment under correct column
      Sum.pollutant <- Sum.pollutant + sum(tmp$nitrate, na.rm = TRUE) 
      Count.pollutant <- Count.pollutant + sum(!is.na(tmp$nitrate))
    } else {
      Sum.pollutant <- Sum.pollutant + sum(tmp$sulfate, na.rm = TRUE)
      Count.pollutant <- Count.pollutant + sum(!is.na(tmp$sulfate))      
    }
  }
  
  Count.pollutant <- max(1, Count.pollutant)  ## Prevents division by 0 in case all records "NA"
  
  setwd(cur)                                  ## Restores working directory to original
  
  return(round(Sum.pollutant/Count.pollutant, digits = 3))       ## Function returns a numeric value of length 1
  
}