pollutantmean <- function(directory, pollutant, id) {
     setwd(directory)
     Count.pollutant <- 0
     Sum.pollutant <- 0
     if (pollutant == "nitrate") {
             for (i in id[1]:id[length(id)]) {
                  Char.RecordNumber <- sprintf("%03d.csv", i)
                  tmp <- read.csv(Char.RecordNumber)
                  Sum.pollutant <- Sum.pollutant + sum(tmp$nitrate, na.rm = false)
                  Count.pollutant <- Count.pollutant + sum(!is.na(tmp$nitrate))
             }
             Count.pollutant <- max(1, Count.pollutant)  ## Prevents division by 0
             return(Sum.pollutant/Count.pollutant)
     }
     if (pollutant == "sulfate") {
             for (i in id[1]:id[length(id)]) {
                  Char.RecordNumber <- sprintf("%03d.csv", i)
                  tmp <- read.csv(Char.RecordNumber)
                  Sum.pollutant <- Sum.pollutant + sum(tmp$sulfate, na.rm = false)
                  Count.pollutant <- Count.pollutant + sum(!is.na(tmp$sulfate))
             }
             Count.pollutant <- max(1, Count.pollutant)  ## Prevents division by 0
             return(Sum.pollutant/Count.pollutant)
     }
     if (pollutant != "nitrate" & pollutant != "sulfate") {
             return(0)
     }
}