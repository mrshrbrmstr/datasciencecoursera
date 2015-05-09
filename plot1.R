## Input Data Into R from current working directory
power_data <- read.csv("./power_consumption.txt", header=T, sep=';', na.strings="?", 
                      nrows=2075259, check.names=F, stringsAsFactors=F, comment.char="", quote='\"')
power_data$Date <- as.Date(power_data$Date, format="%d/%m/%Y")

## Getting the proper subset of the data
data <- subset(power_data, subset=(Date >= "2007-02-01" & Date <= "2007-02-02"))
rm(power_data)

## Putting dates into proper format for plotting
date_and_time <- paste(as.Date(data$Date), data$Time)
data$Datetime <- as.POSIXct(date_and_time)

## Plot 1
hist(data$Global_active_power, main="Global Active Power", 
     xlab="Global Active Power (kilowatts)", ylab="Frequency", col="Red")

## Save plot to file
dev.copy(png, file="plot1.png", height=480, width=480)
dev.off()