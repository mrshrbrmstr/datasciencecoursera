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

## Create the 4th plot

par(mfrow=c(2,2), mar=c(4,4,2,1), oma=c(0,0,2,0))
with(data, {
      plot(Global_active_power~Datetime, type="l", 
           ylab="Global Active Power (kilowatts)", xlab="datetime")
      plot(Voltage~Datetime, type="l", 
           ylab="Voltage (volt)", xlab="datetime")
      plot(Sub_metering_1~Datetime, type="l", 
           ylab="Global Active Power (kilowatts)", xlab="datetime")
            lines(Sub_metering_2~Datetime,col='Red')
            lines(Sub_metering_3~Datetime,col='Blue')
            legend("topright", col=c("black", "red", "blue"), lty=1, lwd=2, bty="n",
             legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
      plot(Global_reactive_power~Datetime, type="l", 
           ylab="Global Rective Power (kilowatts)",xlab="datetime")
})

## Saving to file
dev.copy(png, file="plot4.png", height=480, width=480)
dev.off()