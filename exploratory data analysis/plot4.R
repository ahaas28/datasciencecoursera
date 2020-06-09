## Code to produce plot 4

plot4 <- function(){

  
  ## Reading data  
  data <- read.table("household_power_consumption.txt", header = TRUE, sep = ";", na.strings = "?")
  data1 <- subset(data, Date == "1/2/2007" | Date == "2/2/2007")
  
  dt <- paste(data1$Date, data1$Time)
  dtc <- strptime(dt, format = "%d/%m/%Y %H:%M:%S")
  
  data2 <- cbind(dtc, data1)
  
  
  ## Create png-file
  png(file = "plot4.png")
  
  par(mfcol = c(2,2))

  ## plot1
  plot(data2$dtc, data2$Global_active_power , type = "l", xlab = "", ylab = "Global Active Power")
  
  ## plot2
  plot(data2$dtc, data2$Sub_metering_1 , type = "l", xlab = "", ylab = "Energy sub metering")
  lines(data2$dtc, data2$Sub_metering_2, col = "red")
  lines(data2$dtc, data2$Sub_metering_3, col = "blue")
  legend("topright", bty = "n", lty = "solid", col = c("black", "red", "blue"), legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
  
  ## plot3
  plot(data2$dtc, data2$Voltage , type = "l", xlab = "datetime", ylab = "Voltage")
  
  ## plot4
  plot(data2$dtc, data2$Global_reactive_power , type = "l", xlab = "datetime", ylab = "Global_active_power")

  dev.off()
  
}