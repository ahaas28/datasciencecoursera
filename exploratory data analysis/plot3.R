## Code to construct plot3

plot3 <- function(){
  
  data <- read.table("household_power_consumption.txt", header = TRUE, sep = ";", na.strings = "?")
  data1 <- subset(data, Date == "1/2/2007" | Date == "2/2/2007")
  
  dt <- paste(data1$Date, data1$Time)
  dtc <- strptime(dt, format = "%d/%m/%Y %H:%M:%S")
  
  data2 <- cbind(dtc, data1)
  
  png(file = "plot3.png")
  plot(data2$dtc, data2$Sub_metering_1 , type = "l", xlab = "", ylab = "Energy sub metering")
  lines(data2$dtc, data2$Sub_metering_2, col = "red")
  lines(data2$dtc, data2$Sub_metering_3, col = "blue")
  legend("topright", lty = "solid", col = c("black", "red", "blue"), legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
  
  dev.off()
  
}