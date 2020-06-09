## Code to construct plot2.

plot2 <- function(){
  
  data <- read.table("household_power_consumption.txt", header = TRUE, sep = ";", na.strings = "?")
  data1 <- subset(data, Date == "1/2/2007" | Date == "2/2/2007")
  
  dt <- paste(data1$Date, data1$Time)
  dtc <- strptime(dt, format = "%d/%m/%Y %H:%M:%S")
  
  data2 <- cbind(dtc, data1)
  
  
  png(file = "plot2.png")
  plot(data2$dtc, data2$Global_active_power, type = "l", xlab = "", ylab = "Gobel Active Power (kilowatts")
  dev.off()
  
}