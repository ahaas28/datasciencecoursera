## Code to construct plot 1

plot1 <- function(){
  
  data <- read.table("household_power_consumption.txt", header = TRUE, sep = ";", na.strings = "?")
  data1 <- subset(data, Date == "1/2/2007" | Date == "2/2/2007")
  
  png(file = "plot1.png")
  hist(data1$Global_active_power, col = "red", main = "Global Active Power", xlab = "Global Active Power (kilowatts)")
  dev.off()
  
}
