plot4 <- function() {
  
  ## Read Data into table
  
  temp <- tempfile()
  download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip",temp)
  housedata <- read.table((unz(temp, "household_power_consumption.txt")),
                          header = TRUE,  sep = ";", na.strings = "NULL")
  unlink(temp)
  
  
 ## directory = "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"

 ## housedata <- read.table("household_power_consumption.txt", 
 ##                         header = TRUE,  sep = ";", na.strings = "NULL")
  
  ## convert Date column to Date format
  
  housedata$Date <- as.Date(housedata$Date, format="%d/%m/%Y")
  
  ## Subset the dates that are required
  
  subhouse <- housedata[(housedata$Date=="2007-02-01") | 
                          (housedata$Date=="2007-02-02"),]
  
  ## Define classes for columns
  
  subhouse[,3] <- suppressWarnings(as.numeric(levels(subhouse[,3])[subhouse[,3]]))
  subhouse[,4] <- suppressWarnings(as.numeric(levels(subhouse[,4])[subhouse[,4]]))
  subhouse[,5] <- suppressWarnings(as.numeric(levels(subhouse[,5])[subhouse[,5]]))
  subhouse[,6] <- suppressWarnings(as.numeric(levels(subhouse[,6])[subhouse[,6]]))
  subhouse[,7] <- suppressWarnings(as.numeric(levels(subhouse[,7])[subhouse[,7]]))
  subhouse[,8] <- suppressWarnings(as.numeric(levels(subhouse[,8])[subhouse[,8]]))
  subhouse$Sub_metering_3 <- as.numeric(as.character(subhouse$Sub_metering_3))
  subhouse <- transform(subhouse, timestamp=as.POSIXct(paste(Date, Time)), "%d/%m/%Y %H:%M:%S")
  
  ## Plot 4 Set writer to PNG
  png("./plot4.png", width = 480, height = 480)
  
  ## set margins and 4 graph view
  
  par(mar=c(4,4,3,1.5),mfrow = c(2,2) )
  
  ## plot 1

  with(subhouse, plot(timestamp, Global_active_power, 
                      type="l", 
                      xlab = "", 
                      ylab="Global Active Power"))
  
  ## plot 2
  
  with(subhouse, plot(timestamp, Voltage, 
                      type="l", 
                      xlab = "datetime", 
                      ylab="Voltage"))
  
  ## plot 3
  
  with(subhouse, plot(timestamp, Sub_metering_1, 
                      type="l", 
                      xlab = "", 
                      ylab="Energy sub metering"))
  lines(subhouse$timestamp, subhouse$Sub_metering_2, col = "red")
  lines(subhouse$timestamp, subhouse$Sub_metering_3, col = "blue")
  legend("topright", c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
         lwd=c(.75,.75,.75), 
         lty = c(1,1), 
         col=c("black","red","blue"), 
         cex = .7, 
         bty = "n")
  
  # plot 4
  
  with(subhouse, plot(timestamp, Global_reactive_power, 
                      type="l", 
                      xlab = "datetime", 
                      ylab="Global_reactive_power",
                      ylim = c(0.0, 0.5)))
  axis(2,at = c(0.0,0.1,0.2,0.3,0.4,0.5))
  
  dev.off()
  print("See Working Directory for PNG file: plot4")
  
}