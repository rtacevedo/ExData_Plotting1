plot3 <- function() {
  
  ## Get & Read Data into table
  
  temp <- tempfile()
  download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip",temp)
  housedata <- read.table((unz(temp, "household_power_consumption.txt")),
                          header = TRUE,  sep = ";", na.strings = "NULL")
  unlink(temp)
  
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
  
  ## Plot 3 plot base 
  png("./plot3.png", width = 480, height = 480)
  
  par(mar=c(5.1,4.1,4.1,2.1),mfrow = c(1,1) )
  
  with(subhouse, plot(timestamp, Sub_metering_1, 
                      type="l", 
                      xlab = "", 
                      ylab="Energy sub metering"))
  
  ## add secondary lines
  
  lines(subhouse$timestamp, subhouse$Sub_metering_2, col = "red")
  lines(subhouse$timestamp, subhouse$Sub_metering_3, col = "blue")
  legend("topright", c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), lty = c(1,1), lwd=c(2.5,2.5,2.5), col=c("black","red","blue"))
  
  ## copy to png
  

  dev.off()
  print("See Working Directory for PNG file: plot3")
  
}