plot1 <- function() {
  
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

## Define column classes

subhouse[,3] <- suppressWarnings(as.numeric(levels(subhouse[,3])[subhouse[,3]]))
subhouse[,4] <- suppressWarnings(as.numeric(levels(subhouse[,4])[subhouse[,4]]))
subhouse[,5] <- suppressWarnings(as.numeric(levels(subhouse[,5])[subhouse[,5]]))
subhouse[,6] <- suppressWarnings(as.numeric(levels(subhouse[,6])[subhouse[,6]]))
subhouse[,7] <- suppressWarnings(as.numeric(levels(subhouse[,7])[subhouse[,7]]))
subhouse[,8] <- suppressWarnings(as.numeric(levels(subhouse[,8])[subhouse[,8]]))
subhouse[,9] <- suppressWarnings(as.numeric(levels(subhouse[,9])[subhouse[,9]]))
subhouse <- transform(subhouse, timestamp=as.POSIXct(paste(Date, Time)), "%d/%m/%Y %H:%M:%S")

## Plot 1 - create Plot using Histogram
  par(mar=c(5.1,4.1,4.1,2.1),mfrow = c(1,1) )

  hist(subhouse$Global_active_power, col = "red", main = "Global Active Power", xlab = "Global Active Power (kilowatts)")
  dev.copy(png, file="plot1.png", width=480, height=480)
  dev.off()
  
  print("See Working Directory for PNG file: plot1")

}