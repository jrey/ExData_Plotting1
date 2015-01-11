
# Read and filter data from Dataset
load.data <- function (file = "household_power_consumption.txt") {
  ds <- read.csv(file, header=TRUE, sep=';', stringsAsFactors=FALSE, na.strings="?")
  ds <- subset(ds, Date == "1/2/2007" | Date == "2/2/2007")
  ds$DateTime = strptime(paste(ds$Date,ds$Time, sep=" "),"%d/%m/%Y %H:%M:%S", tz="UTC")
  ds
}

# Utility function to create a PNG graph, receives
#  name: the PNG file name
#  func: a funcion to do the plot
#  ds:   a dataset used by func
plotPNG <- function(name, func, ds) {
  png(filename = name, width = 480, height = 480,
      units = "px", pointsize = 12, bg="white", type = "quartz")
  func(ds)
  dev.off()
}

# Make plot #1 from ds dataset
plot1 <- function(ds) {
  hist(ds$Global_active_power, col="red",
       main="Global Active Power",
       xlab="Global Active Power (Kilowatts)")  
}

# Make plot #2 from ds dataset
plot2 <- function(ds) {
  with(ds, plot(DateTime, Global_active_power,type="l",
                ylab="Global Active Power (Kilowatts)", xlab=""))
}

# Make plot #3 from ds dataset
plot3 <- function(ds) {
  plot(ds$DateTime, ds$Sub_metering_1, type="l", ylab="Energy sub metering", xlab="")
  lines(ds$DateTime, ds$Sub_metering_2, col="red")
  lines(ds$DateTime, ds$Sub_metering_3, col="blue")
  legend('topright',c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),
         lty=1,col=c("black","red","blue"))
}

# Make plot #4 from ds dataset
plot4 <- function(ds) {
  par(mfrow=c(2,2))
  
  plot2(ds)
  
  plot(ds$DateTime, ds$Voltage, type="l", ylab="Voltage", xlab="datetime")

  plot3(ds)

  plot(ds$DateTime, ds$Global_reactive_power, type="l",
       ylab="Global_reactive_power", xlab="datetime")
}

# Make all the plots at once
plotAllPNG <- function(ds) {
  plotPNG("plot1.png", plot1, ds)
  plotPNG("plot2.png", plot2, ds)
  plotPNG("plot3.png", plot3, ds)
  plotPNG("plot4.png", plot4, ds)
}

