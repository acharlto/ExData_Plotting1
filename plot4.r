plot4<-function()
  
{
  #load dplyr library
  library("dplyr")
  #Read data from zip file.  Also removes "?" entries which represent NA
  ds<-read.delim(unzip("exdata_data_household_power_consumption.zip","household_power_consumption.txt"),header = TRUE, sep = ";",na.strings="?")
  #Join the Date and Time columns into an new column called "DateTime"
  ds<-ds%>%mutate(DateTime=paste(Date,Time,sep=" "))
  #convert the "DateTime" column to a datetime format
  ds<-ds%>%mutate(DateTime=as.POSIXct(strptime(DateTime,format = "%d/%m/%Y %H:%M:%S")))
  #Filter dates to only include 2007-02-01 and 2007-02-02
  ds<-ds%>%filter(DateTime>=as.POSIXct("2007-02-01") & DateTime<as.POSIXct("2007-02-03"))
  #save plot as png file
  png("plot4.png",width = 480, height = 480,units = "px")
  #set up 2x2 chart 
  par(mfrow=c(2,2))
  
  #Plot Global Active Power vs time
  plot(ds$DateTime,ds$Global_active_power,type="l",main = "",xlab = "",ylab = "Global Active Power",xaxt='n',yaxt='n')
  #set up y axis
  axis(side=2,at=seq(0,6,2),labels=seq(0,6,2))
  #Set up y axis labels
  axis(side=1,at=seq(as.POSIXct("2007-02-01"),as.POSIXct("2007-02-03"),86400),labels=weekdays(seq(as.POSIXct("2007-02-01"),as.POSIXct("2007-02-03"),86400),abbreviate = TRUE))
  
  #Plot voltage vs time
  plot(ds$DateTime,ds$Voltage,type="l",main = "",xlab = "",ylab = "Voltage",xaxt='n',yaxt='n')
  #set up y axis
  axis(side=2,at=seq(234,246,2),labels=seq(234,246,2))
  #Set up y axis labels
  axis(side=1,at=seq(as.POSIXct("2007-02-01"),as.POSIXct("2007-02-03"),86400),labels=weekdays(seq(as.POSIXct("2007-02-01"),as.POSIXct("2007-02-03"),86400),abbreviate = TRUE))
  
  
  #Plot Sub_metering_1 vs time
  plot(ds$DateTime,ds$Sub_metering_1,type="l",main = "",xlab = "",ylab = "Energy sub metering",xaxt='n',yaxt='n')
  #Plot Sub_metering_2 vs time
  lines(ds$DateTime,ds$Sub_metering_2,type = "l",col="red",main = "",xlab = "",ylab = "",xaxt='n',yaxt='n')
  #Plot Sub_metering_3 vs time
  lines(ds$DateTime,ds$Sub_metering_3,type = "l",col="blue",main = "",xlab = "",ylab = "",xaxt='n',yaxt='n')
  #set numbering on y axis
  axis(side=2,at=seq(0,30,10),labels=seq(0,30,10))
  #set labels on x axis
  axis(side=1,at=seq(as.POSIXct("2007-02-01"),as.POSIXct("2007-02-03"),86400),labels=weekdays(seq(as.POSIXct("2007-02-01"),as.POSIXct("2007-02-03"),86400),abbreviate = TRUE))
  #Add Legend
  legend("topright",legend = c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),col=c("black","red","blue"),lty=1,bty="n")
  
  #Plot Global_reactive_power vs time
  plot(ds$DateTime,ds$Global_reactive_power,type="l",main = "",xlab = "",ylab = "Global_reactive_power",xaxt='n',yaxt='n')
  #set up y axis
  axis(side=2,at=seq(0.0,0.5,0.1),labels=seq(0.0,0.5,0.1))
  #Set up y axis labels
  axis(side=1,at=seq(as.POSIXct("2007-02-01"),as.POSIXct("2007-02-03"),86400),labels=weekdays(seq(as.POSIXct("2007-02-01"),as.POSIXct("2007-02-03"),86400),abbreviate = TRUE))
  
  
  dev.off()
  
}