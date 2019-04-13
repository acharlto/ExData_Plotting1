plot2<-function()
  
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
  png("plot2.png",width = 480, height = 480,units = "px")
  #Plot Global Active Power vs time
  plot(ds$DateTime,ds$Global_active_power,type="l",main = "",xlab = "",ylab = "Global Active Power (kilowatts)",xaxt='n',yaxt='n')
  #set y axis values
  axis(side=2,at=seq(0,6,2),labels=seq(0,6,2))
  
  #could change this to a dyncamic value that calls min and max date
  axis(side=1,at=seq(as.POSIXct("2007-02-01"),as.POSIXct("2007-02-03"),86400),labels=weekdays(seq(as.POSIXct("2007-02-01"),as.POSIXct("2007-02-03"),86400),abbreviate = TRUE))
  
  dev.off()
  
  }