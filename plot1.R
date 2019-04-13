plot1<-function()
  
{
  #load dplyr library
  library("dplyr")
  #Read data from zip file.  Also removes "?" entries which represent NA
  ds<-read.delim(unzip("exdata_data_household_power_consumption.zip","household_power_consumption.txt"),header = TRUE, sep = ";",na.strings="?")
  #Join the Date and Time columns into an new column called "DateTime"
  ds<-ds%>%mutate(Date=as.Date(Date,format="%d/%m/%Y"))
  #convert the "DateTime" column to a datetime format
  ds<-ds%>%filter(Date>=as.Date("2007-02-01") & Date<=as.Date("2007-02-02"))
  
  #save plot as png file
  png("plot1.png",width = 480, height = 480,units = "px")
  #plot histogram
  hist(ds$Global_active_power, main = "Global Active Power", xlab = "Global Active Power (kilowatts)", col="red",yaxt='n',xaxt='n')
  #set y axis label
  axis(side=1,at=seq(0,6,2),labels=seq(0,6,2))
  #set x axis label
  axis(side=2,at=seq(0,1200,200),labels=seq(0,1200,200))
  
  dev.off()
}