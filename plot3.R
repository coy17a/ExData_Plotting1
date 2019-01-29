#packages neeed to be installed in the enviroment before running the script
library(downloader)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(gridExtra)
#importing data to data_Frame from gith-hub repository
fileUrl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
download(fileUrl, dest="dataset.zip", mode="wb") 
unzip ("dataset.zip", exdir = "./")
data <- read.table("./household_power_consumption.txt",sep= ";",header =TRUE)
head(data)

#filterind data 
data$Date <- paste(data$Date , data$Time, sep =" ")
data$Date <- dmy_hms(data$Date)
datefilter <-  (date(data$Date) >= as.Date("2007-02-01") & date(data$Date) <as.Date("2007-02-03"))
data2 <- data[datefilter,]
#change data type to numerix
data2 <- data2 %>%
  mutate_at(c(3:8), as.character)%>%
  mutate_at(c(3:8), as.numeric)
#plot
 ggplot(data = data2_long)+
  aes( x = Date , y = energy, color = sub_metering)+
  geom_line()+
  theme(legend.position=c(0.8,0.8))+
  scale_x_datetime(breaks=c( ymd_hms("2007-02-01 00:00:00"),ymd_hms("2007-02-02 00:00:00"),ymd_hms("2007-02-02 23:59:00")) ,labels =c("Thurs","Friday","Sat"))+
  labs(y= "Energy syb metering")
ggsave("plot3.png", width = 10.9, height = 10.9, units = "cm")