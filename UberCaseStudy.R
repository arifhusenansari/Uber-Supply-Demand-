
if  (!("ggplot2" %in% rownames(installed.packages()))){
  install.packages("ggplot2")
}
if (!("dplyr" %in% rownames(installed.packages()))){
  install.packages("dplyr")
}
if (!("tidyr" %in% rownames(installed.packages()))){
  install.packages("tidyr")
}
if (!("stringr" %in% rownames(installed.packages()))){
  install.packages("stringr")
}
if (!("lubridate" %in% rownames(installed.packages()))){
  install.packages("lubridate")
}
if (!("chron" %in% rownames(installed.packages()))){
  install.packages("chron")
}
if (!("gridExtra" %in% rownames(installed.packages()))){
  install.packages("gridExtra")
}


if (!("reshape2" %in% rownames(installed.packages()))){
  install.packages("reshape2")
}


library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(chron)
library(gridExtra)
library(reshape2)


#--- Required Function Definitions
#-- Derive Requested Time slot.
#-- 1.  08 p.m. to 23:59 a.m.      Night
#-- 2.  12 a.m. to 03:59 a.m.      Late Night
#-- 3.  04 a.m. to 07:59 a.m.      Early Morning
#-- 4.  08 a.m. to 11:59 a.m.      Morning
#-- 5.  12 p.m. to 03:59 p.m.      After Noon
#-- 6.  04 p.m. to 07:59 p.m.      Evening
#-- Function to catgorised requests in time slots. Based on Requested time
timeSlot <- function (x){
  x=chron(times. = x)
  h = hours(x)
  m = minutes(x)
  slot=""
  if ( h >= 20 & h <= 23 ) {
    slot = "Night ( 20:00 to 24:00)"
  } else if ( h==24 | (h >=0 & h <=3)){
    slot = "Late Night (00:00 to 04:00)"
  } else if (h >=4 & h <=7){
    slot = "Early Morning (04:00 to 08:00)"
  } else if (h >=8 & h <=11){
    slot = "Morning (08:00 to 12:00)"
  } else if (h >=12 & h <=15){
    slot = "Afternoon (12:00 to 16:00)"
  } else if (h >=16 & h <=19){
    slot = "Evening (16:00 to 20:00)"
  }
  
  return (slot)
  
}

#-- Logic
#-- This function will calculate the wait_time. 
#-- It will calculate the wait_time if next trip is from "Airport" and on the same day otherwise "NA" will be returned. 
#-- NA Values are handled iin further process. 
calculate_waittime <- function (r){
  
  if ( r["Status"] == "Trip Completed"  & r["Pickup.point"]=="City" ){
    nseq <- as.numeric(r["new_seq"])
    nexttripdetails <- uber[uber$new_seq>nseq & uber$Driver.id == r["Driver.id"] & uber$Status=="Trip Completed" & uber$Requested.Date==r["Requested.Date"],]
    nexttripdetails <- head(nexttripdetails %>% arrange(new_seq),1)
    wait_time= as.numeric(round(difftime(nexttripdetails$Request.timestamp,r["Drop.timestamp"],units =  c("mins")),0))
    if ( length(wait_time)==0 || nexttripdetails$Pickup.point=="City"){
      wait_time =NA
    }    
    return (wait_time)
  } else {
    return (NA)
  }
  
}

#-- Function to replace NA with Average Waiting time for that hour.
replace.na.waittime <- function (r){
  
  if (is.na(r["wait_time"])==TRUE & r["Status"]=="Trip Completed" & r["Pickup.point"]=="City"){
    time <- as.numeric(avg.waittime.byhour[avg.waittime.byhour$Request.Date.Hour==as.numeric(r["Request.Date.Hour"]),2])
    return (time)
  }else{
    return (as.numeric(r["wait_time"]))
  }
}



setwd("E:\\Arif\\Work\\Data Science Course\\Upgrad PGDDS\\Course 2\\Module 5 Uber Case Study")
#-- Load data from the file.
#-- String is loaded as factor to clearly analyse data using feature in factors. Like levels.
uber <- read.csv(file="Uber Request Data.csv",sep=",",stringsAsFactors = T)
# View(ubertemp)
color.theme = c("#e3a711", "#93b856", "#c56055","#55a0d3")
timeslot.level = c("Late Night (00:00 to 04:00)","Early Morning (04:00 to 08:00)","Morning (08:00 to 12:00)","Afternoon (12:00 to 16:00)","Evening (16:00 to 20:00)","Night ( 20:00 to 24:00)")
theme_title <- theme(plot.title = element_text(size = 20, face = "bold"))
bar_chart_text <- geom_text(stat='Identity', aes(label=value),position=position_dodge(width=0.9),vjust=-0.25,hjust=0.25)

#---------------------------------------------------- Data Cleaning.----------------------------------------------------

str(uber)
#-- Check for Missing Values.
summary(uber)

#-- Step 2: Data Cleaning. Solve formatting issue and create derived columns if needed. 
# View(uber)
#-- Data cleaning is required for two columns "Request.timestamp" and "Drop.timestamp". 
#-- Also, we will derive some useful column from those columns. 
#-- Clean data in both time column and derive new column as per requirement. 

uber$Request.timestamp <- (parse_date_time(as.character(uber$Request.timestamp),c("%d%m%y %H%M","%d%m%y %H%M%S")))
uber$Drop.timestamp <- (parse_date_time(as.character(uber$Drop.timestamp),c("%d%m%y %H%M","%d%m%y %H%M%S")))
uber <- uber %>% separate(Request.timestamp,c("Requested.Date","Requested.Time"),sep=" ",remove = F)
uber$Request.Date.Hour <- hours(chron(times. = uber$Requested.Time))
uber$Request.Date.Mins <- minutes(chron(times. = uber$Requested.Time))
uber <- uber %>% separate(Drop.timestamp,c("Drop.Date","Drop.Time"),sep=" ",remove = F)
uber$Drop.Date.Hour <- hours(chron(times. = uber$Drop.Time))
uber$Drop.Date.Mins <- minutes(chron(times. = uber$Drop.Time))
uber$travel.time.mins <- as.numeric(round(difftime(uber$Drop.timestamp,uber$Request.timestamp,units =  c("mins")),0))

#-- This will store the direction of trip. 
#-- E.g "City to Airport", "Airport to City".
uber$trip.direction  <- ""
uber[which(uber$Pickup.point=="Airport"),c("trip.direction")] <- "Airport to City"
uber[which(uber$Pickup.point=="City"),c("trip.direction")] <- "City to Airport"

#-- sanity check with data. 
uber[uber$Pickup.point=="Airport" & uber$trip.direction =="City to Airport",1]
uber[uber$Pickup.point=="City" & uber$trip.direction =="Airport to City",1]
summary(factor(uber$trip.direction))



#-- Derived New seq. Seq is generated in ascending order of requested date. 
uber <- cbind((uber %>% arrange(uber$Request.timestamp)),new_seq=c(1:nrow(uber)))

#-- Allocate Time Slot and Give level based on Order of time.
#-- E.g Morning slot will have 1 level and Late Night will have highest. 

uber$RequestedTimeSlot <- as.factor(sapply(uber$Requested.Time,FUN = timeSlot))
uber$RequestedTimeSlot <- factor(uber$RequestedTimeSlot,levels = timeslot.level )

#-- Calculating Wait time.
#-- Assumption: 
#-- 1. Wait time is calculated for trips only from City To Airport.
#-- 2. Wait time will be NA if driver's next trip's pickup point is city. 
#-- 3. Wait time is calculated if next trip is with in same day. Wait time will be NA, eventhough next trip's pickup point is Airport, because day is changed. 

#-- Create wait_time column with default -1.
#-- Assign values after calculation. 
uber$wait_time <- -1
wait_time <- data.frame(unlist(apply(uber,1,calculate_waittime )))
colnames(wait_time) <- c("wait_time")
uber$wait_time <-wait_time$wait_time
summary(uber$wait_time)

#---------------------------------------------- NA Value treatment -----------------------------------------------------
#-- Based on Summary details. Following columns are having NA values ( Missing Values).
#-- 1. Driver.id
#-- 2. Drop.timestamp.
#-- 3. wait_time
#-- Step 1: Check behaviour of missing values.
#--   1. Driver.id
#-- We can see when Status is "NO Cars Available". 
#-- Value is Driver.id is missing and it's seems logic. Since care is not available, there will not be allocated driver id. 
summary(uber[which(is.na(uber$Driver.id)),c("Status")])

#--   2. Drop.timestamp
summary(uber[which(is.na(uber$Drop.timestamp)),c("Status")])
#-- For this column NA values are for "No Cars Available" and also for "Cancelled".
#-- It's also logical and correct. Sometime car is assign for trip and driver id is allocated but after it's cancelled. 
#-- So for those rows, we can have "NA" in Drop.timestamp.

#-- Note: These missing values are not due to human or machine error. it a real and part of data. 
#-- There is no need to trean NA values in those columns. 

#--   3. wait_time  
#-- Conclude the Reason about NA in wait_time column.
View(uber[uber$Status=="Trip Completed" & uber$Pickup.point=="City" & is.na(uber$wait_time)==TRUE ,])
#-- Analyse Two drivers.
#-- Driver.Id = 155 and 198.
View(uber[uber$Driver.id %in% c(155) & uber$Status=="Trip Completed", ])
View(uber[uber$Driver.id %in% c(198) & uber$Status=="Trip Completed", ])
#-- There are completed trips (To Airport), But I couldn't calculate wait time, because missed next trip from Airport to City.
#-- Driver might have waited for long time and came back to city with empty rides.
#-- We will replace this NA values with mean wait_time in that hour. 
#-- average wait_time by hour
avg.waittime.byhour <- data.frame(group_by(uber[uber$Status=="Trip Completed" & uber$Pickup.point=="City",],Request.Date.Hour) %>% summarise(avg.wait.time=mean(na.omit(wait_time))))




#-- call replace.na.waittime
uber$wait_time <- data.frame(unlist(apply(uber,1,replace.na.waittime)))[,1]
#-- Check excution for missing values
View(uber[uber$Driver.id %in% c(155) & uber$Status=="Trip Completed", ])
View(uber[uber$Driver.id %in% c(198) & uber$Status=="Trip Completed", ])
#-- Check Sample data with values. 
View ( uber[uber$new_seq==16,])

##View(uber)

#----------------------------------------- Data Cleaning and Formatting is done. Now Start WIth EDA -----------------------------------

#-- Assumptions: 
#-- 1.All the cancellations are done by drivers. 

View(uber)


#-- Initially we do analysis in two segments.
#-- 1. Trips from City to Airport
#-- 2. Trips from Airport to City 

uber.citytoairport=uber[uber$trip.direction=="City to Airport",]
uber.airporttocity=uber[uber$trip.direction=="Airport to City",]

#-- 1.  Data building for City to Airport Analysis

uber.citytoairport.comtrip=uber.citytoairport[uber.citytoairport$Status=="Trip Completed",]
uber.citytoairport.cantrip=uber.citytoairport[uber.citytoairport$Status=="Cancelled",]
uber.citytoairport.natrip=uber.citytoairport[uber.citytoairport$Status=="No Cars Available",]


byhour <- group_by(uber.citytoairport,RequestedTimeSlot,Request.Date.Hour)
demand.by.hours <- data.frame(summarise(byhour,Demand=n()))
demand.by.timeslot <- data.frame(group_by(demand.by.hours,RequestedTimeSlot) %>% summarise(Demand=sum(Demand)))

byhour <- group_by(uber.citytoairport.comtrip,RequestedTimeSlot,Request.Date.Hour)
supply.by.hour <- data.frame(summarise(byhour,Supply=n()))
supply.timeslot <- data.frame(group_by(supply.by.hour,RequestedTimeSlot) %>% summarise(Supply=sum(Supply)))

byhour <- group_by(uber.citytoairport.cantrip,RequestedTimeSlot,Request.Date.Hour)
can.by.hour <- data.frame(summarise(byhour,Cancellation=n()))
can.by.timeslot <- data.frame(group_by(can.by.hour,RequestedTimeSlot) %>% summarise(Cancellation=sum(Cancellation)))

byhour <- group_by(uber.citytoairport.natrip,RequestedTimeSlot,Request.Date.Hour)
na.by.hour <- data.frame(summarise(byhour,NotAvailable=n()))
na.by.timeslot <- data.frame(group_by(na.by.hour,RequestedTimeSlot) %>% summarise(NotAvailable=sum(NotAvailable)))

#-- Merge Data
timeslot <- merge(demand.by.timeslot,supply.timeslot,by.x = "RequestedTimeSlot",by.y = "RequestedTimeSlot")
timeslot <- merge(timeslot,can.by.timeslot,by.x = "RequestedTimeSlot",by.y = "RequestedTimeSlot")
timeslot <- merge(timeslot,na.by.timeslot,by.x = "RequestedTimeSlot",by.y = "RequestedTimeSlot") 
timeslot$SupplyDemandGap <- timeslot$Demand - timeslot$Supply
timeslot$SupplyDemandGap.per <- (timeslot$SupplyDemandGap/timeslot$Demand) * 100

hours <- merge(demand.by.hours,supply.by.hour,by.x = "Request.Date.Hour",by.y = "Request.Date.Hour")[,c("Request.Date.Hour","Demand","Supply")]
hours <- merge(hours,can.by.hour,by.x = "Request.Date.Hour",by.y = "Request.Date.Hour")[,-4]
hours <- merge(hours,na.by.hour,by.x = "Request.Date.Hour",by.y = "Request.Date.Hour") [,-5]
hours$SupplyDemandGap <- hours$Demand - hours$Supply
hours$SupplyDemandGap.per <- (hours$SupplyDemandGap/hours$Demand) * 100

#-- 2. Data building for Airport to City Analysis.

uber.airporttocity.comtrip=uber.airporttocity[uber.airporttocity$Status=="Trip Completed",]
uber.airporttocity.cantrip=uber.airporttocity[uber.airporttocity$Status=="Cancelled",]
uber.airporttocity.natrip=uber.airporttocity[uber.airporttocity$Status=="No Cars Available",]


byhour <- group_by(uber.airporttocity,RequestedTimeSlot,Request.Date.Hour)
demand.by.hours <- data.frame(summarise(byhour,Demand=n()))
demand.by.timeslot <- data.frame(group_by(demand.by.hours,RequestedTimeSlot) %>% summarise(Demand=sum(Demand)))

byhour <- group_by(uber.airporttocity.comtrip,RequestedTimeSlot,Request.Date.Hour)
supply.by.hour <- data.frame(summarise(byhour,Supply=n()))
supply.timeslot <- data.frame(group_by(supply.by.hour,RequestedTimeSlot) %>% summarise(Supply=sum(Supply)))

byhour <- group_by(uber.airporttocity.cantrip,RequestedTimeSlot,Request.Date.Hour)
can.by.hour <- data.frame(summarise(byhour,Cancellation=n()))
can.by.timeslot <- data.frame(group_by(can.by.hour,RequestedTimeSlot) %>% summarise(Cancellation=sum(Cancellation)))

byhour <- group_by(uber.airporttocity.natrip,RequestedTimeSlot,Request.Date.Hour)
na.by.hour <- data.frame(summarise(byhour,NotAvailable=n()))
na.by.timeslot <- data.frame(group_by(na.by.hour,RequestedTimeSlot) %>% summarise(NotAvailable=sum(NotAvailable)))

#-- Merge Data
timeslotatoc <-merge(demand.by.timeslot,supply.timeslot,by.x = "RequestedTimeSlot",by.y = "RequestedTimeSlot")
timeslotatoc <- merge(timeslotatoc,can.by.timeslot,by.x = "RequestedTimeSlot",by.y = "RequestedTimeSlot")
timeslotatoc <- merge(timeslotatoc,na.by.timeslot,by.x = "RequestedTimeSlot",by.y = "RequestedTimeSlot") 
timeslotatoc$SupplyDemandGap <- timeslotatoc$Demand - timeslotatoc$Supply
timeslotatoc$SupplyDemandGap.per <- (timeslotatoc$SupplyDemandGap/timeslotatoc$Demand) * 100


hoursatoc <- merge(demand.by.hours,supply.by.hour,by.x = "Request.Date.Hour",by.y = "Request.Date.Hour")[,c("Request.Date.Hour","Demand","Supply")]
hoursatoc <- merge(hoursatoc,can.by.hour,by.x = "Request.Date.Hour",by.y = "Request.Date.Hour")[,-4]
hoursatoc <- merge(hoursatoc,na.by.hour,by.x = "Request.Date.Hour",by.y = "Request.Date.Hour") [,-5]
hoursatoc$SupplyDemandGap <- hoursatoc$Demand - hoursatoc$Supply
hoursatoc$SupplyDemandGap.per <- (hoursatoc$SupplyDemandGap/hoursatoc$Demand) * 100



#------------- City to Airport Supply and Demand Analysis



#-- Plot Supply and Demand gap

d <- melt(hours,id.vars="Request.Date.Hour")
d <- d[!d$variable %in% c("SupplyDemandGap","SupplyDemandGap.per"),]
plotbyhour <- ggplot(d[d$variable %in% c("Demand","Supply"),],aes(x=Request.Date.Hour,y=value,col=variable))+geom_line(size=0.8)
plotbyhour <- plotbyhour+scale_x_continuous("Request.Date.Hour",labels = as.character(d$Request.Date.Hour), breaks = d$Request.Date.Hour)
plotbyhour <- plotbyhour+scale_color_manual(values=color.theme,name="Business Status")
plotbyhour <- plotbyhour+labs(title="City To Airport (Supply and Demand)",y="Trips",x= "Hours")+theme_title
plotbyhour


#-- This due to large number of cancellation between 4 a.m to 12 p.m. 
#-- Not Availability is also problem. But cancellation is very high.
#-- For other time slot cancellation is not a problem but Non Availability is Big Issue. 
plotallbyhour <- ggplot(d,aes(x=Request.Date.Hour,y=value,col=variable))+geom_line(size=0.8)
plotallbyhour <- plotallbyhour+scale_x_continuous("Request.Date.Hour",labels = as.character(d$Request.Date.Hour), breaks = d$Request.Date.Hour)
plotallbyhour <- plotallbyhour+scale_color_manual(values=color.theme,name="Business Status")
plotallbyhour <- plotallbyhour+labs(title="City To Airport (Demand-Supply-Cancellation-Not Availability)",y= "Trips",x="Hours")+theme_title
plotallbyhour 

#-- See the view by time slots
d <- melt(timeslot,id.vars="RequestedTimeSlot")
d <-d[!d$variable %in% c("SupplyDemandGap","SupplyDemandGap.per"),]
plotbytimeslot <- ggplot(d,aes(x=factor(RequestedTimeSlot),y=value,fill=variable))
plotbytimeslot_bar <- plotbytimeslot +geom_bar(stat='Identity',position = position_dodge(width = 0.8))
plotbytimeslot_bar <- plotbytimeslot_bar+bar_chart_text
plotbytimeslot_bar <- plotbytimeslot_bar+scale_fill_manual(values=color.theme,name="Business Status")
plotbytimeslot_bar <- plotbytimeslot_bar+labs(title="City To Airport ( Business Status by Time Slots)",y="Trips",x="Time Slot")
plotbytimeslot_bar <- plotbytimeslot_bar+theme_title
plotbytimeslot_bar



grid.arrange(plotbyhour,plotallbyhour,plotbytimeslot_bar,nrow=3,ncol=1)

#-- Supply and Demand gap
#-- Average Supply and Demand 
#-- Overall Supply and Demand Gap.
hours %>% summarise(mean(SupplyDemandGap.per))
#-- Supply and Demand Gap. (after removing 5 a.m. to 10 p.m.)
hours[!hours$Request.Date.Hour %in% c(5:10),] %>% summarise(mean(SupplyDemandGap.per))
#-- Supply and Demand Gap. (Between 5 a.m. to 10 p.m.)
hours[hours$Request.Date.Hour %in% c(5:10),] %>% summarise(mean(SupplyDemandGap.per))



#-- Based on Graphs. 
#-- . Average Supply and demand gap is 51% for whole day. 
#-- . Supply and demand gap is 44.38%, if we remove 5 a.m. to 10 a.m data.But it's very high during 5 a.m to 10 p.m., that is 70.26%
#-- . But it's very high during 5 a.m. to 10 p.m. 70.26%. This causes overall gap to raise aroung 51%
#-- . Demand is at pick between 4 to 12 monring, means two slots (Early Morning and Morning).
#-- . Cancellation and Non Availability are problems. But cancellation is very high.
#-- . Cancellation is almost dobule the Not Availability. 
#-- . We will Analyse first Cancellaion reason because rides are around but drivers are cancelling requests.
#-- . We can fill that gap by analysing why drivers are cancelling requests. 
#-- . After that we will analyse non availability problem.

#------------------------------------------------------- Cancellation Analysis -------------------------------------------------------

#-- Behaviour of Completed Trips.

#-- Assumption: 
#-- 1. Cancellation is only happening from drivers and not from customers. 


#-- 1. Travel time.
#-- Based on three graphs.
#-- Average Travel time is same through out day from City To Airport. 
#-- There is some increase at 4 a.m morning, but no major impact on cancellation.
p1 <- ggplot(uber.citytoairport.comtrip,aes(x=RequestedTimeSlot,y=travel.time.mins))
p1 <- p1+geom_boxplot()+labs(title="Box plot ( Travel Time by Time Slot )",y="Travel Time",x="Time Slot")+theme_title
p1

p2 <- ggplot(uber.citytoairport.comtrip,aes(x=factor(Request.Date.Hour),y=travel.time.mins))
p2 <- p2+geom_boxplot()+labs(title = "Box plot ( Travel Time by Hour )",y="Travel Time" , x ="Hours")+theme_title
p2

p3 <- ggplot(uber.citytoairport.cantrip,aes(x=Request.Date.Hour))
p3 <- p3+geom_histogram(binwidth = 1)+labs(title = "Cancel Trips ( Distrubution by Hour )",y="Trips") +theme_title
p3 <- p3 + scale_x_continuous(name = "Hours",uber.citytoairport.cantrip$Request.Date.Hour,labels = (uber.citytoairport.cantrip$Request.Date.Hour),breaks =uber.citytoairport.cantrip$Request.Date.Hour)
p3
grid.arrange(p1,p2,p3,nrow=3,ncol=1)

#--2. Waiting Time
#-- Waiting time for two time slots that is "Early Morning (04:00 to 08:00)" and "Morning (08:00 to 12:00)" is high.
#-- Drivers are cancelling trips because it's not economical for them to wait at airport. 
#-- Reason behing High wait time could be less number of flight arriving during those time slot. 
#-- Also, due to more flights are departing demand is very high.

p1 <- ggplot(avg.waittime.byhour,aes(x=Request.Date.Hour,y=avg.wait.time))+geom_line(size=0.8)
p1 <- p1 + scale_x_continuous(avg.waittime.byhour$Request.Date.Hour,labels = (avg.waittime.byhour$Request.Date.Hour),breaks = avg.waittime.byhour$Request.Date.Hour)
p1 <- p1 +labs(title="Wait Time at Airport By Hour", x="Hours",y ="Trips") +theme_title
p1


p2 <- ggplot(uber.citytoairport.cantrip,aes(x=Request.Date.Hour))+geom_histogram(binwidth = 1)
p2 <- p2 +labs(title = "Cancel Trips ( Distrubution by Hour )",y="Trips")+theme_title
p2 <- p2 +scale_x_continuous(name="Hours",uber.citytoairport.cantrip$Request.Date.Hour,labels = (uber.citytoairport.cantrip$Request.Date.Hour),breaks =uber.citytoairport.cantrip$Request.Date.Hour)
p2

grid.arrange(p1,p2,nrow=2,ncol=1)

#---------- Conclusion
#-- Trips are cancelling due to high wait time at airport for completed trips.
#-- "Early Morning (04:00 to 08:00)" and "Morning (08:00 to 12:00)" is high.
#-- It's not economical for drivers to wait on Airport for long hours.

#----------------------------------------------- Not Availability Analysis -------------------------------------------------
#-- For trips from City to Airport. 
#-- Not Availability of Cars are bit High in "Early Morning (04:00 to 08:00)" and "Morning (08:00 to 12:00)" compare to other slots.  


grid.arrange(plotbyhour,plotallbyhour,plotbytimeslot_bar,nrow=3,ncol=1)

#---------- Conclusion
#-- Demand of rides in "Early Morning (04:00 to 08:00)" and "Morning (08:00 to 12:00)" might be high on other routes of city.
#-- In morning slots, Drivers might be busy serving other routes in the city and hence rides are not easily available.



#--------------------------------------------- Airport to City Analysis -----------------------------------------------




#-- Plot Supply and Demand gap

d <- melt(hoursatoc,id.vars="Request.Date.Hour")
d <-d[!d$variable %in% c("SupplyDemandGap","SupplyDemandGap.per"),]
plotbyhouratoc <- ggplot(d[d$variable %in% c("Demand","Supply"),],aes(x=Request.Date.Hour,y=value,col=variable))+geom_line(size=0.8)
plotbyhouratoc <- plotbyhouratoc+scale_x_continuous(name="Request.Date.Hour",labels = as.character(d$Request.Date.Hour), breaks = d$Request.Date.Hour) 
plotbyhouratoc <- plotbyhouratoc+scale_color_manual(values=color.theme,name="Business Status")
plotbyhouratoc <- plotbyhouratoc+ ggtitle("Airport To City (Supply and Demand)")+labs(y="Trips",x="Hours")+theme_title
plotbyhouratoc

#-- This due to large number of cancellation between 4 a.m to 12 p.m. 
#-- Not Availability is also problem. But cancellation is very high.
#-- For other time slot cancellation is not a problem but Non Availability is Big Issue. 
plotallbyhouratoc <- ggplot(d,aes(x=Request.Date.Hour,y=value,col=variable))+geom_line(size=0.8)
plotallbyhouratoc <- plotallbyhouratoc + scale_x_continuous("Request.Date.Hour",labels = as.character(d$Request.Date.Hour), breaks = d$Request.Date.Hour)
plotallbyhouratoc <- plotallbyhouratoc + scale_color_manual(values=color.theme,name="Business Status")
plotallbyhouratoc <- plotallbyhouratoc + ggtitle("Airport To City (Demand-Supply-Cancellation-Not Availability)")+ylab("Trips")+theme_title
plotallbyhouratoc


#-- See the view by time slots
d <- melt(timeslotatoc,id.vars="RequestedTimeSlot")
d <-d[!d$variable %in% c("SupplyDemandGap","SupplyDemandGap.per"),]
plotbytimeslot.atoc <- ggplot(d,aes(x=factor(RequestedTimeSlot),y=value,fill=variable))
plotbytimeslot.atoc <- plotbytimeslot.atoc +geom_bar(stat='Identity',position = position_dodge(width = 0.8))
plotbytimeslot.atoc <- plotbytimeslot.atoc + scale_fill_manual(values=color.theme,name="Business Status")+theme_title+bar_chart_text
plotbytimeslot.atoc <- plotbytimeslot.atoc + labs(title="Aitport To City ( Business Status by Time Slots)", y="Trips",x="Time Slot")
plotbytimeslot.atoc

grid.arrange(plotbyhouratoc,plotallbyhouratoc,plotbytimeslot.atoc,nrow=3,ncol=1)

#-- Overall Supply and Demand Gap.
hoursatoc %>% summarise(mean(SupplyDemandGap.per))
#-- Supply and Demand Gap. (after removing 5 p.m. to 11 p.m.)
hoursatoc[!hoursatoc$Request.Date.Hour %in% c(17:23),] %>% summarise(mean(SupplyDemandGap.per))
#-- Supply and Demand Gap. (Between 5 p.m. to 11 p.m.)
hoursatoc[hoursatoc$Request.Date.Hour %in% c(17:23),] %>% summarise(mean(SupplyDemandGap.per))

#-- Conclusion:
#-- Based On Graphs.
#-- . Average Supply and demand gap is 41% for whole day. 
#-- . Supply and demand gap is 24.85%, Removing 5 p.m. to 11 p.m. information. 
#-- . But it's very high during 5 p.m. to 11 p.m. 75.37%. This causes overall gap to raise around 41%
#-- . Two time slot "Evening (16:00 to 20:00)" and "Night ( 20:00 to 24:00)"  
#-- . Supply and Demand gap is due to Not Availability of Rides.


#----------------------------------------------- Not Availability Analysis -------------------------------------------------

grid.arrange(plotbyhour,plotbyhouratoc,nrow=2,ncol=1)

#------ Conclusion
#-- From the graph we can clearly see. Number of rides coming to Airport are fully utilized for outgoing trips from Airport.
#-- Not Availability is due to less number of rides available that time slot on Airport.
#-- Reasons behind less number of cars might be as below. 
#-- 1. Not Availability is due to less number of rides available on airport during 4 pm. to 12 pm.
#-- 2. Because of less number of departure from the Airport at night, Rides are not available.  






