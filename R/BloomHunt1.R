library(dplyr)
dat_q <- load("../watershedWatch/R/data2003.lakes.rda")
data2003.lakes <- data2003.lakes
## Use all data where secchi and chl a are collected on the same date
chl <- filter(data2003.lakes, ParShortName == "Chlorophyll a" & DepthFromSurface == 1)
secchi <- filter(data2003.lakes, ParShortName == "Secchi Depth")
temp <- filter(data2003.lakes, ParShortName == "Temperature" & DepthFromSurface == 1)
##
colsKeep <- c("ResultID","Concentration","StartDate","StationName",
              "StationLocation", "MaxDepth","Latitude","Longitude","DepthFromSurface")
chlX <- chl[, colsKeep]
secchiX <- secchi[, colsKeep]
tempX <- temp[, colsKeep]
## There are no duplicates of ResultID, but there are of StartDate
## Remove ResultID so we can take the mean of duplicate samples
chlY <- chlX[,-1]
secchiY <- secchiX[,-1]
tempY <- tempX[,-1]
## Remove NA concentrations
chlZ <- chlY[!is.na(chlY$Concentration),]
secchiZ <- secchiY[!is.na(secchiY$Concentration),]
tempZ <- tempY[!is.na(tempY$Concentration),]
## Find multiples on same date
## First, remove time of day component
#install.packages("lubridate")
#Whoops!  Watch Time Zones! jwh used as_date
library(lubridate)
chlZ$date <- as_date(chlZ$StartDate)
secchiZ$date <- as_date(secchiZ$StartDate)
tempZ$date <- as_date(tempZ$StartDate)
##
chlA <- group_by(chlZ, StationName, date)
secchiA <- group_by(secchiZ, StationName, date)
tempA <- group_by(tempZ, StationName, date)
chlMeans <- summarize(chlA, chl=mean(Concentration), sd=sd(Concentration), n=n(),
                      pond=unique(StationLocation), maxD = unique(MaxDepth),
                      lat=unique(Latitude), long=unique(Longitude))
tempMeans <- summarize(tempA, temp=mean(Concentration), sd=sd(Concentration), n=n(),
                      pond=unique(StationLocation), maxD = unique(MaxDepth),
                      lat=unique(Latitude), long=unique(Longitude))
secchiMeans <- summarize(secchiA, secchi=mean(Concentration), sd=sd(Concentration), n=n(),
                      pond=unique(StationLocation), maxD = unique(MaxDepth),
                      lat=unique(Latitude), long=unique(Longitude))
## Combine all data
chlSecchi <- merge(chlMeans,secchiMeans,by=c("StationName","date","pond","maxD",
                                             "lat","long"), all=TRUE)
chlSecchiT <- merge(chlSecchi,tempMeans, by=c("StationName","date","pond","maxD",
                                              "lat","long"),all=TRUE)
colsKeep <- c("StationName","date","pond","maxD","lat","long","chl","secchi","temp")
chlSecchiT <- chlSecchiT[,colsKeep]

