#read in Danish data
insectsDK <- read.delim("cleaned-data/DK_rough_landuse_biomass.txt",sep=" ")
insectsDK$Country <- "Denmark"


#read in German data
insectsDE <- read.delim("cleaned-data/DE_rough_landuse_biomass.txt")
insectsDE$Country <- "Germany"

# #common columns
# myVars <- c("Land_use","Time_band","Country",
#             "Biomass","Biomass_small","Biomass_large",
#             "Date")
# 
# 
# allInsects <- rbind(insectsDK[,myVars],
#                     insectsDE[,myVars])


####merge all german files ###########################################################

allInsects <- insectsDE

library(lubridate)
library(reshape2)

#format Date
allInsects$Date <- as.Date(allInsects$Date)
allInsects$yDay <- yday(allInsects$Date)

#order time band
allInsects$Time_band <- factor(allInsects$Time_band,levels=c("midday","evening"))
#order habitat
allInsects$Land_use <- factor(allInsects$Land_use,levels=c("Urban","Farmland",
                                                           "Dryland","Wetland","Forest"))

#add on environmental data
environData <- read.delim("cleaned-data/environData_DE.txt",as.is=T)
windData <- read.delim("cleaned-data/routeWind_DE.txt",as.is=T)
tempData <- read.delim("cleaned-data/routeTemps_DE.txt",as.is=T)
stopData <- read.delim("cleaned-data/stops_DE.txt",as.is=T)
tlData <- read.delim("cleaned-data/trafficlights_DE.txt",as.is=T)

#merge weather
windData <- melt(windData[,-1],id=c("RouteID","Date"))
names(windData)[which(names(windData)=="value")] <- "Wind"
names(windData)[which(names(windData)=="variable")] <- "Time_band"
windData$Time_band <- as.character(windData$Time_band)
windData$Time_band[which(windData$Time_band=="middayTemp")] <- "midday"
windData$Time_band[which(windData$Time_band=="eveningTemp")] <- "evening"

tempData <- melt(tempData[,-1],id=c("RouteID","Date"))
names(tempData)[which(names(tempData)=="value")] <- "Temp"
names(tempData)[which(names(tempData)=="variable")] <- "Time_band"
tempData$Time_band <- as.character(tempData$Time_band)
tempData$Time_band[which(tempData$Time_band=="middayTemp")] <- "midday"
tempData$Time_band[which(tempData$Time_band=="eveningTemp")] <- "evening"

weatherData <- merge(tempData,windData,by=c("RouteID","Date","Time_band"))
allInsects <- allInsects[,-which(names(allInsects) %in% c("Wind","Temperature"))]
allInsects2 <- merge(allInsects,weatherData,by=c("RouteID","Date","Time_band"),all.x=T)
subset(allInsects2,is.na(Temp))
#Gruen_09 2018-07-04 is missing - but I did this!!! Grr

#insert mean values for now
mean(allInsects2$Temp[allInsects$Time_band=="midday"],na.rm=T)#24.42077
mean(allInsects2$Wind[allInsects$Time_band=="midday"],na.rm=T)#4.163846
mean(allInsects2$Temp[allInsects$Time_band=="evening"],na.rm=T)#23.01231
mean(allInsects2$Wind[allInsects$Time_band=="evening"],na.rm=T)#3.693846
allInsects2$Temp[is.na(allInsects2$Temp)] <- c(24.42077,23.01231)
allInsects2$Wind[is.na(allInsects2$Wind)] <- c(4.163846,3.693846)
allInsects <- allInsects2

#land use data
allInsects <- merge(allInsects,environData,by.x="RouteID",by.y="Codierung",all.x=T)

#stop data
stopData <- merge(tlData,stopData,by="Codierung",all=T)
names(stopData)[which(names(stopData)=="osm")] <- "stops"
allInsects <- merge(allInsects,stopData,by.x="RouteID",by.y="Codierung",all.x=T)

####Denmark??