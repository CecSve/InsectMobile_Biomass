#read in Danish data
insectsDK <- read.delim("cleaned-data/DK_rough_landuse_biomass.txt",sep="\t")
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


####merge all German files ###########################################################

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

####merge all Danish files ###########################################################
allInsects <- insectsDK

# load libraries to reformat date and time
library(lubridate)
library(reshape2)

#format Date
allInsects$Date <- as.Date(allInsects$Date, "%d-%m-%Y")
allInsects$yDay <- yday(allInsects$Date)

#order time band
allInsects$Time_band <- factor(allInsects$Time_band,levels=c("midday","evening"))
#order habitat
allInsects$Land_use <- factor(allInsects$Land_use,levels=c("Urban","Farmland",
                                                           "Dryland","Wetland","Forest"))

# load pilotripids and routeids (prepared by Jesper)
tripids <- read.delim("cleaned-data/DK_pilotTripIdToRouteID.txt")

# load centroid coordinates for each route
coords <- read.delim("covariate-data/DK_ruter2018_pkt_koordinater.txt")

# load traffic light counts per route
tfstops <- read.delim("covariate-data/DK_TrafficLightsCount.txt")

# merging data by new routeIDs (RouteID_JB) so other data can be merged as well
mergedData <-
  merge(allInsects, tripids, by.x = "SampleID", by.y = "PilotTripID")
setdiff(allInsects$SampleID, tripids$PilotTripID) # all metadatasamples are included - yay!

# adding stopping effect (proxy = count of traffic lights on route)
with_tfstops <-
  merge(mergedData,
        tfstops,
        by.x = "RouteID_JB",
        by.y = "routeID",
        all = T)

# removing samples from stops that don't have biomass
mergedData <- with_tfstops %>% drop_na(SampleID)
mergedData <-
  mergedData %>% mutate(Num_trafficLights = replace(Num_trafficLights, is.na(Num_trafficLights), 0)) # recode NAs to zeros for number of traffic ligths on the routes

# add all stops
allstops <- read.delim("covariate-data/DK_ruter2018_countStops.txt")
with_allstops <-
  merge(mergedData,
        allstops,
        by.x = "RouteID_JB",
        by.y = "routeId2018",
        all = T)

# removing samples from stops that don't have biomass
mergedData <- with_allstops %>% drop_na(SampleID)

# adding utm coordinates for route centroids
mergedData <-
  merge(mergedData, coords, by.x = "RouteID_JB", by.y = "routeID")
mergedData <-
  select(mergedData,-OBJECTID) # remove objectid column since it is not needed  

#add on environmental data
environData <- read.delim("cleaned-data/environData_DK.txt",as.is=T)

# merge land use intensity data with merged data
allInsects <- merge(mergedData,environData,by.x="RouteID_JB",by.y="routeID",all.x=T)
