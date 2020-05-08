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

# load libraries required for reformatting and merging data
library(tidyverse)
library(readr)
library(stringr)
library(data.table)
library(ggpubr)
library(tidyr)

# load buffer zone files 
oeko <- read.delim("covariate-data/DK_ruter2018_OekoAreas.txt")
hedge <- read.delim("covariate-data/DK_ruter2018_hegnAreas.txt")
urbangreen <- read.delim("covariate-data/DK_ruter2018_urbGreenAreas.txt")
buf_50m <- read_delim("covariate-data/DK_ruter2018buf50_areas.txt","\t", escape_double = FALSE, trim_ws = TRUE)
buf_250m <- read_delim("covariate-data/DK_ruter2018buf250_areas.txt","\t", escape_double = FALSE, trim_ws = TRUE)
buf_500m <- read_delim("covariate-data/DK_ruter2018buf500_areas.txt","\t", escape_double = FALSE, trim_ws = TRUE)
buf_1000m <- read_delim("covariate-data/DK_ruter2018buf1000_areas.txt","\t", escape_double = FALSE, trim_ws = TRUE)

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

##### Reformatting environmental data to WIDE format with tidyr #######
# transform oekodata from long to wide format prior to merging 
oekocast <-
  oeko %>% pivot_wider(names_from = bufferDist,
                       values_from = propOeko,
                       names_prefix = "propOeko_")

#test <- dcast(melt(oeko, id.vars=c("routeID", "bufferDist", "propOeko")), routeID~bufferDist+propOeko)

# transform hedgedata and urbangreen from long to wide format prior to merging - note! multiple value columns
hedgecast <-
  hedge %>% pivot_wider(
    names_from = bufferDist,
    values_from = c(hegnLength, byHegnLength, hegnMeterPerHa, byHegnMeterPerHa)
  )

urbangreencast <-
  urbangreen %>% pivot_wider(
    names_from = bufferDist,
    values_from = c(urbGreenAreaHa, urbGreenPropArea)
  )

# buffer zone data - include column with buffer distance for each dataset
buf_50m$bufferDist <- 50
buf_250m$bufferDist <- 250
buf_500m$bufferDist <- 500
buf_1000m$bufferDist <- 1000

castbuf50 <- buf_50m %>% pivot_wider(
     names_from = c(type, bufferDist),
     values_from = c(areaProportion)
  )

# make a noew column with the overall category types
buf_50m <- buf_50m %>% mutate(buffertype=recode(type,
                                     "Ekstensiv"="Agriculture",
                                     "Intensiv"= "Agriculture",
                                     "Markblok" = "Agriculture",
                                     "Semi-intensiv" = "Agriculture",
                                     "Sø" = "Wetland",
                                     "Mose" = "Wetland",
                                     "Strandeng" = "Wetland",
                                     "Eng" = "Wetland",
                                     "Hede" = "Dryland",
                                     "Overdrev" = "Dryland",
                                     "Lav bebyggelse" = "Urban",
                                     "Erhverv" = "Urban",
                                     "Høj bebyggelse" = "Urban",
                                     "Bykerne" = "Urban",
                                     "Skov" = "Forest",
                                     "Andet" = "Andet"))

castbuf50_opt1 <- buf_50m %>% pivot_wider(
  names_from = c(buffertype, bufferDist),
  values_from = c(areaProportion)
)

castbuf50_opt2 <- buf_50m %>% pivot_wider(
  names_from = c(type, bufferDist),
  values_from = c(areaProportion)
)

# continue with castbuf50 to make a reduced dataset
colnames(castbuf50)

castbuf50$Agriculture_50 <-
  castbuf50 %>% select(Ekstensiv_50, Intensiv_50, Markblok_50, `Semi-intensiv_50`) %>% rowSums(na.rm = T)

castbuf50$Wetland_50 <-
  castbuf50 %>% select(`Sø_50`, `Mose_50`, Strandeng_50, Eng_50) %>% rowSums(na.rm = T)

castbuf50$Dryland_50 <-
  castbuf50 %>% select(Hede_50, Overdrev_50) %>% rowSums(na.rm = T)

castbuf50$Urban_50 <-
  castbuf50 %>% select(`Lav bebyggelse_50`,
                       Erhverv_50,
                       `Høj bebyggelse_50`,
                       Bykerne_50) %>% rowSums(na.rm = T)

castbuf50 <- dplyr::rename(castbuf50, Forest_50 = Skov_50)

castbuf50_reduced <-
  castbuf50 %>% select(routeID,
                       afgkode,
                       afgDK,
                       Forest_50,
                       Agriculture_50,
                       Wetland_50,
                       Urban_50,
                       Dryland_50)

### MERGING - work in progress #####

# merge buffer data with mergeddata - without casting, not sure if we loose data by this
insectData <- merge(mergedData, hedge, by.x= "RouteID_JB", by.y= "routeID")
insectData2 <- merge(insectData, oeko, by.x= c("RouteID_JB", "bufferDist"), by.y= c("routeID", "bufferDist"))
insectData3 <- merge(insectData2, urbangreen, by.x= c("RouteID_JB", "bufferDist"), by.y= c("routeID", "bufferDist"))

# create a dataframe where each buffer category and mergeData are combined
buf_1000m$bufferDist <- 1000
buf_1000m <- buf_1000m %>% rename(RouteID_JB = routeID)
test <- merge(buf_1000m, insectData3, by = c("RouteID_JB", "bufferDist"), all.y = T)

buf_500m$bufferDist <- 500
buf_500m <- buf_500m %>% rename(RouteID_JB = routeID)
test2 <- merge(buf_500m, test, by = c("RouteID_JB", "type", "afgkode", "afgDK", "areaProportion", "bufferDist"), all.y = T)

buf_250m$bufferDist <- 250
buf_250m <- buf_250m %>% rename(RouteID_JB = routeID)
test3 <- merge(buf_250m, test2,  by = c("RouteID_JB", "bufferDist", "type", "afgkode", "afgDK", "areaProportion", "bufferDist"), all.y = T)

buf_50m$bufferDist <- 50
buf_50m <- buf_50m %>% rename(RouteID_JB = routeID)
allInsects <- merge(buf_50m, test3, by = c("RouteID_JB", "bufferDist", "type", "afgkode", "afgDK", "areaProportion", "bufferDist"), all.y = T)


