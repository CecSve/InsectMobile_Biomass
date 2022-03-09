#read in Danish data
insectsDK <- read.delim("cleaned-data/DK_rough_landuse_biomass.txt",sep="\t")
insectsDK$Country <- "Denmark"

# skip to the section 'merge all Danish files for the Danis analysis'

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

#Agrar 18 PM has missing time data - KJC..

#add on environmental data
environData <- read.delim("cleaned-data/environData_DE.txt",as.is=T)
windData <- read.delim("cleaned-data/routeWind_DE.txt",as.is=T)
tempData <- read.delim("cleaned-data/routeTemps_DE.txt",as.is=T)
stopData <- read.delim("cleaned-data/stops_DE.txt",as.is=T)
tlData <- read.delim("cleaned-data/trafficlights_DE.txt",as.is=T)
luiData <- read.delim("cleaned-data/landuseIntensity_DE.txt",as.is=T)

#merge weather
windData <- reshape2::melt(windData[,-1],id=c("RouteID","Date"))
names(windData)[which(names(windData)=="value")] <- "Wind"
names(windData)[which(names(windData)=="variable")] <- "Time_band"
windData$Time_band <- as.character(windData$Time_band)
windData$Time_band[which(windData$Time_band=="middayTemp")] <- "midday"
windData$Time_band[which(windData$Time_band=="eveningTemp")] <- "evening"

tempData <- reshape2::melt(tempData[,-1],id=c("RouteID","Date"))
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

#land use intensity data
allInsects <- merge(allInsects,luiData,by.x="RouteID",by.y="Codierung",all.x=T)

#stop data
stopData <- merge(tlData,stopData,by="Codierung",all=T)
names(stopData)[which(names(stopData)=="osm")] <- "stops"
allInsects <- merge(allInsects,stopData,by.x="RouteID",by.y="Codierung",all.x=T)

#sort time data to standard each around the time band
allInsects$numberTime <- as.numeric(hm(allInsects$StartTime))#DE

####merge all Danish files ###########################################################
allInsects <- insectsDK

# load libraries to reformat date and time
library(lubridate)
library(reshape2)
library(tidyverse)

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
coords <- read.delim("covariate-data/DK_ruter2018_pkt_koordinater.txt", dec = ",")

# load traffic light counts per route
tfstops <- read.delim("covariate-data/DK_TrafficLightsCount.txt")

# merging data by new routeIDs (RouteID_JB) so other data can be merged as well
mergedData <-
  merge(allInsects, tripids, by.x = "SampleID", by.y = "PilotTripID")
setdiff(allInsects$SampleID, tripids$PilotTripID) # all metadata samples are included - yay!

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

# first replace commas with points for decimals
mergedData$utm_x <- sapply(mergedData$utm_x, gsub, pattern = ",", replacement= ".")
mergedData$utm_y <- sapply(mergedData$utm_y, gsub, pattern = ",", replacement= ".")
str(mergedData)

# change from character to numeric
mergedData$utm_x <- as.numeric(mergedData$utm_x)
mergedData$utm_y <- as.numeric(mergedData$utm_y)
# a land use category has been made in script 02 based on land use intensity along the routes so the Land_use column will be changed to roughLand_use and the new categories will be called Land_use
mergedData <- plyr::rename(mergedData, c("Land_use" = "roughLand_use"))

mergedData <-
  mergedData %>% mutate(
    Land_use = recode(
      roughLand_use,
      "Forest" = "Forest",
      "Urban" = "Urban",
      "Dryland" = "Open uncultivated land",
      "Wetland" = "Wetland",
      "Farmland" = "Agriculture"
    )
  )

# rename stop columns to match DE names
mergedData <- plyr::rename(mergedData, c("Num_trafficLights" = "tr_signals"))
mergedData <- plyr::rename(mergedData, c("COUNT_STOPS" = "stops"))

#add on environmental data
environData <- read.delim("cleaned-data/environData_DK.txt",as.is=T)

# merge land cover data with merged data
allInsects <- merge(mergedData,environData,by.x="RouteID_JB",by.y="routeID",all.x=T)

# add land use intensity for urban and agriculture
landuseUrban <- read.delim("cleaned-data/urban_landuse_intensity_DK.txt",as.is=T)
landuseFarmland <- read.delim("cleaned-data/farmland_landuse_intensity_DK.txt",as.is=T)
landuseWetland <- read.delim("cleaned-data/wetland_landuse_intensity_DK.txt",as.is=T)

# add to allInsects data
allInsects <- merge(allInsects,landuseUrban,by.x="RouteID_JB",by.y="routeID",all.x=T)
allInsects <- merge(allInsects,landuseFarmland,by.x="RouteID_JB",by.y="routeID",all.x=T)
allInsects <- merge(allInsects,landuseWetland,by.x="RouteID_JB",by.y="routeID",all.x=T)

# add two columns for the 1000 buffer for land use intensity analysis, where the land use with highest proportion is added and the corresponding areaProp is listed
allInsects <- allInsects %>% 
  rownames_to_column('id') %>%
  left_join(
    allInsects %>% 
      rownames_to_column('id') %>%
      tidyr::gather(maxLand_use, maxareaProp, Agriculture_1000:Wetland_1000) %>% 
      group_by(id) %>% 
      slice(which.max(maxareaProp)), 
    by = 'id'
  )

# joining introduced .y and .x to headers and should be removed
allInsects <- allInsects[, -grep(".y$", colnames(allInsects))]
names(allInsects) <- gsub(".x","",names(allInsects),fixed = TRUE)
allInsects <- column_to_rownames(allInsects, var = "id")

#sort time data to standard each around the time band
allInsects$numberTime <- as.numeric(hms(allInsects$StartTime))#Denmark

#testing 
rnorm(10)
###sort vars###############################################################################

#centering
allInsects$cyDay <- allInsects$yDay - median(allInsects$yDay)
allInsects$cStops <- log(allInsects$stops+1) - median(log(allInsects$stops+1))
allInsects$cTL <- log(allInsects$tr_signals+1) - median(log(allInsects$tr_signals+1))

#transform to minutes
allInsects$numberTime <- allInsects$numberTime/60 

#centre time around each time band
middayMean <- median(allInsects$numberTime[allInsects$Time_band=="midday"],na.rm=T)#23 for DE, 37.5 for DK
eveningMean <- median(allInsects$numberTime[allInsects$Time_band=="evening"],na.rm=T)#69.5 for DK, 124 for DK

allInsects$cnumberTime <- NA
allInsects$cnumberTime[allInsects$Time_band=="midday"] <- allInsects$numberTime[allInsects$Time_band=="midday"] -middayMean
allInsects$cnumberTime[allInsects$Time_band=="evening"] <- allInsects$numberTime[allInsects$Time_band=="evening"] -eveningMean

#set missing values to mean of time band
allInsects$cnumberTime[is.na(allInsects$cnumberTime)]<- 0

# write output
write.table(allInsects, file = "cleaned-data/DK_allInsects.txt", col.names = T, row.names = F, sep = "\t",dec = ".")

#centering other land use variables
# centreVars<-function(df){
#   require(tidyverse)
#   centring <- function(x)scale(x,scale=F) #this will just centre the variables
#   newd <- df %>% 
#     mutate(across(contains("0"),centring))
#   names(newd) <- sapply(names(newd),function(x)paste0("c",x))
#   df2 <- cbind(df,newd)
#   df2
# }
# 
# allInsects <- centreVars(allInsects)
