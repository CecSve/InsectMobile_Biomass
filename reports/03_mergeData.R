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

#merge
windData <- melt(windData[,-1],id=c("RouteID","Date"))
names(windData)[which(names(windData)=="value")] <- "Wind"
names(windData)[which(names(windData)=="varible")] <- "Time_band"
