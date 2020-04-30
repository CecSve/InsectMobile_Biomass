#read in Danish data
insectsDK <- read.delim("cleaned-data/DK_rough_landuse_biomass.txt",sep=" ")
insectsDK$Country <- "Denmark"


#read in German data
insectsDE <- read.delim("cleaned-data/DE_rough_landuse_biomass.txt")
insectsDE$Country <- "Germany"

#common columns
myVars <- c("Land_use","Time_band","Country",
            "Biomass","Biomass_small","Biomass_large",
            "Date")


allInsects <- rbind(insectsDK[,myVars],
                    insectsDE[,myVars])


#format Date
library(lubridate)
allInsects$Date <- as.Date(allInsects$Date)
allInsects$yDay <- yday(allInsects$Date)

#order time band
allInsects$Time_band <- factor(allInsects$Time_band,levels=c("midday","evening"))
#order habitat
allInsects$Land_use <- factor(allInsects$Land_use,levels=c("Urban","Farmland",
                                                           "Dryland","Wetland","Forest"))

