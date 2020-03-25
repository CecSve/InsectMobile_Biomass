### Landuse intensity analysis for Danish data. First part incorporates parts of the 03_mergeData script  

library(lubridate)
library(ggplot2)
library(plyr)
library(ggpirate)
library(cowplot)
library(wesanderson)

# load data
insectsDK <- read.delim("cleaned-data/DK_rough_landuse_biomass.txt",sep=" ")
str(insectsDK)

#format Date
insectsDK$Date <- as.Date(insectsDK$Date, "%d-%m-%Y")
insectsDK$yDay <- yday(insectsDK$Date)

#order time band
insectsDK$Time_band <- factor(insectsDK$Time_band,levels=c("midday","evening"))
#order habitat
insectsDK$Land_use <- factor(insectsDK$Land_use,levels=c("Urban","Farmland",
                                                           "Dryland","Wetland","Forest"))
