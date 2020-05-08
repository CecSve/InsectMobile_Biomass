# preparing DK landuse data prepared by Jesper Bladt in GIS. README file in H:\Documents\Insektmobilen\Data\Arealanvendelse_Århus\2018_bufferzones_data\Final_buffers_2018 (Cecilie Svenningsens work drive). README should be included in final git submission. 

library(tidyverse)
library(readr)
library(stringr)
library(data.table)
library(ggpubr)

# reformat pilottriptorouteids and rough_landuse.. to tab separated columns for merging lists in the end of the document
DK_pilotTripIdToRouteID <- read.delim("H:/Documents/Insektmobilen/Analysis/InsectMobile_Biomass/cleaned-data/DK_pilotTripIdToRouteID.txt")
#write.table(DK_pilotTripIdToRouteID, file = "cleaned-data/DK_pilotTripIdToRouteID.txt", sep = "\t")

DK_rough_landuse_biomass <- read.csv("H:/Documents/Insektmobilen/Analysis/InsectMobile_Biomass/cleaned-data/DK_rough_landuse_biomass.txt", sep="")
#write.table(DK_rough_landuse_biomass, file = "cleaned-data/DK_rough_landuse_biomass.txt", sep = "\t")

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

# load metadata
metadata <- read.delim("cleaned-data/DK_rough_landuse_biomass.txt", sep = "\t")

# load centroid coordinates for each route
coords <- read.delim("covariate-data/DK_ruter2018_pkt_koordinater.txt")

# load traffic light counts per route
tfstops <- read.delim("covariate-data/DK_TrafficLightsCount.txt")

# merging data by new routeIDs (RouteID_JB) so other data can be merged as well
mergedData <- merge(metadata, tripids, by.x= "SampleID", by.y= "PilotTripID")
setdiff(metadata$SampleID, tripids$PilotTripID) # all metadatasamples are included - yay!

# adding stopping effect (proxy = count of traffic lights on route) 
with_tfstops <- merge(mergedData, tfstops, by.x = "RouteID_JB", by.y = "routeID", all = T)

# removing samples from stops that don't have biomass
mergedData <- with_tfstops %>% drop_na(SampleID)
mergedData <- mergedData %>% mutate(Num_trafficLights = replace(Num_trafficLights,is.na(Num_trafficLights),0)) # recode NAs to zeros for number of traffic ligths on the routes

# add all stops
allstops <- read.delim("covariate-data/DK_ruter2018_countStops.txt")
with_allstops <- merge(mergedData, allstops, by.x = "RouteID_JB", by.y = "routeId2018", all = T)

# removing samples from stops that don't have biomass
mergedData <- with_allstops %>% drop_na(SampleID)

# adding utm coordinates for route centroids
mergedData <- merge(mergedData, coords, by.x= "RouteID_JB", by.y= "routeID")
mergedData <- select(mergedData, -OBJECTID) # remove objectid column since it is not needed  

write.table(mergedData, file = "cleaned-data/DK_mergedData.txt", sep = "\t") # save updated metadata that now contains number of traffic lights on routes + all stops and centroid coordinates for each route

# merge buffer data with mergeddata
hedgeData <- merge(mergedData, hedge, by.x= "RouteID_JB", by.y= "routeID")
oekoData <- oeko %>% rename(RouteID_JB = routeID) %>% inner_join(mergedData, oeko, by = c("RouteID_JB"))
urbangreenData <- urbangreen %>% rename(RouteID_JB = routeID) %>% inner_join(mergedData, urbangreen, by = c("RouteID_JB"))

# save output
write.table(hedgeData, file = "cleaned-data/DK_hedgeData.txt", sep = "\t")
write.table(oekoData, file = "cleaned-data/DK_oekoData.txt", sep = "\t")
write.table(urbangreenData, file = "cleaned-data/DK_urbangreenData.txt", sep = "\t")

# create a dataframe where each buffer category and mergeData are combined
data_50m <- buf_50m %>% rename(RouteID_JB = routeID) %>% inner_join(mergedData, buf_50m, by = c("RouteID_JB"))
data_250m <- buf_250m %>% rename(RouteID_JB = routeID) %>% inner_join(mergedData, buf_250m, by = c("RouteID_JB"))
data_500m <- buf_500m %>% rename(RouteID_JB = routeID) %>% inner_join(mergedData, buf_500m, by = c("RouteID_JB"))
data_1000m <- buf_1000m %>% rename(RouteID_JB = routeID) %>% inner_join(mergedData, buf_1000m, by = c("RouteID_JB"))

# save output
write.table(data_50m, file = "cleaned-data/DK_landusedata_50m.txt", sep = "\t")
write.table(data_250m, file = "cleaned-data/DK_landusedata_250m.txt", sep = "\t")
write.table(data_500m, file = "cleaned-data/DK_landusedata_500m.txt", sep = "\t")
write.table(data_1000m, file = "cleaned-data/DK_landusedata_1000m.txt", sep = "\t")

# combine data
setwd("H:/Documents/Insektmobilen/Analysis/InsectMobile_Biomass/cleaned-data/")

list_of_files <- list.files(recursive = TRUE,
                            pattern = "\\.txt$", 
                            full.names = F)

data <- lapply(list_of_files, read.table, sep = "\t") # create a vector with the dataframes/lists 
names(data) <- stringr::str_replace(list_of_files, pattern = ".txt", replacement = "")

# save data
saveRDS(data, "DK_allLanduseData.Rds")
