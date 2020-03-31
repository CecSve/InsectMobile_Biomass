# preparing DK landuse data prepared by Jesper Bladt in GIS. README file in H:\Documents\Insektmobilen\Data\Arealanvendelse_Århus\2018_bufferzones_data\Final_buffers_2018 (Cecilie Svenningsens work drive). README should be included in final git submission. 

library(tidyverse)
library(readr)
library(ggplot2)

# load buffer zone files 
oeko <- read.delim("covariate-data/DK_ruter2018_OekoAreas.txt")
hedge <- read.delim("covariate-data/DK_ruter2018_hegnAreas.txt")
'50m' <- read_delim("covariate-data/DK_ruter2018buf50_areas.txt","\t", escape_double = FALSE, trim_ws = TRUE)
'250m' <- read_delim("covariate-data/DK_ruter2018buf250_areas.txt","\t", escape_double = FALSE, trim_ws = TRUE)
'500m' <- read_delim("covariate-data/DK_ruter2018buf500_areas.txt","\t", escape_double = FALSE, trim_ws = TRUE)
'1000m' <- read_delim("covariate-data/DK_ruter2018buf1000_areas.txt","\t", escape_double = FALSE, trim_ws = TRUE)

# load pilotripids and routeids (prepared by Jesper)
tripids <- read.delim("cleaned-data/DK_pilotTripIdToRouteID.txt", sep = ";")

# load metadata
metadata <- read.delim("cleaned-data/DK_rough_landuse_biomass.txt", sep = " ")

# load centroid coordinates for each route
coords <- read.delim("covariate-data/DK_ruter2018_pkt_koordinater.txt", sep = ";")

# load traffic light counts per route
stops <- read.delim("covariate-data/DK_TrafficLightsCount.txt")

# merging data
mergedData <- merge(metadata, tripids, by.x= "SampleID", by.y= "PilotTripID")
setdiff(metadata$SampleID, tripids$PilotTripID) # we lose the sample P61.1B. This is beacause no route has been sent to Jesper even though the sample has been processed in lab

# examining stopping effect (proxy = count of traffic lights on route) on biomass
with_stops <- merge(mergedData, stops, by.x = "RouteID_JB", by.y = "routeID", all = T)
  
# not facetted by landuse type
with_stops %>% mutate(Biomass = replace(Biomass,is.na(Biomass),0)) %>% ggplot(aes(with_stops))+
  geom_point(aes(x=log(Num_trafficLights+1),y=log(Biomass+1)))+
  #facet_wrap(~Land_use)+
  stat_smooth(method="lm",aes(x=log(Num_trafficLights+1),y=log(Biomass+1)))

# by landuse
with_stops %>% mutate(Biomass = replace(Biomass,is.na(Biomass),0)) %>% ggplot(aes(with_stops))+
  geom_point(aes(x=log(Num_trafficLights+1),y=log(Biomass+1)))+
  facet_wrap(~Land_use)+
  stat_smooth(method="lm",aes(x=log(Num_trafficLights+1),y=log(Biomass+1)))

# by landuse - small size fraction
with_stops %>% mutate(Biomass_small = replace(Biomass_small,is.na(Biomass_small),0)) %>% ggplot(aes(with_stops))+
  geom_point(aes(x=log(Num_trafficLights+1),y=log(Biomass_small+1)))+
  facet_wrap(~Land_use)+
  stat_smooth(method="lm",aes(x=log(Num_trafficLights+1),y=log(Biomass_small+1)))

# by landuse - large size fraction
with_stops %>% mutate(Biomass_large = replace(Biomass_large,is.na(Biomass_large),0)) %>% ggplot(aes(with_stops))+
  geom_point(aes(x=log(Num_trafficLights+1),y=log(Biomass_large+1)))+
  facet_wrap(~Land_use)+
  stat_smooth(method="lm",aes(x=log(Num_trafficLights+1),y=log(Biomass_large+1)))


