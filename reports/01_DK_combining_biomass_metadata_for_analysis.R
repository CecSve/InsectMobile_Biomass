# Reorganising DK insect biomass data
# Author: Cecilie S. Svenningsen

library(plyr) # splitting data, apply functions and combine data again
library(tidyverse) # data manipulation
library(ggplot2) # graphics
library(digest) # applies a cryptographical hash function to arbitrary R objects
library(forcats) # for working with categorical variables (factors)
library(lubridate) # work with date and time formats
#library(here)
library(magrittr)

setwd("H:/Documents/Insektmobilen/Analysis/InsectMobile_Biomass")

# Prepare datasets for biomass data - should be updated to this code when analysis has finished
#biomass <-read.csv(here::here("raw-data/DK_Biomass_January_2020.csv"),header = TRUE,row.names = NULL,sep = ";") # Biomass data

biomass <-
  read.csv(
    file = "raw-data/Biomass_January_2020.csv",
    header = TRUE,
    row.names = NULL,
    sep = ";"
  )

routeid <-   read.table(
  file = "raw-data/DK_routeID.txt",
  header = TRUE,
  row.names = NULL, sep = ""
)

# Make the columns with biomass data into numeric values instead of factors
cols = c(3:5)
biomass[, cols] %<>% lapply(function(x)
  as.character(as.factor(x))) %>% lapply(function(x)
    as.numeric(as.character(x)))

str(biomass)

# Some samples have not yet been processed and we will exclude those from the analysis
biomass_nozeros <-
  filter(biomass, SampleBiomass_mg > 0) # remove zero from total sample dry weight/biomass

head(biomass_nozeros)
summary(biomass_nozeros)
str(biomass_nozeros)

hist(log(biomass_nozeros$SampleBiomass_mg)) # Normally distributed but we still need to log transform due to a surplus of low weights

# Import metadata - change to here::here when analysis is ready to be published
samplingevent <-
  read.csv(
    file = "raw-data/SamplingEvent.csv",
    header = TRUE,
    row.names = NULL,
    sep = ";",
    check.names = FALSE,
    na.strings = c("", " ", "NA")
  )

meta <- samplingevent %>% filter(FullySampled == 'yes') # Filter function to only select the samples that are correctly sampled
meta_select <-
  select(
    meta,
    'SampleID',
    'LandUSeType',
    'RouteURL',
    'NewRouteURL',
    'Date',
    'StartTime',
    'EndTime',
    'Wind',
    'Temperature'
  ) %>% droplevels # We choose only the variables we're interested in

summary(meta_select$LandUSeType) # Look at the different types of land use categories and how many samples are present in each category
metadata2 <- subset(meta_select,!is.na(meta_select$LandUSeType)) # subset your data further and removes samples that does not have a landuse type
summary(metadata2$LandUSeType)
levels(metadata2$LandUSeType)

# Rename the values from Danish to English
metadata2$LandUSeType <- mapvalues(
  metadata2$LandUSeType,
  from = c(
    'mark',
    'skov',
    'skov, tør',
    'tør',
    'tør, mark',
    'tør, våd',
    'urban',
    'urban, tør, skov',
    'våd',
    'våd, tør'
  ),
  to = c(
    'Farmland',
    'Forest',
    'Forest_dry',
    'Dryland',
    'Dry_agriculture',
    'Dry_wet',
    'Urban',
    'Urban_dry_forest',
    'Wetland',
    'Dry_wet'
  )
)

head(metadata2)
tail(metadata2)
str(metadata2)

data <- inner_join(metadata2, biomass_nozeros, by = "SampleID")  # Join the two tables based on row matches in both tables
head(data)

# choose only the clean land use types
data_landuse <-
  data %>% filter(
    LandUSeType == 'Farmland' |
      LandUSeType == 'Forest' |
      LandUSeType == 'Dryland' |
      LandUSeType == 'Urban' | LandUSeType == 'Wetland'
  ) %>% droplevels

data_landuse$LandUSeType <-
  factor(
    data_landuse$LandUSeType,
    levels = c("Urban", "Farmland", "Dryland", "Wetland", "Forest")
  )

# get summaries of how many samples there is for each variable and their levels
data_landuse %>% group_by(LandUSeType) %>% summarize(count = n()) # count how many samples there is from each coarse land-use category
length(unique(data_landuse[["RouteURL"]])) # count how many routes were sampled - but notice some have received new routes
length(unique(data_landuse[["PID"]])) # count how many pilots that carried out the sampling
length(unique(data_landuse[["SampleID"]])) # count how many samples
data.frame(table(data_landuse$Wind)) # how often were the different wind categories registered
data.frame(table(data_landuse$Temperature)) # how many samples were collected at different temperature intervals
data.frame(table(data_landuse$Date)) # how many samples per day

# change column header to macth DE variable names and drop google maps route links
data <-
  data_landuse %>% select(-RouteURL,-NewRouteURL) %>% rename(
    Land_use = LandUSeType,
    Biomass_small = DryMassSmall_mg,
    Biomass_large = DryMassLarge_mg,
    Biomass = SampleBiomass_mg,
    PilotID = PID
)

# make a column for whether sampling was midday or evening
time <- as.POSIXct(strptime(data$StartTime, "%H:%M"), "UTC")
x = as.POSIXct(strptime(c("120000", "150000", "170000", "195858"), "%H%M%S"), "UTC")
data$Time_band <-
  case_when(between(time, x[1], x[2]) ~ "midday", between(time, x[3], x[4]) ~
              "evening")

# adding a column for route length
data$Route_length <- '5000'

# Add Distance_driven which is 10000 for DK data
data$Distance_driven <- '10000'

# Add 2018 RouteID
data <- left_join(data, routeid, by = "SampleID")

# Change the wind types to Light, Gentle, Moderate, so without breeze and numbers
data <- data %>% mutate(Wind=recode(Wind, 
                                 "Light Breeze 1.6-3.3"="Light breeze",
                                 "Gentle breeze 3.4-5.5"="Gentle breeze",
                                 "Moderate breeze 5.5-7.9"="Moderate breeze"))

# Retrieve mean and max temperature, mean humidity or something similar, average wind speed - DMI

# Centorid x, y of bird atlas quadrant UTM meters for spatial correlation - Jesper

# Add time driven column Time_driven
# convert the time columns to datetimes
test$StartTime <- as.POSIXct(data$StartTime,
                                      format='%H:%M:%S')
test$EndTime   <- as.POSIXct(data$EndTime,
                                      format='%H:%M:%S')

data$Time_driven <- difftime(test$EndTime, test$StartTime, units = "mins") 

# setting route_length and distance_driven as numeric values
data$Route_length <- as.double(data$Route_length)
data$Distance_driven <- as.double(data$Distance_driven)

# Add Velocity (Route_length*2)/Time_driven - we think it could account for some of the variation between samples, especially urban (many stops)
data$Velocity <- (data$Route_length*2)/data$Time_driven

write.table(data, file = "cleaned-data/DK_rough_landuse_biomass.txt")
