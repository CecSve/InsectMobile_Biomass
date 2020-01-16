# Reorganising DK insect biomass data
# Author: Cecilie S. Svenningsen

library(plyr) # splitting data, apply functions and combine data again
library(tidyverse) # data manipulation
library(ggplot2) # graphics
library(digest) # applies a cryptographical hash function to arbitrary R objects
library(forcats) # for working with categorical variables (factors)
library(lubridate) # work with date and time formats
library(here)
library(magrittr)

# Prepare datasets for biomass data
biomass <-
  read.csv(
    here::here("DK_Biomass_January_2020.csv"),
    header = TRUE,
    row.names = NULL,
    sep = ";"
  ) # Biomass data

# Make the columns with biomass data into numeric values instead of factors
cols = c(3:5)
biomass[,cols] %<>% lapply(function(x) as.character(as.factor(x))) %>% lapply(function(x) as.numeric(as.character(x)))

head(biomass)
str(biomass)

# Some samples have not yet been processed and we will exclude those from the analysis
biomass_nozeros <-
  filter(biomass, SampleBiomass_mg > 0) # remove zero from total sample dry weight/biomass

head(biomass_nozeros)
summary(biomass_nozeros)
str(biomass_nozeros)

hist(log(biomass_nozeros$SampleBiomass_mg)) # Normally distributed but we still need to log transform due to a surplus of low weights

# Import metadata
samplingevent <-
  read.csv(
    here::here("DK_SamplingEvent.csv"),
    header = TRUE,
    row.names = NULL,
    sep = ";",
    check.names = FALSE,
    na.strings = c("", " ", "NA")
  )
meta <- samplingevent %>% filter(FullySampled == 'yes') # Filter function to only select the samples that are correctly sampled
meta_select <-  select(meta, 'SampleID', 'LandUSeType', 'RouteURL', 'NewRouteURL', 'Date', 'StartTime', 'EndTime', 'Wind', 'Temperature') %>% droplevels # We choose only the variables we're interested in

summary(meta_select$LandUSeType) # Look at the different types of land use categories and how many samples are present in each category
metadata2 <- subset(meta_select, !is.na(meta_select$LandUSeType)) # subset your data further and removes samples that does not have a landuse type

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
    'agriculture',
    'forest',
    'forest_dry',
    'dry',
    'dry_agriculture',
    'dry_wet',
    'urban',
    'urban_dry_forest',
    'wet',
    'dry_wet'
  )
)

head(metadata2)
tail(metadata2)
str(metadata2)

data <- inner_join(metadata2, biomass_nozeros, by = "SampleID")  # Join the two tables based on row matches in both tables
head(data)

# choose only the clean land use types
data_landuse <-  data %>% filter(LandUSeType == 'agriculture' | LandUSeType == 'forest' | LandUSeType == 'dry' | LandUSeType == 'urban'| LandUSeType =='wet') %>% droplevels

# get summaries of how many samples there is for each variable and their levels
data_landuse %>% group_by(LandUSeType) %>% summarize(count=n()) # count how many samples there is from each coarse land-use category
length(unique(data_landuse[["RouteURL"]])) # count how many routes were sampled - but notice some have received new routes
length(unique(data_landuse[["PID"]])) # count how many pilots that carried out the sampling
data.frame(table(data_landuse$Wind)) # how often were the different wind categories registered
data.frame(table(data_landuse$Temperature)) # how many samples were collected at different temperature intervals
data.frame(table(data_landuse$Date)) # how many samples per day