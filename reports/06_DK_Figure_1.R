# Script for Figure 1 of the biomass paper for DK

# load libraries
library(data.table)
library(sp)
library(tidyverse)
library(raster)

# load merged data
data <-
  read.delim("cleaned-data/DK_mergedData.txt", sep = "\t")

data %>% str()

#format Date
data$Date <- as.Date(data$Date, "%d-%m-%Y")
data$yDay <- yday(data$Date)

#order time band
data$Time_band <- factor(data$Time_band,levels=c("midday","evening"))
#order habitat
data$Land_use <- factor(data$Land_use,levels=c("Urban","Farmland",
                                                         "Dryland","Wetland","Forest"))

# first replace commas with points for decimals
data$utm_x <- sapply(data$utm_x, gsub, pattern = ",", replacement= ".")
data$utm_y <- sapply(data$utm_y, gsub, pattern = ",", replacement= ".")
str(data)

# change from character to numeric
data$utm_x <- as.numeric(data$utm_x)
data$utm_y <- as.numeric(data$utm_y)
