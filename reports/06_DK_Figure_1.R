# Script for Figure 1 of the biomass paper for DK

# load libraries
library(data.table)
library(sp)
library(tidyverse)
library(raster)
library(rgdal)
library(maps)
library(mapproj)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggplot2)
library(sf)
library(rnaturalearthhires)

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

# extract Latitude and Longitude variables and put them into simple data frame called lat.long.df.
lat.long.df <- data.frame(data$utm_x, data$utm_y) 
str(lat.long.df)

coordinates(lat.long.df) <-  ~data.utm_x + data.utm_y #Function coordinates() creates a spatial object
str(lat.long.df) # at this point, this dataset doesn’t have CRS. Spatial data can’t exist without CRS(Coordinates Reference System). Spatial data requires, at least, coordinates and CRS.

proj4string(lat.long.df) <- CRS("+init=epsg:25832")
#this is +proj=utm +zone=32 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs 

head(lat.long.df)

# Now, dataset has coordinates and CRS. Next thing is to convert this to Longitude-Latitude data.
dist.location <- spTransform(lat.long.df, CRS("+init=epsg:4326"))
dist.location

landuse.map <- 
  data.frame(Landuse = data$Land_use,
             lat = dist.location$data.utm_x,
             long = dist.location$data.utm_y)

world.map <- map_data ("world") # not a good base
worldmap <- ne_countries(scale = 'large', type = 'map_units',
                         returnclass = 'sf')
ggplot() + geom_sf(data = worldmap) + theme_bw()

DK.map <- world.map %>% filter(region == "Denmark")

denmark <- worldmap[worldmap$name == 'Denmark',]
ggplot() + geom_sf(data = denmark) + theme_bw() # the island of Bornholm distorts the details, so the map should be cropped

denmark_cropped <- st_crop(denmark, xmin = 8, xmax = 13,
                          ymin = 54.5, ymax = 58)
ggplot() + geom_sf(data = denmark_cropped) + theme_bw()

landuse.map %>% head()
main.landuse.map <- landuse.map[landuse.map$lat < 13, ]
main.data <- data[data$utm_x < 800000, ]

# one way of plotting
DK.map %>%
  ggplot() + 
  geom_map(map = DK.map, 
           aes(x = long, y = lat, map_id = region),
           fill="white", colour = "black") + 
  coord_map() + 
  geom_point(data = landuse.map, 
             aes(x=lat, y = long, colour = data$Land_use), alpha = 0.9, size=2, show.legend = T) + theme_void() + theme(legend.title = element_blank())

# whole of Denmark with better resolution (one plot in the ocean is on the island of Taasinge which is not included in this resolution)
denmark %>%
  ggplot() + 
  geom_sf(data = denmark, 
           fill="white", colour = "black") + 
  coord_sf() + 
  geom_point(data = landuse.map, 
             aes(x=lat, y = long, colour = data$Land_use), alpha = 0.9, size=3, show.legend = T) + theme_void() + theme(legend.title = element_blank())

# another way of plotting - without the coordinates for Bornholm - not necessary
denmark_cropped %>%
  ggplot() + 
  geom_sf(data = denmark_cropped,
          fill="white", colour = "black") + 
  coord_sf() + 
  geom_point(data = main.landuse.map, 
             aes(x=lat, y = long, colour = main.data$Land_use), alpha = 0.9, size=2, show.legend = T) + theme_void() + theme(legend.title = element_blank())


###adding in Germany data############################################

#harmomise danish data
dataDanish <- data
names(dataDanish)[which(names(dataDanish)=="utm_x")] <- "x"
names(dataDanish)[which(names(dataDanish)=="utm_y")] <- "y"
dataDanish <- dataDanish[,c("Land_use","x","y")]

#get and align Germany data
insectsDE <- read.delim("cleaned-data/DE_rough_landuse_biomass.txt")
#the x and y is in "+proj=utm +zone=32 +ellps=GRS80 +units=m +no_defs"
#same utm as Danish data - phew!
dataGermany <- insectsDE[,c("Land_use","x","y")]

#merge both
allData <- rbind(dataDanish,dataGermany)

#get regional data
worldmap <- ne_countries(scale = 'large', type = 'map_units',
                         returnclass = 'sf')
ourMap <- worldmap[worldmap$name %in% c('Denmark','Germany'),]

#transform regional data to utm
ourMap <- st_transform(ourMap,crs=st_crs("+proj=utm +zone=32 +ellps=GRS80 +units=m +no_defs"))
ggplot() + geom_sf(data = ourMap) + theme_void()

#add points
ggplot() + 
  geom_sf(data = ourMap) + 
  geom_point(data = allData,aes(x=x,y=y,colour=Land_use)) +
  theme_void()+
  theme(legend.title = element_blank())


#decide on common color scheme
library(wesanderson)
landuseCols <- wes_palette('Darjeeling1', 5, type = c("discrete"))
ggplot() + 
  geom_sf(data = ourMap,fill="grey95") + 
  geom_point(data = allData,aes(x=x,y=y,colour=Land_use)) +
  theme_void()+
  scale_colour_manual(values=landuseCols)+
  theme(legend.title = element_blank())

ggsave("plots/Fig1.png")
