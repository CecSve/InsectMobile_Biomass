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
library(ggsn)
#library(wesanderson)
library(cowplot)
library(ggsci)

####colour################################################################

#decide on common color scheme
#landuseCols <- wes_palette('Darjeeling1', 5, type = c("discrete"))
#landuseCols <- landuseCols[c(1,4,3,5,2)]

landuseCols <- c("#CC79A7", "#E69F00", "#D55E00", "#56B4E9", "#009E73") # colour friendly, ordered by land cover 
#landuseCols <- landuseCols[c(1,4,3,5,2)]

landuseOrder <- c("Urban","Farmland","Open uncultivated","Wetland","Forest")
landuseOrderDK <- c("Urban","Farmland","Open uncultivated land","Wetland","Forest")

### preparing DK data #######################################

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

### World map #######################################

world.map <- map_data ("world") # not a good base
worldmap <- ne_countries(scale = 'large', type = 'map_units',
                         returnclass = 'sf')
ggplot() + geom_sf(data = worldmap) + theme_bw()

### EU map ########################
# Some EU Contries
some.eu.countries <- c(
  "Portugal", "Spain", "France", "Switzerland", "Germany",
  "Austria", "Belgium", "UK", "Netherlands",
  "Denmark", "Poland", "Italy", 
  "Croatia", "Slovenia", "Hungary", "Slovakia",
  "Czech republic", "Luxembourg"
)

# Retrievethe map data
some.eu.maps <- map_data("world", region = some.eu.countries)

# Compute the centroid as the mean longitude and lattitude
# Used as label coordinate for country's names
region.lab.data <- some.eu.maps %>%
  group_by(region) %>%
  summarise(long = mean(long), lat = mean(lat))

ggplot(some.eu.maps, aes(x = long, y = lat)) + coord_sf() +
  geom_polygon(data=some.eu.maps, aes(group = group, fill = region), colour = "black")+
  geom_text(aes(label = region, fontface = 2), data = region.lab.data,  size = 3, hjust = 0.5)+
  scale_fill_manual(values = c("Portugal" = "white", "Spain" = "white", "France" ="white", "Switzerland" ="white", "Germany" ="cadetblue3", "Austria"="white", "Belgium"="white", "UK"="white", "Netherlands"="white",
                               "Denmark"= "cadetblue", "Poland"="white", "Italy"="white", 
                               "Croatia"="white", "Slovenia"="white", "Hungary"="white", "Slovakia"="white",
                               "Czech Republic"= "white", "Luxembourg" = "white")) + theme_void()+
  theme(legend.position = "none") + scalebar(some.eu.maps, dist = 250, dist_unit = "km", transform = T, model = "WGS84", st.size = 3) + north(some.eu.maps, symbol = 4, scale = 0.07)

### DK map #########################################

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
g1 <- denmark %>%
  ggplot() + 
  geom_sf(data = denmark, 
           fill="white", colour = "black") + 
  coord_sf() + 
  geom_point(data = landuse.map, 
             aes(x=lat, y = long, colour = data$Land_use), size=4, show.legend = F) + theme_void() + scale_colour_manual("Predominant land cover", labels = c("Urban", "Farmland", "Grassland", "Wetland", "Forest"), values = landuseCols) + scalebar(denmark, dist = 25, dist_unit = "km", transform = T, model = "WGS84", st.size = 3) + north(denmark, symbol = 4, scale = 0.07) + panel_border() + labs(subtitle = "A") + theme(plot.subtitle = element_text(face = "bold", size = 20))  

# add DE data
load("H:/Documents/Insektmobilen/Analysis/Biomass/InsectMobile_Biomass/cleaned-data/DEmap.RData")

g2 <- plot(DEmap, main = "Germany") # Fejl i identicalUnits(x) : old version of unit class is no longer allowed

plot_grid(g1, g2,labels=c('A', 'B'))

ggsave("plots/Fig1.png")

# another way of plotting - without the coordinates for Bornholm - not necessary
denmark_cropped %>%
  ggplot() + 
  geom_sf(data = denmark_cropped,
          fill="white", colour = "black") + 
  coord_sf() + 
  geom_point(data = main.landuse.map, 
             aes(x=lat, y = long, colour = main.data$Land_use), alpha = 0.9, size=2, show.legend = T) + theme_void() + theme(legend.title = element_blank())

### DK proportional land cover map #########################
allInsects %>% str()
#order time band

# extract Latitude and Longitude variables and put them into simple data frame called lat.long.df.
lat.long.df <- data.frame(allInsects$utm_x, allInsects$utm_y) 
str(lat.long.df)

coordinates(lat.long.df) <-  ~allInsects.utm_x + allInsects.utm_y #Function coordinates() creates a spatial object
str(lat.long.df) # at this point, this dataset doesn’t have CRS. Spatial data can’t exist without CRS(Coordinates Reference System). Spatial data requires, at least, coordinates and CRS.

proj4string(lat.long.df) <- CRS("+init=epsg:25832")
#this is +proj=utm +zone=32 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs 

head(lat.long.df)

# Now, dataset has coordinates and CRS. Next thing is to convert this to Longitude-Latitude data.
dist.location <- spTransform(lat.long.df, CRS("+init=epsg:4326"))
dist.location

landuse.map <- 
  data.frame(A = allInsects$Urban_1000,
             B = allInsects$Agriculture_1000 ,
             C = allInsects$Open.uncultivated.land_1000,
             D = allInsects$Wetland_1000,
             E = allInsects$Forest_1000,
             lat = dist.location$allInsects.utm_x,
             long = dist.location$allInsects.utm_y)

library(scatterpie)

# whole of Denmark with better resolution (one plot in the ocean is on the island of Taasinge which is not included in this resolution)
n <- nrow(landuse.map)
landuse.map$region <- factor(1:n)
color_list = c('A'="#CC79A7", 'B'="#E69F00", C="#D55E00", D= "#56B4E9", E="#009E73")
landuse.map$radius <- 0.05

# Create pie chart annotations to avoid stretching of pie charts on coordinate system
pie.list <- landuse.map %>% 
  tidyr::gather(type, value, -long, -lat, -region, -radius) %>%
  tidyr::nest(type, value) %>%
  
  # make a pie chart from each row, & convert to grob
  mutate(pie.grob = purrr::map(data,
                               function(d) ggplotGrob(ggplot(d, 
                                                             aes(x = 1, y = value, fill = type)) +
                                                        geom_col(color = "black", alpha = 0.8,
                                                                 show.legend = FALSE)+ scale_fill_manual(values = color_list) + coord_polar(theta = "y") + theme_void()))) %>%
  
  # convert each grob to an annotation_custom layer. I've also adjusted the radius
  # value to a reasonable size (based on my screen resolutions).
  rowwise() %>%
  mutate(radius = radius * 2) %>%
  mutate(subgrob = list(annotation_custom(grob = pie.grob,
                                          ymin = long - radius, ymax = long + radius,
                                          xmin = lat - radius, xmax = lat + radius)))

#p + geom_tile(data = landuse.map %>% tidyr::gather(type, value, -long, -lat, -radius, -region),aes(x = lat, y = long, fill = type), color = "black", width = 0.01, height = 0.01, inherit.aes = FALSE) + pie.list$subgrob 

# plot the monster
denmark %>%
  ggplot() + 
  geom_sf(data = denmark, 
          fill="white", colour = "black") + 
  coord_sf()+ geom_tile(data = landuse.map %>% tidyr::gather(type, value, -long, -lat, -radius, -region),
                        aes(x = lat, y = long, fill = type), 
                        color = "black", width = 0.01, height = 0.01, 
                        inherit.aes = FALSE)+ scale_fill_manual(values = color_list, name = "Proportional land cover", labels = c("Urban", "Farmland", "Grassland", "Wetland", "Forest")) + pie.list$subgrob + theme_void() + scalebar(denmark, dist = 25, dist_unit = "km", transform = T, model = "WGS84", st.size = 3) + north(denmark, symbol = 4, scale = 0.07) + panel_border() + labs(subtitle = "A") + theme(plot.subtitle = element_text(face = "bold", size = 20), legend.position = "none") 

ggsave("plots/Fig1A_correctlandcover.png")

# another way of plotting - without the coordinates for Bornholm - not necessary
denmark_cropped %>%
  ggplot() + 
  geom_sf(data = denmark_cropped,
          fill="white", colour = "black") + 
  coord_sf() + 
  geom_point(data = main.landuse.map, 
             aes(x=lat, y = long, colour = main.data$Land_use), alpha = 0.9, size=2, show.legend = T) + theme_void() + theme(legend.title = element_blank())


###Germany map#############################################

#get world map using code above

#get map of germny
germany <- worldmap[worldmap$name == 'Germany',]
ggplot() + geom_sf(data = germany) + theme_bw() 

#convert coords into lat lon
dataGermany <- insectsDE[,c("Land_use","x","y")]
coordinates(dataGermany) <- c("x","y")
proj4string(dataGermany) <- CRS("+proj=utm +zone=32 +ellps=GRS80 +units=m +no_defs")
dataGermany <- spTransform(dataGermany,CRS(crs(germany)))

#crop germany to bounding box of points
germany <- st_crop(germany,c(xmin=5.85,xmax=15.02,ymin=50.5,ymax=55.05))

landuse.map <- 
  data.frame(Land_use = dataGermany@data$Land_use,
             lat = dataGermany@coords[,1],
             long = dataGermany@coords[,2])
landuse.map$Land_use <- factor(landuse.map$Land_use,
                               levels=c("Urban","Farmland","Dryland",
                                        "Wetland","Forest"))

ggplot(germany) + 
  geom_sf(data = germany, 
          fill="white", colour = "black") + 
  coord_sf() + 
  geom_point(data = landuse.map, 
             aes(x=lat, y = long, colour = Land_use), size=4) + 
  theme_void() + 
  scale_colour_manual("Land cover", labels = c("Urban", "Farmland", "Grassland", "Wetland", "Forest"), values = landuseCols) + 
  theme(plot.subtitle = element_text(face = "bold", size = 20),legend.position = "top") 

ggsave("plots/Fig1-legend.png")


### DE proportional land cover map #########################

# extract Latitude and Longitude variables and put them into simple data frame called lat.long.df.
lat.long.df <- data.frame(allInsects$x, allInsects$y) 
str(lat.long.df)

coordinates(lat.long.df) <-  ~allInsects.x + allInsects.y #Function coordinates() creates a spatial object
str(lat.long.df) # at this point, this dataset doesn’t have CRS. Spatial data can’t exist without CRS(Coordinates Reference System). Spatial data requires, at least, coordinates and CRS.

proj4string(lat.long.df) <- CRS("+proj=utm +zone=32 +ellps=GRS80 +units=m +no_defs")
#this is +proj=utm +zone=32 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs 

head(lat.long.df)

# Now, dataset has coordinates and CRS. Next thing is to convert this to Longitude-Latitude data.
dist.location <- spTransform(lat.long.df, CRS("+init=epsg:4326"))
dist.location

landuse.map <- 
  data.frame(A = allInsects$Urban_1000,
             B = allInsects$Agriculture_1000 ,
             C = allInsects$Open.uncultivated_1000,
             D = allInsects$Wetland_1000,
             E = allInsects$Forest_1000,
             lat = dist.location$allInsects.x,
             long = dist.location$allInsects.y)

library(scatterpie)

# whole of Denmark with better resolution (one plot in the ocean is on the island of Taasinge which is not included in this resolution)
n <- nrow(landuse.map)
landuse.map$region <- factor(1:n)
color_list = c('A'="#CC79A7", 'B'="#E69F00", C="#D55E00", D= "#56B4E9", E="#009E73")
landuse.map$radius <- 0.05

# Create pie chart annotations to avoid stretching of pie charts on coordinate system
pie.list <- landuse.map %>% 
  tidyr::gather(type, value, -long, -lat, -region, -radius) %>%
  tidyr::nest(type, value) %>%
  
  # make a pie chart from each row, & convert to grob
  mutate(pie.grob = purrr::map(data,
                               function(d) ggplotGrob(ggplot(d, 
                                                             aes(x = 1, y = value, fill = type)) +
                                                        geom_col(color = "black", alpha = 0.8,
                                                                 show.legend = FALSE)+ scale_fill_manual(values = color_list) + coord_polar(theta = "y") + theme_void()))) %>%
  
  # convert each grob to an annotation_custom layer. I've also adjusted the radius
  # value to a reasonable size (based on my screen resolutions).
  rowwise() %>%
  mutate(radius = radius * 2) %>%
  mutate(subgrob = list(annotation_custom(grob = pie.grob,
                                          ymin = long - radius, ymax = long + radius,
                                          xmin = lat - radius, xmax = lat + radius)))

# plot the monster
germany %>%
  ggplot() + 
  geom_sf(data = germany, 
          fill="white", colour = "black") + 
  coord_sf()+ 
  geom_tile(data = landuse.map %>% tidyr::gather(type, value, -long, -lat, -radius, -region),
                        aes(x = lat, y = long, fill = type), 
                        color = "black", width = 0.01, height = 0.01, 
                        inherit.aes = FALSE)+ 
  scale_fill_manual(values = color_list, name = "Proportional land cover", labels = c("Urban", "Farmland", "Grassland", "Wetland", "Forest")) + pie.list$subgrob + theme_void() + 
  scalebar(germany, dist = 50, dist_unit = "km", transform = T, model = "WGS84", st.size = 3) + 
  north(germany, symbol = 4, scale = 0.07) + 
  panel_border() + labs(subtitle = "B") + 
  theme(plot.subtitle = element_text(face = "bold", size = 20), legend.position = "none") 

ggsave("plots/Fig1B_correctlandcover.png")

###Germany + Denmark data############################################

#old code - no longer use

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

#reorder and rename levels of Land_use
levels(allData$Land_use) <- c("Urban","Farmland",
                           "Open uncultivated land",
                           "Wetland",
                           "Forest")

#decide on common color scheme
library(wesanderson)
landuseCols <- wes_palette('Darjeeling1', 5, type = c("discrete"))
landuseCols <- landuseCols[c(1,4,3,5,2)]

ggplot() + 
  geom_sf(data = ourMap,fill="grey95") + 
  geom_point(data = allData,aes(x=x,y=y,colour=Land_use),size=rel(1.5)) +
  theme_void()+
  scale_colour_manual(values=landuseCols)+
  theme(legend.title = element_blank())

ggsave("plots/Fig1.png")
