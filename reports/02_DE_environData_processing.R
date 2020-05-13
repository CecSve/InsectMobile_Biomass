#libraries we need
library(rgdal)
library(plyr)
library(reshape2)
library(lubridate)

###land-use###############################################################

#land use data (extracted by Volker see READ ME file in data folder)
tdir <- "C:/Users/db40fysa/Nextcloud/mobileInsect/04_geodata/atkisIntersect_v06"

allFiles <- list.files(tdir)
allFiles <- allFiles[!grepl("README.txt",allFiles)]

#get name of shapefie
Names <- allFiles[grepl(".shp",allFiles)]
Name <- gsub(".shp","",Names)

#remove those with Root on them
Name <- Name[!grepl("Root",Name)]

#get rid of "veg_01_greenland" 
Name <- Name[!Name %in% "veg_01_greenland"]

#for each folder
output <- ldply(Name, function(x){
  
  #read in shape file
  out <- readOGR(dsn=tdir,
               layer=x)

  #just get data frame
  temp <-out@data
  temp$File <- as.character(x)

  if("prop" %in% names(temp)){
    myNames <- c("Name","Codierung","OBJART","OBJART_TXT","prop","layer","File")
    #VEG is sometimes a column - forest, water and heath etc, greenland
  }else if ("dens" %in% names(temp)){
    myNames <- c("Name","Codierung","OBJART","OBJART_TXT","dens","layer","File")
  }else if ("lenDens" %in% names(temp)){
    myNames <- c("Name","Codierung","OBJART","OBJART_TXT","lenDens","layer","File")
  }
  
  temp_2 <- temp[,myNames]
  names(temp_2)[5] <- "value"
  
  if("VEG" %in% names(temp)){
    temp_2$VEG <- as.character(temp$VEG)
  }else{
    temp_2$VEG <- NA
  }
  
  if("Codierun_2" %in% names(temp)){
    temp_2$Codierun_2 <- as.character(temp$Codierun_2)
  }else{
    temp_2$Codierun_2 <- as.character(temp$Codierung)
  }
  
  return(temp_2)
  
})

#greenland is just 1020 (VEG column)
#fallow land too? - included 1200 - but none found
#agriculture is 1010 (VEG column)

#check out objart
unique(output$OBJART)
unique(output$OBJART_TXT)
unique(output$VEG)

#add on land_use data
output$Land_use <- NA

#wetland
output$Land_use[output$OBJART_TXT %in% c("AX_Sumpf")] <- "Wetland"
output$Land_use[grepl("water",output$File)] <- "Wetland"

#forest
output$Land_use[output$OBJART_TXT %in% "AX_Wald"] <- "Forest"
#note this does not include AX_Gehoelz

#agriculture
output$Land_use[grepl("agricultural",output$File)] <- "Agriculture"

#urban
output$Land_use[grepl("urbanArea",output$File)] <- "Urban"

#grassland
output$Land_use[grepl("greenland",output$File)] <- "Open uncultivated"
output$Land_use[output$OBJART_TXT %in% 
                  c("AX_Heide","AX_Moor")] <- "Open uncultivated"

table(output$Land_use)

#maybe use Codierung to match
unique(output$Codierung)
output$Codierung <- gsub("ï¿½","ue",output$Codierung,fixed=TRUE)
output$Codierung <- gsub("?","ue",output$Codierung,fixed=TRUE)
output$Codierung <- gsub("Ã¼","ue",output$Codierung,fixed=TRUE)
unique(output$Codierung)

#extract buffer size
output$Buffer <- sapply(output$File,function(x){
  if(grepl("500m",x)){
    500
  }else if(grepl("1000m",x)){
    1000
  }else if(grepl("250m",x)){
    250
  }else if(grepl("50m",x)){
    50
  }
})

table(output$Buffer)

#change NAs to zeros
output$value[is.na(output$value)] <- 0

#check all codierung in the insect data file
insectsDE$RouteID[insectsDE$RouteID %in% output$Codierung]
insectsDE$RouteID[!insectsDE$RouteID %in% output$Codierung]#yay!!

#remove duplicates - mistake in the data (check w/ Volker)
output <- subset(output,Codierung==Codierun_2)

#subset to the above land-uses
outputI <- output
table(output$Land_use)
output <- subset(output,!is.na(Land_use))

#cast the data
outputCast <- reshape2::dcast(output,Codierung~Land_use+Buffer,value.var="value",fun=sum,na.rm=T)
write.table(outputCast,file="cleaned-data/environData_DE.txt",sep="\t")

###land use intensity###############################################

#water bodies
outputI$Water <- 0
outputI$Water[grepl("water",outputI$File)] <- 1

#roads
outputI$Roads <- 0
outputI$Roads[grepl("roads",outputI$File)] <- 1

#hedges
outputI$Hedges <- 0
outputI$Hedges[grepl("hedgesAvenues",outputI$File)] <- 1

#urban green area
outputI$urbanGreen <- 0
outputI$urbanGreen[grepl("urbanGreen",outputI$File)] <- 1

#extensive (1050,1021,1030,1040) vs intensive (1010) agriculture
outputI$intensiveAgr <- 0
outputI$intensiveAgr[outputI$VEG=="1010"] <- 1
outputI$extensiveAgr <- 0
outputI$extensiveAgr[outputI$VEG %in% 
                       c("1050","1021","1030","1040")] <- 1
#most are 1021 - streobsweise, but little

landuseIntensity <- ddply(outputI,.(Codierung,Buffer),summarise,
      water = sum(value[Water==1]),
      roads = sum(value[Roads==1]),
      hedges = sum(value[Hedges==1]),
      urbangreen = sum(value[urbanGreen==1]),
      intensiveAgr = sum(value[intensiveAgr==1]),
      extensiveAgr = sum(value[extensiveAgr==1]))

#cast the data
outputCastRoads <- reshape2::dcast(landuseIntensity,Codierung~Buffer,
                               value.var=c("roads"),fun=sum)
names(outputCastRoads)[2:5] <- sapply(names(outputCastRoads)[2:5],
                                      function(x)paste0("roads_",x))

outputCastHedges <- reshape2::dcast(landuseIntensity,Codierung~Buffer,
                                   value.var=c("hedges"),fun=sum)
names(outputCastHedges)[2:5] <- sapply(names(outputCastHedges)[2:5],
                                      function(x)paste0("hedges_",x))

outputCastGreen <- reshape2::dcast(landuseIntensity,Codierung~Buffer,
                                   value.var=c("urbangreen"),fun=sum)
names(outputCastGreen)[2:5] <- sapply(names(outputCastGreen)[2:5],
                                      function(x)paste0("urbangreen_",x))

outputCastintAgr <- reshape2::dcast(landuseIntensity,Codierung~Buffer,
                                   value.var=c("intensiveAgr"),fun=sum)
names(outputCastintAgr)[2:5] <- sapply(names(outputCastintAgr)[2:5],
                                      function(x)paste0("intAgr_",x))

outputCastextAgr <- reshape2::dcast(landuseIntensity,Codierung~Buffer,
                                   value.var=c("extensiveAgr"),fun=sum)
names(outputCastextAgr)[2:5] <- sapply(names(outputCastextAgr)[2:5],
                                      function(x)paste0("exAgr_",x))

outputCast <- merge(outputCastRoads,outputCastHedges,by="Codierung")
outputCast <- merge(outputCast,outputCastGreen,by="Codierung")
outputCast <- merge(outputCast,outputCastintAgr ,by="Codierung")
outputCast <- merge(outputCast,outputCastextAgr,by="Codierung")

write.table(outputCast,file="cleaned-data/landuseIntensity_DE.txt",sep="\t")

####temperature#######################################################################

df <- read.delim("cleaned-data/DE_rough_landuse_biomass.txt")
sites <- df[,c("RouteID","Date","x","y")]
coordinates(sites)<-c("x","y")
proj4string(sites)<- "+proj=utm +zone=32 +ellps=GRS80 +units=m +no_defs"
sitesLatLon <- spTransform(sites,CRS("+proj=longlat +datum=WGS84"))
sitesLatLonDF <- sitesLatLon@data

#use R package to get data from DWD
library(rdwd)
#https://bookdown.org/brry/rdwd/

#get the nearesy station according to the following
getStations <- function(x){
  m <- nearbyStations(lat=x[2], 
                      lon=x[1], 
                      radius=40,
                      res="hourly",
                      var=c("air_temperature"),
                      per="historical",
                      mindate=as.Date("2018-07-15"))

  return(m$Stationsname[2])
}

#apply function
sitesLatLonDF$Station <- apply(sitesLatLon@coords,1,getStations)

#change one station name - see missing data section below
sitesLatLonDF$Station[which(sitesLatLonDF$Station=="Mittelnkirchen-Hohenfelde")] <- "Rosengarten-Klecken"

#get list of stations for each coordinates
myStations <- data.frame(Station=unique(sitesLatLonDF$Station))

#get temp data for each of these stations
library(lubridate)
getData <- function(station){
  link <- selectDWD(station,res="hourly",var="air_temperature", per="historical")
  file <- dataDWD(link, read=FALSE, dir="../localdata", quiet=TRUE, force=NA)
  clim <- readDWD(file, varnames=TRUE)
  clim$Year <- year(clim$MESS_DATUM)
  clim$Month <- month(clim$MESS_DATUM)

  #mean temperature for whole of June and July 2018
  if(nrow(subset(clim,(Month %in% 6:7) & Year==2018))==0){
    out <- data.frame(STATIONS_ID=NA,MESS_DATUM=NA,QN_9=NA,TT_TU.Lufttemperatur=NA)
    out$Station <- station
    return(out)
  }else{
    out <- subset(clim,(Month %in% 6:7) & Year==2018)[,1:4]
    out$Station <- station
    return(out)
  }
}

#apply function
tempData <- ldply(myStations$Station,getData)

#any missing data?
subset(tempData,is.na(STATIONS_ID))

# previously:
# just one Mittelnkirchen-Hohenfelde
# subset(sitesLatLon,Station=="Mittelnkirchen-Hohenfelde")#for Gruen_09 
# m <- nearbyStations(lat=53.36881, 
#                 lon=9.602736, 
#                 radius=40,
#                 res="hourly",
#                 var=c("air_temperature"),
#                 per="historical",
#                 mindate=as.Date("2018-07-15"))
# try Rosengarten-Klecken
# getData("Rosengarten-Klecken")
# yep!
# replaced as above

#now get data for each route on each sampling day
sitesLatLonDF$Date <- as.character(sitesLatLonDF$Date)
sitesLatLonDF$Date <- as.Date(sitesLatLonDF$Date,format="%d.%m.%y")
sitesLatLonDF$yday <- yday(sitesLatLonDF$Date)
unique(sitesLatLonDF$yday)

#day yday also for the temperature data
tempData$yday <- yday(tempData$MESS_DATUM)

#merge site and date
sitesLatLonDF <- merge(sitesLatLonDF,tempData,by=c("Station","yday"))

#extract hour data
sitesLatLonDF$hour <- hour(sitesLatLonDF$MESS_DATUM)

#get mean temperature at:
#12-15
#17-20
routeTemps <- ddply(sitesLatLonDF,.(Station,RouteID,Date),summarise,
                       middayTemp = median(TT_TU.Lufttemperatur[hour %in% 12:15]),
                       eveningTemp = median(TT_TU.Lufttemperatur[hour %in% 17:20]))

write.table(routeTemps,file="cleaned-data/routeTemps_DE.txt",sep="\t")

###wind##################################################################

load("cleaned-data/DE_rough_landuse_biomass.RData")
sites <- df[,c("RouteID","Date","x","y")]
coordinates(sites)<-c("x","y")
proj4string(sites)<- "+proj=utm +zone=32 +ellps=GRS80 +units=m +no_defs"
sitesLatLon <- spTransform(sites,CRS("+proj=longlat +datum=WGS84"))
sitesLatLonDF <- sitesLatLon@data

#use R package to get data from DWD
library(rdwd)
#https://bookdown.org/brry/rdwd/

#get the nearesy station according to the following
getStations <- function(x){
  m <- nearbyStations(lat=x[2], 
                      lon=x[1], 
                      radius=40,
                      res="hourly",
                      var=c("wind"),
                      per="historical",
                      mindate=as.Date("2018-07-15"))
  
  return(m$Stationsname[2])
}

#apply function
sitesLatLonDF$Station <- apply(sitesLatLon@coords,1,getStations)

#get list of stations for each coordinates
myStations <- data.frame(Station=unique(sitesLatLonDF$Station))

#get temp data for each of these stations
getData <- function(station){
  link <- selectDWD(station,res="hourly",var="wind", per="historical")
  file <- dataDWD(link, read=FALSE, dir="../localdata", quiet=TRUE, force=NA, overwrite=TRUE)
  clim <- readDWD(file, varnames=TRUE)
  clim$Year <- year(clim$MESS_DATUM)
  clim$Month <- month(clim$MESS_DATUM)
  
  #mean temperature for whole of June and July 2018
  if(nrow(subset(clim,(Month %in% 6:7) & Year==2018))==0){
    out <- data.frame(STATIONS_ID=NA,MESS_DATUM=NA,QN_9=NA,TT_TU.Lufttemperatur=NA)
    out$Station <- station
    return(out)
  }else{
    out <- subset(clim,(Month %in% 6:7) & Year==2018)[,1:4]
    out$Station <- station
    return(out)
  }
}

#apply function
windData <- ldply(myStations$Station,getData)

#any missing data?
subset(windData,is.na(STATIONS_ID))

#now get data for each route on each sampling day
sitesLatLonDF$Date <- as.character(sitesLatLonDF$Date)
sitesLatLonDF$Date <- as.Date(sitesLatLonDF$Date,format="%d.%m.%y")
sitesLatLonDF$yday <- yday(sitesLatLonDF$Date)
unique(sitesLatLonDF$yday)

#day yday also for the temperature data
windData$yday <- yday(windData$MESS_DATUM)

#merge site and date
sitesLatLonDF <- merge(sitesLatLonDF,windData,by=c("Station","yday"))

#extract hour data
sitesLatLonDF$hour <- hour(sitesLatLonDF$MESS_DATUM)

#get mean temperature at:
#12-15
#17-20
routeWind <- ddply(sitesLatLonDF,.(Station,RouteID,Date),summarise,
                    middayTemp = median(F.Windgeschwindigkeit[hour %in% 12:15]),
                    eveningTemp = median(F.Windgeschwindigkeit[hour %in% 17:20]))

write.table(routeWind,file="cleaned-data/routeWind_DE.txt",sep="\t")

###traffic light data########################################################

trafficlights <- readOGR(dsn="C:/Users/db40fysa/Nextcloud/mobileInsect/04_geodata/routes_2018", layer="2018_gefahreneRouten25832")

#get land use data
environData <- read.delim("cleaned-data/environData_DE.txt")

unique(trafficlights$Codierung)
trafficlights$Codierung[!trafficlights$Codierung %in% environData$Codierung]

environData$tl <- trafficlights@data$tr_signals[match(environData$Codierung,
                                                     trafficlights@data$Codierung)]

write.table(trafficlights@data[,c("Codierung","tr_signals")],
            file="cleaned-data/trafficlights_DE.txt",sep="\t")

#examine relationships
environData$Land_use <- sapply(as.character(environData$Codierung),function(x)
  strsplit(x,"_")[[1]][1])

summary(environData$Urban_50[environData$Land_use=="Urban"])
summary(environData$Urban_1000[environData$Land_use=="Urban"])

ggplot(environData)+
  geom_point(aes(x=tl,y=Urban_500,colour=Land_use))

ggplot(environData)+
  geom_point(aes(x=tl,y=Urban_500))+
  facet_wrap(~Land_use)

#no correlation between traffic lights and cover within each land-use

###stopping data############################################################

stops <- readOGR(dsn="C:/Users/db40fysa/Nextcloud/mobileInsect/04_geodata/stops", layer="stops20200429atRoutesBuffer25mDisCenCount")

#get land use data
environData <- read.delim("cleaned-data/environData_DE.txt")

unique(stops$Codierung)
unique(stops$osm_id_cou)

stops$Codierung[!stops$Codierung %in% environData$Codierung]

environData$osm <- stops@data$osm_id_cou[match(environData$Codierung,
                                                      stops@data$Codierung)]
environData$osm <- as.numeric(as.character(environData$osm))
environData$osm[is.na(environData$osm)] <- 0

#infer zeros for missing values

write.table(environData[,c("Codierung","osm")],
            file="cleaned-data/stops_DE.txt",sep="\t")

#examine relationships
environData$Land_use <- sapply(as.character(environData$Codierung),function(x)
  strsplit(x,"_")[[1]][1])

summary(environData$Urban_50[environData$Land_use=="Urban"])
summary(environData$Urban_1000[environData$Land_use=="Urban"])

ggplot(environData)+
  geom_point(aes(x=osm,y=Urban_500,colour=Land_use))

ggplot(environData)+
  geom_point(aes(x=tl,y=Urban_500))+
  facet_wrap(~Land_use)

###end
