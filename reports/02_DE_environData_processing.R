#libraries we need
library(rgdal)
library(plyr)
library(reshape2)
library(lubridate)

###land-use###############################################################

#land use data (extracted by Volker see READ ME file in data folder)
setwd("C:/Users/db40fysa/Nextcloud/mobileInsect/04_geodata/atkis_v05")
allFiles <- list.files()
allFiles <- allFiles[!grepl("README.txt",allFiles)]

#get name of shapefie
Names <- allFiles[grepl(".shp",allFiles)]
Name <- gsub(".shp","",Names)

#remove those with Root on them
Name <- Name[!grepl("Root",Name)]

#for each folder
output <- ldply(Name, function(x){
  
  #read in shape file
  out <- readOGR(dsn=getwd(),
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
    temp_2$VEG <- temp$VEG
  }else{
    temp_2$VEG <- NA
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
output$Land_use[output$OBJART %in% c(43005,43006)] <- "Wetland"

#forest
output$Land_use[output$OBJART_TXT %in% "AX_Wald"] <- "Forest"

#agriculture
output$Land_use[grepl("agricultural",output$File)] <- "Agriculture"

#urban
output$Land_use[grepl("urbanArea",output$File)] <- "Urban"

#grassland
output$Land_use[grepl("greenland",output$File)] <- "Grassland"

#subset to the above land-uses
table(output$Land_use)
output <- subset(output,!is.na(Land_use))

#change NAs to zeros
output$value[is.na(output$value)] <- 0

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

#check all codierung in the insect data file
df$RouteID[df$RouteID %in% output$Codierung]
df$RouteID[!df$RouteID %in% output$Codierung]#yay!!

#cast the data
outputCast <- dcast(output,Codierung~Land_use+Buffer,value.var="value",fun=sum,na.rm=T)
write.table(outputCast,file="cleaned-data/environData_DE.txt",sep="\t")

####temperature#######################################################################

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

unique(trafficlights$Codierung)
trafficlights$Codierung[!trafficlights$Codierung %in% outputCast$Codierung]

outputCast$tl <- trafficlights@data$tr_signals[match(outputCast$Codierung,
                                                     trafficlights@data$Codierung)]

write.table(trafficlights@data[,c("Codierung","tr_signals")],
            file="cleaned-data/trafficlights_DE.txt",sep="\t")
