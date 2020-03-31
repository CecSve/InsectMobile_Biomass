#inseckMobile:

#note: what i call route_no here is rather the sample no
library(ggplot2)
library(plyr)
library(reshape2)
library(lubridate)

#sort image data################################################################################

#read in dataset files
setwd("C:/Users/db40fysa/Nextcloud/mobileInsect/13_data/jenaData")
images<-read.delim("stats_images.txt",as.is=T)

#exclude ones with wrong label in them
images$Image_Name<-gsub("_wrong label","",images$Image_Name)

#extract metadata

#habitat
images$habitat <- sapply(images$Image_Name,function(x)strsplit(x,"_")[[1]][1])
images$habitat[which(images$habitat=="GrÃ¼n")] <- "Gruen"
images$habitat[which(images$habitat=="Freucht")] <- "Feucht"
unique(images$habitat)

#route no
images$route_no <- as.character(sapply(images$Image_Name,function(x)strsplit(x,"_")[[1]][2]))
images$route_no <- gsub("-17-20","",images$route_no)
images$route_no <- gsub("-12-15","",images$route_no)
images$route_temp <- images$route_no
images$route_no <- as.numeric(images$route_no)
unique(images$route_no)

#time
images$time <- NA
images$time[grepl("12-15",images$Image_Name)] <- "12-15"
images$time[grepl("12_15",images$Image_Name)] <- "12-15"
images$time[grepl("17-20",images$Image_Name)] <- "17-20"
images$time[grepl("17_20",images$Image_Name)] <- "17-20"
unique(images$time)

#set habitat levels
images$habitat <- factor(images$habitat, levels=c("Urban","Agrar","Gruen","Feucht","Wald")) 

#from Suzanne:
#"As far as we can see, Agrar 09 should be Wald 09. 
#And the route Gr?n 4 had to be taken out because of quality issues of the route itself."
images <- subset(images, !(habitat=="Gruen" & route_no==4))
images$habitat[images$habitat=="Agrar" & images$route_no==9] <- "Wald"
table(images$habitat)

#keep in codierung
images$route_temp <- sapply(images$route_temp,
                            function(x)ifelse(nchar(x)==1,paste0("0",x),x))
images$Codierung <- paste(images$habitat,images$route_temp,sep="_")

##summarise data####################################################################

#per habitat and route and time
summaryData <- ddply(images,.(habitat,route_no,time,Codierung),summarise,
                     nuObjects=length(unique(Object_ID)),
                     meanSize=median(Size_cm),
                     sumBiomass=sum(Biomass_Area_cm2),
                     sumBiomassP=sum(Biomass_Percentage))

summaryData$Urban <- ifelse(summaryData$habitat=="Urban",1,0)
summaryData$Agrar <- ifelse(summaryData$habitat=="Agrar",1,0)
summaryData$Gruen <- ifelse(summaryData$habitat=="Gruen",1,0)
summaryData$Feucht <- ifelse(summaryData$habitat=="Feucht",1,0)
summaryData$Wald <- ifelse(summaryData$habitat=="Wald",1,0)

nrow(summaryData)#129

##dry weight###############################################################################

#compare biomass from fotos with dry weight
dryWeights <- read.csv("C:/Users/db40fysa/Nextcloud/mobileInsect/13_data/dryWeight/Dry weight_Insektenmobil_Julianas lab book_16_05_2019-she.csv",as.is=T)

#get habitat
dryWeights$habitat <- sapply(dryWeights$Sample.ID,function(x)strsplit(as.character(x),"_")[[1]][1])
dryWeights <- subset(dryWeights,!habitat %in% c("Blank","Blank filter"))
dryWeights <- subset(dryWeights,!is.na(habitat))
dryWeights$habitat[dryWeights$habitat=="Grün"]<- "Gruen"
unique(dryWeights$habitat)

#get Codierung
dryWeights$route_no <- sapply(dryWeights$Sample.ID,function(x)strsplit(as.character(x),"_")[[1]][2])
dryWeights$Codierung <- paste(dryWeights$habitat,dryWeights$route_no,sep="_")

#get route number
dryWeights$route_no <- as.numeric(dryWeights$route_no)
unique(dryWeights$route_no)

#get time
dryWeights$time <- sapply(dryWeights$Sample.ID,function(x)strsplit(as.character(x),"_")[[1]][3])
unique(dryWeights$time)

#get total biomass per habitat, route and time
summary(dryWeights$Dry.mass..mg.)
summaryDry <- ddply(dryWeights,.(habitat,route_no,time,Codierung,Size),
                    summarise,dryBiomass=sum(Dry.mass..mg.))
summaryDry <- dcast(summaryDry,habitat+route_no+time+Codierung~Size,value.var="dryBiomass")
summaryDry$`< 10 mm`[is.na(summaryDry$`< 10 mm`)] <- 0
summaryDry$`> 10 mm`[is.na(summaryDry$`> 10 mm`)] <- 0

#any less than zero, set to zero
summary(summaryDry$`< 10 mm`)
summary(summaryDry$`> 10 mm`)
summaryDry$`< 10 mm`[summaryDry$`< 10 mm`< 0] <- 0
summaryDry$`> 10 mm`[summaryDry$`> 10 mm`< 0] <- 0

#total
summaryDry$Biomass <- summaryDry$`< 10 mm`+summaryDry$`> 10 mm` 
names(summaryDry)[5] <- 'Biomass_small'
names(summaryDry)[6] <- 'Biomass_large'
summary(summaryDry$Biomass)

#merge with image based mass data

#check they match
summaryData$Codierung[!summaryData$Codierung %in% summaryDry$Codierung]
summaryDry$Codierung[!summaryDry$Codierung %in% summaryData$Codierung]

#remove the problematic datasets
summaryDry <- subset(summaryDry,Codierung!="Gruen_04")

#merge
allBiomass <- merge(summaryData,summaryDry,by=c("habitat","time","route_no","Codierung"),all=T)
subset(allBiomass,is.na(dryWeights))#none missing
subset(allBiomass,is.na(sumBiomass))#5 missing - based on images

#these samples are:
#Urban 17-20        5         dry weight according to lab book of Juliana: 0
#Agrar 12-15       17         dry weight according to lab book of Juliana:62
#Gruen 12-15        4         We had to take the sample out due to quality control (route was partly on a motorway-like street and driver had to go faster than 50km/h…)
#Gruen 17-20        4         We had to take the sample out due to quality control
#Feucht 17-20      13         Dry weight according to lab book of Juliana: -3

#add the zeros for the others???
#we only use dry biomass in the paper anyway
allBiomass$sumBiomass[is.na(allBiomass$sumBiomass)] <- 0

#plot
summary(allBiomass$sumBiomass)
summary(allBiomass$Biomass)

qplot(sumBiomass,Biomass,data=allBiomass,colour=habitat,shape=time)+
  scale_y_log10()+scale_x_log10()+
  theme_bw()+
  ylab("Biomass (weighted)")+xlab("Biomass (image-based)")
#missing 4 site combinations!

cor.test(allBiomass$sumBiomass[!is.na(allBiomass$sumBiomass)],
         allBiomass$Biomass[!is.na(allBiomass$sumBiomass)])#0.8940259

# ###route length info############################################################################## 
# 
# routelength <- read.csv("C:/Users/db40fysa/Nextcloud/mobileInsect/13_data/metaData/2018_gefahreneRouten25832.csv")
# unique(routelength$Codierung)
# routelength$route_no <- sapply(as.character(routelength$Codierung),function(x)strsplit(x,"_")[[1]][2])
# routelength$route_no <- as.numeric(routelength$route_no)
# routelength$habitat <- sapply(as.character(routelength$Codierung),function(x)strsplit(x,"_")[[1]][1])
# routelength$habitat <- gsub("GrÃ¼n","Gruen",routelength$habitat)
# 
# #merge with data
# summaryData <- merge(summaryData,routelength,by=c("habitat","route_no"),all.x=T)
# subset(summaryData,is.na(length))
# 
# ggplot(summaryData,aes(x=length,y=sumBiomass))+
#   geom_point(aes(x=length,y=sumBiomass))+
#   facet_wrap(~habitat,scales="free")+
#   geom_smooth(method="lm")

##effect of time and day of sampling#########################################################################

routelength <- read.csv("C:/Users/db40fysa/Nextcloud/mobileInsect/13_data/metaData/190503_volunteer sampling protocols 2018_d1-sh-for Diana_DBedits.csv")

#time
routelength$time <- sapply(as.character(routelength$Codierung),function(x)strsplit(x,"_")[[1]][3])

#Codierung
unique(routelength$Codierung)
routelength$Codierung <- gsub("Grün","Gruen",routelength$Codierung)
routelength$Codierung <- gsub("_12-15","",routelength$Codierung)
routelength$Codierung <- gsub("_17-20","",routelength$Codierung)
routelength <- subset(routelength,Codierung!="")

#remove Gruen 04 - problematic
routelength <- subset(routelength,Codierung!="Gruen_04")

#route number
routelength$route_no <- sapply(as.character(routelength$Codierung),function(x)strsplit(x,"_")[[1]][2])
routelength$route_no <- as.numeric(routelength$route_no)

#habitat
routelength$habitat <- sapply(as.character(routelength$Codierung),function(x)strsplit(x,"_")[[1]][1])

#check all routes in it
routelength$Codierung[!routelength$Codierung %in% allBiomass$Codierung]
allBiomass$Codierung[!allBiomass$Codierung %in% allBiomass$Codierung]
#woohoo!!

#add missing date info - assume PM for Agrar_18 is same as AM
unique(routelength$Datum)
routelength$Datum <- as.character(routelength$Datum)
routelength$Datum[routelength$Datum==""] <- "06.07.18"#KJC's route

#merge with data
summaryData <- merge(allBiomass,routelength,by=c("Codierung","time","habitat","route_no"),all=T)
summaryData$Time <- as.POSIXct(summaryData$Startzeit,format="%H:%M")
summaryData$Date <- as.Date(summaryData$Datum,format="%d.%m.%y")
summaryData$Julian_day <- yday(summaryData$Date)

#anlysis of time at a finer scale
ggplot(summaryData,aes(x=Time,y=sumBiomass))+
  geom_point(aes(colour=habitat))+
  geom_smooth(method="lm")+
  facet_wrap(~time,scales="free")+
  scale_y_log10()

#analysis of date at a finer scale
ggplot(summaryData,aes(x=Julian_day,y=nuObjects))+
  geom_point(aes(colour=habitat))+
  geom_smooth(method="lm")+
  facet_wrap(~time,scales="free")+
  scale_y_log10()

####revised length data and coordinates####################################################

#read in a version 4 shapefile
library(rgdal)
temp <- readOGR(dsn="C:/Users/db40fysa/Nextcloud/mobileInsect/04_geodata/atkis_v04",
                layer="ver_01_streets1000m")
tempDF <- temp@data
lengthDF <- unique(tempDF[,c("Codierung","mean_x","mean_y","length")])

#change Grün to Gruen
lengthDF$Codierung <- gsub("Grün","Gruen",lengthDF$Codierung)

#add to summaryData
summaryData$Codierung[!summaryData$Codierung %in% lengthDF$Codierung]#none
lengthDF$Codierung[!lengthDF$Codierung %in% summaryData$Codierung]#none
#woohoo!!

#we are missing the land-use data for Feucht_18
summaryData$newLength <- lengthDF$length[match(summaryData$Codierung,lengthDF$Codierung)]
summaryData$x <- lengthDF$mean_x[match(summaryData$Codierung,lengthDF$Codierung)]
summaryData$y <- lengthDF$mean_y[match(summaryData$Codierung,lengthDF$Codierung)]

#look at summaries
summary(summaryData$x)
summary(summaryData$y)
summary(summaryData$newLength)

#format data to common format###############################################################

myVars <- c("habitat","time","Biomass","Biomass_small","Biomass_large",
            "Name",
            "Datum",
            "x","y",
            "newLength",#"Length.of.route.by.GIS" - is the old one
            "Codierung",
            "Startzeit","Ende",
            "Wind..schwach.mittel.stark.",
            "Temp_DB") 
all(myVars %in% names(summaryData))

df <- summaryData[,myVars]
names(df) <- c("Land_use","Time_band","Biomass","Biomass_small","Biomass_large",
               "PilotID","Date","x","y","Route_length",
               "RouteID","StartTime","EndTime","Wind","Temperature")

###clean each variable##########################################################

#remove German
df$RouteID <- gsub("ün","uen",df$RouteID)

#land use
levels(df$Land_use) <- c("Urban","Farmland","Dryland","Wetland","Forest")
df$Land_use <- factor(df$Land_use,levels=c("Urban","Farmland","Dryland","Wetland","Forest"))

#time band
df$Time_band <- factor(df$Time_band)
levels(df$Time_band) <- c("midday","evening")

#add Time_driven - diff between start and end time
df$StartTime2 <- as.POSIXct(df$StartTime,format="%H:%M")
df$EndTime2 <- as.POSIXct(df$EndTime,format="%H:%M")
df$Time_driven <- as.numeric(df$EndTime2 - df$StartTime2)
df <- df[,-which(names(df) %in% c("StartTime2","EndTime2"))]
summary(df$Time_driven)
#1 NA

#Distance  -assume as constant - try and fix later with more precise info
df$Distance_driven <- 10000

#Velocity
df$Velocity <- df$Distance_driven/df$Time_driven
summary(df$Velocity )

#standardized temp
unique(df$Temperature)
df$Temperature <- as.character(df$Temperature)
df$Temperature[which(df$Temperature=="25-30")] <- "25-30+"
df$Temperature[which(df$Temperature=="30+")] <- "25-30+"
df$Temperature <- as.factor(df$Temperature)

#wind temp
#Light (1.6-3.3 m/s)
#Gentle (3.4 - 5.5 m/s)
#Moderate (5.5-7.9 m/s)

#retrieve 
#temperature -  mean and max
#pet
#humdity - mean and max
#(wind)
#2 m above ground
#DWD

#change pilot id into a number
sort(unique(df$PilotID))
#Melanie Huk/Kownatzki Melanie/Thomas Huk
df$PilotID <- as.numeric(as.factor(df$PilotID))

#format data
df$Date <- as.Date(df$Date, format="%d.%m.%y")

#save processed data file
save(df,file="cleaned-data/DE_rough_landuse_biomass.RData")
