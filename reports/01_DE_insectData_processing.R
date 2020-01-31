#inseckMobile:

#note: what i call route_no here is rather the sample no
library(ggplot2)
library(plyr)
library(reshape2)

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

#fix mistakes####################################################################

#from Suzanne:
#"As far as we can see, Agrar 09 should be Wald 09. 
#And the route Gr?n 4 had to be taken out because of quality issues of the route itself."

images <- subset(images, !(habitat=="Gruen" & route_no==4))
images$habitat[images$habitat=="Agrar" & images$route_no==9] <- "Wald"
table(images$habitat)

##summarise data####################################################################

#per habitat and route and time
summaryData <- ddply(images,.(habitat,route_no,time),summarise,
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

routelength <- read.csv("C:/Users/db40fysa/Nextcloud/mobileInsect/13_data/metaData/190503_volunteer sampling protocols 2018_d1-sh-for Diana.csv")

unique(routelength$Codierung)
routelength$route_no <- sapply(as.character(routelength$Codierung.coding),function(x)strsplit(x,"_")[[1]][2])
routelength$route_no <- as.numeric(routelength$route_no)
routelength$habitat <- sapply(as.character(routelength$Codierung.coding),function(x)strsplit(x,"_")[[1]][1])
routelength$habitat <- gsub("Grün","Gruen",routelength$habitat)
routelength$time <- sapply(as.character(routelength$Codierung.coding),function(x)strsplit(x,"_")[[1]][3])

#Codierung
routelength$Codierung <- routelength$Codierung.coding
routelength$Codierung <- gsub("_12-15","",routelength$Codierung)
routelength$Codierung <- gsub("_17-20","",routelength$Codierung)

#merge with data
summaryData <- merge(summaryData,routelength,by=c("habitat","route_no","time"),all.x=T)

#anlysis of time at a finer scale
#format time
summaryData$Time <- as.POSIXct(summaryData$Startzeit.starting.time,format="%H:%M")
ggplot(summaryData,aes(x=Time,y=sumBiomass))+
  geom_point(aes(colour=habitat))+
  geom_smooth(method="lm")+
  facet_wrap(~time,scales="free")+
  scale_y_log10()

#analysis of date at a finer scale
summaryData$Date <- as.Date(summaryData$Datum.date,format="%d.%m.%y")
library(lubridate)
summaryData$Julian_day <- yday(summaryData$Date)

ggplot(summaryData,aes(x=Julian_day,y=SamplesummaryData..mg.))+
  geom_point(aes(colour=Landuse))+
  geom_smooth(method="lm")+
  facet_wrap(~Time,scales="free")+
  scale_y_log10()

##dry weight###############################################################################

#compare biomass from fotos with dry weight
dryWeights <- read.csv("C:/Users/db40fysa/Nextcloud/mobileInsect/13_data/dryWeight/Dry weight_Insektenmobil_Julianas lab book_16_05_2019-she.csv",as.is=T)

#get habitat
dryWeights$habitat <- sapply(dryWeights$Sample.ID,function(x)strsplit(as.character(x),"_")[[1]][1])
dryWeights <- subset(dryWeights,!habitat %in% c("Blank","Blank filter"))
dryWeights <- subset(dryWeights,!is.na(habitat))
dryWeights$habitat[dryWeights$habitat=="Grün"]<- "Gruen"
unique(dryWeights$habitat)

#get route number
dryWeights$route_no <- sapply(dryWeights$Sample.ID,function(x)strsplit(as.character(x),"_")[[1]][2])
dryWeights$route_no <- as.numeric(dryWeights$route_no)
unique(dryWeights$route_no)

#get time
dryWeights$time <- sapply(dryWeights$Sample.ID,function(x)strsplit(as.character(x),"_")[[1]][3])
unique(dryWeights$time)

#get total biomass per habitat, route and time
summary(dryWeights$Dry.mass..mg.)
summaryDry <- ddply(dryWeights,.(habitat,route_no,time,Size),
                    summarise,dryBiomass=sum(Dry.mass..mg.))
summaryDry <- dcast(summaryDry,habitat+route_no+time~Size,value.var="dryBiomass")
summaryDry$`< 10 mm`[is.na(summaryDry$`< 10 mm`)] <- 0
summaryDry$`> 10 mm`[is.na(summaryDry$`> 10 mm`)] <- 0

#any less than zero, set to zero
summary(summaryDry$`< 10 mm`)
summary(summaryDry$`> 10 mm`)
summaryDry$`< 10 mm`[summaryDry$`< 10 mm`< 0] <- 0
summaryDry$`> 10 mm`[summaryDry$`> 10 mm`< 0] <- 0

#total
summaryDry$Biomass <- summaryDry$`< 10 mm`+summaryDry$`> 10 mm` 
names(summaryDry)[4] <- 'Biomass_small'
names(summaryDry)[5] <- 'Biomass_large'
summary(summaryDry$Biomass)


#merge with image based mass data
allBiomass <- merge(summaryData,summaryDry,by=c("habitat","time","route_no"),all=T)
subset(allBiomass,is.na(sumBiomass))#5 missing
subset(allBiomass,is.na(dryWeights))#none missing

#plot
summary(allBiomass$sumBiomass)
summary(allBiomass$dryBiomass)
qplot(sumBiomass,dryBiomass,data=allBiomass,colour=habitat,shape=time)+
  scale_y_log10()+scale_x_log10()+
  theme_bw()+
  ylab("Biomass (weighted)")+xlab("Biomass (image-based)")
#missing 4 site combinations!

qplot(sumBiomass,dryBiomass,data=allBiomass)+
  facet_wrap(time~habitat,scales="free")+
  scale_y_log10()+scale_x_log10()

cor.test(allBiomass$sumBiomass,allBiomass$dryBiomass)#0.9050286

#format data to common format###############################################################

myVars <- c("habitat","time","Biomass","Biomass_small","Biomass_large",
            "Name.d..Freiwilligen.volunteer.s.name",
            "Datum.date","Length.of.route.by.GIS",
            "Codierung",
            "Startzeit.starting.time","Ende.end.time",
            "Wind..schwach.mittel.stark.",
            "Temperatur..15.20..20.25..25.30.Grad.Celsius.") 

df <- allBiomass[,myVars]

names(df) <- c("Land_use","Time_band","Biomass","Biomass_small","Biomass_large","PilotID","Date","Route_length",
               "RouteID","StartTime","EndTime","Wind","Temperature")

df$RouteID <- gsub("ün","uen",df$RouteID)
levels(df$Land_use) <- c("Urban","Farmland","Dryland","Wetland","Forest")
df$Land_use <- factor(df$Land_use,levels=c("Urban","Farmland","Dryland","Wetland","Forest"))
df$Time_band <- factor(df$Time_band)
levels(df$Time_band) <- c("midday","evening")


#retrieve 
#temperature -  mean and max
#pet
#humdity - mean and max
#(wind)
#2 m above ground
#DWD

#add Time_driven - diff between start and end time
#add Distance_driven - in m
#add Velocity -  Distance driven/Time_driven 

#X, Y for each route
#utm in m

#standardized temp
#15-20
#20-25
#25-30+

#wind temp
#Light (1.6-3.3 m/s)
#Gentle (3.4 - 5.5 m/s)
#Moderate (5.5-7.9 m/s)

#save processed data file
save(df,file="cleaned-data/DE_rough_landuse_biomass.RData")