setwd("C:/Users/db40fysa/Nextcloud/mobileInsect/04_geodata/atkis_v05")
allFiles <- list.files()
allFiles <- allFiles[!grepl("README.txt",allFiles)]

#get name of shapefie
Names <- allFiles[grepl(".shp",allFiles)]
Name <- gsub(".shp","",Names)

#remove those with Root on them
Name <- Name[!grepl("Root",Name)]

#libraries we need
library(rgdal)
library(plyr)
library(reshape2)

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
library(reshape2)
outputCast <- dcast(output,Codierung~Land_use+Buffer,value.var="value",fun=sum,na.rm=T)
write.table(outputCast,file="cleaned-data/environData_DE.txt",sep="\t")
