setwd("C:/Users/db40fysa/Nextcloud/mobileInsect/04_geodata/atkis_v3")
allFiles <- list.files()
allFiles <- allFiles[!grepl("README.txt",allFiles)]

#libraries we need
library(rgdal)
library(plyr)
library(reshape2)

#for each folder
output <- ldply(allFiles, function(x){

  #get name of shapefie
  allNames <- list.files(x)  
  Names <- allNames[grepl(".shp",allNames)]
  Name <- gsub(".shp","",Names)
  
  #read in shape file
  out <- readOGR(dsn=x,
               layer=Name)

  #just get data frame
  temp <-out@data
  temp$File <- as.character(x)
  
  if("prop" %in% names(temp)){
    temp <- temp[,c("Name","Codierung","prop","layer","File")]
    names(temp)[3] <- "value"
    return(temp)
    
  }else if ("dens" %in% names(temp)){
    temp <- temp[,c("Name","Codierung","dens","layer","File")]
    names(temp)[3] <- "value"
    return(temp)
  }
  
})

#change NAs to zeros
output$value[is.na(output$value)] <- 0

#maybe use Codierung to match
unique(output$Codierung)
output$Codierung <- gsub("ï¿½","ue",output$Codierung,fixed=TRUE)
output$Codierung <- gsub("?","ue",output$Codierung,fixed=TRUE)
output$Codierung <- gsub("Ã¼","ue",output$Codierung,fixed=TRUE)
unique(output$Codierung)

#extract buffer size
output$Buffer <- sapply(output$layer,function(x){
  if(grepl("500m",x)){
    500
  }else if(grepl("1000m",x)){
    1000
  }else if(grepl("250m",x)){
    250
  }
})

#check habitat type matches with raw data file

#extract layer info
output$LU <- sapply(as.character(output$layer),function(x){
  split <- strsplit(x,"_")[[1]]
  split[length(split)]
})
output$LU <- gsub("250m","",output$LU)
output$LU <- gsub("500m","",output$LU)
output$LU <- gsub("1000m","",output$LU)
table(output$LU)

#check all codierung in the insect data file
df$RouteID[df$RouteID %in% output$Codierung]
df$RouteID[!df$RouteID %in% output$Codierung]#yay!!

#cast the data
outputCast <- dcast(output,Codierung~LU+Buffer,value.var="value",fun=sum)
#Agrar_01,agricultural_250
df <- merge(df,output,by.x="RouteID",by.y="Codierung",all.x=T)
