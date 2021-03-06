# preparing DK landuse data prepared by Jesper Bladt in GIS. README file in H:\Documents\Insektmobilen\Data\Arealanvendelse_Århus\2018_bufferzones_data\Final_buffers_2018 (Cecilie Svenningsens work drive). README should be included in final git submission. 

# load libraries required for reformatting and merging data
library(tidyverse)
library(readr)
library(stringr)
library(data.table)
library(ggpubr)
library(tidyr)

### load buffer zone files #### 
#oeko <- read.delim("covariate-data/DK_ruter2018_OekoAreas.txt") # oeko is now part of the buffer zone data
hedge <- read.delim("covariate-data/DK_ruter2018_hegnAreas.txt")
urbangreen <- read.delim("covariate-data/DK_ruter2018_urbGreenAreas.txt")
nep <- read_delim("covariate-data/ruter2018buf500_NEP.txt","\t", escape_double = FALSE, trim_ws = TRUE)
buf_50m <- read_delim("covariate-data/ruter2018buf50_areas.txt","\t", escape_double = FALSE, trim_ws = TRUE)
buf_250m <- read_delim("covariate-data/ruter2018buf250_areas.txt","\t", escape_double = FALSE, trim_ws = TRUE)
buf_500m <- read_delim("covariate-data/ruter2018buf500_areas.txt","\t", escape_double = FALSE, trim_ws = TRUE)
buf_1000m <- read_delim("covariate-data/ruter2018buf1000_areas.txt","\t", escape_double = FALSE, trim_ws = TRUE)

##### WIDE format with tidyr: reformatting environmental data #######
# transform oekodata from long to wide format prior to merging 
#oekocast <- oeko %>% pivot_wider(names_from = bufferDist, values_from = propOeko, names_prefix = "propOeko_")

# rename the land use categories to _organic if it is registered as organic
setDT(buf_50m)
buf_50m[oeko == "1", type := paste0(type, "_", "organic")]
setDT(buf_250m)
buf_250m[oeko == "1", type := paste0(type, "_", "organic")]
setDT(buf_500m)
buf_500m[oeko == "1", type := paste0(type, "_", "organic")]
setDT(buf_1000m)
buf_1000m[oeko == "1", type := paste0(type, "_", "organic")]

#test <- dcast(melt(oeko, id.vars=c("routeID", "bufferDist", "propOeko")), routeID~bufferDist+propOeko)

# transform hedgedata and urbangreen from long to wide format prior to merging - note! multiple value columns
hedgecast <-
  hedge %>% pivot_wider(
    names_from = bufferDist,
    values_from = c(hegnLength, byHegnLength, hegnMeterPerHa, byHegnMeterPerHa)
  )

urbangreencast <-
  urbangreen %>% pivot_wider(
    names_from = bufferDist,
    values_from = c(urbGreenAreaHa, urbGreenPropArea)
  )

# buffer zone data - include column with buffer distance for each dataset
buf_50m$bufferDist <- 50
buf_250m$bufferDist <- 250
buf_500m$bufferDist <- 500
buf_1000m$bufferDist <- 1000

#add on land_use data (following 02_DE script to create DK_environData)
buf_50m$propLand_use <- NA
buf_250m$propLand_use <- NA
buf_500m$propLand_use <- NA
buf_1000m$propLand_use <- NA

#wetland
buf_50m$propLand_use[buf_50m$type %in% c("Sø", "Mose", "Sø_organic", "Mose_organic")] <- "Wetland"
buf_250m$propLand_use[buf_250m$type %in% c("Sø", "Mose", "Sø_organic", "Mose_organic")] <- "Wetland"
buf_500m$propLand_use[buf_500m$type %in% c("Sø", "Mose", "Sø_organic", "Mose_organic")] <- "Wetland"
buf_1000m$propLand_use[buf_1000m$type %in% c("Sø", "Mose", "Sø_organic", "Mose_organic")] <- "Wetland"

#forest
buf_50m$propLand_use[buf_50m$type %in% c("Skov","Skov_organic")] <- "Forest"
buf_250m$propLand_use[buf_250m$type %in% c("Skov","Skov_organic")] <- "Forest"
buf_500m$propLand_use[buf_500m$type %in% c("Skov","Skov_organic")] <- "Forest"
buf_1000m$propLand_use[buf_1000m$type %in% c("Skov","Skov_organic")] <- "Forest"

#agriculture
buf_50m$propLand_use[buf_50m$type %in% c("Ekstensiv", "Ekstensiv_organic", "Intensiv", "Intensiv_organic", "Markblok", "Markblok_organic", "Semi-intensiv", "Semi-intensiv_organic")] <- "Agriculture"
buf_250m$propLand_use[buf_250m$type %in% c("Ekstensiv", "Ekstensiv_organic", "Intensiv", "Intensiv_organic", "Markblok", "Markblok_organic", "Semi-intensiv", "Semi-intensiv_organic")] <- "Agriculture"
buf_500m$propLand_use[buf_500m$type %in% c("Ekstensiv", "Ekstensiv_organic", "Intensiv", "Intensiv_organic", "Markblok", "Markblok_organic", "Semi-intensiv", "Semi-intensiv_organic")] <- "Agriculture"
buf_1000m$propLand_use[buf_1000m$type %in% c("Ekstensiv", "Ekstensiv_organic", "Intensiv", "Intensiv_organic", "Markblok", "Markblok_organic", "Semi-intensiv", "Semi-intensiv_organic")] <- "Agriculture"

#urban
buf_50m$propLand_use[buf_50m$type %in% c("Lav bebyggelse", "Erhverv", "Høj bebyggelse", "Bykerne")] <- "Urban"
buf_250m$propLand_use[buf_250m$type %in% c("Lav bebyggelse", "Erhverv", "Høj bebyggelse", "Bykerne")] <- "Urban"
buf_500m$propLand_use[buf_500m$type %in% c("Lav bebyggelse", "Erhverv", "Høj bebyggelse", "Bykerne")] <- "Urban"
buf_1000m$propLand_use[buf_1000m$type %in% c("Lav bebyggelse", "Erhverv", "Høj bebyggelse", "Bykerne")] <- "Urban"

#open uncultivated land
#temp <- subset(buf_1000m,type=="Hede")
#nrow(temp)
#max(temp$areaProportion)

# based on the code above we will make a separate category for heathland
buf_50m$propLand_use[buf_50m$type %in% c("Overdrev", "Overdrev_organic", "Eng", "Eng_organic", "Strandeng", "Strandeng_organic")] <- "Open uncultivated land"
buf_250m$propLand_use[buf_250m$type %in% c("Overdrev", "Overdrev_organic", "Eng", "Eng_organic", "Strandeng", "Strandeng_organic")] <- "Open uncultivated land"
buf_500m$propLand_use[buf_500m$type %in% c("Overdrev", "Overdrev_organic", "Eng", "Eng_organic", "Strandeng", "Strandeng_organic")] <- "Open uncultivated land"
buf_1000m$propLand_use[buf_1000m$type %in% c("Overdrev", "Overdrev_organic", "Eng", "Eng_organic", "Strandeng", "Strandeng_organic")] <- "Open uncultivated land"

buf_50m$propLand_use[buf_50m$type %in% c("Hede", "Hede_organic")] <- "Heathland"
buf_250m$propLand_use[buf_250m$type %in% c("Hede", "Hede_organic")] <- "Heathland"
buf_500m$propLand_use[buf_500m$type %in% c("Hede", "Hede_organic")] <- "Heathland"
buf_1000m$propLand_use[buf_1000m$type %in% c("Hede", "Hede_organic")] <- "Heathland"

#unspecified category
buf_50m$propLand_use[buf_50m$type %in% "Andet"] <- "Unspecified land cover"
buf_250m$propLand_use[buf_250m$type %in% "Andet"] <- "Unspecified land cover"
buf_500m$propLand_use[buf_500m$type %in% "Andet"] <- "Unspecified land cover"
buf_1000m$propLand_use[buf_1000m$type %in% "Andet"] <- "Unspecified land cover"

#subset to the above land-uses for each buffer
# 50
table(buf_50m$propLand_use)
output50 <- subset(buf_50m,!is.na(propLand_use))

#250
table(buf_250m$propLand_use)
output250 <- subset(buf_250m,!is.na(propLand_use))

#500
table(buf_500m$propLand_use)
output500 <- subset(buf_500m,!is.na(propLand_use))

#1000
table(buf_1000m$propLand_use)
output1000 <- subset(buf_1000m,!is.na(propLand_use))

#cast the data
outputCast50 <- reshape2::dcast(output50,routeID~propLand_use+bufferDist,value.var="areaProportion",fun=sum,na.rm=T)
outputCast250 <- reshape2::dcast(output250,routeID~propLand_use+bufferDist,value.var="areaProportion",fun=sum,na.rm=T)
outputCast500 <- reshape2::dcast(output500,routeID~propLand_use+bufferDist,value.var="areaProportion",fun=sum,na.rm=T)
outputCast1000 <- reshape2::dcast(output1000,routeID~propLand_use+bufferDist,value.var="areaProportion",fun=sum,na.rm=T)

#merge buffer zone cast outputs
outputCast <- merge(outputCast50, outputCast250, by = "routeID")
outputCast <- merge(outputCast, outputCast500, by = "routeID")
outputCast <- merge(outputCast, outputCast1000, by = "routeID")

#merge with oeko, urbangreen, and hedge cast outputs
#outputCast <- merge(outputCast, oekocast, by = "routeID")
outputCast <- merge(outputCast, hedgecast, by = "routeID")
outputCast <- merge(outputCast, urbangreencast, by = "routeID")

write.table(outputCast,file="cleaned-data/environData_DK.txt",sep="\t")

### Urban land use reformatting intensity data for analysis in script 06 #####
hedge <- read.delim("covariate-data/DK_ruter2018_hegnAreas.txt")
urbangreen <- read.delim("covariate-data/DK_ruter2018_urbGreenAreas.txt")
nep <- read_delim("covariate-data/ruter2018buf500_NEP.txt","\t", escape_double = FALSE, trim_ws = TRUE)
buf_50m <- read_delim("covariate-data/ruter2018buf50_areas.txt","\t", escape_double = FALSE, trim_ws = TRUE)
buf_250m <- read_delim("covariate-data/ruter2018buf250_areas.txt","\t", escape_double = FALSE, trim_ws = TRUE)
buf_500m <- read_delim("covariate-data/ruter2018buf500_areas.txt","\t", escape_double = FALSE, trim_ws = TRUE)
buf_1000m <- read_delim("covariate-data/ruter2018buf1000_areas.txt","\t", escape_double = FALSE, trim_ws = TRUE)

##### Urban reformatting environmental data to WIDE format with tidyr #######
# transform oekodata from long to wide format prior to merging 
#oekocast <- oeko %>% pivot_wider(names_from = bufferDist, values_from = propOeko, names_prefix = "propOeko_")

# rename the land use categories to _organic if it is registered as organic
setDT(buf_50m)
buf_50m[oeko == "1", type := paste0(type, "_", "organic")]
setDT(buf_250m)
buf_250m[oeko == "1", type := paste0(type, "_", "organic")]
setDT(buf_500m)
buf_500m[oeko == "1", type := paste0(type, "_", "organic")]
setDT(buf_1000m)
buf_1000m[oeko == "1", type := paste0(type, "_", "organic")]

#test <- dcast(melt(oeko, id.vars=c("routeID", "bufferDist", "propOeko")), routeID~bufferDist+propOeko)

# transform hedgedata and urbangreen from long to wide format prior to merging - note! multiple value columns
hedgecast <-
  hedge %>% pivot_wider(
    names_from = bufferDist,
    values_from = c(hegnLength, byHegnLength, hegnMeterPerHa, byHegnMeterPerHa)
  )

urbangreencast <-
  urbangreen %>% pivot_wider(
    names_from = bufferDist,
    values_from = c(urbGreenAreaHa, urbGreenPropArea)
  )

# buffer zone data - include column with buffer distance for each dataset
buf_50m$bufferDist <- 50
buf_250m$bufferDist <- 250
buf_500m$bufferDist <- 500
buf_1000m$bufferDist <- 1000

#add on land_use data (following 02_DE script to create DK_environData)
buf_50m$propLand_use <- NA
buf_250m$propLand_use <- NA
buf_500m$propLand_use <- NA
buf_1000m$propLand_use <- NA

#urban
buf_50m$propLand_use[buf_50m$type %in% c("Lav bebyggelse", "Erhverv", "Høj bebyggelse", "Bykerne")] <- "Urban"
buf_250m$propLand_use[buf_250m$type %in% c("Lav bebyggelse", "Erhverv", "Høj bebyggelse", "Bykerne")] <- "Urban"
buf_500m$propLand_use[buf_500m$type %in% c("Lav bebyggelse", "Erhverv", "Høj bebyggelse", "Bykerne")] <- "Urban"
buf_1000m$propLand_use[buf_1000m$type %in% c("Lav bebyggelse", "Erhverv", "Høj bebyggelse", "Bykerne")] <- "Urban"

#subset to the above land-uses for each buffer
# 50
table(buf_50m$propLand_use)
output50 <- subset(buf_50m,!is.na(propLand_use))

#250
table(buf_250m$propLand_use)
output250 <- subset(buf_250m,!is.na(propLand_use))

#500
table(buf_500m$propLand_use)
output500 <- subset(buf_500m,!is.na(propLand_use))

#1000
table(buf_1000m$propLand_use)
output1000 <- subset(buf_1000m,!is.na(propLand_use))

#cast the data
outputCast50 <- reshape2::dcast(output50,routeID~propLand_use+bufferDist,value.var="areaProportion",fun=sum,na.rm=T)
outputCast250 <- reshape2::dcast(output250,routeID~propLand_use+bufferDist,value.var="areaProportion",fun=sum,na.rm=T)
outputCast500 <- reshape2::dcast(output500,routeID~propLand_use+bufferDist,value.var="areaProportion",fun=sum,na.rm=T)
outputCast1000 <- reshape2::dcast(output1000,routeID~propLand_use+bufferDist,value.var="areaProportion",fun=sum,na.rm=T)

urbanoutputCast_1000 <- dcast(output1000,routeID~type+bufferDist,value.var="areaProportion",fun=sum,na.rm=T)
urbanoutputCast_500 <- dcast(output500,routeID~type+bufferDist,value.var="areaProportion",fun=sum,na.rm=T)
urbanoutputCast_250 <- dcast(output250,routeID~type+bufferDist,value.var="areaProportion",fun=sum,na.rm=T)
urbanoutputCast_50 <- dcast(output50,routeID~type+bufferDist,value.var="areaProportion",fun=sum,na.rm=T)

urbanoutputCast <- merge(urbanoutputCast_1000, urbanoutputCast_500, by = "routeID")
urbanoutputCast <- merge(urbanoutputCast, urbanoutputCast_250, by = "routeID")
urbanoutputCast <- merge(urbanoutputCast, urbanoutputCast_50, by = "routeID")

write.table(urbanoutputCast,file="cleaned-data/urban_landuse_intensity_DK.txt",sep="\t")

### Farmland reformatting farmland land use intensity data for analysis in script 06 #####
hedge <- read.delim("covariate-data/DK_ruter2018_hegnAreas.txt")
urbangreen <- read.delim("covariate-data/DK_ruter2018_urbGreenAreas.txt")
nep <- read_delim("covariate-data/ruter2018buf500_NEP.txt","\t", escape_double = FALSE, trim_ws = TRUE)
buf_50m <- read_delim("covariate-data/ruter2018buf50_areas.txt","\t", escape_double = FALSE, trim_ws = TRUE)
buf_250m <- read_delim("covariate-data/ruter2018buf250_areas.txt","\t", escape_double = FALSE, trim_ws = TRUE)
buf_500m <- read_delim("covariate-data/ruter2018buf500_areas.txt","\t", escape_double = FALSE, trim_ws = TRUE)
buf_1000m <- read_delim("covariate-data/ruter2018buf1000_areas.txt","\t", escape_double = FALSE, trim_ws = TRUE)

##### Farmland reformatting environmental data to WIDE format with tidyr #######
# transform oekodata from long to wide format prior to merging 
#oekocast <- oeko %>% pivot_wider(names_from = bufferDist, values_from = propOeko, names_prefix = "propOeko_")

# rename the land use categories to _organic if it is registered as organic
setDT(buf_50m)
buf_50m[oeko == "1", type := paste0(type, "_", "organic")]
setDT(buf_250m)
buf_250m[oeko == "1", type := paste0(type, "_", "organic")]
setDT(buf_500m)
buf_500m[oeko == "1", type := paste0(type, "_", "organic")]
setDT(buf_1000m)
buf_1000m[oeko == "1", type := paste0(type, "_", "organic")]

#test <- dcast(melt(oeko, id.vars=c("routeID", "bufferDist", "propOeko")), routeID~bufferDist+propOeko)

# transform hedgedata and urbangreen from long to wide format prior to merging - note! multiple value columns
hedgecast <-
  hedge %>% pivot_wider(
    names_from = bufferDist,
    values_from = c(hegnLength, byHegnLength, hegnMeterPerHa, byHegnMeterPerHa)
  )

urbangreencast <-
  urbangreen %>% pivot_wider(
    names_from = bufferDist,
    values_from = c(urbGreenAreaHa, urbGreenPropArea)
  )

# buffer zone data - include column with buffer distance for each dataset
buf_50m$bufferDist <- 50
buf_250m$bufferDist <- 250
buf_500m$bufferDist <- 500
buf_1000m$bufferDist <- 1000

#add on land_use data (following 02_DE script to create DK_environData)
buf_50m$propLand_use <- NA
buf_250m$propLand_use <- NA
buf_500m$propLand_use <- NA
buf_1000m$propLand_use <- NA

#agriculture
buf_50m$propLand_use[buf_50m$type %in% c("Ekstensiv", "Ekstensiv_organic", "Intensiv", "Intensiv_organic", "Markblok", "Markblok_organic", "Semi-intensiv", "Semi-intensiv_organic")] <- "Agriculture"
buf_250m$propLand_use[buf_250m$type %in% c("Ekstensiv", "Ekstensiv_organic", "Intensiv", "Intensiv_organic", "Markblok", "Markblok_organic", "Semi-intensiv", "Semi-intensiv_organic")] <- "Agriculture"
buf_500m$propLand_use[buf_500m$type %in% c("Ekstensiv", "Ekstensiv_organic", "Intensiv", "Intensiv_organic", "Markblok", "Markblok_organic", "Semi-intensiv", "Semi-intensiv_organic")] <- "Agriculture"
buf_1000m$propLand_use[buf_1000m$type %in% c("Ekstensiv", "Ekstensiv_organic", "Intensiv", "Intensiv_organic", "Markblok", "Markblok_organic", "Semi-intensiv", "Semi-intensiv_organic")] <- "Agriculture"

#subset to the above land-uses for each buffer
# 50
table(buf_50m$propLand_use)
output50 <- subset(buf_50m,!is.na(propLand_use))

#250
table(buf_250m$propLand_use)
output250 <- subset(buf_250m,!is.na(propLand_use))

#500
table(buf_500m$propLand_use)
output500 <- subset(buf_500m,!is.na(propLand_use))

#1000
table(buf_1000m$propLand_use)
output1000 <- subset(buf_1000m,!is.na(propLand_use))

farmlandoutputCast_1000 <- dcast(output1000,routeID~type+bufferDist,value.var="areaProportion",fun=sum,na.rm=T)
farmlandoutputCast_500 <- dcast(output500,routeID~type+bufferDist,value.var="areaProportion",fun=sum,na.rm=T)
farmlandoutputCast_250 <- dcast(output250,routeID~type+bufferDist,value.var="areaProportion",fun=sum,na.rm=T)
farmlandoutputCast_50 <- dcast(output50,routeID~type+bufferDist,value.var="areaProportion",fun=sum,na.rm=T)

farmlandoutputCast <- merge(farmlandoutputCast_1000, farmlandoutputCast_500, by = "routeID")
farmlandoutputCast <- merge(farmlandoutputCast, farmlandoutputCast_250, by = "routeID")
farmlandoutputCast <- merge(farmlandoutputCast, farmlandoutputCast_50, by = "routeID")

write.table(farmlandoutputCast,file="cleaned-data/farmland_landuse_intensity_DK.txt",sep="\t")

### Wetland reformatting wetland land use intensity data for analysis in script 06 #####
hedge <- read.delim("covariate-data/DK_ruter2018_hegnAreas.txt")
urbangreen <- read.delim("covariate-data/DK_ruter2018_urbGreenAreas.txt")
nep <- read_delim("covariate-data/ruter2018buf500_NEP.txt","\t", escape_double = FALSE, trim_ws = TRUE)
buf_50m <- read_delim("covariate-data/ruter2018buf50_areas.txt","\t", escape_double = FALSE, trim_ws = TRUE)
buf_250m <- read_delim("covariate-data/ruter2018buf250_areas.txt","\t", escape_double = FALSE, trim_ws = TRUE)
buf_500m <- read_delim("covariate-data/ruter2018buf500_areas.txt","\t", escape_double = FALSE, trim_ws = TRUE)
buf_1000m <- read_delim("covariate-data/ruter2018buf1000_areas.txt","\t", escape_double = FALSE, trim_ws = TRUE)

##### Wetland reformatting environmental data to WIDE format with tidyr #######
# transform oekodata from long to wide format prior to merging 
#oekocast <- oeko %>% pivot_wider(names_from = bufferDist, values_from = propOeko, names_prefix = "propOeko_")

# rename the land use categories to _organic if it is registered as organic
setDT(buf_50m)
buf_50m[oeko == "1", type := paste0(type, "_", "organic")]
setDT(buf_250m)
buf_250m[oeko == "1", type := paste0(type, "_", "organic")]
setDT(buf_500m)
buf_500m[oeko == "1", type := paste0(type, "_", "organic")]
setDT(buf_1000m)
buf_1000m[oeko == "1", type := paste0(type, "_", "organic")]

#test <- dcast(melt(oeko, id.vars=c("routeID", "bufferDist", "propOeko")), routeID~bufferDist+propOeko)

# transform hedgedata and urbangreen from long to wide format prior to merging - note! multiple value columns
hedgecast <-
  hedge %>% pivot_wider(
    names_from = bufferDist,
    values_from = c(hegnLength, byHegnLength, hegnMeterPerHa, byHegnMeterPerHa)
  )

urbangreencast <-
  urbangreen %>% pivot_wider(
    names_from = bufferDist,
    values_from = c(urbGreenAreaHa, urbGreenPropArea)
  )

# buffer zone data - include column with buffer distance for each dataset
buf_50m$bufferDist <- 50
buf_250m$bufferDist <- 250
buf_500m$bufferDist <- 500
buf_1000m$bufferDist <- 1000

#add on land_use data (following 02_DE script to create DK_environData)
buf_50m$propLand_use <- NA
buf_250m$propLand_use <- NA
buf_500m$propLand_use <- NA
buf_1000m$propLand_use <- NA

#wetland
buf_50m$propLand_use[buf_50m$type %in% c("Sø", "Mose", "Sø_organic", "Mose_organic")] <- "Wetland"
buf_250m$propLand_use[buf_250m$type %in% c("Sø", "Mose", "Sø_organic", "Mose_organic")] <- "Wetland"
buf_500m$propLand_use[buf_500m$type %in% c("Sø", "Mose", "Sø_organic", "Mose_organic")] <- "Wetland"
buf_1000m$propLand_use[buf_1000m$type %in% c("Sø", "Mose", "Sø_organic", "Mose_organic")] <- "Wetland"

#subset to the above land-uses for each buffer
# 50
table(buf_50m$propLand_use)
output50 <- subset(buf_50m,!is.na(propLand_use))

#250
table(buf_250m$propLand_use)
output250 <- subset(buf_250m,!is.na(propLand_use))

#500
table(buf_500m$propLand_use)
output500 <- subset(buf_500m,!is.na(propLand_use))

#1000
table(buf_1000m$propLand_use)
output1000 <- subset(buf_1000m,!is.na(propLand_use))

wetlandoutputCast_1000 <- dcast(output1000,routeID~type+bufferDist,value.var="areaProportion",fun=sum,na.rm=T)
wetlandoutputCast_500 <- dcast(output500,routeID~type+bufferDist,value.var="areaProportion",fun=sum,na.rm=T)
wetlandoutputCast_250 <- dcast(output250,routeID~type+bufferDist,value.var="areaProportion",fun=sum,na.rm=T)
wetlandoutputCast_50 <- dcast(output50,routeID~type+bufferDist,value.var="areaProportion",fun=sum,na.rm=T)

wetlandoutputCast <- merge(wetlandoutputCast_1000, wetlandoutputCast_500, by = "routeID")
wetlandoutputCast <- merge(wetlandoutputCast, wetlandoutputCast_250, by = "routeID")
wetlandoutputCast <- merge(wetlandoutputCast, wetlandoutputCast_50, by = "routeID")

write.table(wetlandoutputCast,file="cleaned-data/wetland_landuse_intensity_DK.txt",sep="\t")

### grassland land use intensity #################
### Urban land use reformatting intensity data for analysis in script 06 #####
#nep <- read_delim("covariate-data/ruter2018buf500_NEP.txt","\t", escape_double = FALSE, trim_ws = TRUE)
buf_50m <- read_delim("covariate-data/ruter2018buf50_areas.txt","\t", escape_double = FALSE, trim_ws = TRUE)
buf_250m <- read_delim("covariate-data/ruter2018buf250_areas.txt","\t", escape_double = FALSE, trim_ws = TRUE)
buf_500m <- read_delim("covariate-data/ruter2018buf500_areas.txt","\t", escape_double = FALSE, trim_ws = TRUE)
buf_1000m <- read_delim("covariate-data/ruter2018buf1000_areas.txt","\t", escape_double = FALSE, trim_ws = TRUE)

##### Urban reformatting environmental data to WIDE format with tidyr #######
# transform oekodata from long to wide format prior to merging 
#oekocast <- oeko %>% pivot_wider(names_from = bufferDist, values_from = propOeko, names_prefix = "propOeko_")

# rename the land use categories to _organic if it is registered as organic
setDT(buf_50m)
buf_50m[oeko == "1", type := paste0(type, "_", "organic")]
setDT(buf_250m)
buf_250m[oeko == "1", type := paste0(type, "_", "organic")]
setDT(buf_500m)
buf_500m[oeko == "1", type := paste0(type, "_", "organic")]
setDT(buf_1000m)
buf_1000m[oeko == "1", type := paste0(type, "_", "organic")]

# buffer zone data - include column with buffer distance for each dataset
buf_50m$bufferDist <- 50
buf_250m$bufferDist <- 250
buf_500m$bufferDist <- 500
buf_1000m$bufferDist <- 1000

#add on land_use data (following 02_DE script to create DK_environData)
buf_50m$propLand_use <- NA
buf_250m$propLand_use <- NA
buf_500m$propLand_use <- NA
buf_1000m$propLand_use <- NA

# based on the code above we will make a separate category for heathland
buf_50m$propLand_use[buf_50m$type %in% c("Overdrev", "Overdrev_organic", "Eng", "Eng_organic", "Strandeng", "Strandeng_organic")] <- "Open uncultivated land"
buf_250m$propLand_use[buf_250m$type %in% c("Overdrev", "Overdrev_organic", "Eng", "Eng_organic", "Strandeng", "Strandeng_organic")] <- "Open uncultivated land"
buf_500m$propLand_use[buf_500m$type %in% c("Overdrev", "Overdrev_organic", "Eng", "Eng_organic", "Strandeng", "Strandeng_organic")] <- "Open uncultivated land"
buf_1000m$propLand_use[buf_1000m$type %in% c("Overdrev", "Overdrev_organic", "Eng", "Eng_organic", "Strandeng", "Strandeng_organic")] <- "Open uncultivated land"

#subset to the above land-uses for each buffer
# 50
table(buf_50m$propLand_use)
output50 <- subset(buf_50m,!is.na(propLand_use))

#250
table(buf_250m$propLand_use)
output250 <- subset(buf_250m,!is.na(propLand_use))

#500
table(buf_500m$propLand_use)
output500 <- subset(buf_500m,!is.na(propLand_use))

#1000
table(buf_1000m$propLand_use)
output1000 <- subset(buf_1000m,!is.na(propLand_use))

#cast the data
outputCast50 <- reshape2::dcast(output50,routeID~propLand_use+bufferDist,value.var="areaProportion",fun=sum,na.rm=T)
outputCast250 <- reshape2::dcast(output250,routeID~propLand_use+bufferDist,value.var="areaProportion",fun=sum,na.rm=T)
outputCast500 <- reshape2::dcast(output500,routeID~propLand_use+bufferDist,value.var="areaProportion",fun=sum,na.rm=T)
outputCast1000 <- reshape2::dcast(output1000,routeID~propLand_use+bufferDist,value.var="areaProportion",fun=sum,na.rm=T)

grasslandoutputCast_1000 <- dcast(output1000,routeID~type+bufferDist,value.var="areaProportion",fun=sum,na.rm=T)
grasslandoutputCast_500 <- dcast(output500,routeID~type+bufferDist,value.var="areaProportion",fun=sum,na.rm=T)
grasslandoutputCast_250 <- dcast(output250,routeID~type+bufferDist,value.var="areaProportion",fun=sum,na.rm=T)
grasslandoutputCast_50 <- dcast(output50,routeID~type+bufferDist,value.var="areaProportion",fun=sum,na.rm=T)

grasslandoutputCast <- merge(grasslandoutputCast_1000, grasslandoutputCast_500, by = "routeID")
grasslandoutputCast <- merge(grasslandoutputCast, grasslandoutputCast_250, by = "routeID")
grasslandoutputCast <- merge(grasslandoutputCast, grasslandoutputCast_50, by = "routeID")

write.table(grasslandoutputCast,file="cleaned-data/grassland_landuse_intensity_DK.txt",sep="\t")
