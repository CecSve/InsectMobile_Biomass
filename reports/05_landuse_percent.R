#get land use data
land_use <- read.delim("cleaned-data/environData_DE.txt")

#check all route data present
all(allInsects$RouteID %in% land_use$Codierung)

#merge insect and land use
allInsects <- merge(allInsects,land_use,by.x="RouteID",by.y="Codierung")

#get buffer size with largest effect for each land use
library(lme4)
library(lmerTest)
getEffect <- function(model){summary(model)$coef[2,]}

#agriculture
hist(allInsects$Agriculture_1000)
lme50 <- lmer(log(Biomass+1) ~ Agriculture_50 + Time_band + 
               Time_band:as.numeric(StartTime) + yDay + 
               (1|RouteID) + (1|PilotID), data=allInsects)
lme250 <- lmer(log(Biomass+1) ~ Agriculture_250 + Time_band + 
                Time_band:as.numeric(StartTime) + yDay + 
                (1|RouteID) + (1|PilotID), data=allInsects)
lme500 <- lmer(log(Biomass+1) ~ Agriculture_500 + Time_band + 
                 Time_band:as.numeric(StartTime) + yDay + 
                 (1|RouteID) + (1|PilotID), data=allInsects)
lme1000 <- lmer(log(Biomass+1) ~ Agriculture_1000 + Time_band + 
                 Time_band:as.numeric(StartTime) + yDay + 
                 (1|RouteID) + (1|PilotID), data=allInsects)
outAgri <- rbind(getEffect(lme50),getEffect(lme250),getEffect(lme500),getEffect(lme1000))

#urban
hist(allInsects$Urban_1000)
lme50 <- lmer(log(Biomass+1) ~ Urban_50 + Time_band + 
                Time_band:as.numeric(StartTime) + yDay + 
                (1|RouteID) + (1|PilotID), data=allInsects)
lme250 <- lmer(log(Biomass+1) ~ Urban_250 + Time_band + 
                 Time_band:as.numeric(StartTime) + yDay + 
                 (1|RouteID) + (1|PilotID), data=allInsects)
lme500 <- lmer(log(Biomass+1) ~ Urban_500 + Time_band + 
                 Time_band:as.numeric(StartTime) + yDay + 
                 (1|RouteID) + (1|PilotID), data=allInsects)
lme1000 <- lmer(log(Biomass+1) ~ Urban_1000 + Time_band + 
                  Time_band:as.numeric(StartTime) + yDay + 
                  (1|RouteID) + (1|PilotID), data=allInsects)
outUrban <- rbind(getEffect(lme50),getEffect(lme250),getEffect(lme500),getEffect(lme1000))

#grassland
lme50 <- lmer(log(Biomass+1) ~ Grassland_50 + Time_band + 
                Time_band:as.numeric(StartTime) + yDay + 
                (1|RouteID) + (1|PilotID), data=allInsects)
lme250 <- lmer(log(Biomass+1) ~ Grassland_250 + Time_band + 
                 Time_band:as.numeric(StartTime) + yDay + 
                 (1|RouteID) + (1|PilotID), data=allInsects)
lme500 <- lmer(log(Biomass+1) ~ Grassland_500 + Time_band + 
                 Time_band:as.numeric(StartTime) + yDay + 
                 (1|RouteID) + (1|PilotID), data=allInsects)
lme1000 <- lmer(log(Biomass+1) ~ Grassland_1000 + Time_band + 
                  Time_band:as.numeric(StartTime) + yDay + 
                  (1|RouteID) + (1|PilotID), data=allInsects)
outGrassland <- rbind(getEffect(lme50),getEffect(lme250),getEffect(lme500),getEffect(lme1000))

#forest
lme50 <- lmer(log(Biomass+1) ~ Forest_50 + Time_band + 
                Time_band:as.numeric(StartTime) + yDay + 
                (1|RouteID) + (1|PilotID), data=allInsects)
lme250 <- lmer(log(Biomass+1) ~ Forest_250 + Time_band + 
                 Time_band:as.numeric(StartTime) + yDay + 
                 (1|RouteID) + (1|PilotID), data=allInsects)
lme500 <- lmer(log(Biomass+1) ~ Forest_500 + Time_band + 
                 Time_band:as.numeric(StartTime) + yDay + 
                 (1|RouteID) + (1|PilotID), data=allInsects)
lme1000 <- lmer(log(Biomass+1) ~ Forest_1000 + Time_band + 
                  Time_band:as.numeric(StartTime) + yDay + 
                  (1|RouteID) + (1|PilotID), data=allInsects)
outForest<- rbind(getEffect(lme50),getEffect(lme250),getEffect(lme500),getEffect(lme1000))

#effects seem to be largest at 1000m
lme1000 <- lmer(log(Biomass+1) ~ log(Agriculture_1000+1) + log(Urban_1000+1)+Time_band + 
                  Time_band:as.numeric(StartTime) + yDay + 
                  (1|RouteID) + (1|PilotID), data=allInsects)
summary(lme1000)
