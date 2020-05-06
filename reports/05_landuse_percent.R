#run mergeData script

library(cowplot)
library(ggplot2)

####colour################################################################

#decide on common color scheme
landuseCols <- wes_palette('Darjeeling1', 5, type = c("discrete"))

###plot land cover##############################################

qU <- ggplot(allInsects)+
              geom_point(aes(x=Urban_1000,y=(Biomass+1)),
                         col=landuseCols[1])+
              scale_y_log10() +
              theme_bw() +
              xlab("Urban cover") +ylab("Biomass")

qF <- ggplot(allInsects)+
  geom_point(aes(x=Agriculture_1000,y=(Biomass+1)),
             col=landuseCols[2])+
  scale_y_log10() +
  theme_bw() +
  xlab("Farmland cover") +ylab("Biomass")

qD <- ggplot(allInsects)+
  geom_point(aes(x=Grassland_1000,y=(Biomass+1)),
             col=landuseCols[3])+
  scale_y_log10() +
  theme_bw() +
  xlab("Dryland cover") +ylab("Biomass")

qW <- ggplot(allInsects)+
  geom_point(aes(x=Wetland_1000,y=(Biomass+1)),
             col=landuseCols[4])+
  scale_y_log10() +
  theme_bw() +
  xlab("Wetland cover") +ylab("Biomass")

qFo <- ggplot(allInsects)+
  geom_point(aes(x=Forest_1000,y=(Biomass+1)),
             col=landuseCols[5])+
  scale_y_log10() +
  theme_bw() +
  xlab("Forest cover") +ylab("Biomass")

plot_grid(qU,qF,qD,qW,qFo)

###plot buffers#################################################

#farmland
b50 <- ggplot(allInsects)+
  geom_point(aes(x=Agriculture_50,y=(Biomass+1)),
             col=landuseCols[2])+
  scale_y_log10() +
  theme_bw() +
  xlab("Farmland cover at 50m") +ylab("Biomass")

b250 <- ggplot(allInsects)+
  geom_point(aes(x=Agriculture_250,y=(Biomass+1)),
             col=landuseCols[2])+
  scale_y_log10() +
  theme_bw() +
  xlab("Farmland cover at 250m") +ylab("Biomass")

b1000 <- ggplot(allInsects)+
  geom_point(aes(x=Agriculture_1000,y=(Biomass+1)),
             col=landuseCols[2])+
  scale_y_log10() +
  theme_bw() +
  xlab("Farmland cover at 1000m") +ylab("Biomass")

plot_grid(b50,b250,b1000,ncol=1)

#urban
b50 <- ggplot(allInsects)+
  geom_point(aes(x=Urban_50,y=(Biomass+1)),
             col=landuseCols[2])+
  scale_y_log10() +
  theme_bw() +
  xlab("Urban cover at 50m") +ylab("Biomass")

b250 <- ggplot(allInsects)+
  geom_point(aes(x=Urban_250,y=(Biomass+1)),
             col=landuseCols[2])+
  scale_y_log10() +
  theme_bw() +
  xlab("Urban cover at 250m") +ylab("Biomass")

b1000 <- ggplot(allInsects)+
  geom_point(aes(x=Urban_1000,y=(Biomass+1)),
             col=landuseCols[2])+
  scale_y_log10() +
  theme_bw() +
  xlab("Urban cover at 1000m") +ylab("Biomass")

plot_grid(b50,b250,b1000,ncol=1)

###analysis####################################################

#spatial effect is small so we ignore it for the moment

library(lme4)
library(lmerTest)
getEffect <- function(model){
  coefs <- summary(model)$coef[2,]
  temp <- confint(model)
  cis <- temp[5,]
  data.frame(t(coefs),t(cis))
  }

#agriculture
hist(allInsects$Agriculture_1000)
lme50 <- lmer(log(Biomass+1) ~ Agriculture_50 + Time_band + 
                Time_band:cnumberTime + cTL + cyDay + 
               (1|RouteID) + (1|PilotID), data=allInsects)
lme250 <- lmer(log(Biomass+1) ~ Agriculture_250 + Time_band + 
                 Time_band:cnumberTime + cTL + cyDay +  
                (1|RouteID) + (1|PilotID), data=allInsects)
lme500 <- lmer(log(Biomass+1) ~ Agriculture_500 + Time_band + 
                 Time_band:cnumberTime + cTL + cyDay +  
                 (1|RouteID) + (1|PilotID), data=allInsects)
lme1000 <- lmer(log(Biomass+1) ~ Agriculture_1000 + Time_band + 
                  Time_band:cnumberTime + cTL + cyDay +  
                 (1|RouteID) + (1|PilotID), data=allInsects)
outAgri <- rbind(getEffect(lme50),getEffect(lme250),getEffect(lme500),getEffect(lme1000))
outAgri <- as.data.frame(outAgri)
outAgri$Buffer <- c(50,250,500,1000)
outAgri$Land_use <- "Farmland"

#urban
hist(allInsects$Urban_1000)#should we log it?
lme50 <- lmer(log(Biomass+1) ~ Urban_50 + Time_band + 
                Time_band:cnumberTime + cTL + cyDay + 
                (1|RouteID) + (1|PilotID), data=allInsects)
lme250 <- lmer(log(Biomass+1) ~ Urban_250 + Time_band + 
                 Time_band:cnumberTime + cTL + cyDay +  
                 (1|RouteID) + (1|PilotID), data=allInsects)
lme500 <- lmer(log(Biomass+1) ~ Urban_500 + Time_band + 
                 Time_band:cnumberTime + cTL + cyDay +  
                 (1|RouteID) + (1|PilotID), data=allInsects)
lme1000 <- lmer(log(Biomass+1) ~ Urban_1000 + Time_band + 
                  Time_band:cnumberTime + cTL + cyDay +  
                  (1|RouteID) + (1|PilotID), data=allInsects)
outUrban <- rbind(getEffect(lme50),getEffect(lme250),getEffect(lme500),getEffect(lme1000))
outUrban <- as.data.frame(outUrban)
outUrban$Buffer <- c(50,250,500,1000)
outUrban$Land_use <- "Urban"

#grassland
hist(allInsects$Grassland_250)#log??
lme50 <- lmer(log(Biomass+1) ~ Grassland_50 + Time_band + 
                Time_band:cnumberTime + cTL + cyDay + 
                (1|RouteID) + (1|PilotID), data=allInsects)
lme250 <- lmer(log(Biomass+1) ~ Grassland_250 + Time_band + 
                 Time_band:cnumberTime + cTL + cyDay +  
                 (1|RouteID) + (1|PilotID), data=allInsects)
lme500 <- lmer(log(Biomass+1) ~ Grassland_500 + Time_band + 
                 Time_band:cnumberTime + cTL + cyDay +  
                 (1|RouteID) + (1|PilotID), data=allInsects)
lme1000 <- lmer(log(Biomass+1) ~ Grassland_1000 + Time_band + 
                  Time_band:cnumberTime + cTL + cyDay +  
                  (1|RouteID) + (1|PilotID), data=allInsects)
outGrassland <- rbind(getEffect(lme50),getEffect(lme250),getEffect(lme500),getEffect(lme1000))
outGrassland <- as.data.frame(outGrassland)
outGrassland$Buffer <- c(50,250,500,1000)
outGrassland$Land_use <- "Grassland"

#forest
hist(allInsects$Forest_250)#log??
lme50 <- lmer(log(Biomass+1) ~ Forest_50 + Time_band + 
                Time_band:cnumberTime + cTL + cyDay + 
                (1|RouteID) + (1|PilotID), data=allInsects)
lme250 <- lmer(log(Biomass+1) ~ Forest_250 + Time_band + 
                 Time_band:cnumberTime + cTL + cyDay +  
                 (1|RouteID) + (1|PilotID), data=allInsects)
lme500 <- lmer(log(Biomass+1) ~ Forest_500 + Time_band + 
                 Time_band:cnumberTime + cTL + cyDay +  
                 (1|RouteID) + (1|PilotID), data=allInsects)
lme1000 <- lmer(log(Biomass+1) ~ Forest_1000 + Time_band + 
                  Time_band:cnumberTime + cTL + cyDay +  
                  (1|RouteID) + (1|PilotID), data=allInsects)
outForest<- rbind(getEffect(lme50),getEffect(lme250),getEffect(lme500),getEffect(lme1000))
outForest <- as.data.frame(outForest)
outForest$Buffer <- c(50,250,500,1000)
outForest$Land_use <- "Forest"

#wetland
hist(allInsects$Wetland_1000)#log??
lme50 <- lmer(log(Biomass+1) ~ Wetland_50 + Time_band + 
                Time_band:cnumberTime + cTL + cyDay + 
                (1|RouteID) + (1|PilotID), data=allInsects)
lme250 <- lmer(log(Biomass+1) ~ Wetland_250 + Time_band + 
                 Time_band:cnumberTime + cTL + cyDay +  
                 (1|RouteID) + (1|PilotID), data=allInsects)
lme500 <- lmer(log(Biomass+1) ~ Wetland_500 + Time_band + 
                 Time_band:cnumberTime + cTL + cyDay +  
                 (1|RouteID) + (1|PilotID), data=allInsects)
lme1000 <- lmer(log(Biomass+1) ~ Wetland_1000 + Time_band + 
                  Time_band:cnumberTime + cTL + cyDay +  
                  (1|RouteID) + (1|PilotID), data=allInsects)
outWetland<- rbind(getEffect(lme50),getEffect(lme250),getEffect(lme500),getEffect(lme1000))
outWetland <- as.data.frame(outWetland)
outWetland$Buffer <- c(50,250,500,1000)
outWetland$Land_use <- "Wetland"


#combine all effects
outAll <- rbind(outForest,outAgri,outUrban,outWetland,outGrassland)

#plot efffects
ggplot(outAll)+
  geom_crossbar(aes(x=factor(Buffer),y=Estimate,
                    ymin=X2.5..,ymax=X97.5..))+
  facet_wrap(~Land_use)+
  coord_flip()+
  theme_bw()

ggplot(subset(outAll,Land_use!="Wetland"))+
  geom_crossbar(aes(x=factor(Buffer),y=Estimate,
                    ymin=X2.5..,ymax=X97.5..,
                    fill=Land_use),
                width=0.5)+
  facet_wrap(~Land_use)+
  scale_fill_manual(values=landuseCols[-5])+
  coord_flip()+
  theme_bw()+
  geom_hline(yintercept=0,colour="red",linetype="dashed")+
  xlab("Buffer size (m)") + ylab("Effect of percent change on biomass")


