#run report 03_mergeData

#devtools::install_github("mikabr/ggpirate")
library(ggplot2)
library(plyr)
library(ggpirate)
library(cowplot)
library(wesanderson)

####colour################################################################

#decide on common color scheme
landuseCols <- wes_palette('Darjeeling1', 5, type = c("discrete"))

###Fig 2############################################################

#total biomass
ggplot(allInsects,aes(x=Land_use, y=(Biomass+1)))+
  geom_pirate(aes(colour=Time_band,fill=Time_band),
              show.legend=TRUE,
              bars=FALSE)+
  theme_bw()+
  scale_y_log10()+
  xlab("Land cover")+ylab("Total insect biomass")

#ignoring time-band
ggplot(allInsects,aes(x=Land_use, y=(Biomass+1)))+
  geom_pirate(aes(colour=Land_use,fill=Land_use),
              show.legend=FALSE,
              bars=FALSE)+
  theme_bw()+
  scale_y_log10()+
  scale_color_manual(values=landuseCols)+
  scale_fill_manual(values=landuseCols)+
  xlab("Land cover")+ylab("Total insect biomass")


#split by size
g1 <- ggplot(allInsects,aes(x=Land_use, y=log(Biomass_large+1),colour=Land_use))+
  geom_boxplot(fill="white")+
  scale_color_manual(values=landuseCols)+
  theme_bw()+
  facet_wrap(~Country)+
  theme(legend.position = "none")+
  xlab("Land cover")+ylab("Large insect biomass")

g2 <- ggplot(allInsects,aes(x=Land_use, y=log(Biomass_small+1),colour=Land_use))+
  geom_boxplot(fill="white")+
  scale_color_manual(values=landuseCols)+
  theme_bw()+
  facet_wrap(~Country)+
  theme(legend.position = "none")+
  xlab("Land cover")+ylab("Small insect biomass")

plot_grid(g1,g2,ncol=1)

###sort vars###############################################################

#centreing
allInsects$cyDay <- allInsects$yDay - median(allInsects$yDay)
allInsects$cWind <- allInsects$Wind - median(allInsects$Wind)
allInsects$cTemp <- allInsects$Temp - median(allInsects$Temp)
allInsects$cStops <- log(allInsects$stops+1) - median(log(allInsects$stops+1))
allInsects$cTL <- log(allInsects$tr_signals+1) - median(log(allInsects$tr_signals+1))

#sort time data to standard each around the time band
allInsects$numberTime <- as.numeric(allInsects$StartTime)
median(allInsects$numberTime[allInsects$Time_band=="midday"])#23
median(allInsects$numberTime[allInsects$Time_band=="evening"])#69.5
allInsects$cnumberTime <- NA
allInsects$cnumberTime[allInsects$Time_band=="midday"] <- allInsects$numberTime[allInsects$Time_band=="midday"] -23
allInsects$cnumberTime[allInsects$Time_band=="evening"] <- allInsects$numberTime[allInsects$Time_band=="evening"] -69.5

table(allInsects$Route_length,allInsects$PilotID)
hist(allInsects$cStops)

###lm##############################################################

hist(log(allInsects$Biomass+1))

lm1 <- lm(log(Biomass+1) ~cStops, 
          data=allInsects)
summary(lm1)

lm1 <- lm(log(Biomass+1) ~ cTL, 
          data=allInsects)#stronger
summary(lm1)

lm1 <- lm(log(Biomass+1) ~ Land_use + Time_band*cnumberTime + 
            cTemp +cWind + cyDay + cStops, 
          data=allInsects)
summary(lm1)

lm1 <- lm(log(Biomass+1) ~ Land_use + Time_band*cnumberTime + 
            cTemp +cWind + cyDay + cTL, 
          data=allInsects)
summary(lm1)

#plus random effects##############################################################

library(lme4)
library(lmerTest)
library(MuMIn)
#Full model
lme1 <- lmer(log(Biomass+1) ~ Land_use + Time_band + 
               Time_band:cnumberTime + cyDay + 
               cTemp + cWind + cStops +
               (1|RouteID), data=allInsects)

summary(lme1)
r.squaredGLMM(lme1)
#        R2m       R2c
#[1,] 0.3344578 0.8464894

lme1 <- lmer(log(Biomass+1) ~ Land_use + Time_band + 
               Time_band:cnumberTime + cyDay + 
               cTemp + cWind + cStops +
               (1|PilotID), data=allInsects)
#R2m       R2c
#[1,] 0.2926235 0.7436184

#just keep route as ID

#plus spatial models################################################################

library(nlme)

#jitter x and y slightly - fix leter
allInsects$x2 <- allInsects$x + rnorm(length(allInsects$x),0,10)
allInsects$y2 <- allInsects$y + rnorm(length(allInsects$y),0,10)

gls1 <- lme(log(Biomass+1) ~ Land_use + Time_band + 
              Time_band:cnumberTime + cyDay + cTemp + cWind + cTL,
            random=~1|PilotID/RouteID,
            correlation=corExp(form=~x2+y2|PilotID/RouteID),
            data=allInsects,na.action=na.omit)
summary(gls1)

#remove wind, then remove temp
#effect of TL disappears when pilot is put in the model
#effect of year day disappears when pilot is put in the model

#final model
gls1 <- lme(log(Biomass+1) ~ Land_use + Time_band + 
              Time_band:cnumberTime + cTL,
            random=~1|PilotID/RouteID,
            correlation=corExp(form=~x2+y2|PilotID/RouteID),
            data=allInsects,na.action=na.omit)
summary(gls1)
#keep in TL even if not significant

r.squaredGLMM(gls1)
#R2m       R2c
#[1,] 0.2552812 0.8441772
#alot explained by pilot and route
