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
landuseCols <- landuseCols[c(1,4,3,5,2)]

landuseOrder <- c("Urban","Farmland","Open uncultivated","Wetland","Forest")
landuseOrderDK <- c("Urban","Farmland","Open uncultivated land","Wetland","Forest")

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

ggplot(allInsects,aes(x=Land_use, y=(Biomass+1)))+
  geom_boxplot(aes(colour=Land_use))+
  theme_bw()+
  scale_y_log10()+
  scale_color_manual(values=landuseCols)+
  scale_fill_manual(values=landuseCols)+
  xlab("Land cover")+ylab("Total insect biomass")+
  theme(legend.position = "none")


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

#add total biomass and correct colours and labels
g3 <- ggplot(allInsects,aes(x=Land_use, y=(Biomass),colour=Land_use))+
  geom_boxplot(fill="white", lwd = 1.2)+
  scale_color_manual(values=landuseCols)+
  scale_y_log10() +
  theme_bw(base_size = 15)+
  theme(legend.position = "none")+
  xlab("Land cover")+ylab("Insect biomass (log-transformed)") + scale_x_discrete(labels=c("Agriculture" = "Farmland"))

plot_grid(g1,g2,ncol=1)

###sort vars###############################################################

#centreing
allInsects$cyDay <- allInsects$yDay - median(allInsects$yDay)
allInsects$cWind <- allInsects$Wind - median(allInsects$Wind) # only works for DE data
allInsects$cTemp <- allInsects$Temp - median(allInsects$Temp) # only works for DE data
allInsects$cStops <- log(allInsects$stops+1) - median(log(allInsects$stops+1))
allInsects$cTL <- log(allInsects$tr_signals+1) - median(log(allInsects$tr_signals+1))

#sort time data to standard each around the time band
allInsects$numberTime <- as.numeric(allInsects$StartTime)
middayMean <- median(allInsects$numberTime[allInsects$Time_band=="midday"])#23 for DE, 37.5 for DK
eveningMean <- median(allInsects$numberTime[allInsects$Time_band=="evening"])#69.5 for DK, 124 for DK
allInsects$cnumberTime <- NA
allInsects$cnumberTime[allInsects$Time_band=="midday"] <- allInsects$numberTime[allInsects$Time_band=="midday"] -middayMean
allInsects$cnumberTime[allInsects$Time_band=="evening"] <- allInsects$numberTime[allInsects$Time_band=="evening"] -eveningMean

table(allInsects$Route_length,allInsects$PilotID)
hist(allInsects$cStops)
hist(allInsects$cTL)

###lm##############################################################

hist(log(allInsects$Biomass+1))

lm1 <- lm(log(Biomass+1) ~cStops, 
          data=allInsects)
summary(lm1)

lm1 <- lm(log(Biomass+1) ~ cTL, 
          data=allInsects)#stronger for DE, not stronger for DK
summary(lm1)

#DE
lm1 <- lm(log(Biomass+1) ~ Land_use + Time_band*cnumberTime + 
            cTemp +cWind + cyDay + cStops, 
          data=allInsects)
summary(lm1)

#DK - without detailed weather data
lm1 <- lm(log(Biomass+1) ~ Land_use + Time_band*cnumberTime + 
            Temperature +Wind + cyDay + cStops, 
          data=allInsects)
summary(lm1)

#DE
lm1 <- lm(log(Biomass+1) ~ Land_use + Time_band*cnumberTime + 
            cTemp +cWind + cyDay + cTL, 
          data=allInsects)
summary(lm1)

#DK - without detailed weather data
lm1 <- lm(log(Biomass+1) ~ Land_use + Time_band*cnumberTime + 
            Temperature +Wind + cyDay + cTL, 
          data=allInsects)
summary(lm1)

#plus random effects##############################################################

library(lme4)
library(lmerTest)
library(MuMIn)

#Full model- DE
lme1 <- lmer(log(Biomass+1) ~ Land_use + Time_band + 
               Time_band:cnumberTime + cyDay + 
               cTemp + cWind + cStops +
               (1|RouteID), data=allInsects)

summary(lme1)
r.squaredGLMM(lme1)
#        R2m       R2c
#[1,] 0.3344578 0.8464894

#Full model- DK (no detailed weather data)
lme1 <- lmer(log(Biomass+1) ~ Land_use + Time_band + 
               Time_band:cnumberTime + cyDay + 
               Temperature + Wind + cStops +
               (1|RouteID_JB), data=allInsects)

summary(lme1)
r.squaredGLMM(lme1)
#R2m       R2c
#[1,] 0.3580548 0.6487728

# DE
lme1 <- lmer(log(Biomass+1) ~ Land_use + Time_band + 
               Time_band:cnumberTime + cyDay + 
               cTemp + cWind + cStops +
               (1|PilotID), data=allInsects)
#R2m       R2c
#[1,] 0.2926235 0.7436184

#just keep route as ID

# DK
lme1 <- lmer(log(Biomass+1) ~ Land_use + Time_band + 
               Time_band:cnumberTime + cyDay + 
               Temperature + Wind + cStops +
               (1|PilotID), data=allInsects)
#R2m       R2c
#[1,] 0.3427775 0.6070411

#plus spatial models################################################################

library(nlme)

#DE jitter x and y slightly - fix later
allInsects$x2 <- allInsects$x + rnorm(length(allInsects$x),0,10)
allInsects$y2 <- allInsects$y + rnorm(length(allInsects$y),0,10)

# for DK jitter x and y slightly - fix later
allInsects$x2 <- allInsects$utm_x + rnorm(length(allInsects$utm_x),0,10)
allInsects$y2 <- allInsects$utm_y + rnorm(length(allInsects$utm_y),0,10)

# DE
gls1 <- lme(log(Biomass+1) ~ Land_use + Time_band + 
              Time_band:cnumberTime + cyDay + cTL,
            random=~1|PilotID/RouteID,
            correlation=corExp(form=~x2+y2|PilotID/RouteID),
            data=allInsects,na.action=na.omit)
summary(gls1)

#DK
gls1 <- lme(log(Biomass+1) ~ Land_use + Time_band + 
              Time_band:cnumberTime + cyDay + Temperature + Wind + cTL,
            random=~1|PilotID/RouteID,
            correlation=corExp(form=~x2+y2|PilotID/RouteID),
            data=allInsects,na.action=na.omit)
summary(gls1)

#remove wind, then remove temp
#effect of TL disappears when pilot is put in the model
#effect of year day disappears when pilot is put in the model

#final model DE
gls1 <- lme(log(Biomass+1) ~ Land_use + Time_band + 
              Time_band:cnumberTime + cTL,
            random=~1|PilotID/RouteID,
            correlation=corExp(form=~x2+y2|PilotID/RouteID),
            data=allInsects,na.action=na.omit)
summary(gls1)
#keep in TL even if not significant

r.squaredGLMM(gls1)
# Summary DE
#R2m       R2c
#[1,] 0.2552812 0.8441772
#alot explained by pilot and route

#final model DK - use cStops instead of cTL
gls1 <- lme(log(Biomass+1) ~ Land_use + Time_band + 
              Time_band:cnumberTime + cyDay + cStops,
            random=~1|PilotID/RouteID_JB,
            correlation=corExp(form=~x2+y2|PilotID/RouteID_JB),
            data=allInsects,na.action=na.omit)
summary(gls1)

# set open uncultivated land as reference instead of urban to get summry statistics for urban
test <- within(allInsects, Land_use <- relevel(Land_use, ref = "Open uncultivated land"))
gls1 <- lme(log(Biomass) ~ Land_use + Time_band + 
              Time_band:cnumberTime + cyDay + cStops,
            random=~1|PilotID/RouteID_JB,
            correlation=corExp(form=~x2+y2|PilotID/RouteID_JB),
            data=test,na.action=na.omit)
summary(gls1)

#keep in TL even if not significant
r.squaredGLMM(gls1)
# Summary DK
#R2m       R2c
#[1,] 0.2686265 0.6022614

library(lsmeans)
emm1 <- emmeans(gls1, specs = pairwise ~ Land_use, type = "response") # get results on untransformed response
emm1$emmeans
emm1$contrasts
emm1$contrasts %>% summary(infer = TRUE)
plot(emm1, comparisons = TRUE) + theme_bw() + 
  labs(x = "Estimated marginal mean (biomass)", y = "Land use category") + scale_y_discrete(labels=c("Agriculture" = "Farmland")) # The blue bars are confidence intervals for the EMMs, and the red arrows are for the comparisons among them. If an arrow from one mean overlaps an arrow from another group, the difference is not significant.

