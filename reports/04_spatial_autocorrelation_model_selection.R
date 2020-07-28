# run script 03 first

### Load required libraries ########################################
library(nlme) # Fit and compare Gaussian linear and nonlinear mixed-effects models
library(lme4)# Fit linear and generalized linear mixed-effects models
library(lmerTest) # Provides p-values in type I, II or III anova and summary tables for lmer model fits (cf. lme4) 
library(ncf) # visualise spatial autocorrelation
library(DHARMa) # test spatial autocorrelation
library(MuMIn)

### Denmark ########################################

# for DK jitter x and y slightly - fix later
allInsects$x2 <- allInsects$utm_x + rnorm(length(allInsects$utm_x),0,10)
allInsects$y2 <- allInsects$utm_y + rnorm(length(allInsects$utm_y),0,10)

### spatial autocorrelation ###########
#plot residuals
#full model
unique_coords <- distinct(allInsects, RouteID_JB, .keep_all = TRUE) # make dataframe with only one coordinate per row to test spatial autocorrelation for unique coordinates
unique_coords <- droplevels(unique_coords)

lme1000 <- lme4::lmer(log(Biomass+1) ~ 
                        (Agriculture_1000) + 
                        (Urban_1000) +
                        (Open.uncultivated.land_1000)+
                        (Wetland_1000) +
                        (Forest_250) +
                        Time_band + 
                        Time_band:cnumberTime + cTL + cyDay + 
                        (1|RouteID_JB) + (1|PilotID), data=unique_coords)
summary(lme1000)

unique_coords$resids <- as.numeric(residuals(lme1000))
unique_coords$resids_binary <- as.factor(ifelse(unique_coords$resids>0,1,-1))
qplot(x,y,data=unique_coords,colour=resids)+
  scale_colour_viridis_c()
qplot(x,y,data=allInsects,colour=resids_binary)+
  scale_colour_viridis_d()

# visualise it 
spline.autocorrelation_glm = spline.correlog(unique_coords$x2, unique_coords$y2, residuals(lme1000), latlon=T, resamp=100)
plot(spline.autocorrelation_glm)
summary(spline.autocorrelation_glm)

autocorrelation_glm = correlog(unique_coords$x2, unique_coords$y2, residuals(lme1000), increment = 1000, latlon=T, resamp=100)
plot(autocorrelation_glm)
autocorrelation_glm$correlation

#test it
simulationOutput <- simulateResiduals(fittedModel = lme1000, plot = T)
#residuals(simulationOutput)
#residuals(simulationOutput, quantileFunction = qnorm, outlierValues = c(-7,7))
testDispersion(lme1000)
testOutliers(simulationOutput)

res = simulateResiduals(lme1000)
testSpatialAutocorrelation(res, x =  unique_coords$x2, y = unique_coords$y2)

### model selection ###########
gls1 <- lme(log(Biomass+1) ~ Agriculture_1000 + 
              Forest_250 +
              Wetland_50 +
              Time_band + 
              Time_band:cnumberTime + 
              cStops + 
              cyDay,
            random=~1|PilotID/RouteID,
            correlation=corExp(form=~x2+y2|PilotID/RouteID,nugget=TRUE),
            data=allInsects)
summary(gls1)

#range     nugget 
#0.1668    0.1198  
AICc(gls1)#925.34

gls1 <- lme(log(Biomass+1) ~ Agriculture_1000 + 
              Forest_250 +
              Wetland_50 +
              Time_band + 
              Time_band:cnumberTime + 
              cStops + 
              cyDay,
            random=~1|PilotID,
            correlation=corExp(form=~x2+y2|PilotID,nugget=TRUE),
            data=allInsects)

summary(gls1)
#Parameter estimate(s):
#  range     nugget 
#0.1109   0.2492 
AICc(gls1)#923

gls1 <- lme(log(Biomass+1) ~ Agriculture_1000 + 
              Forest_250 +
              Wetland_50 +
              Time_band + 
              Time_band:cnumberTime + 
              cStops + 
              cyDay,
            random=~1|PilotID/RouteID,
            correlation=corExp(form=~x2+y2|PilotID/RouteID,nugget=FALSE),
            data=allInsects)
summary(gls1)

#range     nugget 
#0.165       
AICc(gls1)#923.2

gls1 <- lme(log(Biomass+1) ~ Agriculture_1000 + 
              Forest_250 +
              Wetland_50 +
              Time_band + 
              Time_band:cnumberTime + 
              cStops + 
              cyDay,
            random=~1|PilotID,
            correlation=corExp(form=~x2+y2|PilotID,nugget=FALSE),
            data=allInsects)
summary(gls1)
#range 
#0.1311   
AICc(gls1) # 923.3

gls1 <- lme(log(Biomass+1) ~ Agriculture_1000 + 
              Forest_250 +
              Wetland_50 +
              Time_band + 
              Time_band:cnumberTime + 
              cStops + 
              cyDay,
            random=~1|PilotID/RouteID,
            data=allInsects)
summary(gls1)
AICc(gls1) # 921

#final model DK - use cStops instead of cTL - sam story as without spatial correlation
gls1 <- lme(
  log(Biomass + 1) ~ #Urban_1000 + 
    Agriculture_1000 +
    Open.uncultivated.land_1000 +
    Wetland_50 +
    Forest_250 +
    Time_band +
    Time_band:cnumberTime +
    cStops +
    cyDay,
  random =  ~ 1 | PilotID / RouteID,
  data = allInsects
)
summary(gls1)
AICc(gls1) # 915 (best fit is 913, but with urban that should be removed due to corelation with stops
#keep in TL even if not significant

r.squaredGLMM(gls1)
#       R2m       R2c
# 0.3599319 0.6504751

summary(gls1) %>% intervals(which = "fixed")

### Germany ########################################

###check DE spatial autocorrelation#################################

#DE jitter x and y slightly - fix later
allInsects$x2 <- allInsects$x + rnorm(length(allInsects$x),0,10)
allInsects$y2 <- allInsects$y + rnorm(length(allInsects$y),0,10)

#plot residuals
#full model
lme1000 <- lme4::lmer(log(Biomass+1) ~ 
                        Agriculture_1000 + 
                        Urban_1000 +
                        Open.uncultivated_1000+
                        Wetland_1000 +
                        Forest_250 +
                        Time_band + 
                        Time_band:cnumberTime + 
                        cStops + cyDay + 
                        (1|PilotID) + (1|RouteID), data=allInsects)

allInsects$resids <- as.numeric(residuals(lme1000))
allInsects$resids_binary <- as.factor(ifelse(allInsects$resids>0,1,-1))
qplot(x,y,data=allInsects,colour=resids)+
  scale_colour_viridis_c()
qplot(x,y,data=allInsects,colour=resids_binary)+
  scale_colour_viridis_d()

#plot autocorrelation
correlog1 <- correlog(allInsects$x2, allInsects$y2, residuals(lme1000),
                      na.rm=T, increment=1000, resamp=0)
plot(correlog1$correlation, type="b", pch=16, cex=1.5, lwd=1.5,
     xlab="distance", ylab="Moran's I", cex.lab=2, cex.axis=1.5); abline(h=0)


spline.autocorrelation_glm = spline.correlog(allInsects$x2, allInsects$y2, 
                                             residuals(lme1000), 
                                             latlon=F, resamp=100)
plot(spline.autocorrelation_glm)
summary(spline.autocorrelation_glm)

#test it
res = simulateResiduals(lme1000)
res = recalculateResiduals(res, group = factor(allInsects$RouteID))#first aggregate by route
testSpatialAutocorrelation(res, 
                           x =  unique(allInsects$x), 
                            y = unique(allInsects$y))
#says highly significant still

### model selection ###########

# spatial models
gls1 <- lme(log(Biomass+1) ~ Agriculture_1000 + 
              Urban_1000 +
              Open.uncultivated_1000+
              Wetland_1000 +
              Forest_250 +
              Time_band + 
              Time_band:cnumberTime + 
              cTL + 
              cyDay,
            random=~1|PilotID/RouteID,
            correlation=corExp(form=~x2+y2|PilotID/RouteID,nugget=TRUE),
            data=allInsects)
AICc(gls1)#459.6515

gls1 <- lme(log(Biomass+1) ~ Agriculture_1000 + 
              Urban_1000 +
              Open.uncultivated_1000+
              Wetland_1000 +
              Forest_250 +
              Time_band + 
              Time_band:cnumberTime + 
              cTL + 
              cyDay,
            random=~1|PilotID,
            correlation=corExp(form=~x2+y2|PilotID,nugget=TRUE),
            data=allInsects)
#AIC 449.9827

gls1 <- lme(log(Biomass+1) ~ Agriculture_1000 + 
              Urban_1000 +
              Open.uncultivated_1000+
              Wetland_1000 +
              Forest_250 + 
              Time_band + 
              Time_band:cnumberTime + 
              cTL + 
              cyDay,
            random=~1|PilotID/RouteID,
            correlation=corExp(form=~x2+y2|PilotID/RouteID,nugget=FALSE),
            data=allInsects)
#457.059

gls1 <- lme(log(Biomass+1) ~ Agriculture_1000 + 
              Urban_1000 +
              Open.uncultivated_1000+
              Wetland_1000 +
              Forest_250 +
              Time_band + 
              Time_band:cnumberTime + 
              cTL + 
              cyDay,
            random=~1|PilotID,
            correlation=corExp(form=~x2+y2|PilotID,nugget=FALSE),
            data=allInsects)
#AICc 465.7069

gls1 <- lme(log(Biomass+1) ~ Agriculture_1000 + 
              Urban_1000 +
              Open.uncultivated_1000+
              Wetland_1000 +
              Forest_250 +
              Time_band + 
              Time_band:cnumberTime + 
              cTL + 
              cyDay,
            random=~1|PilotID/RouteID,
            data=allInsects)
#AICc(gls1)
#454.5108

#formal test
gls1 <- lme(log(Biomass+1) ~ Agriculture_1000 + 
              Urban_1000 +
              Open.uncultivated_1000+
              Wetland_1000 +
              Forest_250 +
              Time_band + 
              Time_band:cnumberTime + 
              cTL + 
              cyDay,
            random=~1|PilotID,
            correlation=corExp(form=~x2+y2|PilotID,nugget=TRUE),
            data=allInsects)

gls2 <- lme(log(Biomass+1) ~ Agriculture_1000 + 
              Urban_1000 +
              Open.uncultivated_1000+
              Wetland_1000 +
              Forest_250 +
              Time_band + 
              Time_band:cnumberTime + 
              cTL + 
              cyDay,
            random=~1|PilotID/RouteID,
            data=allInsects)
anova(gls1,gls2)
#spatial model is better
