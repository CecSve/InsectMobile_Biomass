#run mergeData script
#run the 'sort vars' section in script 04

library(cowplot)
library(ggplot2)
#library(wesanderson)
library(ggpubr)
library(scales)
library(ggpmisc)
library(grid)
library(gridExtra)
library(corrplot)
library(car)
library(lme4)
library(lmerTest)
library(nlme)
library(effects)

####colour################################################################

#decide on common color scheme
#landuseCols <- wes_palette('Darjeeling1', 5, type = c("discrete"))
#landuseCols <- landuseCols[c(1,4,3,5,2)]

landuseCols <- c("#CC79A7", "#E69F00", "#D55E00", "#56B4E9", "#009E73", "darkgrey") # colour friendly, ordered by land cover 

landuseOrder <- c("Urban","Farmland","Grassland","Wetland","Forest")
landuseOrder <- c("Urban","Farmland","Grassland","Wetland","Forest", "Unspecified")


###transformations for land use#####################################

### DE plot land cover##############################################

qU <- ggplot(allInsects,aes(x=sqrt(Urban_1000),y=(Biomass+1)))+
              geom_point(col=landuseCols[1])+
              scale_y_log10() +
              theme_bw() +
              geom_smooth(method="lm",color="grey70")+
              xlab("Urban cover") +ylab("Biomass")

qF <- ggplot(allInsects,aes(x=sqrt(Agriculture_1000),y=(Biomass+1)))+
  geom_point(col=landuseCols[2])+
  scale_y_log10() +
  theme_bw() +
  geom_smooth(method="lm",color="grey70")+
  xlab("Farmland cover") +ylab("Biomass")

qD <- ggplot(allInsects,aes(x=sqrt(Open.uncultivated_1000),y=(Biomass+1)))+
  geom_point(col=landuseCols[3])+
  scale_y_log10() +
  theme_bw() +
  geom_smooth(method="lm",color="grey70")+
  xlab("Open uncultivated land cover") +ylab("Biomass")

qW <- ggplot(allInsects,aes(x=sqrt(Wetland_1000),y=(Biomass+1)))+
  geom_point(col=landuseCols[4])+
  scale_y_log10() +
  theme_bw() +
  geom_smooth(method="lm",color="grey70")+
  xlab("Wetland cover") +ylab("Biomass")

qFo <- ggplot(allInsects,aes(x=sqrt(Forest_1000),y=(Biomass+1)))+
  geom_point(col=landuseCols[5])+
  scale_y_log10() +
  theme_bw() +
  geom_smooth(method="lm",color="grey70")+
  xlab("Forest cover") +ylab("Biomass")

plot_grid(qU,qF,qD,qW,qFo,ncol=1)
ggsave("plots/Landcover_percent.png",width=3,height=8)

### DK effect plots ##################################################
# the best fit model with all 5 landcovers
gls1 <- lme(log(Biomass+1) ~ Agriculture_1000 + 
              Urban_1000 +
              Open.uncultivated.land_1000 +
              Forest_250 +
              Wetland_50 +
              Time_band +
              Time_band:cnumberTime + 
              cStops + 
              cyDay,
            random=~1|PilotID/RouteID_JB,
            data=allInsects)

summary(gls1)

gls1.alleffects <- allEffects(gls1)
plot(gls1.alleffects, 'Urban_1000', ylab="Biomass")
effectdata <- as.data.frame(gls1.alleffects, row.names=NULL, optional=TRUE)

eall.lm1 <- predictorEffects(gls1)
plot(eall.lm1, lines=list(multiline=TRUE))
plot(predictorEffects(gls1, ~ Agriculture_1000 + Urban_1000 + Open.uncultivated.land_1000 + Forest_250 + Wetland_50 + cnumberTime, residuals = T), partial.residuals=list(smooth=TRUE, span=0.50, lty = "dashed"))

### ggplot effect plot ####
temp <- effectdata$Agriculture_1000
temp$landcover <- "Agriculture_1000"
farm <- temp %>% 
  rename(
    propcover = Agriculture_1000
  )%>% select(landcover, propcover, fit, se, lower, upper)

# urban
temp <- effectdata$Urban_1000
temp$landcover <- "Urban_1000"
urb <- temp %>% 
  rename(
    propcover = Urban_1000
  )%>% select(landcover, propcover, fit, se, lower, upper)

# Open.uncultivated.land
temp <- effectdata$Open.uncultivated.land_1000
temp$landcover <- "Grassland_1000"
grass <- temp %>% 
  rename(
    propcover = Open.uncultivated.land_1000
  ) %>% select(landcover, propcover, fit, se, lower, upper)

# Wetland
temp <- effectdata$Wetland_50
temp$landcover <- "Wetland_50"
wet <- temp %>% 
  rename(
    propcover = Wetland_50
  )%>% select(landcover, propcover, fit, se, lower, upper)

# Forest
temp <- effectdata$Forest_250
temp$landcover <- "Forest_250"
forest <- temp %>% 
  rename(
    propcover = Forest_250
  ) %>% select(landcover, propcover, fit, se, lower, upper)

test <- rbind(urb, farm, grass, wet, forest)

# Visualization
effectplot <- test %>% mutate(
  landcover = fct_relevel(
    landcover,
    "Urban_1000",
    "Agriculture_1000",
    "Grassland_1000",
    "Wetland_50",
    "Forest_250"
  )
) %>% ggplot(aes(x = propcover, y = fit, fill = landcover)) +
  geom_line(aes(color = landcover), size = 2) +
  scale_color_manual(
    values = landuseCols,
    labels = c(
      "Urban cover (1000 m)",
      "Farmland cover (1000 m)",
      "Grassland cover (1000 m)",
      "Wetland cover (50 m)",
      "Forest cover (250 m)"
    )
  ) + theme_minimal_grid() + theme(
    plot.subtitle = element_text(size = 20, face = "bold"),
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    legend.position = "bottom"
  ) + scale_x_continuous(
    limits = c(0, 1),
    labels = function(x)
    paste0(x * 100, "%"))  + scale_y_continuous(
    limits = c(2.5, 7),
    labels = function(x)
      paste0(x * 1, "%")
  ) + geom_ribbon(
    aes(
      ymin = fit-se,
      ymax = fit+se,
      group = landcover
    ),
    linetype = 2,
    alpha = 0.2,
    show.legend = F
  ) + labs(
    x = "Land cover",
    y = "Effect change on biomass",
    subtitle = "A",
    colour = "Land cover type"
  ) + scale_fill_manual(values = landuseCols)

# Another vizualisation of proportional cover differences
test %>% mutate(
  landcover = fct_relevel(
    landcover,
    "Urban_1000",
    "Agriculture_1000",
    "Grassland_1000",
    "Wetland_50",
    "Forest_250"
  )
) %>% ggplot(aes(propcover, fit, color = landcover)) + geom_point() + geom_errorbar(aes(ymin = fit - se, ymax = fit + se), width = 0.4) + theme_bw(base_size = 12) + scale_colour_manual(values = landuseCols) + labs(x = "Land cover") + scale_x_continuous(limits = c(0, 1), labels = function(x) paste0(x * 100, "%")) + facet_wrap(~landcover, scales = "free")

save_plot("plots/DK_effect_landcover.png", effectplot, base_width = 10, base_height = 6)

### DE plot buffers#################################################

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

###land use check###################################################

#check land covers within a buffer of the same size
#DE check
data1000m <- allInsects[,grepl("_1000",names(allInsects))]
summary(apply(data1000m[,1:5],1,sum))#does not exceed 100
data500m <- allInsects[,grepl("_500",names(allInsects))]
summary(apply(data500m[,1:5],1,sum))#does not exceed 100
data250m <- allInsects[,grepl("_250",names(allInsects))]
summary(apply(data250m[,1:5],1,sum))#exceeds by 0.01 - probably just rounding error
data50m <- allInsects[,grepl("_50",names(allInsects))]
data50m <- data50m[,!grepl("_500",names(data50m))]
summary(apply(data50m[,1:5],1,sum))#does not exceed 100

# DK check had no values above 100 % cover           

###covariation check#########################################

#check whether explanatory variables are strongly correlated
cor(allInsects[,c("cStops","cTL",names(allInsects)[grepl("_1000",names(allInsects))])])
cor(allInsects[,c("cStops","cTL",names(allInsects)[grepl("_500",names(allInsects))])])
cor(allInsects[,c("cStops","cTL",names(allInsects)[grepl("_250",names(allInsects))])])
cor(allInsects[,c("cStops","cTL",names(allInsects)[grepl("_50",names(allInsects))])])
#correlations between stops and urban cover...

###pca analysis###############################################
#taken from the Quick R website

mydata <- allInsects[,c("cStops","cTL",names(allInsects)[grepl("_1000",names(allInsects))])]
names(mydata)
mydata <- mydata[,2:9]

fit <- princomp(mydata, cor=TRUE)
summary(fit) # print variance accounted for
loadings(fit) # pc loadings
plot(fit,type="lines") # scree plot
fit$scores # the principal components
biplot(fit)

#with ggplot
library(ggfortify)
autoplot(fit)
autoplot(fit, data = allInsects, colour = 'Land_use',
         loadings = TRUE, 
         loadings.colour = 'black',
         loadings.label = TRUE, 
         loadings.label.size = 4) + scale_colour_manual(values = landuseCols)

###analysis####################################################

# run 04 to add variables to allInsects used for analysis 

#spatial effect is small so we ignore it for the moment

library(lme4)
library(lmerTest)

#function to extract summary components of model
getEffect <- function(model){
  coefs <- summary(model)$coef[2,]
  temp <- confint(model)
  cis <- temp[5,]
  data.frame(t(coefs),t(cis))
  }

fitModels <- function(variable){
  myformula()

}

###DE simple#########################################################

#agriculture
hist(allInsects$Agriculture_1000)
lme50 <- lmer(log(Biomass+1) ~ sqrt(Agriculture_50) + Time_band + 
                Time_band:cnumberTime + cTL + cyDay + 
               (1|RouteID) + (1|PilotID), data=allInsects)
lme250 <- lmer(log(Biomass+1) ~ sqrt(Agriculture_250) + Time_band + 
                 Time_band:cnumberTime + cTL + cyDay +  
                (1|RouteID) + (1|PilotID), data=allInsects)
lme500 <- lmer(log(Biomass+1) ~ sqrt(Agriculture_500) + Time_band + 
                 Time_band:cnumberTime + cTL + cyDay +  
                 (1|RouteID) + (1|PilotID), data=allInsects)
lme1000 <- lmer(log(Biomass+1) ~ sqrt(Agriculture_1000) + Time_band + 
                  Time_band:cnumberTime + cTL + cyDay +  
                 (1|RouteID) + (1|PilotID), data=allInsects)
outAgri <- rbind(getEffect(lme50),getEffect(lme250),getEffect(lme500),getEffect(lme1000))
outAgri <- as.data.frame(outAgri)
outAgri$Buffer <- c(50,250,500,1000)
outAgri$Land_use <- "Farmland"

#urban
hist(allInsects$Urban_1000)#should we log it?
lme50 <- lmer(log(Biomass+1) ~ sqrt(Urban_50) + Time_band + 
                Time_band:cnumberTime + cTL + cyDay + 
                (1|RouteID) + (1|PilotID), data=allInsects)
lme250 <- lmer(log(Biomass+1) ~ sqrt(Urban_250) + Time_band + 
                 Time_band:cnumberTime + cTL + cyDay +  
                 (1|RouteID) + (1|PilotID), data=allInsects)
lme500 <- lmer(log(Biomass+1) ~ sqrt(Urban_500) + Time_band + 
                 Time_band:cnumberTime + cTL + cyDay +  
                 (1|RouteID) + (1|PilotID), data=allInsects)
lme1000 <- lmer(log(Biomass+1) ~ sqrt(Urban_1000) + Time_band + 
                  Time_band:cnumberTime + cTL + cyDay +  
                  (1|RouteID) + (1|PilotID), data=allInsects)
outUrban <- rbind(getEffect(lme50),getEffect(lme250),getEffect(lme500),getEffect(lme1000))
outUrban <- as.data.frame(outUrban)
outUrban$Buffer <- c(50,250,500,1000)
outUrban$Land_use <- "Urban"

#Open.uncultivated
hist(allInsects$Open.uncultivated_250)#log??
lme50 <- lmer(log(Biomass+1) ~ sqrt(Open.uncultivated_50) + Time_band + 
                Time_band:cnumberTime + cTL + cyDay + 
                (1|RouteID) + (1|PilotID), data=allInsects)
lme250 <- lmer(log(Biomass+1) ~ sqrt(Open.uncultivated_250) + Time_band + 
                 Time_band:cnumberTime + cTL + cyDay +  
                 (1|RouteID) + (1|PilotID), data=allInsects)
lme500 <- lmer(log(Biomass+1) ~ sqrt(Open.uncultivated_500) + Time_band + 
                 Time_band:cnumberTime + cTL + cyDay +  
                 (1|RouteID) + (1|PilotID), data=allInsects)
lme1000 <- lmer(log(Biomass+1) ~ sqrt(Open.uncultivated_1000) + Time_band + 
                  Time_band:cnumberTime + cTL + cyDay +  
                  (1|RouteID) + (1|PilotID), data=allInsects)
outOpen.uncultivated <- rbind(getEffect(lme50),getEffect(lme250),getEffect(lme500),getEffect(lme1000))
outOpen.uncultivated <- as.data.frame(outOpen.uncultivated)
outOpen.uncultivated$Buffer <- c(50,250,500,1000)
outOpen.uncultivated$Land_use <- "Open uncultivated"

#forest
hist(allInsects$Forest_250)#log??
lme50 <- lmer(log(Biomass+1) ~ sqrt(Forest_50) + Time_band + 
                Time_band:cnumberTime + cTL + cyDay + 
                (1|RouteID) + (1|PilotID), data=allInsects)
lme250 <- lmer(log(Biomass+1) ~ sqrt(Forest_250) + Time_band + 
                 Time_band:cnumberTime + cTL + cyDay +  
                 (1|RouteID) + (1|PilotID), data=allInsects)
lme500 <- lmer(log(Biomass+1) ~ sqrt(Forest_500) + Time_band + 
                 Time_band:cnumberTime + cTL + cyDay +  
                 (1|RouteID) + (1|PilotID), data=allInsects)
lme1000 <- lmer(log(Biomass+1) ~ sqrt(Forest_1000) + Time_band + 
                  Time_band:cnumberTime + cTL + cyDay +  
                  (1|RouteID) + (1|PilotID), data=allInsects)
outForest<- rbind(getEffect(lme50),getEffect(lme250),getEffect(lme500),getEffect(lme1000))
outForest <- as.data.frame(outForest)
outForest$Buffer <- c(50,250,500,1000)
outForest$Land_use <- "Forest"

#wetland
hist(allInsects$Wetland_1000)#log??
lme50 <- lmer(log(Biomass+1) ~ sqrt(Wetland_50) + Time_band + 
                 Time_band:cnumberTime + cTL + cyDay +  
                 (1|RouteID) + (1|PilotID), data=allInsects)
lme250 <- lmer(log(Biomass+1) ~ sqrt(Wetland_250) + Time_band + 
                 Time_band:cnumberTime + cTL + cyDay +  
                 (1|RouteID) + (1|PilotID), data=allInsects)
lme500 <- lmer(log(Biomass+1) ~ sqrt(Wetland_500) + Time_band + 
                 Time_band:cnumberTime + cTL + cyDay +  
                 (1|RouteID) + (1|PilotID), data=allInsects)
lme1000 <- lmer(log(Biomass+1) ~ sqrt(Wetland_1000) + Time_band + 
                  Time_band:cnumberTime + cTL + cyDay +  
                  (1|RouteID) + (1|PilotID), data=allInsects)
outWetland<- rbind(getEffect(lme50),getEffect(lme250),getEffect(lme500),getEffect(lme1000))
outWetland <- as.data.frame(outWetland)
outWetland$Buffer <- c(50,250,500,1000)
outWetland$Land_use <- "Wetland"

###DE multiple regression########################################

#full model
lme1000 <- lmer(log(Biomass+1) ~ 
                  sqrt(Agriculture_1000) + 
                  sqrt(Urban_1000) +
                  sqrt(Open.uncultivated_1000)+
                  sqrt(Wetland_1000) +
                  sqrt(Forest_250) +
                  Time_band + 
                  Time_band:cnumberTime + cTL + cyDay + 
                  (1|RouteID) + (1|PilotID), data=allInsects)

#final
lme1000 <- lmer(log(Biomass+1) ~ 
                  sqrt(Agriculture_1000) + 
                  sqrt(Forest_250) +
                Time_band + 
                Time_band:cnumberTime + cTL + cyDay + 
                (1|RouteID) + (1|PilotID), data=allInsects)
summary(lme1000)

library(MuMIn)
r.squaredGLMM(lme1000)
#           R2m       R2c
#[1,] 0.3625144 0.8403325

#add in other land uses to this final model to get their effect for the table in the paper

#check variance inflation factor
library(car)
vif(lme1000)

lme1000 <- lmer(log(Biomass+1) ~ 
                  sqrt(Agriculture_1000) + 
                  sqrt(Forest_250) +
                  sqrt(Urban_1000) +
                  Time_band + 
                  Time_band:cnumberTime + cTL + cyDay + 
                  (1|RouteID) + (1|PilotID), data=allInsects)
#some issue for the model with urban cover
vif(lme1000)


#test timeband interactions
lme1000 <- lmer(log(Biomass+1) ~ 
                  Time_band*sqrt(Agriculture_1000) + 
                  sqrt(Forest_250) +
                  Time_band + 
                  Time_band:cnumberTime + cTL + cyDay + 
                  (1|RouteID) + (1|PilotID), data=allInsects)
summary(lme1000)

lme1000 <- lmer(log(Biomass+1) ~ 
                  sqrt(Agriculture_1000) + 
                  Time_band*sqrt(Forest_250) +
                  Time_band + 
                  Time_band:cnumberTime + cTL + cyDay + 
                  (1|RouteID) + (1|PilotID), data=allInsects)
summary(lme1000)

lme1000 <- lmer(log(Biomass+1) ~ 
                  sqrt(Agriculture_1000) + 
                  sqrt(Forest_250) +
                  Time_band*sqrt(Urban_1000)+
                  Time_band + 
                  Time_band:cnumberTime + cTL + cyDay + 
                  (1|RouteID) + (1|PilotID), data=allInsects)
summary(lme1000)

# spatial models
gls1 <- lme(log(Biomass+1) ~ Agriculture_1000 + 
              Forest_250 +
              Time_band + 
              Time_band:cnumberTime + 
              cTL + 
              cyDay,
            random=~1|PilotID/RouteID,
            correlation=corExp(form=~x2+y2|PilotID/RouteID,nugget=TRUE),
            data=allInsects)
#0.0003768015
#     range     nugget 
#45.8854417  0.148841
AICc(gls1)#451.1499

gls1 <- lme(log(Biomass+1) ~ Agriculture_1000 + 
              Forest_250 +
              Time_band + 
              Time_band:cnumberTime + 
              cTL + 
              cyDay,
            random=~1|PilotID,
            correlation=corExp(form=~x2+y2|PilotID,nugget=TRUE),
            data=allInsects)
#Parameter estimate(s):
#  range     nugget 
#45.8854582  0.1488418
#AIC 448.687

gls1 <- lme(log(Biomass+1) ~ Agriculture_1000 + 
              Forest_250 +
              Time_band + 
              Time_band:cnumberTime + 
              cTL + 
              cyDay,
            random=~1|PilotID/RouteID,
            correlation=corExp(form=~x2+y2|PilotID/RouteID,nugget=FALSE),
            data=allInsects)
#range 
#6.984794
#451.5497

gls1 <- lme(log(Biomass+1) ~ Agriculture_1000 + 
              Forest_250 +
              Time_band + 
              Time_band:cnumberTime + 
              cTL + 
              cyDay,
            random=~1|PilotID,
            correlation=corExp(form=~x2+y2|PilotID,nugget=FALSE),
            data=allInsects)
#range 
#24.58072 
#AICc 450.6248

gls1 <- lme(log(Biomass+1) ~ Agriculture_1000 + 
              Forest_250 +
              Time_band + 
              Time_band:cnumberTime + 
              cTL + 
              cyDay,
            random=~1|PilotID/RouteID,
            data=allInsects)
#AICc(gls1)449.7846

#spatial models
gls1 <- lme(log(Biomass+1) ~ Agriculture_1000 + 
              Forest_250 +
              Time_band + 
              Time_band:cnumberTime + 
              cTL + 
              cyDay,
            random=~1|PilotID/RouteID,
            correlation=corExp(form=~x2+y2|PilotID/RouteID,nugget=TRUE),
            data=allInsects)
#0.0003768015
#     range     nugget 
#45.8854417  0.148841
AICc(gls1)#451.1499

gls1 <- lme(log(Biomass+1) ~ Agriculture_1000 + 
              Forest_250 +
              Time_band + 
              Time_band:cnumberTime + 
              cTL + 
              cyDay,
            random=~1|PilotID,
            correlation=corExp(form=~x2+y2|PilotID,nugget=TRUE),
            data=allInsects)
#Parameter estimate(s):
#  range     nugget 
#45.8854582  0.1488418
#AIC 448.687

gls1 <- lme(log(Biomass+1) ~ Agriculture_1000 + 
              Forest_250 +
              Time_band + 
              Time_band:cnumberTime + 
              cTL + 
              cyDay,
            random=~1|PilotID/RouteID,
            correlation=corExp(form=~x2+y2|PilotID/RouteID,nugget=FALSE),
            data=allInsects)
#range 
#6.984794
#451.5497

gls1 <- lme(log(Biomass+1) ~ Agriculture_1000 + 
              Forest_250 +
              Time_band + 
              Time_band:cnumberTime + 
              cTL + 
              cyDay,
            random=~1|PilotID,
            correlation=corExp(form=~x2+y2|PilotID,nugget=FALSE),
            data=allInsects)
#range 
#24.58072 
#AICc 450.6248

gls1 <- lme(log(Biomass+1) ~ Agriculture_1000 + 
              Forest_250 +
              Time_band + 
              Time_band:cnumberTime + 
              cTL + 
              cyDay,
            random=~1|PilotID/RouteID,
            data=allInsects)
#AICc(gls1)449.7846


###check DE spatial autocorrelation#################################

#DE jitter x and y slightly - fix later
allInsects$x2 <- allInsects$x + rnorm(length(allInsects$x),0,10)
allInsects$y2 <- allInsects$y + rnorm(length(allInsects$y),0,10)

#plot residuals
#full model
lme1000 <- lme4::lmer(log(Biomass+1) ~ 
                  sqrt(Agriculture_1000) + 
                  sqrt(Urban_1000) +
                  sqrt(Open.uncultivated_1000)+
                  sqrt(Wetland_1000) +
                  sqrt(Forest_250) +
                  Time_band + 
                  Time_band:cnumberTime + cTL + cyDay + 
                    (1|PilotID), data=allInsects)

allInsects$resids <- as.numeric(residuals(lme1000))
allInsects$resids_binary <- as.factor(ifelse(allInsects$resids>0,1,-1))
qplot(x,y,data=allInsects,colour=resids)+
  scale_colour_viridis_c()
qplot(x,y,data=allInsects,colour=resids_binary)+
  scale_colour_viridis_d()


#plot autocorrelation
library(ncf)
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
library(DHARMa)
res = simulateResiduals(lme1000)
testSpatialAutocorrelation(res, x =  allInsects$x2, y = allInsects$y2)

###DK simple#########################################################
# NB! changed cTL to cStops since more data for DK 
str(allInsects)
#agriculture
hist(allInsects$Agriculture_500)
hist(sqrt(allInsects$Agriculture_500)) # does not help
lme50 <- lmer(log(Biomass+1) ~ (Agriculture_50) + Time_band + 
                Time_band:cnumberTime + cStops + cyDay + 
                (1|RouteID_JB) + (1|PilotID), data=allInsects)
lme250 <- lmer(log(Biomass+1) ~ (Agriculture_250) + Time_band + 
                 Time_band:cnumberTime + cStops + cyDay +  
                 (1|RouteID_JB) + (1|PilotID), data=allInsects)
lme500 <- lmer(log(Biomass+1) ~ (Agriculture_500) + Time_band + 
                 Time_band:cnumberTime + cStops + cyDay +  
                 (1|RouteID_JB) + (1|PilotID), data=allInsects)
lme1000 <- lmer(log(Biomass+1) ~ (Agriculture_1000) + Time_band + 
                  Time_band:cnumberTime + cStops + cyDay +  
                  (1|RouteID_JB) + (1|PilotID), data=allInsects)
outAgri <- rbind(getEffect(lme50),getEffect(lme250),getEffect(lme500),getEffect(lme1000))
outAgri <- as.data.frame(outAgri)
outAgri$Buffer <- c(50,250,500,1000)
outAgri$Land_use <- "Farmland"

#urban
hist(allInsects$Urban_1000)#should we log it?
hist(sqrt(allInsects$Urban_1000)) #sqrt is bettwe
lme50 <- lmer(log(Biomass+1) ~ (Urban_50) + Time_band + 
                Time_band:cnumberTime + cStops + cyDay + 
                (1|RouteID_JB) + (1|PilotID), data=allInsects)
lme250 <- lmer(log(Biomass+1) ~ (Urban_250) + Time_band + 
                 Time_band:cnumberTime + cStops + cyDay +  
                 (1|RouteID_JB) + (1|PilotID), data=allInsects)
lme500 <- lmer(log(Biomass+1) ~ (Urban_500) + Time_band + 
                 Time_band:cnumberTime + cStops + cyDay +  
                 (1|RouteID_JB) + (1|PilotID), data=allInsects)
lme1000 <- lmer(log(Biomass+1) ~ (Urban_1000) + Time_band + 
                  Time_band:cnumberTime + cStops + cyDay +  
                  (1|RouteID_JB) + (1|PilotID), data=allInsects)
outUrban <- rbind(getEffect(lme50),getEffect(lme250),getEffect(lme500),getEffect(lme1000))
outUrban <- as.data.frame(outUrban)
outUrban$Buffer <- c(50,250,500,1000)
outUrban$Land_use <- "Urban"

#Open.uncultivated
hist(allInsects$Open.uncultivated.land_250)#log??
lme50 <- lmer(log(Biomass+1) ~ (Open.uncultivated.land_50) + Time_band + 
                Time_band:cnumberTime + cStops + cyDay + 
                (1|RouteID_JB) + (1|PilotID), data=allInsects)
lme250 <- lmer(log(Biomass+1) ~ (Open.uncultivated.land_250) + Time_band + 
                 Time_band:cnumberTime + cStops + cyDay +  
                 (1|RouteID_JB) + (1|PilotID), data=allInsects)
lme500 <- lmer(log(Biomass+1) ~ (Open.uncultivated.land_500) + Time_band + 
                 Time_band:cnumberTime + cStops + cyDay +  
                 (1|RouteID_JB) + (1|PilotID), data=allInsects)
lme1000 <- lmer(log(Biomass+1) ~ (Open.uncultivated.land_1000) + Time_band + 
                  Time_band:cnumberTime + cStops + cyDay +  
                  (1|RouteID_JB) + (1|PilotID), data=allInsects)
outOpen.uncultivated <- rbind(getEffect(lme50),getEffect(lme250),getEffect(lme500),getEffect(lme1000))
outOpen.uncultivated <- as.data.frame(outOpen.uncultivated)
outOpen.uncultivated$Buffer <- c(50,250,500,1000)
outOpen.uncultivated$Land_use <- "Grassland"

#forest
hist(allInsects$Forest_250)#log??
lme50 <- lmer(log(Biomass+1) ~ (Forest_50) + Time_band + 
                Time_band:cnumberTime + cStops + cyDay + 
                (1|RouteID_JB) + (1|PilotID), data=allInsects)
lme250 <- lmer(log(Biomass+1) ~ (Forest_250) + Time_band + 
                 Time_band:cnumberTime + cStops + cyDay +  
                 (1|RouteID_JB) + (1|PilotID), data=allInsects)
lme500 <- lmer(log(Biomass+1) ~ (Forest_500) + Time_band + 
                 Time_band:cnumberTime + cStops + cyDay +  
                 (1|RouteID_JB) + (1|PilotID), data=allInsects)
lme1000 <- lmer(log(Biomass+1) ~ (Forest_1000) + Time_band + 
                  Time_band:cnumberTime + cStops + cyDay +  
                  (1|RouteID_JB) + (1|PilotID), data=allInsects)
outForest<- rbind(getEffect(lme50),getEffect(lme250),getEffect(lme500),getEffect(lme1000))
outForest <- as.data.frame(outForest)
outForest$Buffer <- c(50,250,500,1000)
outForest$Land_use <- "Forest"

#wetland
hist(allInsects$Wetland_1000)#log??
lme50 <- lmer(log(Biomass+1) ~ (Wetland_50) + Time_band + 
                Time_band:cnumberTime + cStops + cyDay +  
                (1|RouteID_JB) + (1|PilotID), data=allInsects)
lme250 <- lmer(log(Biomass+1) ~ (Wetland_250) + Time_band + 
                 Time_band:cnumberTime + cStops + cyDay +  
                 (1|RouteID_JB) + (1|PilotID), data=allInsects)
lme500 <- lmer(log(Biomass+1) ~ (Wetland_500) + Time_band + 
                 Time_band:cnumberTime + cStops + cyDay +  
                 (1|RouteID_JB) + (1|PilotID), data=allInsects)
lme1000 <- lmer(log(Biomass+1) ~ (Wetland_1000) + Time_band + 
                  Time_band:cnumberTime + cStops + cyDay +  
                  (1|RouteID_JB) + (1|PilotID), data=allInsects)
outWetland<- rbind(getEffect(lme50),getEffect(lme250),getEffect(lme500),getEffect(lme1000))
outWetland <- as.data.frame(outWetland)
outWetland$Buffer <- c(50,250,500,1000)
outWetland$Land_use <- "Wetland"

#unspecified
hist(allInsects$Unspecified.land.cover_1000)#log??
hist(log(allInsects$Unspecified.land.cover_1000))
lme50 <- lmer(log(Biomass+1) ~ log(Unspecified.land.cover_50) + Time_band + 
                Time_band:cnumberTime + cStops + cyDay +  
                (1|RouteID_JB) + (1|PilotID), data=allInsects)
lme250 <- lmer(log(Biomass+1) ~ log(Unspecified.land.cover_250) + Time_band + 
                 Time_band:cnumberTime + cStops + cyDay +  
                 (1|RouteID_JB) + (1|PilotID), data=allInsects)
lme500 <- lmer(log(Biomass+1) ~ log(Unspecified.land.cover_500) + Time_band + 
                 Time_band:cnumberTime + cStops + cyDay +  
                 (1|RouteID_JB) + (1|PilotID), data=allInsects)
lme1000 <- lmer(log(Biomass+1) ~ log(Unspecified.land.cover_1000) + Time_band + 
                  Time_band:cnumberTime + cStops + cyDay +  
                  (1|RouteID_JB) + (1|PilotID), data=allInsects)
outUnspecified<- rbind(getEffect(lme50),getEffect(lme250),getEffect(lme500),getEffect(lme1000))
outUnspecified <- as.data.frame(outUnspecified)
outUnspecified$Buffer <- c(50,250,500,1000)
outUnspecified$Land_use <- "Unspecified"

###DK multiple regression########################################

# used cStops instead of cTL for DK data

#full model
lme1000 <- lmer(log(Biomass+1) ~ 
                  (Agriculture_1000) + 
                  sqrt(Urban_1000) +
                  sqrt(Open.uncultivated.land_50)+
                  sqrt(Wetland_50) +
                  sqrt(Forest_250) +
                  #sqrt(Unspecified.land.cover_500) +
                  Time_band + 
                  Land_use:Time_band +
                  Time_band:cnumberTime + cStops + cyDay + 
                  (1|RouteID_JB) + (1|PilotID), data=allInsects)
summary(lme1000)

#final
lme1000 <- lmer(log(Biomass+1) ~ 
                  (Agriculture_1000) + 
                  #sqrt(Urban_1000) +
                  #sqrt(Open.uncultivated.land_50)+
                  sqrt(Wetland_50) +
                  sqrt(Forest_250) +
                  Time_band + 
                  Time_band:cnumberTime + cStops + cyDay + 
                  (1|RouteID_JB) + (1|PilotID), data=allInsects)
summary(lme1000)

library(MuMIn)
r.squaredGLMM(lme1000)
#           R2m       R2c
#[1,] 0.3625144 0.8403325

#add in other land uses to this final model to get their effect
#for the table in the paper

#check variance inflation factor
library(car)
vif(lme1000)

lme1000 <- lmer(log(Biomass+1) ~ 
                  sqrt(Agriculture_1000) + 
                  sqrt(Forest_250) +
                  sqrt(Open.uncultivated.land_50)+
                  sqrt(Wetland_50) +
                  sqrt(Urban_1000) +
                  Time_band + 
                  Time_band:cnumberTime + cStops + cyDay + 
                  (1|RouteID_JB) + (1|PilotID), data=allInsects)
#some issue
vif(lme1000)

#as spatial model


###plot buffer effects########################################

#DE
outAll <- rbind(outForest,outAgri,outUrban,outWetland,outOpen.uncultivated)
outAll$Land_use <- factor(outAll$Land_use,levels=landuseOrder)

ggplot(outAll)+
  geom_crossbar(aes(x=factor(Buffer),y=Estimate,
                    ymin=X2.5..,ymax=X97.5..,
                    fill=Land_use),
                width=0.5)+
  facet_wrap(~Land_use,scales="free",ncol=1)+
  scale_fill_manual(values=landuseCols)+
  coord_flip()+
  theme_bw()+
  theme(legend.position = "none")+
  geom_hline(yintercept=0,colour="black",linetype="dashed")+
  xlab("Buffer size (m)") + ylab("Effect of land cover on biomass")

ggsave("plots/Landcover_buffer.png",width=3,height=8)


# Final DK plot 
#combine all effects
outAll <- rbind(outForest,outAgri,outUrban,outWetland,outOpen.uncultivated)
outAll$Land_use <- factor(outAll$Land_use,levels=landuseOrder)

ggplot(outAll)+
  geom_crossbar(aes(x=factor(Buffer),y=Estimate,
                    ymin=X2.5..,ymax=X97.5..,
                    fill=Land_use),
                width=0.5)+
  facet_wrap(~Land_use,scales="free",ncol=1)+
  scale_fill_manual(values=landuseCols)+
  coord_flip()+
  theme_bw()+
  theme(legend.position = "none")+
  geom_hline(yintercept=0,colour="black",linetype="dashed")+
  xlab("Buffer size (m)") + ylab("Effect of land cover on biomass") + labs(subtitle = "A")+ theme(plot.subtitle=element_text(size=18, face="bold", color="black"))

ggsave("plots/DK_Landcover_buffer.png",width=3,height=8)

#DK plus spatial models################################################################

library(nlme)

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
                        (1|RouteID) + (1|PilotID), data=unique_coords)
summary(lme1000)

unique_coords$resids <- as.numeric(residuals(lme1000))
unique_coords$resids_binary <- as.factor(ifelse(unique_coords$resids>0,1,-1))
qplot(x,y,data=unique_coords,colour=resids)+
  scale_colour_viridis_c()
qplot(x,y,data=allInsects,colour=resids_binary)+
  scale_colour_viridis_d()

# visualise it 
library(ncf)
spline.autocorrelation_glm = spline.correlog(unique_coords$x2, unique_coords$y2, residuals(lme1000), latlon=T, resamp=100)
plot(spline.autocorrelation_glm)
summary(spline.autocorrelation_glm)

autocorrelation_glm = correlog(unique_coords$x2, unique_coords$y2, residuals(lme1000), increment = 1000, latlon=T, resamp=100)
plot(autocorrelation_glm)
autocorrelation_glm$correlation
mantel.test(unique_coords$x2, unique_coords$y2, residuals(lme1000), resamp = 100, latlon = T) #does not work

#test it
library(DHARMa)
simulationOutput <- simulateResiduals(fittedModel = lme1000, plot = T)
#residuals(simulationOutput)
#residuals(simulationOutput, quantileFunction = qnorm, outlierValues = c(-7,7))
testDispersion(lme1000)
testOutliers(simulationOutput)

res = simulateResiduals(lme1000)
testSpatialAutocorrelation(res, x =  unique_coords$x2, y = unique_coords$y2)

# calculating summaries per group since we have several observations per location
#simulationOutput = recalculateResiduals(simulationOutput, group = allInsects$RouteID_JB)

# spatial models (DK) 
# full model
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
AICc(gls1) # 915 (best fit is 913, but with insignificant wetland and grassland)

# model selection
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

#test timeband interactions
lme1000 <- lmer(log(Biomass+1) ~ 
                  Urban_1000 +
                  Time_band*(Agriculture_1000) + 
                  Open.uncultivated.land_1000 +
                  (Forest_500) +
                  (Wetland_50) +
                  Time_band + 
                  Time_band:cnumberTime + cStops + cyDay + 
                  (1|RouteID_JB) + (1|PilotID), data=allInsects)
summary(lme1000)

lme1000 <- lmer(log(Biomass+1) ~ 
                  Urban_1000 +
                  Agriculture_1000 + 
                  Open.uncultivated.land_1000*Time_band +
                  Forest_500 +
                  Wetland_50 +
                  Time_band + 
                  Time_band:cnumberTime + cStops + cyDay + 
                  (1|RouteID_JB) + (1|PilotID), data=allInsects)
summary(lme1000)

lme1000 <- lmer(log(Biomass+1) ~ 
                  Urban_1000*Time_band +
                  Agriculture_1000 + 
                  Open.uncultivated.land_1000 +
                  Forest_500 +
                  Wetland_50 +
                  Time_band + 
                  Time_band:cnumberTime + cStops + cyDay + 
                  (1|RouteID_JB) + (1|PilotID), data=allInsects)
summary(lme1000)

lme1000 <- lmer(log(Biomass+1) ~ 
                  Urban_1000 +
                  Agriculture_1000 + 
                  Open.uncultivated.land_1000 +
                  Forest_500*Time_band +
                  Wetland_50 +
                  Time_band + 
                  Time_band:cnumberTime + cStops + cyDay + 
                  (1|RouteID_JB) + (1|PilotID), data=allInsects)
summary(lme1000)

lme1000 <- lmer(log(Biomass+1) ~ 
                  Urban_1000 +
                  Agriculture_1000 + 
                  Open.uncultivated.land_1000 +
                  Forest_500 +
                  Wetland_50*Time_band +
                  Time_band + 
                  Time_band:cnumberTime + cStops + cyDay + 
                  (1|RouteID_JB) + (1|PilotID), data=allInsects)
summary(lme1000)

###DE biomass predictions%##############################

library(lme4)
library(lmerTest)

lme1000 <- lmer(log(Biomass+1) ~ 
                  sqrt(Forest_1000) + 
                  Time_band + 
                  Time_band:cnumberTime +
                  log(tr_signals+1) + cyDay + 
                  (1|RouteID) + (1|PilotID), 
                data=allInsects)
summary(lme1000)
newData = data.frame(Forest_1000=0.5,
                     tr_signals=0,
                     cyDay = 0,
                     Time_band = "midday",
                     cnumberTime = 0)


#make predictions
exp(predict(lme1000,newdata=newData,re.form=NA))

predFun <- function(fit) {
  predict(fit,newData,re.form=NA)
}

bb <- bootMer(lme1000,nsim=1000,FUN=predFun,seed=101)
exp(quantile(bb$t,c(0.025,0.975)))

###DK biomass predictions%##############################

library(lme4)
library(lmerTest)

# urban
lme1000 <- lmer(log(Biomass+1) ~ 
                  Urban_1000 + 
                  Time_band + 
                  Time_band:cnumberTime +
                  log(cStops+1) + cyDay + 
                  (1|RouteID_JB) + (1|PilotID), 
                data=allInsects)
summary(lme1000)
newData = data.frame(Urban_1000=0.5,
                     cStops=0,
                     cyDay = 0,
                     Time_band = "midday",
                     cnumberTime = 0)

predFun <- function(fit) {
  predict(fit,newData,re.form=NA)
}

#make predictions
Urban1 <- t(as_tibble(exp(predict(lme1000,newdata=newData,re.form=NA))))

bb <- bootMer(lme1000,nsim=1000,FUN=predFun,seed=101)
urb <- bb[["data"]]
Urban2 <- t(as_tibble(exp(quantile(bb$t,c(0.025,0.975)))))

Urban <- cbind(Urban1, Urban2)
Urban <- as.data.frame(Urban)
colnames(Urban)
names(Urban)[1] <- "predBiomass"
names(Urban)[2] <- "lowCI"
names(Urban)[3] <- "highCI"
row.names(Urban) <- "Urban"

# farmland
lme1000 <- lmer(log(Biomass+1) ~ 
                  Agriculture_1000 + 
                  Time_band + 
                  Time_band:cnumberTime +
                  log(cStops+1) + cyDay + 
                  (1|RouteID_JB) + (1|PilotID), 
                data=allInsects)
summary(lme1000)
newData = data.frame(Agriculture_1000=0.5,
                     cStops=0,
                     cyDay = 0,
                     Time_band = "midday",
                     cnumberTime = 0)


#make predictions
Farmland1 <- t(as_tibble(exp(predict(lme1000,newdata=newData,re.form=NA))))

predFun <- function(fit) {
  predict(fit,newData,re.form=NA)
}

bb <- bootMer(lme1000,nsim=1000,FUN=predFun,seed=101)
farm <- bb[["data"]]
Farmland2 <- t(as_tibble(exp(quantile(bb$t,c(0.025,0.975)))))

Farmland <- cbind(Farmland1, Farmland2)
Farmland <- as.data.frame(Farmland)
colnames(Farmland)
names(Farmland)[1] <- "predBiomass"
names(Farmland)[2] <- "lowCI"
names(Farmland)[3] <- "highCI"
row.names(Farmland) <- "Farmland"

# Grassland
lme1000 <- lmer(log(Biomass+1) ~ 
                  Open.uncultivated.land_1000 + 
                  Time_band + 
                  Time_band:cnumberTime +
                  log(cStops+1) + cyDay + 
                  (1|RouteID_JB) + (1|PilotID), 
                data=allInsects)
summary(lme1000)
newData = data.frame(Open.uncultivated.land_1000=0.5,
                     cStops=0,
                     cyDay = 0,
                     Time_band = "midday",
                     cnumberTime = 0)

#make predictions
Grassland1 <- t(as_tibble(exp(predict(lme1000,newdata=newData,re.form=NA))))

predFun <- function(fit) {
  predict(fit,newData,re.form=NA)
}

bb <- bootMer(lme1000,nsim=1000,FUN=predFun,seed=101)
grass <- bb[["data"]]
Grassland2 <- t(as_tibble(exp(quantile(bb$t,c(0.025,0.975)))))

Grassland <- cbind(Grassland1, Grassland2)
Grassland <- as.data.frame(Grassland)
colnames(Grassland)
names(Grassland)[1] <- "predBiomass"
names(Grassland)[2] <- "lowCI"
names(Grassland)[3] <- "highCI"
row.names(Grassland) <- "Grassland"

# wetland
lme1000 <- lmer(log(Biomass+1) ~ 
                  Wetland_1000 + 
                  Time_band + 
                  Time_band:cnumberTime +
                  log(cStops+1) + cyDay + 
                  (1|RouteID_JB) + (1|PilotID), 
                data=allInsects)
summary(lme1000)
newData = data.frame(Wetland_1000=0.5,
                     cStops=0,
                     cyDay = 0,
                     Time_band = "midday",
                     cnumberTime = 0)


#make predictions
wetland1 <- t(as_tibble(exp(predict(lme1000,newdata=newData,re.form=NA))))

predFun <- function(fit) {
  predict(fit,newData,re.form=NA)
}

bb <- bootMer(lme1000,nsim=1000,FUN=predFun,seed=101)
wet <- bb[["data"]]
wetland2 <- t(as_tibble(exp(quantile(bb$t,c(0.025,0.975)))))

Wetland <- cbind(wetland1, wetland2)
Wetland <- as.data.frame(Wetland)
colnames(Wetland)
names(Wetland)[1] <- "predBiomass"
names(Wetland)[2] <- "lowCI"
names(Wetland)[3] <- "highCI"
row.names(Wetland) <- "Wetland"

# Forest
lme1000 <- lmer(log(Biomass+1) ~ 
                  Forest_1000 + 
                  Time_band + 
                  Time_band:cnumberTime +
                  log(cStops+1) + cyDay + 
                  (1|RouteID_JB) + (1|PilotID), 
                data=allInsects)
summary(lme1000)
newData = data.frame(Forest_1000=0.5,
                     cStops=0,
                     cyDay = 0,
                     Time_band = "midday",
                     cnumberTime = 0)


#make predictions
Forest1 <- t(as_tibble(exp(predict(lme1000,newdata=newData,re.form=NA))))

predFun <- function(fit) {
  predict(fit,newData,re.form=NA)
}

bb <- bootMer(lme1000,nsim=1000,FUN=predFun,seed=101)
fors <- bb[["data"]]
Forest2 <- t(as_tibble(exp(quantile(bb$t,c(0.025,0.975)))))

Forest <- cbind(Forest1, Forest2)
Forest <- as.data.frame(Forest)
colnames(Forest)
names(Forest)[1] <- "predBiomass"
names(Forest)[2] <- "lowCI"
names(Forest)[3] <- "highCI"
row.names(Forest) <- "Forest"

predConfData <- rbind(Urban, Farmland)
predConfData <- rbind(predConfData, Grassland)
predConfData <- rbind(predConfData, Wetland)
predConfData <- rbind(predConfData, Forest)
#predConfData <- rbind(predConfData, Unspecified)

predConfData <- rownames_to_column(predConfData, var = "landcover")

# plot
p <- predConfData %>%
  mutate(landcover = fct_relevel(
    landcover,
    "Urban",
    "Farmland",
    "Grassland",
    "Wetland",
    "Forest"
  )) %>% ggplot(aes(landcover, predBiomass, colour = landcover))

finalplot <-
  p + geom_pointrange(aes(ymin = lowCI, ymax = highCI),
                      size = 1.5,
                      show.legend = F) + scale_colour_manual(values = landuseCols) + theme_minimal_grid() + theme(legend.title = element_blank(),
                        legend.key = element_rect(size = 0.1),
                        legend.key.size = unit(1, 'cm')
                      ) + labs(x = "\nLand cover", y = "Predicted biomass (mg) and 95% CIs\n", subtitle = "A") + theme(plot.subtitle = element_text(size = 20, face = "bold")) + scale_y_log10()

save_plot("plots/DK_predicted_biomass.png", finalplot, base_width = 8, base_height = 5)

### combining the predicted data to re-run model and calculate effects ####
predeffect <- merge(urb, farm)
predeffect <- merge(predeffect, grass)
predeffect <- merge(predeffect, wet)
predeffect <- merge(predeffect, fors)

predeffect <- predeffect %>% rename(Biomass = `log(Biomass + 1)`) # be mindful that biomass is +1 and logtransformed here, the sme for stops
predeffect <- predeffect %>% rename(cStops = `log(cStops + 1)`) 
 
# run model
gls1 <- lme(Biomass ~ Agriculture_1000 + 
              Urban_1000 +
              Open.uncultivated.land_1000 +
              Forest_1000 +
              Wetland_1000 +
              Time_band +
              Time_band:cnumberTime + 
              cStops + 
              cyDay,
            random=~1|PilotID/RouteID_JB,
            data=predeffect)

summary(gls1)

gls1.alleffects <- allEffects(gls1)
plot(gls1.alleffects, 'Urban_1000', ylab="Biomass")
effectdata <- as.data.frame(gls1.alleffects, row.names=NULL, optional=TRUE)

eall.lm1 <- predictorEffects(gls1)
plot(eall.lm1, lines=list(multiline=TRUE))
plot(predictorEffects(gls1, ~ Agriculture_1000 + Urban_1000 + Open.uncultivated.land_1000 + Forest_1000 + Wetland_1000 + cnumberTime, residuals = T), partial.residuals=list(smooth=TRUE, span=0.50, lty = "dashed"))

### ggplot effect plot ####
temp <- effectdata$Agriculture_1000
temp$landcover <- "Agriculture_1000"
farm <- temp %>% 
  rename(
    propcover = Agriculture_1000
  )%>% select(landcover, propcover, fit, se, lower, upper)

# urban
temp <- effectdata$Urban_1000
temp$landcover <- "Urban_1000"
urb <- temp %>% 
  rename(
    propcover = Urban_1000
  )%>% select(landcover, propcover, fit, se, lower, upper)

# Open.uncultivated.land
temp <- effectdata$Open.uncultivated.land_1000
temp$landcover <- "Grassland_1000"
grass <- temp %>% 
  rename(
    propcover = Open.uncultivated.land_1000
  ) %>% select(landcover, propcover, fit, se, lower, upper)

# Wetland
temp <- effectdata$Wetland_1000
temp$landcover <- "Wetland_1000"
wet <- temp %>% 
  rename(
    propcover = Wetland_1000
  )%>% select(landcover, propcover, fit, se, lower, upper)

# Forest
temp <- effectdata$Forest_1000
temp$landcover <- "Forest_1000"
forest <- temp %>% 
  rename(
    propcover = Forest_1000
  ) %>% select(landcover, propcover, fit, se, lower, upper)

test <- rbind(urb, farm, grass, wet, forest)

# Visualization
effectplot <- test %>% mutate(
  landcover = fct_relevel(
    landcover,
    "Urban_1000",
    "Agriculture_1000",
    "Grassland_1000",
    "Wetland_1000",
    "Forest_1000"
  )
) %>% ggplot(aes(x = propcover, y = fit, fill = landcover)) +
  geom_line(aes(color = landcover), size = 2) +
  scale_color_manual(
    values = landuseCols,
    labels = c(
      "Urban cover",
      "Farmland cover",
      "Grassland cover",
      "Wetland cover",
      "Forest cover"
    )
  ) + theme_minimal_grid() + theme(
    plot.subtitle = element_text(size = 20, face = "bold"),
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    legend.position = "bottom"
  ) + scale_x_continuous(
    limits = c(0, 1),
    labels = function(x)
      paste0(x * 100, "%"))  + scale_y_continuous(
        limits = c(2.5, 8),
        labels = function(x)
          paste0(x * 1, "%")
      ) + geom_ribbon(
        aes(
          ymin = fit-se,
          ymax = fit+se,
          group = landcover
        ),
        linetype = 2,
        alpha = 0.2,
        show.legend = F
      ) + labs(
        x = "Land cover",
        y = "Predicted effect change on biomass",
        subtitle = "A",
        colour = "Land cover type"
      ) + scale_fill_manual(values = landuseCols)

save_plot("plots/DK_predictedeffect_landcover.png", effectplot, base_width = 10, base_height = 6)

### intensive vs organic farming ###############################
# Intensive
lme1000 <- lmer(log(Biomass+1) ~ 
                  Intensiv_1000 +
                  Ekstensiv_1000 +
                  Semi.intensiv_1000 +
                  Markblok_1000 +
                  Time_band + 
                  Time_band:cnumberTime +
                  log(cStops+1) + cyDay + 
                  (1|RouteID_JB) + (1|PilotID), 
                data=allInsects)
summary(lme1000)
newData = data.frame(Intensiv_1000=0.5, Ekstensiv_1000 = 0.5, Semi.intensiv_1000 = 0.5, Markblok_1000 = 0.5, cStops=0, cyDay = 0, Time_band = "midday",cnumberTime = 0)

#make predictions
Intensiv1 <- t(as_tibble(exp(predict(lme1000,newdata=newData,re.form=NA))))

predFun <- function(fit) {
  predict(fit,newData,re.form=NA)
}

bb <- bootMer(lme1000,nsim=1000,FUN=predFun,seed=101)
conventional <- bb[["data"]]
Intensiv2 <- t(as_tibble(exp(quantile(bb$t,c(0.025,0.975)))))

Intensiv <- cbind(Intensiv1, Intensiv2)
Intensiv <- as.data.frame(Intensiv)
colnames(Intensiv)
names(Intensiv)[1] <- "predBiomass"
names(Intensiv)[2] <- "lowCI"
names(Intensiv)[3] <- "highCI"
row.names(Intensiv) <- "Intensive"

# propOeko
lme1000 <- lmer(log(Biomass+1) ~ 
                  Intensiv_organic_1000 +
                  Ekstensiv_organic_1000 +
                  Semi.intensiv_organic_1000 +
                  Markblok_organic_1000 +  
                  Time_band + 
                  Time_band:cnumberTime +
                  log(cStops+1) + cyDay + 
                  (1|RouteID_JB) + (1|PilotID), 
                data=allInsects)
summary(lme1000)
newData = data.frame(Intensiv_organic_1000=0.5, Ekstensiv_organic_1000 = 0.5, Semi.intensiv_organic_1000 = 0.5, Markblok_organic_1000 = 0.5,
                     cStops=0,
                     cyDay = 0,
                     Time_band = "midday",
                     cnumberTime = 0)

#make predictions
propOeko1 <- t(as_tibble(exp(predict(lme1000,newdata=newData,re.form=NA))))

predFun <- function(fit) {
  predict(fit,newData,re.form=NA)
}

bb <- bootMer(lme1000,nsim=1000,FUN=predFun,seed=101)
organic <- bb[["data"]]
propOeko2 <- t(as_tibble(exp(quantile(bb$t,c(0.025,0.975)))))

propOeko <- cbind(propOeko1, propOeko2)
propOeko <- as.data.frame(propOeko)
colnames(propOeko)
names(propOeko)[1] <- "predBiomass"
names(propOeko)[2] <- "lowCI"
names(propOeko)[3] <- "highCI"
row.names(propOeko) <- "Organic"

predConfData <- rbind(Intensiv, propOeko)
predConfData <- rownames_to_column(predConfData, var = "Farmland_type")

# plot

p <- predConfData %>% ggplot(aes(Farmland_type, predBiomass, colour = Farmland_type))
finalplot <- p + geom_pointrange(aes(ymin = lowCI, ymax = highCI), size =1.5) + scale_colour_manual(values = c("#F09018", "#E3B622" )) + theme_minimal_grid() + theme(legend.title = element_blank(), legend.key = element_rect(size = 0.1), legend.key.size = unit(1, 'cm')) + labs(x = "\nFarming system", y = "Predicted biomass (mg) and 95% CIs\n", subtitle = "A") + theme(plot.subtitle = element_text(size = 20, face = "bold")) + scale_y_log10()

save_plot("plots/DK_predicted_biomass_farmtype.png", finalplot, base_width = 8, base_height = 5)

### combining the predicted data to re-run model and calculate effects ####
predeffect <- merge(conventional, organic)

predeffect <- predeffect %>% rename(Biomass = `log(Biomass + 1)`) # be mindful that biomass is +1 and logtransformed here, the sme for stops
predeffect <- predeffect %>% rename(cStops = `log(cStops + 1)`) 

# run model
gls1 <- lme(Biomass ~ Intensiv_1000 +
              Ekstensiv_1000 +
              Semi.intensiv_1000 +
              Markblok_1000 + Intensiv_organic_1000 +
              Ekstensiv_organic_1000 +
              Semi.intensiv_organic_1000 +
              Markblok_organic_1000 + 
              Time_band +
              Time_band:cnumberTime + 
              cStops + 
              cyDay,
            random=~1|PilotID/RouteID_JB,
            data=predeffect)

summary(gls1)

gls1.alleffects <- allEffects(gls1)
effectdata <- as.data.frame(gls1.alleffects, row.names=NULL, optional=TRUE)

eall.lm1 <- predictorEffects(gls1)
plot(eall.lm1, lines=list(multiline=TRUE))
plot(predictorEffects(gls1, ~ Intensiv_1000 + Ekstensiv_1000 + Semi.intensiv_1000 + Intensiv_organic_1000 + Ekstensiv_organic_1000 + Semi.intensiv_organic_1000 + cnumberTime, residuals = T), partial.residuals=list(smooth=TRUE, span=0.50, lty = "dashed"))

### ggplot effect plot ####
temp <- effectdata$Intensiv_1000
temp$landcover <- "Intensiv_1000"
farm <- temp %>% 
  rename(
    propcover = Intensiv_1000
  )%>% select(landcover, propcover, fit, se, lower, upper)

# urban
temp <- effectdata$Ekstensiv_1000
temp$landcover <- "Ekstensiv_1000"
urb <- temp %>% 
  rename(
    propcover = Ekstensiv_1000
  )%>% select(landcover, propcover, fit, se, lower, upper)

# Open.uncultivated.land
temp <- effectdata$Semi.intensiv_1000
temp$landcover <- "Semi.intensiv_1000"
grass <- temp %>% 
  rename(
    propcover = Semi.intensiv_1000
  ) %>% select(landcover, propcover, fit, se, lower, upper)

# Wetland
temp <- effectdata$Intensiv_organic_1000
temp$landcover <- "Intensiv_organic_1000"
wet <- temp %>% 
  rename(
    propcover = Intensiv_organic_1000
  )%>% select(landcover, propcover, fit, se, lower, upper)

# Forest
temp <- effectdata$Ekstensiv_organic_1000
temp$landcover <- "Ekstensiv_organic_1000"
forest <- temp %>% 
  rename(
    propcover = Ekstensiv_organic_1000
  ) %>% select(landcover, propcover, fit, se, lower, upper)

# Forest
temp <- effectdata$Semi.intensiv_organic_1000
temp$landcover <- "Semi.intensiv_organic_1000"
semiint <- temp %>% 
  rename(
    propcover = Semi.intensiv_organic_1000
  ) %>% select(landcover, propcover, fit, se, lower, upper)


test <- rbind(urb, farm, grass, wet, forest, semiint)

# Visualization
effectplot <- test %>% mutate(
  landcover = fct_relevel(
    landcover,
    "Intensiv_1000",
    "Intensiv_organic_1000",
    "Semi.intensiv_1000",
    "Semi.intensiv_organic_1000",
    "Ekstensiv_1000",
    "Ekstensiv_organic_1000"
  )
) %>% ggplot(aes(x = propcover, y = fit, fill = landcover)) +
  geom_line(aes(color = landcover), size = 2) +
  scale_color_manual(
    values = landuseCols,
    labels = c(
      "Intensive cover", "Intensive organic cover",
        "Semi-intensive",
        "Semi-intensive organic cover",
        "Exstensive cover",
        "Exstensive organic cover" 
    )
  ) + theme_minimal_grid() + theme(
    plot.subtitle = element_text(size = 20, face = "bold"),
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    legend.position = "bottom"
  ) + scale_x_continuous(
    limits = c(0, 1),
    labels = function(x)
      paste0(x * 100, "%"))  + scale_y_continuous(
        limits = c(4.3, 5.8),
        labels = function(x)
          paste0(x * 1, "%")
      ) + geom_ribbon(
        aes(
          ymin = fit-se,
          ymax = fit+se,
          group = landcover
        ),
        linetype = 2,
        alpha = 0.2,
        show.legend = F
      ) + labs(
        x = "Land cover",
        y = "Predicted effect change on biomass",
        subtitle = "A",
        colour = "Land cover type"
      ) + scale_fill_manual(values = landuseCols)

#save_plot("plots/DK_predictedeffect_landcover.png", effectplot, base_width = 10, base_height = 6)
