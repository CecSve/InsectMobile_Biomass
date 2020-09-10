#run mergeData script

#### Load required libraries ################################################################
library(cowplot) # for visuals
library(ggplot2) # for visuals
library(ggpubr) # for visuals
library(scales) # for visuals
library(ggpmisc) # ggplot extensions, for visuals
library(grid) # for visuals
library(gridExtra) # for visuals
library(car) # Companion to Applied Regression
library(lme4) # Fit linear and generalized linear mixed-effects models
library(lmerTest) # Provides p-values in type I, II or III anova and summary tables for lmer model fits (cf. lme4) 
library(nlme) # Fit and compare Gaussian linear and nonlinear mixed-effects models
library(effects) # Graphical and tabular effect displays, e.g., of interactions, for various statistical models with linear predictors
library(MuMIn)#AIC, R2

#### Set colour scheme ################################################################

landuseCols <- c("#CC79A7", "#E69F00", "#D55E00", "#56B4E9", "#009E73", "darkgrey") # colour friendly, ordered by land cover 

landuseOrder <- c("Urban","Farmland","Grassland","Wetland","Forest")
#landuseOrder <- c("Urban","Farmland","Grassland","Wetland","Forest", "Unspecified") # if including unspecified/other category

#### General check-ups ################################################################
### Land cover check ###################################################

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

# DK check had no values above 100% cover           

### Covariation check #########################################

#check whether explanatory variables are strongly correlated
cor(allInsects[,c("cStops","cTL",names(allInsects)[grepl("_1000",names(allInsects))])])
cor(allInsects[,c("cStops","cTL",names(allInsects)[grepl("_500",names(allInsects))])])
cor(allInsects[,c("cStops","cTL",names(allInsects)[grepl("_250",names(allInsects))])])
cor(allInsects[,c("cStops","cTL",names(allInsects)[grepl("_50",names(allInsects))])])
#correlations between stops and urban cover...

### Denmark ##############################################

### Figure 3: the data ##########################
qU <- ggplot(allInsects,aes(x=Urban_1000,y=(Biomass+1)))+
  geom_point(col=landuseCols[1])+
  scale_y_log10() +
  theme_bw() +
  geom_smooth(method="lm",color="grey70") + scale_x_continuous(
    labels = function(x)
      paste0(x * 100, "%")) +
  xlab("") +ylab("Biomass (mg)") + labs(subtitle = "Urban cover") + theme(plot.subtitle = element_text(size = 12, face = "bold"), plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))

qF <- ggplot(allInsects,aes(x=Agriculture_1000,y=(Biomass+1)))+
  geom_point(col=landuseCols[2])+
  scale_y_log10() +
  theme_bw() +
  geom_smooth(method="lm",color="grey70")+scale_x_continuous(
    labels = function(x)
      paste0(x * 100, "%")) +
  xlab("") +ylab("Biomass (mg)") + labs(subtitle = "Farmland cover") + theme(plot.subtitle = element_text(size = 12, face = "bold"), plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))

qD <- ggplot(allInsects,aes(x=Open.uncultivated.land_1000,y=(Biomass+1)))+
  geom_point(col=landuseCols[3])+
  scale_y_log10() +
  theme_bw() +
  geom_smooth(method="lm",color="grey70")+scale_x_continuous(
    #limits = c(0, 0.16),
    labels = function(x)
      paste0(x * 100, "%")) +
  xlab("") +ylab("Biomass (mg)") + labs(subtitle = "Grassland cover") + theme(plot.subtitle = element_text(size = 12, face = "bold"), plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))

qW <- ggplot(allInsects,aes(x=Wetland_1000,y=(Biomass+1)))+
  geom_point(col=landuseCols[4])+
  scale_y_log10() +
  theme_bw() +
  geom_smooth(method="lm",color="grey70")+scale_x_continuous(
    labels = function(x)
      paste0(x * 100, "%")) +
  xlab("") +ylab("Biomass (mg)") + labs(subtitle = "Wetland cover") + theme(plot.subtitle = element_text(size = 12, face = "bold"), plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))

qFo <- ggplot(allInsects,aes(x=Forest_1000,y=(Biomass+1)))+
  geom_point(col=landuseCols[5])+
  scale_y_log10() +
  theme_bw() +
  geom_smooth(method="lm",color="grey70")+scale_x_continuous(
    labels = function(x)
      paste0(x * 100, "%")) +
  xlab("") +ylab("Biomass (mg)") + labs(subtitle = "Forest cover") + theme(plot.subtitle = element_text(size = 12, face = "bold"), plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))

fig3 <- plot_grid(qU,qF,qD,qW,qFo,ncol=1)

save_plot("plots/DK_Landcover_percent.tiff", fig3, base_width = 4, base_height = 12, dpi = 1200)
#ggsave("plots/Landcover_percent.png",width=12,height=4)

# alternative to figure 3
allInsects.long <- allInsects %>% 
  select(Biomass, Time_band, numberTime, Urban_1000, Agriculture_1000, Open.uncultivated.land_1000, Wetland_50, Forest_250) %>% pivot_longer(-c(Biomass, Time_band, numberTime), names_to = "landcover", values_to = "cover")

head(allInsects.long)


#colorset = c('Urban_1000'=landuseCols[1],'Agriculture_1000'=landuseCols[2],'Open.uncultivated.land_1000'=landuseCols[3],'Wetland_50'=landuseCols[4], 'Forest_250' =landuseCols[5])

sampling_time <- allInsects.long %>% mutate(
  landcover = fct_relevel(
    landcover,
    "Urban_1000",
    "Agriculture_1000",
    "Open.uncultivated.land_1000",
    "Wetland_50",
    "Forest_250"
  )
) %>% ggplot(aes(cover, log(Biomass+1), colour = landcover)) + geom_point() + scale_colour_manual(values = landuseCols, labels = c(
  "Urban cover (1000 m)",
  "Farmland cover (1000 m)",
  "Grassland cover (1000 m)",
  "Wetland cover (50 m)",
  "Forest cover (250 m)"
)) + geom_smooth(method=lm, aes(fill = landcover), alpha = 0.1, size =1.5, show.legend = F) + scale_fill_manual(values = landuseCols)  + scale_x_continuous(limits = c(0,1) , labels = function(x)
  paste0(x * 100, "%")) + labs(x = "Proportional cover", y= "log(biomass +1) (mg)", colour = "Land cover", subtitle = "A: Denmark") + theme(plot.subtitle = element_text(size = 20, face = "bold"),legend.title = element_text(), legend.text = element_text(size = 8))

###DK pie chart#####################################

library(dplyr)
library(tidyr)

routeMeans <- allInsects %>% 
  group_by(RouteID_JB) %>%
  dplyr::summarise(meanBiomass = mean(Biomass))

allInsects <- inner_join(allInsects,routeMeans,by="RouteID_JB")

#remove duplicates
allInsects_pie <- allInsects %>%
  select(RouteID_JB,meanBiomass,Agriculture_1000,
         Forest_1000,Open.uncultivated.land_1000,
         Urban_1000,Wetland_1000) %>%
  distinct()

#fill in missing column
allInsects_pie$totalLand <- apply(allInsects_pie[,3:7],1,sum)
allInsects_pie$Other_1000 <- 1-allInsects_pie$totalLand

#divide up biomass into quantiles
allInsects_pie$BiomassCats <- cut_number(allInsects_pie$meanBiomass,n=5)

#mean land cover per biomass cats
allInsects_cat <- allInsects_pie %>%
  group_by(BiomassCats) %>%
  dplyr::summarise(Agriculture_1000 = mean(Agriculture_1000),
                   Forest_1000 = mean(Forest_1000),
                   Open.uncultivated.land_1000 = mean(Open.uncultivated.land_1000),
                   Urban_1000 = mean(Urban_1000),
                   Wetland_1000 = mean(Wetland_1000),
                   Other_1000 = mean(Other_1000))

#plot data by biomass categories
allInsects_melt <- gather(allInsects_cat, Land_cover, value, -BiomassCats)

#plot
allInsects_melt <- allInsects_melt %>% mutate(
  Land_cover = fct_relevel(
    Land_cover,
    "Urban_1000",
    "Agriculture_1000",
    "Open.uncultivated.land_1000",
    "Wetland_1000",
    "Forest_1000",
    "Other_1000"))  

biomass.labs <- c("[3,48.8]"="3 mg", "(48.8,84.2]"="48.8-84.2 mg", "(84.2,135]"="84.2-135 mg", "(135,262]"="135-262 mg", "(262,4.36e+03]"="4360 mg")
#names(biomass.labs) <- c("3-48.8 mg", "48.8-84.2 mg", "84.2-135 mg", "135-262 mg", "262-4360 mg")

fig3_2 <- ggplot(allInsects_melt,aes(x="",y=value,fill=Land_cover, order = Land_cover))+
  geom_bar(stat="identity")+
  facet_grid(~BiomassCats, labeller = labeller(BiomassCats=biomass.labs))+
  coord_polar("y")+
  theme_classic() + 
  theme(axis.line = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  theme(legend.position="top") + scale_fill_manual(values = landuseCols, name = "Land cover", labels = c("Urban", "Farmland", "Grassland", "Wetland", "Forest", "Other")) +guides(fill=guide_legend(nrow=1,byrow=TRUE))

save_plot("plots/Landcover_biomass_proportions.png", fig3_2, base_width = 12, base_height = 6)

### Linear Mixed Effects Model: Land covers (Table 1) #################
# used cStops instead of cTL for DK data

#full and final model
lme1000 <- lmer(log(Biomass+1) ~ 
                  Agriculture_1000 + 
                  Urban_1000 +
                  Open.uncultivated.land_1000+ # test if outlier drives the pattern
                  Wetland_50 +
                  Forest_250 +
                  Time_band + 
                  Time_band:cnumberTime + cStops + cyDay + 
                  (1|RouteID_JB) + (1|PilotID), data=subset(allInsects, Open.uncultivated.land_1000 < 0.2))
# data=subset(allInsects, Open.uncultivated.land_1000 < 0.2)
summary(lme1000)

library(MuMIn)
r.squaredGLMM(lme1000)
#           R2m       R2c
#[1,] 0.3625144 0.8403325

#add in other land uses to this final model to get their effect
#for the table in the paper

#check variance inflation factor
vif(lme1000)

### AIC check ##############################################

library(MuMIn)
options(na.action = "na.fail")
dd <- dredge(lme1000)
subset(dd, delta < 2)

# Visualize the model selection table:
par(mar = c(3,5,6,4))
plot(dd, labAsExpr = TRUE)

# Model average models with delta AICc < 4
#model.avg(dd, subset = delta < 2)

# best model with AIC 901.4
lme1000 <- lmer(log(Biomass+1) ~ 
                  Agriculture_1000 + 
                  Urban_1000 +
                  Open.uncultivated.land_1000 + # test if grassland outlier modifies results
                  Wetland_50 +
                  Forest_250 +
                  Time_band + cyDay + 
                  (1|RouteID_JB) + (1|PilotID), data=subset(allInsects, Open.uncultivated.land_1000 < 0.2))
# data=subset(allInsects, Open.uncultivated.land_1000 < 0.2)
summary(lme1000)
r.squaredGLMM(lme1000)

### Figure 4: effect plots ##########################
gls1.alleffects <- allEffects(lme1000)
plot(gls1.alleffects, 'Urban_1000', ylab="Biomass")
effectdata <- as.data.frame(gls1.alleffects, row.names=NULL, optional=TRUE)

eall.lm1 <- predictorEffects(lme1000)
plot(eall.lm1, lines=list(multiline=TRUE))
plot(predictorEffects(lme1000, ~ Agriculture_1000 + Urban_1000 + Open.uncultivated.land_1000 + Forest_250 + Wetland_50 + cnumberTime, residuals = T), partial.residuals=list(smooth=TRUE, span=0.50, lty = "dashed"))

### ggplot effect plot ####
temp <- effectdata$Agriculture_1000
temp$landcover <- "Agriculture_1000"
farm <- temp %>% 
  dplyr::rename(
    propcover = Agriculture_1000
  )%>% select(landcover, propcover, fit, se, lower, upper)

# urban
temp <- effectdata$Urban_1000
temp$landcover <- "Urban_1000"
urb <- temp %>% 
  dplyr::rename(
    propcover = Urban_1000
  )%>% select(landcover, propcover, fit, se, lower, upper)

# Open.uncultivated.land
temp <- effectdata$Open.uncultivated.land_1000
temp$landcover <- "Grassland_1000"
grass <- temp %>% 
  dplyr::rename(
    propcover = Open.uncultivated.land_1000
  ) %>% select(landcover, propcover, fit, se, lower, upper)

# Wetland
temp <- effectdata$Wetland_50
temp$landcover <- "Wetland_50"
wet <- temp %>% 
  dplyr::rename(
    propcover = Wetland_50
  )%>% select(landcover, propcover, fit, se, lower, upper)

# Forest
temp <- effectdata$Forest_250
temp$landcover <- "Forest_250"
forest <- temp %>% 
  dplyr::rename(
    propcover = Forest_250
  ) %>% select(landcover, propcover, fit, se, lower, upper)

# Timeband
temp <- effectdata$`Time_band:cnumberTime`
#temp$landcover <- "Time_band:cnumberTime"
time <- temp

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
      paste0(x * 100, "%")) + scale_y_continuous(limits = c(1.5, 7)) + geom_ribbon(
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
        y = "log Predicted biomass (mg)",
        subtitle = "A: Denmark",
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

### Figure 5: time band ###############################
maxs <- c("Urban_1000", "Agriculture_1000", "Forest_1000")
facet_labs <- c("Midday", "Evening")
names(facet_labs) <- c("midday", "evening")

test <- allInsects[allInsects$Time_band=="midday",]
min(test$cnumberTime)
max(test$cnumberTime)

midday_plot <- allInsects[allInsects$Time_band=="midday",] %>% filter(maxLand_use %in% maxs) %>% mutate(
  maxLand_use = fct_relevel(
    maxLand_use,
    "Urban_1000",
    "Agriculture_1000",
    "Forest_1000"
  )
) %>% ggplot(aes((cnumberTime), log(Biomass+1), colour = maxLand_use)) + geom_point() + geom_smooth(method=lm, alpha = 0.3, size =1.5, show.legend = F)+ scale_colour_manual(values = landuseCols[c(1,2,5)], labels = c(
  "Urban",
  "Farmland",
  "Forest"
)) + facet_grid(.~Time_band, labeller = labeller(Time_band = facet_labs)) + scale_fill_manual(values = c("darkgrey", "darkgrey")) + labs(x = "", y= "log(biomass +1) (mg)", colour = "Sampling time") + theme_minimal() + theme(axis.text.x = element_text(), plot.subtitle = element_text(size = 20, face = "bold"),legend.title = element_blank(), legend.text = element_text(size = 8), legend.position = "bottom")

midday_plot <- midday_plot + scale_x_continuous(breaks = c(-77.5, 10, 87.5), labels = c("12.00", "13.30", "15.00")) + ylim(0,8)

test <- allInsects[allInsects$Time_band=="evening",]
min(test$cnumberTime)
max(test$cnumberTime)

evening_plot <- allInsects[allInsects$Time_band=="evening",] %>% filter(maxLand_use %in% maxs) %>% mutate(
  maxLand_use = fct_relevel(
    maxLand_use,
    "Urban_1000",
    "Agriculture_1000",
    "Forest_1000"
  )
) %>% ggplot(aes((cnumberTime), log(Biomass+1), colour = maxLand_use)) + geom_point() + geom_smooth(method=lm, alpha = 0.3, size =1.5, show.legend = F)+ scale_colour_manual(values = landuseCols[c(1,2,5)], labels = c(
  "Urban",
  "Farmland",
  "Forest"
)) + facet_grid(.~Time_band, labeller = labeller(Time_band = facet_labs)) + scale_fill_manual(values = c("darkgrey", "darkgrey")) + labs(x = "", y= "log(biomass +1) (mg)", colour = "Sampling time") + theme_minimal() + theme(axis.text.x = element_text(), plot.subtitle = element_text(size = 20, face = "bold"),legend.title = element_blank(), legend.text = element_text(size = 8), legend.position = "bottom")

evening_plot <- evening_plot + scale_x_continuous(breaks = c(-74, 21, 95), labels = c("17.00", "18.30", "20.00"))+ ylim(0,8)

plot_row <- plot_grid(midday_plot, evening_plot)

# now add the title
title <- ggdraw() + 
  draw_label(
    "A: Denmark",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )

sampling_time <- plot_grid(
  title, plot_row,
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.1, 1)
)

save_plot("H:/Documents/Insektmobilen/Analysis/InsectMobile_Biomass/plots/DK_sampling_time_maxcover.png", sampling_time, base_width = 10, base_height = 6)

# I subset to just the variables wanted 
allInsects.long <- allInsects %>% 
  select(Biomass, Time_band, cnumberTime, Urban_1000, Agriculture_1000, Open.uncultivated.land_1000, Wetland_50, Forest_250) %>% pivot_longer(-c(Biomass, cnumberTime, Time_band), names_to = "landcover", values_to = "cover")

head(allInsects.long)


#colorset = c('Urban_1000'=landuseCols[1],'Agriculture_1000'=landuseCols[2],'Open.uncultivated.land_1000'=landuseCols[3],'Wetland_50'=landuseCols[4], 'Forest_250' =landuseCols[5])

sampling_time <- allInsects.long  %>% ggplot(aes(cnumberTime, log(Biomass+1), colour = Time_band)) + geom_point() + scale_colour_manual(values = c("lightgrey", "darkgrey"), labels = c(
  "Midday",
  "Evening"
)) + geom_smooth(method=lm, aes(fill = Time_band), alpha = 0.1, size =1.5, show.legend = F) + scale_fill_manual(values = c("lightgrey", "darkgrey"))  + labs(x = "Standardised time", y= "log(biomass +1) (mg)", colour = "Land cover", subtitle = "A: Denmark") + theme(axis.text = element_blank(), plot.subtitle = element_text(size = 20, face = "bold"),legend.title = element_blank(), legend.text = element_text(size = 8), legend.position = "bottom")


### Test of land cover diffs##############################

Ztest <- function(beta1,se1,beta2,se2){
  myZ <- (beta1 - beta2)/sqrt(beta1^2 + beta2^2)
  pvalue = 2*pnorm(abs(myZ), lower.tail = F)
  return(pvalue)
}

mySummary <-  summary(lme1000)$coefficients

#Difference between Agriculture and Open uncultivated
# we predicted (1) to find lower biomass in agricultural areas compared to open semi-natural habitats (wetland and grassland) due to agricultural practices such as pesticide use, lower habitat complexity and increased human disturbance
Ztest(mySummary["Agriculture_1000","Estimate"],mySummary["Agriculture_1000","Std. Error"],
      mySummary["Open.uncultivated.land_1000","Estimate"],mySummary["Open.uncultivated.land_1000","Std. Error"])

Ztest(mySummary["Agriculture_1000","Estimate"],mySummary["Agriculture_1000","Std. Error"],
      mySummary["Wetland_50","Estimate"],mySummary["Wetland_50","Std. Error"])

#Difference between Urban and Open uncultivated
# we predicted (2) that urban cover would have the lowest biomass among all land covers due to the high proportion of impervious surfaces and low proportion of blue and green space.
Ztest(mySummary["Urban_1000","Estimate"],mySummary["Urban_1000","Std. Error"],
      mySummary["Open.uncultivated.land_1000","Estimate"],mySummary["Open.uncultivated.land_1000","Std. Error"])

Ztest(mySummary["Urban_1000","Estimate"],mySummary["Urban_1000","Std. Error"],
      mySummary["Wetland_50","Estimate"],mySummary["Wetland_50","Std. Error"])

Ztest(mySummary["Urban_1000","Estimate"],mySummary["Urban_1000","Std. Error"],
      mySummary["Forest_250","Estimate"],mySummary["Forest_250","Std. Error"])

Ztest(mySummary["Urban_1000","Estimate"],mySummary["Urban_1000","Std. Error"],
      mySummary["Agriculture_1000","Estimate"],mySummary["Agriculture_1000","Std. Error"])

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
                      ) + labs(x = "\nLand cover", y = "Predicted biomass (mg) and 95% CIs\n", subtitle = "B") + theme(plot.subtitle = element_text(size = 20, face = "bold")) + scale_y_log10()

save_plot("plots/DK_predicted_biomass.png", finalplot, base_width = 8, base_height = 5)

### combining the predicted data to re-run model and calculate effects ####
predeffect <- merge(urb, farm)
predeffect <- merge(predeffect, grass)
predeffect <- merge(predeffect, wet)
predeffect <- merge(predeffect, fors)

predeffect <- predeffect %>% rename(Biomass = `log(Biomass + 1)`) # be mindful that biomass is +1 and logtransformed here, the sme for stops
predeffect <- predeffect %>% rename(cStops = `log(cStops + 1)`) 

# run model
library(nlme)
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

summary(gls1) # urban non-significant
exp(1.867667-1) # farmland: 2.381349
exp(3.345569-1) # grassland: 10.43921
exp(2.202820-1) # forest: 3.329493
exp(2.369767-1) # wetland: 3.934434
exp(0.319483-1) # evening: 0.5063551

library(effects)
gls1.alleffects <- allEffects(gls1)
#plot(gls1.alleffects, 'Urban_1000', ylab="Biomass")
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
        limits = c(2.5, 7.5),
        labels = function(x)
          paste0(((x-1)) * 1, "%")) + geom_ribbon( # -1 since +1 is used in biomass response, but exp() also? - then it introduced a lot of decimals, but gives the right effect size
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
            subtitle = "B",
            colour = "Land cover type"
          ) + scale_fill_manual(values = landuseCols)

save_plot("plots/DK_predictedeffect_landcover.png", effectplot, base_width = 10, base_height = 6)

### Test timeband interactions  ################################
lme1000 <- lmer(log(Biomass+1) ~ 
                  Urban_1000 +
                  Time_band*Agriculture_1000 + 
                  Open.uncultivated.land_1000 +
                  Forest_250 +
                  Wetland_50 +
                  Time_band + 
                  cyDay + 
                  (1|RouteID_JB) + (1|PilotID), data=allInsects)
summary(lme1000)

lme1000 <- lmer(log(Biomass+1) ~ 
                  Urban_1000 +
                  Agriculture_1000 + 
                  Open.uncultivated.land_1000*Time_band +
                  Forest_250 +
                  Wetland_50 +
                  Time_band + 
                  cyDay + 
                  (1|RouteID_JB) + (1|PilotID), data=allInsects)
summary(lme1000)

lme1000 <- lmer(log(Biomass+1) ~ 
                  Urban_1000*Time_band +
                  Agriculture_1000 + 
                  Open.uncultivated.land_1000 +
                  Forest_250 +
                  Wetland_50 +
                  Time_band + 
                  cyDay + 
                  (1|RouteID_JB) + (1|PilotID), data=allInsects)
summary(lme1000)

lme1000 <- lmer(log(Biomass+1) ~ 
                  Urban_1000 +
                  Agriculture_1000 + 
                  Open.uncultivated.land_1000 +
                  Forest_250*Time_band +
                  Wetland_50 +
                  Time_band + 
                  cyDay + 
                  (1|RouteID_JB) + (1|PilotID), data=allInsects)
summary(lme1000)

lme1000 <- lmer(log(Biomass+1) ~ 
                  Urban_1000 +
                  Agriculture_1000 + 
                  Open.uncultivated.land_1000 +
                  Forest_250 +
                  Wetland_50*Time_band +
                  Time_band + 
                  cyDay + 
                  (1|RouteID_JB) + (1|PilotID), data=allInsects)
summary(lme1000)

### Germany ##############################################

### Figure 3: the data ##########################

qU <- ggplot(allInsects,aes(x=Urban_1000,y=(Biomass+1)))+
  geom_point(col=landuseCols[1])+
  scale_y_log10() +
  theme_bw() +
  geom_smooth(method="lm",color="grey70") + 
  scale_x_continuous(
    labels = function(x)
      paste0(x, "%")) +
  xlab("") +
  ylab("Biomass (mg)") + 
  labs(subtitle = "Urban cover") + 
  theme(plot.subtitle = element_text(size = 12, face = "bold"), plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))

qF <- ggplot(allInsects,aes(x=Agriculture_1000,y=(Biomass+1)))+
  geom_point(col=landuseCols[2])+
  scale_y_log10() +
  theme_bw() +
  geom_smooth(method="lm",color="grey70") + 
  scale_x_continuous(
    labels = function(x)
      paste0(x, "%")) +
  xlab("") +
  ylab("Biomass (mg)") + 
  labs(subtitle = "Farmland cover") + 
  theme(plot.subtitle = element_text(size = 12, face = "bold"), plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))

qD <- ggplot(allInsects,aes(x=Open.uncultivated_1000,y=(Biomass+1)))+
  geom_point(col=landuseCols[3])+
  scale_y_log10() +
  theme_bw() +
  geom_smooth(method="lm",color="grey70") + 
  scale_x_continuous(
    labels = function(x)
      paste0(x, "%")) +
  xlab("") +
  ylab("Biomass (mg)") + 
  labs(subtitle = "Grassland cover") + 
  theme(plot.subtitle = element_text(size = 12, face = "bold"), plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))

qW <- ggplot(allInsects,aes(x=Wetland_1000,y=(Biomass+1)))+
  geom_point(col=landuseCols[4])+
  scale_y_log10() +
  theme_bw() +
  geom_smooth(method="lm",color="grey70") + 
  scale_x_continuous(
    labels = function(x)
      paste0(x, "%")) +
  xlab("") +
  ylab("Biomass (mg)") + 
  labs(subtitle = "Wetland cover") + 
  theme(plot.subtitle = element_text(size = 12, face = "bold"), plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))

qFo <- ggplot(allInsects,aes(x=Forest_1000,y=(Biomass+1)))+
  geom_point(col=landuseCols[5])+
  scale_y_log10() +
  theme_bw() +
  geom_smooth(method="lm",color="grey70") + 
  scale_x_continuous(
    labels = function(x)
      paste0(x, "%")) +
  xlab("") +
  ylab("Biomass (mg)") + 
  labs(subtitle = "Forest cover") + 
  theme(plot.subtitle = element_text(size = 12, face = "bold"), plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))

plot_grid(qU,qF,qD,qW,qFo,ncol=1)

ggsave("plots/Landcover_percent.png",width=4,height=12)

###pie chart#####################################

library(dplyr)
library(tidyr)

routeMeans <- allInsects %>% 
  group_by(RouteID) %>%
  summarise(meanBiomass = mean(Biomass))

allInsects <- inner_join(allInsects,routeMeans,by="RouteID")

#remove duplicates
allInsects <- allInsects %>%
  select(RouteID,meanBiomass,Agriculture_1000,
         Forest_1000,Open.uncultivated_1000,
         Urban_1000,Wetland_1000) %>%
  distinct()

#fill in missing column
allInsects$totalLand <- apply(allInsects[,3:7],1,sum)
allInsects$Other_1000 <- 100-allInsects$totalLand

#divide up biomass into quantiles
allInsects$BiomassCats <- cut_number(allInsects$meanBiomass,n=5)

#mean land cover per biomass cats
allInsects_cat <- allInsects %>%
  group_by(BiomassCats) %>%
  summarise(Agriculture_1000 = mean(Agriculture_1000),
            Forest_1000 = mean(Forest_1000),
            Open.uncultivated_1000 = mean(Open.uncultivated_1000),
            Urban_1000 = mean(Urban_1000),
            Wetland_1000 = mean(Wetland_1000),
            Other_1000 = mean(Other_1000))

#plot data by biomass categories
allInsects_melt <- gather(allInsects_cat, Land_cover, value, -BiomassCats)

#plot
library(forcats)
allInsects_melt <- allInsects_melt %>% mutate(
  Land_cover = fct_relevel(
    Land_cover,
    "Urban_1000",
    "Agriculture_1000",
    "Open.uncultivated_1000",
    "Wetland_1000",
    "Forest_1000",
    "Other_1000"))  

levels(allInsects_melt$BiomassCats)

biomass.labs <- c("[0,46]"=" < 46 mg", "(46,115]"="486-115 mg", "(115,209]"="115-209 mg", 
                  "(209,502]"="209-502 mg", "(502,1.38e+03]"="> 502 mg")


ggplot(allInsects_melt,aes(x="",y=value,fill=Land_cover, order = Land_cover))+
  geom_bar(stat="identity")+
  facet_grid(~BiomassCats, labeller = labeller(BiomassCats=biomass.labs))+
  coord_polar("y")+
  theme_classic() + 
  theme(axis.line = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  theme(legend.position="top") + scale_fill_manual(values = landuseCols, name = "Land cover", labels = c("Urban", "Farmland", "Grassland", "Wetland", "Forest", "Other")) +guides(fill=guide_legend(nrow=1,byrow=TRUE))

ggsave("plots/piecharts_DE.png",width=12,height=6)

### Linear Mixed Effects Model: Land covers (Table 1) #################

#full model and final
lme1000 <- lmer(log(Biomass+1) ~ 
                  Agriculture_1000 + 
                  Urban_1000 +
                  Open.uncultivated_1000+
                  Wetland_1000 +
                  Forest_250 +
                  Time_band + 
                  Time_band:cnumberTime + 
                  cStops + 
                  cyDay + 
                  (1|RouteID) + (1|PilotID), data=allInsects)
summary(lme1000)
vif(lme1000)
r.squaredGLMM(lme1000)
#R2m       R2c
#[1,] 0.3593968 0.8313989

#with sqrt
lme1000 <- lmer(log(Biomass+1) ~ 
                  sqrt(Agriculture_1000) + 
                  sqrt(Urban_1000) +
                  sqrt(Open.uncultivated_1000) +
                  sqrt(Wetland_1000) +
                  sqrt(Forest_250) +
                  Time_band + 
                  Time_band:cnumberTime + 
                  cStops + 
                  cyDay + 
                  (1|RouteID) + (1|PilotID), data=allInsects)
summary(lme1000)
vif(lme1000)
#now agriculture is most important... but urban comes out on top with AIC
#r.squaredGLMM(lme1000)
#R2m       R2c
#[1,] 0.3582978 0.8328536

#with model simplification
lme1000 <- lmer(log(Biomass+1) ~ 
                  Urban_1000 +
                  Time_band + 
                  Time_band:cnumberTime + 
                  (1|RouteID) + (1|PilotID), data=allInsects)
summary(lme1000)
r.squaredGLMM(lme1000)
#R2m       R2c
#[1,] 0.3197659 0.8265884

#as gls
lme1000 <- lme(log(Biomass+1) ~ Agriculture_1000 + 
              Urban_1000 +
              Open.uncultivated_1000+
              Wetland_1000 +
              Forest_250 +
              Time_band + 
              Time_band:cnumberTime + 
              cStops + 
              cyDay,
            random=~1|PilotID,
            correlation=corExp(form=~x2+y2|PilotID,nugget=TRUE),
            data=allInsects)
summary(lme1000)
vif(lme1000)
r.squaredGLMM(lme1000)

### AIC check ##############################################

library(MuMIn)
options(na.action = "na.fail")
dd <- dredge(lme1000)
subset(dd, delta < 2)

# Visualize the model selection table:
par(mar = c(3,5,6,4))
plot(dd, labAsExpr = TRUE)

# Model average models with delta AICc < 4
model.avg(dd, subset = delta < 2)

#only urban remains in best set

lme1000 <- lmer(log(Biomass+1) ~ 
                  Urban_1000 +
                  Time_band + 
                  (1|RouteID) + (1|PilotID), data=allInsects)
summary(lme1000)
#r.squaredGLMM(lme1000)
R2m       R2c
#[1,] 0.3028254 0.8204277

### Figure 4: effect plots ##########################

predEffects <- predictorEffects(lme1000)

getEffects <- function(predEffects,var){
  temp <- predEffects[var]
  data.frame(fit = temp[[1]]$fit,
             se = temp[[1]]$se,
             lower = temp[[1]]$lower,
             upper = temp[[1]]$upper,
             propcover = temp[[1]]$x[,1],
             landcover = rep(var,length(temp[[1]]$fit)))
}

#apply to each land cover
urb <- getEffects(predEffects,var="Urban_1000")
farm <- getEffects(predEffects,var="Agriculture_1000")
grass <- getEffects(predEffects,var="Open.uncultivated_1000")
wet <- getEffects(predEffects,var="Wetland_1000")
forest <- getEffects(predEffects,var="Forest_250")
test <- rbind(urb, farm, grass, wet, forest)

# Visualization
test_relevel <- test %>% mutate(
  landcover = fct_relevel(
    landcover,
    "Urban_1000",
    "Agriculture_1000",
    "Open.uncultivated_1000",
    "Wetland_1000",
    "Forest_250"
  )) 

effectplot <- test_relevel %>% 
  ggplot(aes(x = propcover, y = fit, fill = landcover)) +
  geom_line(aes(color = landcover), size = 2) +
  scale_color_manual(
    values = landuseCols,
    labels = c(
      "Urban cover (1000 m)",
      "Farmland cover (1000 m)",
      "Grassland cover (1000 m)",
      "Wetland cover (1000 m)",
      "Forest cover (250 m)")) + 
    theme_minimal_grid() + 
  theme(
    plot.subtitle = element_text(size = 20, face = "bold"),
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    legend.position = "bottom") + 
  scale_x_continuous(
    limits = c(0, 100),
    labels = function(x)
      paste0(x, "%"))  + 
  scale_y_continuous(limits = c(1.5, 7)) +
  #      labels = function(x)
  #        paste0(x * 1, "%")) + 
  geom_ribbon(
        aes(
          ymin = fit-se,
          ymax = fit+se,
          group = landcover),
        linetype = 2,
        alpha = 0.2,
        show.legend = F) + 
  labs(
        x = "Land cover",
        y = "log Predicted biomass (mg)",
        subtitle = "B: Germany",
        colour = "Land cover type") + 
  scale_fill_manual(values = landuseCols)

save_plot("plots/DE_effect_landcover.png", effectplot, base_width = 10, base_height = 6)

### Test of land cover diffs##############################

Ztest <- function(beta1,se1,beta2,se2){
  myZ <- (beta1 - beta2)/sqrt(beta1^2 + beta2^2)
  pvalue = 2*pnorm(abs(myZ), lower.tail = F)
  return(pvalue)
}

mySummary <-  summary(lme1000)$coefficients

#Difference between Agriculture and Open uncultivated
Ztest(mySummary["Agriculture_1000","Estimate"],mySummary["Agriculture_1000","Std. Error"],
     mySummary["Open.uncultivated_1000","Estimate"],mySummary["Open.uncultivated_1000","Std. Error"])

#Difference between Urban and Open uncultivated
Ztest(mySummary["Urban_1000","Estimate"],mySummary["Urban_1000","Std. Error"],
      mySummary["Open.uncultivated_1000","Estimate"],mySummary["Open.uncultivated_1000","Std. Error"])

###PCA axes as variables##################################

#run script in script 07 to get PCA axes variables

lme1000 <- lmer(log(Biomass+1) ~ 
                  Urbanization_gradient +
                  Forest_gradient +
                  Time_band + 
                  Time_band:cnumberTime + 
                  cStops + 
                  cyDay + 
                  (1|RouteID) + (1|PilotID), data=allInsects)
summary(lme1000)
vif(lme1000)
r.squaredGLMM(lme1000)

#Fixed effects:
#  Estimate Std. Error         df t value Pr(>|t|)    
#(Intercept)                   4.754e+00  2.604e-01  2.657e+01  18.259  < 2e-16 ***
#  Urbanization_gradient        -4.042e-01  1.881e-01  5.966e+01  -2.150  0.03566 *  
#  Forest_gradient              -2.180e-01  1.672e-01  5.724e+01  -1.304  0.19753    
#Time_bandevening              3.808e-01  1.152e-01  6.398e+01   3.305  0.00156 ** 
#cStops                       -3.212e-01  1.905e-01  5.009e+01  -1.686  0.09807 .  
#cyDay                        -6.489e-02  4.313e-02  5.681e+01  -1.504  0.13800    
#Time_bandmidday:cnumberTime  -4.606e-05  1.805e-03  8.419e+01  -0.026  0.97970    
#Time_bandevening:cnumberTime  5.106e-03  2.062e-03  8.362e+01   2.476  0.01531 *
  

lme1000 <- lmer(log(Biomass+1) ~ 
                  Urbanization_gradient * Time_band +
                  Forest_gradient * Time_band +
                  Time_band + 
                  Time_band:cnumberTime + 
                  cStops + 
                  cyDay + 
                  (1|RouteID) + (1|PilotID), data=allInsects)
summary(lme1000)
#Urbanization_gradient:Time_bandevening  0.2234471  0.1122381 61.6377367   1.991  0.05094


### DE biomass predictions% ##############################

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

### Test timeband interactions ################################

lme1000 <- lmer(log(Biomass+1) ~ 
                  Time_band*Agriculture_1000 + 
                  Time_band + 
                  Time_band:cnumberTime + cStops + cyDay + 
                  (1|RouteID) + (1|PilotID), data=allInsects)
summary(lme1000)

lme1000 <- lmer(log(Biomass+1) ~ 
                  Time_band*Urban_1000 + 
                  Time_band + 
                  Time_band:cnumberTime + cStops + cyDay + 
                  (1|RouteID) + (1|PilotID), data=allInsects)
summary(lme1000)


ggplot(allInsects,aes(x=as.numeric(Time_band), y=log(Biomass+1)))+
  geom_jitter(aes(colour=Land_use))+
  stat_smooth(aes(colour=Land_use),method="lm",se=FALSE)


### Fig 5 DE #####################################################

#identify maxLanduse at each route
allInsects$maxLand_use <- apply(allInsects,1,function(x)which.max(x[c("Agriculture_1000","Forest_1000",
                                                                      "Open.uncultivated_1000","Wetland_1000",
                                                                      "Urban_1000")]))
allInsects$maxLand_use[allInsects$maxLand_use==1]<- "Agriculture_1000"
allInsects$maxLand_use[allInsects$maxLand_use==2]<- "Forest_1000"
allInsects$maxLand_use[allInsects$maxLand_use==3]<- "Open.uncultivated_1000"
allInsects$maxLand_use[allInsects$maxLand_use==4]<- "Wetland_1000"
allInsects$maxLand_use[allInsects$maxLand_use==5]<- "Urban_1000"


maxs <- c("Urban_1000", "Agriculture_1000", "Forest_1000")
facet_labs <- c("Midday", "Evening")
names(facet_labs) <- c("midday", "evening")

test <- allInsects[allInsects$Time_band=="midday",]
min(test$cnumberTime)
max(test$cnumberTime)

midday_plot <- allInsects[allInsects$Time_band=="midday",] %>% filter(maxLand_use %in% maxs) %>% mutate(
  maxLand_use = fct_relevel(
    maxLand_use,
    "Urban_1000",
    "Agriculture_1000",
    "Forest_1000"
  )
) %>% ggplot(aes((cnumberTime), log(Biomass+1), colour = maxLand_use)) + geom_point() + geom_smooth(method=lm, alpha = 0.3, size =1.5, show.legend = F)+ scale_colour_manual(values = landuseCols[c(1,2,5)], labels = c(
  "Urban",
  "Farmland",
  "Forest"
)) + facet_grid(.~Time_band, labeller = labeller(Time_band = facet_labs)) + scale_fill_manual(values = c("darkgrey", "darkgrey")) + labs(x = "", y= "log(biomass +1) (mg)", colour = "Sampling time") + theme_minimal() + theme(axis.text.x = element_text(), plot.subtitle = element_text(size = 20, face = "bold"),legend.title = element_blank(), legend.text = element_text(size = 8), legend.position = "bottom")

midday_plot <- midday_plot + scale_x_continuous(breaks = c(-77.5, 10, 87.5), labels = c("12.00", "13.30", "15.00")) + ylim(0,8)

test <- allInsects[allInsects$Time_band=="evening",]
min(test$cnumberTime)
max(test$cnumberTime)

evening_plot <- allInsects[allInsects$Time_band=="evening",] %>% filter(maxLand_use %in% maxs) %>% mutate(
  maxLand_use = fct_relevel(
    maxLand_use,
    "Urban_1000",
    "Agriculture_1000",
    "Forest_1000"
  )
) %>% ggplot(aes((cnumberTime), log(Biomass+1), colour = maxLand_use)) + geom_point() + geom_smooth(method=lm, alpha = 0.3, size =1.5, show.legend = F)+ scale_colour_manual(values = landuseCols[c(1,2,5)], labels = c(
  "Urban",
  "Farmland",
  "Forest"
)) + facet_grid(.~Time_band, labeller = labeller(Time_band = facet_labs)) + scale_fill_manual(values = c("darkgrey", "darkgrey")) + labs(x = "", y= "log(biomass +1) (mg)", colour = "Sampling time") + theme_minimal() + theme(axis.text.x = element_text(), plot.subtitle = element_text(size = 20, face = "bold"),legend.title = element_blank(), legend.text = element_text(size = 8), legend.position = "bottom")

evening_plot <- evening_plot + scale_x_continuous(breaks = c(-74, 21, 95), labels = c("17.00", "18.30", "20.00"))+ ylim(0,8)

plot_row <- plot_grid(midday_plot, evening_plot)

# now add the title
title <- ggdraw() + 
  draw_label(
    "B: Germany",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )

sampling_time <- plot_grid(
  title, plot_row,
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.1, 1)
)

save_plot("plots/DE_sampling_time_maxcover.png", sampling_time, base_width = 10, base_height = 6)
