#run 03mergeData script 

### Load required libraries ###########################
library(cowplot)
library(ggplot2)
library(wesanderson)
library(ggpubr)
library(scales)
library(psych)
library(ggfortify)
library(lme4)
library(lmerTest)
library(effects)
library(corrplot)
library(MuMIn)
library(sjPlot)

#### Set colour scheme ##############################################################

landuseCols <- c("#CC79A7", "#E69F00", "#D55E00", "#56B4E9", "#009E73", "darkgrey") # colour friendly, ordered by land cover 

### DK urban##############################################
allInsects <- read.delim("cleaned-data/DK_allInsects.txt")

# change land covers to be 0-100 instead of 0-1
allInsects[, c(26:49, 70:137,139)] <- allInsects[, c(26:49, 70:137,139)]*100

### urban greenness analysis (1st review) ####

# since we find a strong negative effect of urban cover in the main land cover analysis, we wish to explore whether green space might remedy some of this negative effect. To test this, we need to extract the urban green land use variables, hedges and urban green areas, and calculate their porportional cover within urban land cover. 

# first examine general correlations
names(allInsects)
someInsects <- allInsects[,c(12,22, 48, 62, 70, 74:77)] 
colnames(someInsects)
colnames(someInsects) <- c("Biomass", "Stops", "Urban", "Hedges", "Urban green", "Inner city", "Commercial", "Residential", "Multistory")

p <- cor(someInsects, use="pairwise.complete.obs")

# add significance
res1 <- cor.mtest(someInsects, conf.level = .95)
res2 <- cor.mtest(someInsects, conf.level = .99)

png(height=7, width=7, units="in", file="plots/corr_urban_landuse.jpeg", type = "cairo-png", res = 300)

# with correlation coefficient instead of p-values, coloured boxes = significant at a 0.05 level
corrplot(p, method = "color",
         type = "upper", order = "original", number.cex = .7,
         addCoef.col = "black", # Add coefficient of correlation
         tl.col = "black", tl.srt = 90, # Text label color and rotation
         # Combine with significance
         p.mat = res1$p, sig.level = 0.05, insig = "blank", 
         # hide correlation coefficient on the principal diagonal 
         diag = FALSE, mar=c(0,0,1,0))

corrplot(p, method = "color",
         type = "upper", order = "AOE", number.cex = .7,
         addCoef.col = "black", # Add coefficient of correlation
         tl.col = "black", tl.srt = 90, # Text label color and rotation
         # Combine with significance
         p.mat = res1$p, sig.level = 0.05, insig = "blank", 
         # hide correlation coefficient on the principal diagonal 
         diag = FALSE, mar=c(0,0,1,0))

dev.off()

### PCA urban ##################
names(someInsects)
mydata <- someInsects[,c(3,5:9)]

fit <- princomp(mydata, cor=TRUE)
biplot(fit)

#with ggplot
autoplot(fit)
autoplot(fit, data = mydata, 
                        loadings = TRUE, 
                        loadings.colour = 'black',
                        loadings.label = TRUE, 
                        loadings.label.size = 5) + 
  scale_colour_manual(values = landuseCols[1:6])+
  theme_bw() + labs(colour = "Land cover")

# the main axis explains most of the variation - the general urban cover effect.
#cowplot::save_plot("plots/pca_landuse_urban.png", dk_autoplot, base_height = 8, base_width = 12)

pca_rotated <- psych::principal(mydata, rotate="varimax", nfactors=2, scores=TRUE)
biplot(pca_rotated, main = "")
print(pca_rotated)

### subset to urban routes #########
# we choose a % cover threshold 
quantile(allInsects$Urban_1000)
data <- subset(allInsects, allInsects$Urban_1000 > 6.67550)

#make data proportional
#data$propHedge <- (data$byHegnMeterPerHa_1000/data$Urban_1000)
data$propurbGreen <- (data$urbGreenPropArea_1000/data$Urban_1000)*100
data$propLargecity <- (data$Bykerne_1000/data$Urban_1000)*100
data$propCommercial <- (data$Erhverv_1000/data$Urban_1000)*100
data$propMultistory <- (data$Høj.bebyggelse_1000/data$Urban_1000)*100
tail(data)

# merge greenness
data$propGreen <- data$propurbGreen 
#data$propMajorcity <- data$propLargecity + data$propCommercial #+ data$propMultistory
mean(data$propGreen)
max(data$propGreen)
median(data$propGreen)

#mean(data$propMajorcity)
#max(data$propMajorcity)
#median(data$propMajorcity)

data <- data %>% mutate_if(is.numeric, function(x) ifelse(is.infinite(x), 0, x))

### correlation plot proportional urban ###################
someInsects <- data[,c("Biomass", "cStops", "Urban_1000", "propGreen")]

colnames(someInsects)
colnames(someInsects) <- c("Biomass", "Stops", "Urban", "propGreen")

p <- cor(someInsects, use="pairwise.complete.obs")

# add significance
res1 <- cor.mtest(someInsects, conf.level = .95)
res2 <- cor.mtest(someInsects, conf.level = .99)

# with correlation coefficient instead of p-values, coloured boxes = significant at a 0.05 level

png(units="in", width=7, height=7, file="plots/corr_propurban_landuse.jpeg", type = "cairo-png", res = 300)

corrplot(p, method = "color",
         type = "upper", order = "original", number.cex = .7,
         addCoef.col = "black", # Add coefficient of correlation
         tl.col = "black", tl.srt = 60, # Text label color and rotation
         # Combine with significance
         p.mat = res1$p, sig.level = 0.05, insig = "blank", 
         # hide correlation coefficient on the principal diagonal 
         diag = FALSE, mar=c(0,0,1,0))

dev.off()

# now, after we have calculated the proportional green cover within urban, there is not a strong correlation between urban and greenness variable

lme1000 <- lmer(log(Biomass+1) ~ 
                  #Urban_1000 +
                  propGreen + 
                  Time_band + 
                  Time_band:cnumberTime + cStops + cyDay + 
                  (1|RouteID_JB) + (1|PilotID), data=data)
summary(lme1000)
tab_model(lme1000, digits = 3, show.intercept = F, collapse.ci = T, pred.labels = c("Prop. green cover", "Evening vs midday", "Potential stops", "Day of year", "Time within midday", "Time within evening"))
car::vif(lme1000)

gls1.alleffects <- allEffects(lme1000)
effectdata <- as.data.frame(gls1.alleffects, row.names=NULL, optional=TRUE)

eall.lm1 <- predictorEffects(lme1000)
plot(eall.lm1, lines=list(multiline=TRUE))

### ggplot effect plot ####
# Urbangreen_gradient 
temp <- effectdata$propGreen
temp$landuse <- "propGreen"
propGreen <- temp %>% 
  dplyr::rename(
    propcover = propGreen
  )%>% dplyr::select(landuse, propcover, fit, se, lower, upper)

prop_data <- propGreen
str(prop_data)

# Visualization
landuse_urban <- prop_data %>% ggplot(aes(x = propcover, y = fit, fill = landuse)) +
  geom_line(aes(color = landuse), size = 2) +
  scale_color_manual(
    values = c("#FA0081"), labels = "Prop. urban green cover"
  ) + theme_minimal_grid() + theme(
    plot.subtitle = element_text(size = 20, face = "bold"),
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    legend.position = "bottom"
  ) + scale_x_continuous(
    limits = c(20, 70),
    labels = function(x)
      paste0(x, "%")) + geom_ribbon(
        aes(
          ymin = fit-se,
          ymax = fit+se,
          group = landuse
        ),
        linetype = 2,
        alpha = 0.2,
        show.legend = F
      )  + labs(
        x = "",
        y = "log Predicted biomass (mg)",
        subtitle = "A",
        colour = "Land use type"
      ) + scale_fill_manual(values =  c("#FA0081")) + guides(colour = guide_legend(nrow = 1))

#save_plot("plots/DK_urbanlanduse_cover_greenness.png", landuse_urban, base_width = 8, base_height = 6)

### PCA propUrban ##############
names(data)
someInsects <- data[,c(12,22, 48, 70, 74:77)] 
colnames(someInsects)
#colnames(someInsects) <- c("Biomass", "Stops", "Urban", "Urban green", "Inner city", "Commercial", "Residential", "Multistory")

#make data proportional
someInsects$propurbGreen <- (someInsects$urbGreenPropArea_1000/someInsects$Urban_1000)*100
someInsects$propInnercity <- (someInsects$Bykerne_1000/someInsects$Urban_1000)*100
someInsects$propCommercial <- (someInsects$Erhverv_1000/someInsects$Urban_1000)*100
someInsects$propResidential <- (someInsects$Lav.bebyggelse_1000/someInsects$Urban_1000)*100
someInsects$propMultistory <- (someInsects$Høj.bebyggelse_1000/someInsects$Urban_1000)*100
names(someInsects)

fit <- princomp(someInsects[,c(3, 9:13)], cor=TRUE)
#with ggplot
biplot(fit)
autoplot(fit)
dk_autoplot <- autoplot(fit, data = mydata, 
         loadings = TRUE, 
         loadings.colour = 'black',
         loadings.label = TRUE, 
         loadings.label.size = 5) + 
  scale_colour_manual(values = landuseCols[1:6])+
  theme_bw() + labs(colour = "Land cover")

cowplot::save_plot("plots/pca_proplanduse_urban.png", dk_autoplot, base_height = 10, base_width = 12)

pca_rotated <- psych::principal(someInsects[,c(3, 9:13)], 
                                rotate="varimax", nfactors=2, scores=TRUE)
biplot(pca_rotated, main = "")
print(pca_rotated)

#model selection with these pca axes
data$Urbanization_gradient <- pca_rotated$scores[,1]
data$Greening_gradient <- pca_rotated$scores[,2]

### plotting #############################################

#plot gradient (split into factor just for plotting purposes)

#general gradient
data$General_gradient_factor <- cut(data$Urbanization_gradient,3)
ggplot(data,aes(x=Urban_1000,y=log(Biomass+1),
                group=General_gradient_factor))+
  geom_smooth(method=lm,aes(colour=General_gradient_factor),se=F)+
  scale_colour_viridis_d()

#greening gradient
data$Greening_gradient_factor <- cut(data$Greening_gradient,3)
ggplot(data,aes(x=Urban_1000,y=log(Biomass+1),
                group=Greening_gradient_factor))+
  geom_smooth(method=lm,aes(colour=Greening_gradient_factor),se=F)+
  scale_colour_viridis_d()

#both gradients
ggplot(data,aes(x=Urban_1000,y=log(Biomass+1)))+
  geom_smooth(method=lm,se=F)+
  facet_grid(Greening_gradient_factor~General_gradient_factor)

#urban gradient alone
data$urban_gradient_factor <- cut(data$Urban,5)
ggplot(data,aes(x=Urban,y=log(Biomass+1),
                group=urban_gradient_factor))+
  geom_smooth(method="lm",aes(colour=urban_gradient_factor),se=F)

ggplot(data,aes(x=Urban,y=log(Biomass+1)))+
  geom_smooth(method="loess")

#add PCA axes scores to the dataset
lme1000 <- lmer(log(Biomass+1) ~ 
                  Greening_gradient + 
                  Urbanization_gradient +
                  Time_band + 
                  Time_band:cnumberTime + cStops + cyDay + 
                  (1|RouteID_JB) + (1|PilotID), data=data)
summary(lme1000)
tab_model(lme1000, show.intercept = F, collapse.ci = T, pred.labels = c("Urban green gradient", "Urbanization gradient", "Time band: evening vs midday", "Potential stops", "Day of year", "Time within midday", "Time within evening"))
#r.squaredGLMM(lme1000)
car::vif(lme1000)

# pairwise comparison 
pair.ht <- glht(lme1000, linfct = c("Greening_gradient - Urbanization_gradient = 0"))
summary(pair.ht) 
confint(pair.ht)

### DK farmland ##########################################

g1 <- ggplot(subset(allInsects, maxLand_use = "Agriculture_1000"),
             aes(x=sqrt(hegnMeterPerHa_1000),y=(Biomass+1)))+
  geom_point(col=landuseCols[3])+
  scale_y_log10() +
  theme_bw() +
  geom_smooth(method="lm",color="grey70")+
  xlab("Hedgerows m per ha") +ylab("Biomass")

g2 <- ggplot(subset(allInsects, maxLand_use = "Agriculture_1000"),
             aes(sqrt(x=Ekstensiv_organic_1000),y=(Biomass+1)))+
  geom_point(col=landuseCols[3])+
  scale_y_log10() +
  theme_bw() +
  geom_smooth(method="lm",color="grey70")+
  xlab("Organic extensive farmland cover") +ylab("")

g3 <- ggplot(subset(allInsects, maxLand_use = "Agriculture_1000"),
             aes(sqrt(x=Ekstensiv_1000), y=(Biomass+1)))+
  geom_point(col=landuseCols[3])+
  scale_y_log10() +
  theme_bw() +
  geom_smooth(method="lm",color="grey70")+
  xlab("Extensive farmland cover") +ylab("")

g4 <- ggplot(subset(allInsects, maxLand_use = "Agriculture_1000"),
             aes(sqrt(x=Semi.intensiv_1000), y=(Biomass+1)))+
  geom_point(col=landuseCols[3])+
  scale_y_log10() +
  theme_bw() +
  geom_smooth(method="lm",color="grey70")+
  xlab("Semi-intensive farmland cover") +ylab("Biomass")

g5 <- ggplot(subset(allInsects, maxLand_use = "Agriculture_1000"),
             aes(x=Intensiv_1000, y=(Biomass+1)))+
  geom_point(col=landuseCols[3])+
  scale_y_log10() +
  theme_bw() +
  geom_smooth(method="lm",color="grey70")+
  xlab("Intensive farmland cover") +ylab("")

g6 <- ggplot(subset(allInsects, maxLand_use = "Agriculture_1000"),
             aes(sqrt(x=Markblok_1000), y=(Biomass+1)))+
  geom_point(col=landuseCols[3])+
  scale_y_log10() +
  theme_bw() +
  geom_smooth(method="lm",color="grey70")+
  xlab("Unspecified crop cover") +ylab("")

cowplot::plot_grid(g1,g2,g3,g4,g5,g6)

### PCA farmland ####
mydata <- allInsects[,c("cStops",names(allInsects)[grepl("_1000",names(allInsects))])]
names(mydata)
mydata <- mydata[,c(2, 18:21,24:25)]
names(mydata) <- gsub("_1000","",names(mydata))
mydata <- mydata %>% drop_na(Ekstensiv)
#mydata <- plyr::rename(mydata, c("hegnMeterPerHa" = "Hedgerows"))
mydata <- plyr::rename(mydata, c("Ekstensiv" = "Extensive"))
mydata <- plyr::rename(mydata, c("Ekstensiv_organic" = "Organic extensive"))
mydata <- plyr::rename(mydata, c("Intensiv" = "Intensive"))
mydata <- plyr::rename(mydata, c("Intensiv_organic" = "Organic intensive"))
mydata <- plyr::rename(mydata, c("Semi.intensiv" = "Semi-intensive"))
mydata <- plyr::rename(mydata, c("Semi.intensiv_organic" = "Organic semi-intensive cover"))

fit <- princomp(mydata, cor=TRUE)
fit_data <- allInsects %>% drop_na(Ekstensiv_1000)

#with ggplot
autoplot(fit)
dk_autoplot <- autoplot(fit, data = fit_data, 
                        loadings = TRUE, 
                        loadings.colour = 'black',
                        loadings.label = TRUE, 
                        loadings.label.size = 5) + 
  scale_colour_manual(values = landuseCols[1:6])+
  theme_bw() + labs(colour = "Land cover")

cowplot::save_plot("plots/pca_landuse_farmland.png", dk_autoplot, base_height = 8, base_width = 12)

#packageurl <- "https://cran.r-project.org/src/contrib/Archive/mnormt/mnormt_1.5-7.tar.gz"
#install.packages(packageurl, repos=NULL, type="source")
pca_rotated <- psych::principal(mydata, rotate="varimax", nfactors=2, scores=TRUE)
biplot(pca_rotated, main = "")
print(pca_rotated)

#### correlation plot farmland use (not proportional) ############
names(data)
someInsects <- data[,c(12,142, 44, 90:93, 96:97)]
colnames(someInsects)
colnames(someInsects) <- c("Biomass", "Stops", "Farmland", "Extensive", "OrganicExtensive", "Intensive", "OrganicIntensive", "semiIntensive", "semiOrganicIntensive")

p <- cor(someInsects, use="pairwise.complete.obs")

# add significance
res1 <- cor.mtest(someInsects, conf.level = .95)
res2 <- cor.mtest(someInsects, conf.level = .99)

png(height=7, width=7, units = "in", file="plots/DK_farmland_use.jpeg", type = "cairo-png", res = 300)

# with correlation coefficient instead of p-values, coloured boxes = significant at a 0.05 level
corrplot(p, method = "color",
         type = "upper", order = "original", number.cex = .7,
         addCoef.col = "black", # Add coefficient of correlation
         tl.col = "black", tl.srt = 90, # Text label color and rotation
         # Combine with significance
         p.mat = res1$p, sig.level = 0.05, insig = "blank", 
         # hide correlation coefficient on the principal diagonal 
         diag = FALSE, mar=c(0,0,1,0))

dev.off()

# with correlation coefficient instead of p-values, coloured boxes = significant at a 0.05 level
corrplot(p, method = "color",
         type = "upper", order = "AOE", number.cex = .7,
         addCoef.col = "black", # Add coefficient of correlation
         tl.col = "black", tl.srt = 90, # Text label color and rotation
         # Combine with significance
         p.mat = res1$p, sig.level = 0.05, insig = "blank", 
         # hide correlation coefficient on the principal diagonal 
         diag = FALSE, mar=c(0,0,1,0))

dev.off()

### DK farmland proportional cover ######################################
# calculate the proportional cover of the land use intensity variables within the most land cover routes
quantile(allInsects$Agriculture_1000)

data <- subset(allInsects, allInsects$Agriculture_1000 > 60.07200)
data$propOrgExtensive <- (data$Ekstensiv_organic_1000/data$Agriculture_1000)*100
data$propExtensive <- (data$Ekstensiv_1000/data$Agriculture_1000)*100
data$propSemiIntensive <- (data$Semi.intensiv_1000/data$Agriculture_1000)*100
data$propOrgSemiIntensive <- (data$Semi.intensiv_organic_1000/data$Agriculture_1000)*100
data$propIntensive <- (data$`Intensiv_1000`/data$Agriculture_1000)*100
data$propOrgIntensive <- (data$Intensiv_organic_1000/data$Agriculture_1000)*100
tail(data)

data <- data %>% mutate_if(is.numeric, function(x) ifelse(is.infinite(x), 0, x))
# make NAs zeros
data <- data %>%
  dplyr::mutate_all(funs(ifelse(is.na(.), 0, .)))

# merge covers that have similar farming practices
data$propOrganic_farmland <- data$propOrgExtensive + data$propOrgIntensive + data$propOrgSemiIntensive
max(data$propOrganic_farmland) # does not exceed 100
mean(data$propOrganic_farmland)
median(data$propOrganic_farmland)

data$propConventional_farmland <-  data$propIntensive + data$propExtensive + data$propSemiIntensive
max(data$propConventional_farmland) # does not exceed 100
mean(data$propConventional_farmland)
median(data$propConventional_farmland)

### correlation plot proportional agriculture ###################
names(data)
someInsects <- data[,c(12,142, 44, 145:150)]
colnames(someInsects)
colnames(someInsects) <- c("Biomass", "Stops", "Farmland", "propOrgExtensive", "propExtensive", "propSemiIntensive", "propOrgSemiIntensive", "propIntensive", "propOrgIntensive")

p <- cor(someInsects, use="pairwise.complete.obs")

# add significance
res1 <- cor.mtest(someInsects, conf.level = .95)
res2 <- cor.mtest(someInsects, conf.level = .99)

png(height=7, width=7, units = "in", file="plots/prop_farmland_use.jpeg", type = "cairo-png", res = 300)

# with correlation coefficient instead of p-values, coloured boxes = significant at a 0.05 level
corrplot(p, method = "color",
         type = "upper", order = "original", number.cex = .7,
         addCoef.col = "black", # Add coefficient of correlation
         tl.col = "black", tl.srt = 90, # Text label color and rotation
         # Combine with significance
         p.mat = res1$p, sig.level = 0.05, insig = "blank", 
         # hide correlation coefficient on the principal diagonal 
         diag = FALSE, mar=c(0,0,1,0))

dev.off()

# the combined farming practices
names(data)
someInsects <- data[,c(12,142, 44, 151:152)]
colnames(someInsects)
colnames(someInsects) <- c("Biomass", "Stops", "Farmland", "propOrganic", "propConventional")

p <- cor(someInsects, use="pairwise.complete.obs")

# add significance
res1 <- cor.mtest(someInsects, conf.level = .95)
res2 <- cor.mtest(someInsects, conf.level = .99)

png(height=7, width=7, units = "in", file="plots/prop_farmland_practice.jpeg", type = "cairo-png", res = 300)

# with correlation coefficient instead of p-values, coloured boxes = significant at a 0.05 level
corrplot(p, method = "color",
         type = "upper", order = "original", number.cex = .7,
         addCoef.col = "black", # Add coefficient of correlation
         tl.col = "black", tl.srt = 90, # Text label color and rotation
         # Combine with significance
         p.mat = res1$p, sig.level = 0.05, insig = "blank", 
         # hide correlation coefficient on the principal diagonal 
         diag = FALSE, mar=c(0,0,1,0))
dev.off()

names(data)
mean(data$Intensiv_1000)

# model
lmer1000 <- lmer(
  log(Biomass + 1) ~
    propOrgExtensive + 
    propExtensive +
    propOrgSemiIntensive +
    propSemiIntensive +
    propIntensive +
    #propOrgIntensive +
    Time_band + Time_band:cnumberTime + cStops + cyDay +
    (1 | RouteID_JB) + (1 | PilotID),
  data = data
)

summary(lmer1000)
car::vif(lmer1000)

tab_model(lmer1000, show.intercept = F, digits = 3, collapse.ci = T, pred.labels = c("Prop. organic extensive", "Prop. extensive", "Prop. organic semi-intensive", "Prop. semi-intensive",  "Prop. intensive", "Evening vs midday", "Potential stops", "Day of year", "Time within time band"))

# pairwise comparison between interaction terms
pair.ht <- glht(lmer1000, linfct = c("propIntensive - propSemiIntensive = 0", "propIntensive - propOrgSemiIntensive = 0", "propIntensive - propExtensive = 0", "propIntensive - propOrgExtensive = 0"))
summary(pair.ht) # trend toward higher biomass in organic farmland
confint(pair.ht)

# effect
gls1.alleffects <- allEffects(lmer1000)
effectdata <- as.data.frame(gls1.alleffects, row.names=NULL, optional=TRUE)

eall.lm1 <- predictorEffects(lmer1000)
#effectdata <- as.data.frame(eall.lm1, row.names=NULL, optional=TRUE)
plot(eall.lm1, lines=list(multiline=TRUE))
#plot(predictorEffects(lmeurban1000, ~ urbGreenPropArea_1000 + byHegnMeterPerHa_1000 + Bykerne_1000 + Lav.bebyggelse_1000 + Høj.bebyggelse_1000 + Erhverv_1000 + cnumberTime, residuals = T), partial.residuals=list(smooth=TRUE, span=0.50, lty = "dashed"))

### ggplot effect plot ####
names(effectdata)
temp <- effectdata$propOrgExtensive
temp$landuse <- "propOrgExtensive"
propOrgExtensive <- temp %>% 
  dplyr::rename(
    propcover = propOrgExtensive
  )%>% dplyr::select(landuse, propcover, fit, se, lower, upper)

temp <- effectdata$propExtensive
temp$landuse <- "propExtensive"
propExtensive <- temp %>% 
  dplyr::rename(
    propcover = propExtensive
  )%>% dplyr::select(landuse, propcover, fit, se, lower, upper)

temp <- effectdata$propOrgSemiIntensive
temp$landuse <- "propOrgSemiIntensive"
propOrgSemiIntensive <- temp %>% 
  dplyr::rename(
    propcover = propOrgSemiIntensive
  )%>% dplyr::select(landuse, propcover, fit, se, lower, upper)

temp <- effectdata$propSemiIntensive
temp$landuse <- "propSemiIntensive"
propSemiIntensive <- temp %>% 
  dplyr::rename(
    propcover = propSemiIntensive
  )%>% dplyr::select(landuse, propcover, fit, se, lower, upper)

temp <- effectdata$propIntensive
temp$landuse <- "propIntensive"
propIntensive <- temp %>% 
  dplyr::rename(
    propcover = propIntensive
  )%>% dplyr::select(landuse, propcover, fit, se, lower, upper)

test <- rbind(propOrgExtensive, propExtensive, propOrgSemiIntensive, propSemiIntensive, propIntensive)

# Visualization
effectplot_farmland_1 <- test %>% mutate(
  landuse = fct_relevel(
    landuse,
    "propOrgExtensive",
    "propExtensive",
    "propOrgSemiIntensive",
    "propSemiIntensive",
    "propIntensive"
  )
)%>% ggplot(aes(x = propcover, y = fit, fill = landuse)) +
  geom_line(aes(color = landuse), size = 2) +
  scale_color_manual(
    values = c("#29994B", "#44A1E3", "#55E680", "#3E7AFA", "#3C46F0"),
    labels = c(
      "Prop. organic extensive", 
      "Prop. extensive", 
      "Prop. organic semi-intensive", 
      "Prop. semi-intensive",  "Prop. intensive"
    )
  ) + theme_minimal_grid() + theme(
    plot.subtitle = element_text(size = 20, face = "bold"),
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    legend.position = "none"
  ) + scale_x_continuous(
    limits = c(0, 15),
    labels = function(x)
      paste0(x, "%")) + geom_ribbon(
        aes(
          ymin = fit-se,
          ymax = fit+se,
          group = landuse
        ),
        linetype = 2,
        alpha = 0.2,
        show.legend = F
      ) + labs(
        x = "",
        y = "log Predicted biomass (mg)",
        subtitle = "B",
        colour = "Land use type"
      ) + scale_fill_manual(values =  c("#29994B", "#44A1E3", "#55E680", "#3E7AFA", "#3C46F0")) + guides(colour = guide_legend(nrow = 1))

effectplot_farmland_2 <- test %>% mutate(
  landuse = fct_relevel(
    landuse,
    "propOrgExtensive",
    "propExtensive",
    "propOrgSemiIntensive",
    "propSemiIntensive",
    "propIntensive"
  )
)%>% ggplot(aes(x = propcover, y = fit, fill = landuse)) +
  geom_line(aes(color = landuse), size = 2) +
  scale_color_manual(
    values = c("#29994B", "#44A1E3", "#55E680", "#3E7AFA", "#3C46F0"),
    labels = c(
      "Prop. organic extensive", 
      "Prop. extensive", 
      "Prop. organic semi-intensive", 
      "Prop. semi-intensive",  "Prop. intensive"
    )
  ) + theme_minimal_grid() + theme(
    plot.subtitle = element_text(size = 20, face = "bold"),
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    legend.position = "bottom"
  ) + scale_x_continuous(
    limits = c(40, 100),
    labels = function(x)
      paste0(x, "%")) + geom_ribbon(
        aes(
          ymin = fit-se,
          ymax = fit+se,
          group = landuse
        ),
        linetype = 2,
        alpha = 0.2,
        show.legend = F
      ) + labs(
        x = "Land use intensity cover",
        y = "log Predicted biomass (mg)",
        subtitle = "C",
        colour = "Land use type"
      ) + scale_fill_manual(values =  c("#29994B", "#44A1E3", "#55E680", "#3E7AFA", "#3C46F0")) + guides(colour = guide_legend(nrow = 1))

#cowplot::save_plot("plots/farmland_landuse_prop_farming_practice.jpeg", effectplot_farmland)

landuse_plots <- cowplot::plot_grid(landuse_urban, effectplot_farmland_1, effectplot_farmland_2, ncol = 1)

cowplot::save_plot("plots/propLanduse_plot.jpeg", landuse_plots, base_height = 12, base_width = 10)

### PCA propFarmland ####
mydata <- data
names(mydata)
mydata <- mydata[,c(44,145:150)]
names(mydata)
colnames(mydata) <- c("Farmland", "propOrgExtensive", "propExtensive", "propSemiIntensive", "propOrgSemiIntensive", "propIntensive", "propOrgIntensive")

fit <- princomp(mydata, cor=TRUE)

#with ggplot
autoplot(fit)
dk_autoplot <- autoplot(fit, data = fit_data, 
                        loadings = TRUE, 
                        loadings.colour = 'black',
                        loadings.label = TRUE, 
                        loadings.label.size = 5) + 
  scale_colour_manual(values = landuseCols[1:6])+
  theme_bw() + labs(colour = "Land cover")

cowplot::save_plot("plots/pca_landuse_propfarmland.png", dk_autoplot, base_height = 8, base_width = 12)

pca_rotated <- psych::principal(mydata, rotate="varimax", nfactors=2, scores=TRUE)
biplot(pca_rotated, main = "")
print(pca_rotated)

#ggsave("plots/pca_with_rotation_propfarmland_1000_DK.png", width = 12, height = 10)

#add PCA axes scores to the dataset
data$Organic_gradient <- pca_rotated$scores[,1]
data$Conventional_extensive_gradient <- pca_rotated$scores[,2]

lme1000 <- lmer(log(Biomass+1) ~ 
                  Organic_gradient + Conventional_extensive_gradient + 
                  Time_band + 
                  Time_band:cnumberTime + cStops + cyDay + 
                  (1|RouteID_JB) + (1|PilotID), data= data)
summary(lme1000)
tab_model(lme1000, show.intercept = F, collapse.ci = T, digits = 3, pred.labels = c("Organic gradient", "Conventional extensive/semi-intensive gradient", "Time band: evening vs midday", "Potential stops", "Day of year", "Time within time band"))
car::vif(lme1000)

# pairwise comparison to farmland
pair.ht <- glht(lme1000, linfct = c("Conventional_extensive_gradient - Organic_gradient = 0"))
summary(pair.ht) # semi-natural covers have higher biomass than farmland, but it is only significant for grassland, urban has significantly lower biomass
confint(pair.ht)
