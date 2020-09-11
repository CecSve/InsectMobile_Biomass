#run 03mergeData script and some 04 to get standardised time etc

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

#### Set colour scheme ################################################################

landuseCols <- c("#CC79A7", "#E69F00", "#D55E00", "#56B4E9", "#009E73", "darkgrey") # colour friendly, ordered by land cover 

### DK urban##############################################
### correlation plot urban ###################
someInsects <- allInsects[,c(12,22, 48, 62, 70, 74:77)]
colnames(someInsects)
colnames(someInsects) <- c("Biomass", "Stops", "Urban", "Hedge", "Urban green", "Inner city", "Commercial", "Multistory", "Residential")

p <- cor(someInsects, use="pairwise.complete.obs")

# add significance
res1 <- cor.mtest(someInsects, conf.level = .95)
res2 <- cor.mtest(someInsects, conf.level = .99)

# with correlation coefficient instead of p-values, coloured boxes = significant at a 0.05 level
corrplot(p, method = "color", col = landuseCols,
         type = "upper", order = "original", number.cex = .7,
         addCoef.col = "black", # Add coefficient of correlation
         tl.col = "black", tl.srt = 90, # Text label color and rotation
         # Combine with significance
         p.mat = res1$p, sig.level = 0.05, insig = "blank", 
         # hide correlation coefficient on the principal diagonal 
         diag = FALSE, mar=c(0,0,1,0))

### PCA urban ##################
mydata <- allInsects[,c("cStops",names(allInsects)[grepl("_1000",names(allInsects))])]
names(mydata)
mydata <- mydata[,c(6, 11,13:15,17)]
names(mydata) <- gsub("_1000","",names(mydata))
mydata <- plyr::rename(mydata, c("byHegnMeterPerHa" = "Hedge"))
mydata <- plyr::rename(mydata, c("urbGreenPropArea" = "Urban green cover"))
mydata <- plyr::rename(mydata, c("Bykerne" = "Inner city cover"))
mydata <- plyr::rename(mydata, c("Erhverv" = "Commercial cover"))
mydata <- plyr::rename(mydata, c("Lav.bebyggelse" = "Residential cover"))

fit <- princomp(mydata, cor=TRUE)

#with ggplot
autoplot(fit)
dk_autoplot <- autoplot(fit, data = allInsects, 
                        loadings = TRUE, 
                        loadings.colour = 'black',
                        loadings.label = TRUE, 
                        loadings.label.size = 5) + 
  scale_colour_manual(values = landuseCols[1:6])+
  theme_bw() + labs(colour = "Land cover")

save_plot("plots/pca_landuse_urban.png", dk_autoplot, base_height = 8, base_width = 12)

#packageurl <- "https://cran.r-project.org/src/contrib/Archive/mnormt/mnormt_1.5-7.tar.gz"
#install.packages(packageurl, repos=NULL, type="source")
pca_rotated <- psych::principal(mydata, rotate="varimax", nfactors=2, scores=TRUE)
biplot(pca_rotated, main = "")
print(pca_rotated)

ggsave("plots/pca_with_rotation_1000_DK.png")

#add PCA axes scores to the dataset
allInsects$Urbangreen_gradient <- pca_rotated$scores[,1]
allInsects$Largecity_gradient <- pca_rotated$scores[,2]

lme1000 <- lmer(log(Biomass+1) ~ 
                  Urbangreen_gradient + 
                  Urban_1000 +
                  Largecity_gradient +
                  Time_band + 
                  Time_band:cnumberTime + cStops + cyDay + 
                  (1|RouteID_JB) + (1|PilotID), data=subset(allInsects, maxLand_use = "Urban_1000"))
summary(lme1000)

r.squaredGLMM(lme1000)

### IN MS: DK urban proportional land use #######################################
# calculate the proportional cover of the land use intensity variables within the most land cover routes
data <- subset(allInsects, maxLand_use = "Urban_1000")
data$propHedge <- (data$byHegnMeterPerHa_1000/data$Urban_1000)
data$propurbGreen <- (data$urbGreenPropArea_1000/data$Urban_1000)*100
data$propinnerCity <- (data$Bykerne_1000/data$Urban_1000)*100
data$propresidential <- (data$Lav.bebyggelse_1000/data$Urban_1000)*100
data$propmultistory <- (data$`Høj.bebyggelse_1000`/data$Urban_1000)*100
data$propcommercial <- (data$Erhverv_1000/data$Urban_1000)*100
tail(data)

data <- data %>% mutate_if(is.numeric, function(x) ifelse(is.infinite(x), 0, x))

### correlation plot proportional urban ###################
someInsects <- data[,c(12,142, 48, 145:150)]
colnames(someInsects)
colnames(someInsects) <- c("Biomass", "Stops", "Urban", "propHedge", "propUrban green", "propInner city", "propResidential", "propMultistory", "propCommercial")

p <- cor(someInsects, use="pairwise.complete.obs")

# add significance
res1 <- cor.mtest(someInsects, conf.level = .95)
res2 <- cor.mtest(someInsects, conf.level = .99)

# with correlation coefficient instead of p-values, coloured boxes = significant at a 0.05 level
corrplot(p, method = "color", col = landuseCols,
         type = "upper", order = "original", number.cex = .7,
         addCoef.col = "black", # Add coefficient of correlation
         tl.col = "black", tl.srt = 90, # Text label color and rotation
         # Combine with significance
         p.mat = res1$p, sig.level = 0.05, insig = "blank", 
         # hide correlation coefficient on the principal diagonal 
         diag = FALSE, mar=c(0,0,1,0))

# model
lmeurban1000 <- lmer(log(Biomass+1) ~ 
                  #Urban_1000*propHedge + 
                  #Urban_1000*propurbGreen +
                  #Urban_1000*propinnerCity +
                  #Urban_1000*propresidential +
                  #Urban_1000*propmultistory +
                  Urban_1000*propcommercial +
                  Time_band + 
                  Time_band:cnumberTime + cStops + cyDay + 
                  (1|RouteID_JB) + (1|PilotID), data = data)

summary(lmeurban1000)
#r.squaredGLMM(lmeurban1000)

vif(lmeurban1000)

# model for visualisation (without interactions to get the effect)
lmeurban1000 <- lmer(log(Biomass+1) ~ 
                       Urban_1000 +
                       propHedge + 
                       propurbGreen +
                       #Urban_1000*propinnerCity +
                       propresidential +
                       #Urban_1000*propmultistory +
                       propcommercial +
                       Time_band + 
                       Time_band:cnumberTime + cStops + cyDay + 
                       (1|RouteID_JB) + (1|PilotID), data = data)

summary(lmeurban1000)
r.squaredGLMM(lmeurban1000)

# effect
gls1.alleffects <- allEffects(lmeurban1000)
effectdata <- as.data.frame(gls1.alleffects, row.names=NULL, optional=TRUE)

eall.lm1 <- predictorEffects(lmeurban1000)
#effectdata <- as.data.frame(eall.lm1, row.names=NULL, optional=TRUE)
plot(eall.lm1, lines=list(multiline=TRUE))
#plot(predictorEffects(lmeurban1000, ~ urbGreenPropArea_1000 + byHegnMeterPerHa_1000 + Bykerne_1000 + Lav.bebyggelse_1000 + Høj.bebyggelse_1000 + Erhverv_1000 + cnumberTime, residuals = T), partial.residuals=list(smooth=TRUE, span=0.50, lty = "dashed"))

### ggplot effect plot ####
# urbGreenPropArea_1000
temp <- effectdata$Urban_1000
temp$landuse <- "Urban"
Urban <- temp %>% 
  dplyr::rename(
    propcover = Urban_1000
  )%>% dplyr::select(landuse, propcover, fit, se, lower, upper)

# propHedge
temp <- effectdata$propHedge
temp$landuse <- "Hedge"
hedge <- temp %>% 
  dplyr::rename(
    propcover = propHedge
  )%>% dplyr::select(landuse, propcover, fit, se, lower, upper)
hedge$propcover <- hedge$propcover /10

# propresidential
temp <- effectdata$propresidential
temp$landuse <- "Residential"
residential <- temp %>% 
  dplyr::rename(
    propcover = propresidential
  )%>% dplyr::select(landuse, propcover, fit, se, lower, upper)

# propcommercial
temp <- effectdata$propcommercial
temp$landuse <- "Commercial"
commercial <- temp %>% 
  dplyr::rename(
    propcover = propcommercial
  )%>% dplyr::select(landuse, propcover, fit, se, lower, upper)

# urban
temp <- effectdata$propurbGreen
temp$landuse <- "Urban green"
urbgreen <- temp %>% 
  dplyr::rename(
    propcover = propurbGreen
  )%>% dplyr::select(landuse, propcover, fit, se, lower, upper)

test <- rbind(commercial, residential, hedge, urbgreen)
str(test)

# Visualization
effectplot_urban <- test %>% dplyr::mutate(
  landuse = fct_relevel(
    landuse,
    "Commercial",
    "Residential",
    "Hedge",
    "Urban green"
  )
)%>% ggplot(aes(x = propcover, y = fit, fill = landuse)) +
  geom_line(aes(color = landuse), size = 2) +
  scale_color_manual(
    values = c("#FA0081", "#FB9BD6", "#7A275A", "#C74093")
  ) + theme_minimal_grid() + theme(
    plot.subtitle = element_text(size = 20, face = "bold"),
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    legend.position = "bottom"
  ) + scale_x_continuous(
    limits = c(0, 100),
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
        x = "Land use intensity cover within urban land cover",
        y = "log Predicted biomass (mg)",
        subtitle = "A",
        colour = "Land use type"
      ) + scale_fill_manual(values =  c("#FA0081", "#FB9BD6", "#7A275A", "#C74093")) + guides(colour = guide_legend(nrow = 1))

save_plot("plots/DK_estimated_biomass_urbanlanduse.png", effectplot_urban, base_width = 8, base_height = 6)

### urban analysis with combined columns ###################
str(mydata)

# we need more variables
mydata <- allInsects[,c("Biomass", "cStops", "cyDay", "Time_band", "cnumberTime", "RouteID_JB", "PilotID", "maxLand_use", "maxareaProp", names(allInsects)[grepl("_1000",names(allInsects))])]
names(mydata)
mydata <- mydata[,c(1:9, 14,19:25)]
names(mydata) <- gsub("_1000","",names(mydata))

# merge the categories into groups based on varimax rotated PCA
# first the large city - we don't use multistory buildings since they are correlated with hedges
test_mydata <- mydata %>% mutate(largecity = Erhverv + Bykerne)

# urban green - without hedge since it is highly correlated with multistory buildings
test_mydata <- test_mydata %>% mutate(urbangreen = urbGreenPropArea + byHegnMeterPerHa + Lav.bebyggelse)
str(test_mydata)

test2_mydata <- test_mydata %>% mutate(urbangreen = ifelse(urbangreen > 0, urbangreen / 1000, urbangreen)) # divide by 1000 to get meter proportional cover
#test2_mydata <- as.data.table(test_mydata)[maxLand_use == "Urban_1000", urbangreen := urbangreen / 100000000000] # propcover of urban is skewed by one decimal so we will divide by 10000 square km
str(test2_mydata)

#full model
lmeurban1000 <- lmer(log(Biomass+1) ~ 
                       Urban +     
                       largecity +
                       urbangreen +
                       Time_band + 
                       Time_band:cnumberTime + cyDay + cStops +
                       (1|RouteID_JB) + (1|PilotID), data=subset(test2_mydata, maxLand_use = "Urban_1000")) # without stops

summary(lmeurban1000)
r.squaredGLMM(lmeurban1000)

# effect
gls1.alleffects <- allEffects(lmeurban1000)
effectdata <- as.data.frame(gls1.alleffects, row.names=NULL, optional=TRUE)

eall.lm1 <- predictorEffects(lmeurban1000)
effectdata_test <- as.data.frame(eall.lm1, row.names=NULL, optional=TRUE)
plot(eall.lm1, lines=list(multiline=TRUE))
#plot(predictorEffects(lmeurban1000, ~ urbGreenPropArea_1000 + byHegnMeterPerHa_1000 + Bykerne_1000 + Lav.bebyggelse_1000 + Høj.bebyggelse_1000 + Erhverv_1000 + cnumberTime, residuals = T), partial.residuals=list(smooth=TRUE, span=0.50, lty = "dashed"))

### ggplot effect plot ####
temp <- effectdata$Urban
temp$landuse <- "Urban"
urban <- temp %>% 
  dplyr::rename(
    propcover = Urban
  )%>% select(landuse, propcover, fit, se, lower, upper)

temp <- effectdata$largecity
temp$landuse <- "largecity"
largecity <- temp %>% 
  dplyr::rename(
    propcover = largecity
  )%>% select(landuse, propcover, fit, se, lower, upper)

temp <- effectdata$urbangreen
temp$landuse <- "urbangreen"
green <- temp %>% 
  dplyr::rename(
    propcover = urbangreen
  )%>% select(landuse, propcover, fit, se, lower, upper)

test <- rbind(urban, largecity, green)

# Visualization
effectplot_urban <- test %>% mutate(
  landuse = fct_relevel(
    landuse,
    "Urban",
    "largecity",
    "green"
  )
) %>%  ggplot(aes(x = propcover, y = fit, fill = "#CC79A7")) +
  geom_line(aes(color = landuse), size = 2) +
  scale_color_manual(
    values = c("#A66287", "#663C53", "#DB2563"),
    labels = c(
      "Urban cover",
      "Large cities",
      "Urban green cover"
    )
  ) + theme_minimal_grid() + theme(
    plot.subtitle = element_text(size = 20, face = "bold"),
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    legend.position = "bottom"
  )  + scale_x_continuous(
    limits = c(0, 1),
    labels = function(x)
      paste0(x * 100, "%")) + geom_ribbon(
        aes(
          ymin = fit-se,
          ymax = fit+se,
          group = landuse
        ),
        linetype = 2,
        alpha = 0.2,
        show.legend = F
      ) + labs(
        x = "Urban land use cover",
        y = "log Predicted biomass (mg)",
        subtitle = "A",
        colour = "Land use type"
      ) + scale_fill_manual(values = "#CC79A7")

save_plot("plots/DK_estimated_biomass_urbanlanduse.png", effectplot_urban, base_width = 8, base_height = 6)

### DK farmland##########################################

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

plot_grid(g1,g2,g3,g4,g5,g6)

### PCA ###
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

save_plot("plots/pca_landuse_farmland.png", dk_autoplot, base_height = 8, base_width = 12)

library(psych)
#packageurl <- "https://cran.r-project.org/src/contrib/Archive/mnormt/mnormt_1.5-7.tar.gz"
#install.packages(packageurl, repos=NULL, type="source")
pca_rotated <- psych::principal(mydata, rotate="varimax", nfactors=2, scores=TRUE)
biplot(pca_rotated, main = "")
print(pca_rotated)

ggsave("plots/pca_with_rotation_farmland_1000_DK.png")

#add PCA axes scores to the dataset
fit_data$FarmlandIntensive_gradient <- pca_rotated$scores[,1]
fit_data$OrganicExtensive_gradient <- pca_rotated$scores[,2]

lme1000 <- lmer(log(Biomass+1) ~ 
                  FarmlandIntensive_gradient + 
                  OrganicExtensive_gradient +
                  Time_band + 
                  Time_band:cnumberTime + cStops + cyDay + 
                  (1|RouteID_JB) + (1|PilotID), data=subset(fit_data, maxLand_use = "Agriculture_1000"))
summary(lme1000)

library(MuMIn)
r.squaredGLMM(lme1000)


### IN MS: DK agriculture proportional cover ######################################
# calculate the proportional cover of the land use intensity variables within the most land cover routes
data <- subset(allInsects, maxLand_use = "Agriculture_1000")
data$propOrgExtensive <- (data$Ekstensiv_organic_1000/data$Agriculture_1000)*100
data$propExtensive <- (data$Ekstensiv_1000/data$Agriculture_1000)*100
data$propSemiIntensive <- (data$Semi.intensiv_1000/data$Agriculture_1000)*100
data$propOrgSemiIntensive <- (data$Semi.intensiv_organic_1000/data$Agriculture_1000)*100
data$propIntensive <- (data$`Intensiv_1000`/data$Agriculture_1000)*100
data$propOrgIntensive <- (data$Intensiv_organic_1000/data$Agriculture_1000)*100
tail(data)

data <- data %>% mutate_if(is.numeric, function(x) ifelse(is.infinite(x), 0, x))

### correlation plot proportional agriculture ###################
names(data)
someInsects <- data[,c(12,142, 44, 145:150)]
colnames(someInsects)
colnames(someInsects) <- c("Biomass", "Stops", "Farmland", "propOrgExtensive", "propExtensive", "propSemiIntensive", "propOrgSemiIntensive", "propIntensive", "propOrgIntensive")

p <- cor(someInsects, use="pairwise.complete.obs")

# add significance
res1 <- cor.mtest(someInsects, conf.level = .95)
res2 <- cor.mtest(someInsects, conf.level = .99)

# with correlation coefficient instead of p-values, coloured boxes = significant at a 0.05 level
corrplot(p, method = "color", col = landuseCols,
         type = "upper", order = "original", number.cex = .7,
         addCoef.col = "black", # Add coefficient of correlation
         tl.col = "black", tl.srt = 90, # Text label color and rotation
         # Combine with significance
         p.mat = res1$p, sig.level = 0.05, insig = "blank", 
         # hide correlation coefficient on the principal diagonal 
         diag = FALSE, mar=c(0,0,1,0))

# model
lmer1000 <- lmer(log(Biomass+1) ~ 
                       #Agriculture_1000*propOrgExtensive + 
                   #Agriculture_1000*propExtensive +
                       #Agriculture_1000*propSemiIntensive +
                   #Agriculture_1000*propOrgSemiIntensive +
                   #Agriculture_1000*propIntensive +
                   Agriculture_1000*propOrgIntensive +
                       Time_band + 
                       Time_band:cnumberTime + cStops + cyDay + 
                       (1|RouteID_JB) + (1|PilotID), data = data)

summary(lmer1000)
#r.squaredGLMM(lmeurban1000)

vif(lmer1000)

# model for visualisation (without interactions to get the effect)
lmer1000 <- lmer(log(Biomass+1) ~ 
                   Agriculture_1000 +
                   propOrgExtensive + 
                   propExtensive +
                   propSemiIntensive +
                   propOrgSemiIntensive +
                   propIntensive +
                   propOrgIntensive +
                   Time_band + 
                   Time_band:cnumberTime + cStops + cyDay + 
                   (1|RouteID_JB) + (1|PilotID), data = data)

summary(lmer1000)
r.squaredGLMM(lmer1000)

# effect
gls1.alleffects <- allEffects(lmer1000)
effectdata <- as.data.frame(gls1.alleffects, row.names=NULL, optional=TRUE)

eall.lm1 <- predictorEffects(lmer1000)
#effectdata <- as.data.frame(eall.lm1, row.names=NULL, optional=TRUE)
plot(eall.lm1, lines=list(multiline=TRUE))
#plot(predictorEffects(lmeurban1000, ~ urbGreenPropArea_1000 + byHegnMeterPerHa_1000 + Bykerne_1000 + Lav.bebyggelse_1000 + Høj.bebyggelse_1000 + Erhverv_1000 + cnumberTime, residuals = T), partial.residuals=list(smooth=TRUE, span=0.50, lty = "dashed"))

### ggplot effect plot ####
temp <- effectdata$Agriculture_1000
temp$landuse <- "Agriculture_1000"
Agriculture_1000 <- temp %>% 
  dplyr::rename(
    propcover = Agriculture_1000
  )%>% dplyr::select(landuse, propcover, fit, se, lower, upper)

# Ekstensiv_organic_1000
temp <- effectdata$propOrgExtensive
temp$landuse <- "propOrgExtensive"
ex_org <- temp %>% 
  dplyr::rename(
    propcover = propOrgExtensive
  )%>% dplyr::select(landuse, propcover, fit, se, lower, upper)

# Ekstensiv_1000
temp <- effectdata$propExtensive
temp$landuse <- "propExtensive"
ex <- temp %>% 
  dplyr::rename(
    propcover = propExtensive
  )%>% dplyr::select(landuse, propcover, fit, se, lower, upper)

# Semi.intensiv_1000
temp <- effectdata$propSemiIntensive
temp$landuse <- "propSemiIntensive"
semi <- temp %>% 
  dplyr::rename(
    propcover = propSemiIntensive
  )%>% dplyr::select(landuse, propcover, fit, se, lower, upper)

# Semi.intensiv_organic_1000
temp <- effectdata$propOrgSemiIntensive
temp$landuse <- "propOrgSemiIntensive"
semi_org <- temp %>% 
  dplyr::rename(
    propcover = propOrgSemiIntensive
  )%>% dplyr::select(landuse, propcover, fit, se, lower, upper)

# Intensiv_1000
temp <- effectdata$propIntensive
temp$landuse <- "propIntensive"
int <- temp %>% 
  dplyr::rename(
    propcover = propIntensive
  )%>% dplyr::select(landuse, propcover, fit, se, lower, upper)

# Intensiv_organic_1000
temp <- effectdata$propOrgIntensive
temp$landuse <- "propOrgIntensive"
int_org <- temp %>% 
  dplyr::rename(
    propcover = propOrgIntensive
  )%>% dplyr::select(landuse, propcover, fit, se, lower, upper)

test <- rbind(ex, ex_org, semi, semi_org, int, int_org)

# Visualization
effectplot_farmland <- test %>% mutate(
  landuse = fct_relevel(
    landuse,
    "propExtensive",
    "propOrgExtensive",
    "propSemiIntensive",
    "propOrgSemiIntensive",
    "propIntensive",
    "propOrgIntensive"
  )
)%>% ggplot(aes(x = propcover, y = fit, fill = landuse)) +
  geom_line(aes(color = landuse), size = 2) +
  scale_color_manual(
    values = c("#9BA8BD", "#AABB97", "#6E81D1", "#89C254", "#315DC7", "#1D3D05"),
    labels = c(
      "Extensive",
      "Organic extensive",
      "Semi-intensive",
      "Organic semi-intensive",
      "Intensive",
      "Organic intensive"
    )
  ) + theme_minimal_grid() + theme(
    plot.subtitle = element_text(size = 20, face = "bold"),
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    legend.position = "bottom"
  ) + scale_x_continuous(
    #limits = c(0, 0.40),
    labels = function(x)
      paste0(x * 100, "%")) + geom_ribbon(
        aes(
          ymin = fit-se,
          ymax = fit+se,
          group = landuse
        ),
        linetype = 2,
        alpha = 0.2,
        show.legend = F
      ) + labs(
        x = "Land use intensity cover within farmland cover",
        y = "log Predicted biomass (mg)",
        subtitle = "B",
        colour = "Land use type"
      ) + scale_fill_manual(values =  c("#9BA8BD", "#AABB97", "#6E81D1", "#89C254", "#315DC7", "#1D3D05")) + guides(colour = guide_legend(nrow = 1))

effectplot_farmland

effectplot_farmland_zoom <- test %>% dplyr::mutate(
  landuse = fct_relevel(
    landuse,
    "propExtensive",
    "propOrgExtensive",
    "propSemiIntensive",
    "propOrgSemiIntensive",
    "propIntensive",
    "propOrgIntensive"
  )
)%>% ggplot(aes(x = propcover, y = fit, fill = landuse)) +
  geom_line(aes(color = landuse), size = 2, show.legend = F) +
  scale_color_manual(
    values = c("#9BA8BD", "#AABB97", "#6E81D1", "#89C254", "#315DC7", "#1D3D05"),
    labels = c(
      "Extensive",
      "Organic extensive",
      "Semi-intensive",
      "Organic semi-intensive",
      "Intensive",
      "Organic intensive"
    )
  ) + theme_minimal_grid() + theme(
    plot.subtitle = element_text(size = 20, face = "bold"),
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    legend.position = "bottom"
  ) + scale_y_continuous(limits = c(4.4, 5.25)) + scale_x_continuous(
    limits = c(0, 0.10),
    labels = function(x)
      paste0(x * 100, "%")) + geom_ribbon(
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
        y = "",
        #subtitle = "B",
        colour = "Land use type"
      ) + scale_fill_manual(values =  c("#9BA8BD", "#AABB97", "#6E81D1", "#89C254", "#315DC7", "#1D3D05")) + guides(colour = guide_legend(nrow = 1)) + theme(panel.background = element_rect(fill = "white"), plot.margin = margin(0, 0, 0, 0, "cm"), panel.border = element_rect(colour = "darkgrey"))

#effectplot <- plot_grid(effectplot_urban, effectplot_farmland)
effectplot_farmland
effectplot_farmland_zoom

effectplot_farmland + annotation_custom(ggplotGrob(effectplot_farmland_zoom), xmin = -0.02, xmax = 0.4, ymin = 6, ymax = 9)

ggsave("plots/zoom_farmland_landuse.png")

# correlation plot 
mydata <- allInsects[,c("Biomass", "cStops",names(allInsects)[grepl("_1000",names(allInsects))])]
names(mydata)
someInsects <- mydata[,c(1:3, 19:22,25:26)]
colnames(someInsects)
colnames(someInsects) <- c("Biomass", "Stops", "Farmland", "Extensive", "Ekstensive organic", "Intensive", "Intensive organic", "Semi-intensive", "Semi-intensive organic")

p <- cor(someInsects, use="pairwise.complete.obs")

# add significance
res1 <- cor.mtest(someInsects, conf.level = .95)
res2 <- cor.mtest(someInsects, conf.level = .99)

# with correlation coefficient instead of p-values, coloured boxes = significant at a 0.05 level
corrplot(p, method = "color", col = landuseCols,
         type = "upper", order = "original", number.cex = .7,
         addCoef.col = "black", # Add coefficient of correlation
         tl.col = "black", tl.srt = 90, # Text label color and rotation
         # Combine with significance
         p.mat = res1$p, sig.level = 0.05, insig = "blank", 
         # hide correlation coefficient on the principal diagonal 
         diag = FALSE, title = "", mar=c(0,0,1,0))

### farmland analysis ###################################
str(mydata)

# we need more variables
mydata <- allInsects[,c("Biomass", "cStops", "cyDay", "Time_band", "cnumberTime", "RouteID_JB", "PilotID", "maxLand_use", "maxareaProp", names(allInsects)[grepl("_1000",names(allInsects))])]
names(mydata)
mydata <- mydata[,c(1:9, 10, 26:29, 32:33)]
names(mydata) <- gsub("_1000","",names(mydata))

# merge the categories into groups based on varimax rotated PCA
# first the large city - we don't use multistory buildings since they are correlated with hedges
test_mydata <- mydata %>% mutate(FarmlandIntensive_gradient = Intensiv)

# urban green - without hedge since it is highly correlated with multistory buildings
test_mydata <- test_mydata %>% mutate(OrganicExtensive_gradient = Ekstensiv + Ekstensiv_organic + Intensiv_organic + Semi.intensiv + Semi.intensiv_organic)
str(test_mydata)
test_mydata <- test_mydata %>% drop_na(FarmlandIntensive_gradient)

#test2_mydata <- as.data.table(test_mydata)[maxLand_use == "Urban_1000", urbangreen := urbangreen / 100000000000] # propcover of urban is skewed by one decimal so we will divide by 10000 square km

lme1000 <- lmer(log(Biomass+1) ~ 
                  Agriculture +
                  FarmlandIntensive_gradient + 
                  OrganicExtensive_gradient +
                  Time_band + 
                  Time_band:cnumberTime + cStops + cyDay + 
                  (1|RouteID_JB) + (1|PilotID), data=subset(test_mydata, maxLand_use = "Agriculture_1000"))
summary(lme1000)

library(MuMIn)
r.squaredGLMM(lme1000)

# effect
gls1.alleffects <- allEffects(lme1000)
effectdata <- as.data.frame(gls1.alleffects, row.names=NULL, optional=TRUE)

eall.lm1 <- predictorEffects(lme1000)
effectdata_test <- as.data.frame(eall.lm1, row.names=NULL, optional=TRUE)
plot(eall.lm1, lines=list(multiline=TRUE))

### ggplot effect plot ####
temp <- effectdata$Agriculture
temp$landuse <- "Agriculture"
Agriculture <- temp %>% 
  dplyr::rename(
    propcover = Agriculture
  )%>% dplyr::select(landuse, propcover, fit, se, lower, upper)

temp <- effectdata$FarmlandIntensive_gradient
temp$landuse <- "FarmlandIntensive_gradient"
FarmlandIntensive_gradient <- temp %>% 
  dplyr::rename(
    propcover = FarmlandIntensive_gradient
  )%>% dplyr::select(landuse, propcover, fit, se, lower, upper)

temp <- effectdata$OrganicExtensive_gradient
temp$landuse <- "OrganicExtensive_gradient"
OrganicExtensive_gradient <- temp %>% 
  dplyr::rename(
    propcover = OrganicExtensive_gradient
  )%>% dplyr::select(landuse, propcover, fit, se, lower, upper)

test <- rbind(Agriculture, FarmlandIntensive_gradient, OrganicExtensive_gradient)

# Visualization
effectplot_farmland <- test %>% mutate(
  landuse = fct_relevel(
    landuse,
    "Agriculture",
    "FarmlandIntensive_gradient",
    "OrganicExtensive_gradient",
  )
)%>% ggplot(aes(x = propcover, y = fit, fill = landuse)) +
  geom_line(aes(color = landuse), size = 2) +
  scale_color_manual(
    values = c("#9BA8BD", "#6E81D1", "#89C254"),
    labels = c(
      "Farmland cover",
      "Intensive cover",
      "Organic/semi-intensive/extensive cover"
    )
  ) + theme_minimal_grid() + theme(
    plot.subtitle = element_text(size = 20, face = "bold"),
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    legend.position = "bottom"
  ) + scale_x_continuous(
    limits = c(0, 1),
    labels = function(x)
      paste0(x * 100, "%")) + geom_ribbon(
        aes(
          ymin = fit-se,
          ymax = fit+se,
          group = landuse
        ),
        linetype = 2,
        alpha = 0.2,
        show.legend = F
      ) + labs(
        x = "Agricultural land use cover",
        y = "log Predicted biomass (mg)",
        subtitle = "B",
        colour = "Land use type"
      ) + scale_fill_manual(values =  c("#9BA8BD","#6E81D1", "#89C254")) + guides(colour = guide_legend(nrow = 1))

save_plot("plots/DK_effect_landuse_farmland.png", effectplot_farmland, base_width = 10, base_height = 6)
