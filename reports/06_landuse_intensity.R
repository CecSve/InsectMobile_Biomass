#run 03mergeData script and some 04 to get standardised time etc

library(cowplot)
library(ggplot2)
library(wesanderson)
library(ggpubr)
library(scales)

### DE urban##############################################

g1 <- ggplot(subset(allInsects,Land_use=="Urban"),
       aes(x=sqrt(urbangreen_1000),y=(Biomass+1)))+
  geom_point(col=landuseCols[1])+
  scale_y_log10() +
  theme_bw() +
  geom_smooth(method="lm",color="grey70")+
  xlab("Urban green space cover") +ylab("Biomass")

g2 <- ggplot(subset(allInsects,Land_use=="Urban"),
             aes(x=sqrt(roads_1000),y=(Biomass+1)))+
  geom_point(col=landuseCols[1])+
  scale_y_log10() +
  theme_bw() +
  geom_smooth(method="lm",color="grey70")+
  xlab("Road density") +ylab("Biomass")

g3 <- ggplot(subset(allInsects,Land_use=="Urban"),
  aes(x=sqrt(Urban_1000),y=(Biomass+1)))+
  geom_point(col=landuseCols[1])+
  scale_y_log10() +
  theme_bw() +
  geom_smooth(method="lm",color="grey70")+
  xlab("Urban cover") +ylab("Biomass")

plot_grid(g1,g2,g3,nrow=1)

### DE farmland##########################################

g1 <- ggplot(subset(allInsects,Land_use=="Farmland"),
             aes(x=sqrt(hedges_1000),y=(Biomass+1)))+
  geom_point(col=landuseCols[2])+
  scale_y_log10() +
  theme_bw() +
  geom_smooth(method="lm",color="grey70")+
  xlab("Hedges") +ylab("Biomass")

g2 <- ggplot(subset(allInsects,Land_use=="Farmland"),
             aes(x=exAgr_1000,y=(Biomass+1)))+
  geom_point(col=landuseCols[2])+
  scale_y_log10() +
  theme_bw() +
  geom_smooth(method="lm",color="grey70")+
  xlab("Extensive farmland") +ylab("Biomass")

g3 <- ggplot(subset(allInsects,Land_use=="Farmland"),
             aes(x=intAgr_1000,y=(Biomass+1)))+
  geom_point(col=landuseCols[2])+
  scale_y_log10() +
  theme_bw() +
  geom_smooth(method="lm",color="grey70")+
  xlab("Farmland") +ylab("Biomass")

plot_grid(g1,g2,g3,nrow=1)

### DK urban##############################################
mydata <- allInsects[,c("cStops",names(allInsects)[grepl("_1000",names(allInsects))])]
names(mydata)
mydata <- mydata[,c(11,13:15,17)]
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

library(psych)
#packageurl <- "https://cran.r-project.org/src/contrib/Archive/mnormt/mnormt_1.5-7.tar.gz"
#install.packages(packageurl, repos=NULL, type="source")
pca_rotated <- psych::principal(mydata, rotate="varimax", nfactors=2, scores=TRUE)
biplot(pca_rotated)
print(pca_rotated)

ggsave("plots/pca_with_rotation_1000_DK.png")

#add PCA axes scores to the dataset
allInsects$Urbangreen_gradient <- pca_rotated$scores[,1]
allInsects$Largecity_gradient <- pca_rotated$scores[,2]

lme1000 <- lmer(log(Biomass+1) ~ 
                  Urbangreen_gradient + 
                  Largecity_gradient +
                  Time_band + 
                  Time_band:cnumberTime + cStops + cyDay + 
                  (1|RouteID_JB) + (1|PilotID), data=subset(allInsects, maxLand_use = "Urban_1000"))
summary(lme1000)

library(MuMIn)
r.squaredGLMM(lme1000)

# effect
gls1.alleffects <- allEffects(lme1000)
effectdata <- as.data.frame(gls1.alleffects, row.names=NULL, optional=TRUE)

eall.lm1 <- predictorEffects(lme1000)
plot(eall.lm1, lines=list(multiline=TRUE))
#plot(predictorEffects(lmeurban1000, ~ urbGreenPropArea_1000 + byHegnMeterPerHa_1000 + Bykerne_1000 + Lav.bebyggelse_1000 + Høj.bebyggelse_1000 + Erhverv_1000 + cnumberTime, residuals = T), partial.residuals=list(smooth=TRUE, span=0.50, lty = "dashed"))

### ggplot effect plot ####
temp <- effectdata$Urbangreen_gradient
temp$landuse <- "Urbangreen_gradient"
urbgreen <- temp %>% 
  dplyr::rename(
    propcover = Urbangreen_gradient
  )%>% select(landuse, propcover, fit, se, lower, upper)

# Høj.bebyggelse_1000
temp <- effectdata$Largecity_gradient
temp$landuse <- "Largecity_gradient"
largecity <- temp %>% 
  dplyr::rename(
    propcover = Largecity_gradient
  )%>% select(landuse, propcover, fit, se, lower, upper)

test <- rbind(urbgreen, largecity)

# Visualization
effectplot_urban <- test %>% ggplot(aes(x = propcover, y = fit, fill = "#CC79A7")) +
  geom_line(aes(color = landuse), size = 2) +
  scale_color_manual(
    values = c("#A66287", "#663C53"),
    labels = c(
      "Urban green gradient",
      "Large city gradient"
    )
  ) + theme_minimal_grid() + theme(
    plot.subtitle = element_text(size = 20, face = "bold"),
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    legend.position = "bottom"
  )  + geom_ribbon(
        aes(
          ymin = fit-se,
          ymax = fit+se,
          group = landuse
        ),
        linetype = 2,
        alpha = 0.2,
        show.legend = F
      ) + labs(
        x = "Urban land use gradient",
        y = "log Predicted biomass (mg)",
        subtitle = "A",
        colour = "Land use type"
      ) + scale_fill_manual(values = "#CC79A7")

save_plot("plots/DK_effect_landuse_urban.png", effectplot_urban, base_width = 10, base_height = 6)

#g1 <- ggplot(subset(allInsects, maxLand_use = "Urban_1000"),aes(x=urbGreenPropArea_1000,y=(Biomass+1)))+geom_point(col=landuseCols[1])+scale_y_log10() +theme_bw() +geom_smooth(method="lm",color="grey70")+xlab("Urban green space cover") +ylab("Biomass")

#g2 <- ggplot(subset(allInsects, maxLand_use = "Urban_1000"),aes(x=byHegnMeterPerHa_1000,y=(Biomass+1)))+geom_point(col=landuseCols[1])+scale_y_log10() +theme_bw() +geom_smooth(method="lm",color="grey70")+xlab("Hedges m per ha") +ylab(" ")

g3 <- ggplot(subset(allInsects, maxLand_use = "Urban_1000"),
             aes(x=Bykerne_1000,y=(Biomass+1)))+
  geom_point(col=landuseCols[1])+
  scale_y_log10() +
  theme_bw() +
  geom_smooth(method="lm",color="grey70")+
  xlab("Inner city cover (large cities)") +ylab(" ")

#g4 <- ggplot(subset(allInsects, maxLand_use = "Urban_1000"),aes(x=Lav.bebyggelse_1000,y=(Biomass+1)))+geom_point(col=landuseCols[1])+scale_y_log10() +theme_bw() +geom_smooth(method="lm",color="grey70")+xlab("Residential area cover") +ylab("Biomass")

g5 <- ggplot(subset(allInsects, maxLand_use = "Urban_1000"),
             aes(x=Høj.bebyggelse_1000,y=(Biomass+1)))+
  geom_point(col=landuseCols[1])+
  scale_y_log10() +
  theme_bw() +
  geom_smooth(method="lm",color="grey70")+
  xlab("Multistory buildings cover (large cities)") +ylab(" ")

#g6 <- ggplot(subset(allInsects, maxLand_use = "Urban_1000"),aes(x=Erhverv_1000,y=(Biomass+1)))+geom_point(col=landuseCols[1])+scale_y_log10() +theme_bw() +geom_smooth(method="lm",color="grey70")+xlab("Commercial buildings cover") +ylab(" ")

plot_grid(g3, g5)
ggsave("plots/DK_landuseintensity_urban.png", width = 12, height = 6)

### DK urban lmer #######################################
library(lme4)
library(lmerTest)

#full model
mean(allInsects$Urban_1000)
quantile(allInsects$Urban_1000, 0.1) # 0.02915
lmeurban1000 <- lmer(log(Biomass+1) ~ 
                  urbGreenPropArea_1000 + 
                  byHegnMeterPerHa_1000 +
                  Bykerne_1000 +
                  Lav.bebyggelse_1000 +
                  `Høj.bebyggelse_1000` +
                  Erhverv_1000 +
                  Time_band + 
                  Time_band:cnumberTime + cStops + cyDay + 
                  (1|RouteID_JB) + (1|PilotID), data=subset(allInsects, maxLand_use = "Urban_1000"))

summary(lmeurban1000)

# as reduced as it can be
lmeurban1000 <- lmer(log(Biomass+1) ~ 
                       #sqrt(urbGreenPropArea_1000) + 
                       #(byHegnMeterPerHa_1000) +
                       Bykerne_1000 +
                       #(Lav.bebyggelse_1000) +
                       Høj.bebyggelse_1000 +
                       #sqrt(Erhverv_1000) +
                       Time_band + 
                       Time_band:cnumberTime + cStops + cyDay + 
                       (1|RouteID_JB) + (1|PilotID), data=subset(allInsects, maxLand_use = "Urban_1000"))
summary(lmeurban1000)

library(MuMIn)
r.squaredGLMM(lmeurban1000)

# effect
gls1.alleffects <- allEffects(lmeurban1000)
effectdata <- as.data.frame(gls1.alleffects, row.names=NULL, optional=TRUE)

eall.lm1 <- predictorEffects(lmeurban1000)
plot(eall.lm1, lines=list(multiline=TRUE))
#plot(predictorEffects(lmeurban1000, ~ urbGreenPropArea_1000 + byHegnMeterPerHa_1000 + Bykerne_1000 + Lav.bebyggelse_1000 + Høj.bebyggelse_1000 + Erhverv_1000 + cnumberTime, residuals = T), partial.residuals=list(smooth=TRUE, span=0.50, lty = "dashed"))

### ggplot effect plot ####
temp <- effectdata$Bykerne_1000
temp$landuse <- "Bykerne_1000"
innercity <- temp %>% 
  dplyr::rename(
    propcover = Bykerne_1000
  )%>% select(landuse, propcover, fit, se, lower, upper)

# Høj.bebyggelse_1000
temp <- effectdata$Høj.bebyggelse_1000
temp$landuse <- "Høj.bebyggelse_1000"
multistory <- temp %>% 
  dplyr::rename(
    propcover = Høj.bebyggelse_1000
  )%>% select(landuse, propcover, fit, se, lower, upper)

test <- rbind(innercity, multistory)

# Visualization
effectplot_urban <- test %>% ggplot(aes(x = propcover, y = fit, fill = "#CC79A7")) +
  geom_line(aes(color = landuse), size = 2) +
  scale_color_manual(
    values = c("#A66287", "#663C53"),
    labels = c(
      "Inner city",
      "Multistory buildings"
    )
  ) + theme_minimal_grid() + theme(
    plot.subtitle = element_text(size = 20, face = "bold"),
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    legend.position = "bottom"
  ) + scale_x_continuous(
    limits = c(0, 0.20),
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

#save_plot("plots/DK_estimated_biomass_urbanlanduse.png", finalplot, base_width = 8, base_height = 6)

# correlation plot 
someInsects <- allInsects[,c(12,141, 62, 70, 74:77)]
colnames(someInsects)
colnames(someInsects) <- c("Biomass", "Stops", "Hedge", "Urban green", "Inner city", "Commercial", "Multistory", "Residential")

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
         diag = FALSE, title = "Correlation of urban land use variables", mar=c(0,0,1,0))

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
mydata <- mydata[,c(18:21,24:25)]
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
biplot(pca_rotated)
print(pca_rotated)

ggsave("plots/pca_with_rotation_1000_DK.png")

#add PCA axes scores to the dataset
fit_data$Farmingpractice_gradient <- pca_rotated$scores[,1]
fit_data$Greeness_gradient <- pca_rotated$scores[,2]

lme1000 <- lmer(log(Biomass+1) ~ 
                  Farmingpractice_gradient + 
                  Greeness_gradient +
                  Time_band + 
                  Time_band:cnumberTime + cStops + cyDay + 
                  (1|RouteID_JB) + (1|PilotID), data=subset(fit_data, maxLand_use = "Agriculture_1000"))
summary(lme1000)

library(MuMIn)
r.squaredGLMM(lme1000)


### DK agriculture lmer ######################################
test <- subset(allInsects, maxLand_use = "Agriculture_1000")
test <- test %>% drop_na(Intensiv_organic_1000)

#full model (without weather)
lme1000 <- lmer(log(Biomass+1) ~ hegnMeterPerHa_1000 + Ekstensiv_organic_1000 + Ekstensiv_1000 + Semi.intensiv_1000 + Semi.intensiv_organic_1000 + Intensiv_1000 + Intensiv_organic_1000 + Time_band + Time_band:cnumberTime + cStops + cyDay + (1|RouteID_JB) + (1|PilotID), data= test)

summary(lme1000)

r.squaredGLMM(lme1000)

# effect
gls1.alleffects <- allEffects(lme1000)
effectdata <- as.data.frame(gls1.alleffects, row.names=NULL, optional=TRUE)

eall.lm1 <- predictorEffects(lme1000)
plot(eall.lm1, lines=list(multiline=TRUE))

### ggplot effect plot ####
temp <- effectdata$hegnMeterPerHa_1000
temp$landuse <- "hegnMeterPerHa_1000"
hedge <- temp %>% 
  dplyr::rename(
    propcover = hegnMeterPerHa_1000
  )%>% select(landuse, propcover, fit, se, lower, upper)

# Ekstensiv_organic_1000
temp <- effectdata$Ekstensiv_organic_1000
temp$landuse <- "Ekstensiv_organic_1000"
ex_org <- temp %>% 
  dplyr::rename(
    propcover = Ekstensiv_organic_1000
  )%>% select(landuse, propcover, fit, se, lower, upper)

# Ekstensiv_1000
temp <- effectdata$Ekstensiv_1000
temp$landuse <- "Ekstensiv_1000"
ex <- temp %>% 
  dplyr::rename(
    propcover = Ekstensiv_1000
  )%>% select(landuse, propcover, fit, se, lower, upper)

# Semi.intensiv_1000
temp <- effectdata$Semi.intensiv_1000
temp$landuse <- "Semi.intensiv_1000"
semi <- temp %>% 
  dplyr::rename(
    propcover = Semi.intensiv_1000
  )%>% select(landuse, propcover, fit, se, lower, upper)

# Semi.intensiv_organic_1000
temp <- effectdata$Semi.intensiv_organic_1000
temp$landuse <- "Semi.intensiv_organic_1000"
semi_org <- temp %>% 
  dplyr::rename(
    propcover = Semi.intensiv_organic_1000
  )%>% select(landuse, propcover, fit, se, lower, upper)

# Intensiv_1000
temp <- effectdata$Intensiv_1000
temp$landuse <- "Intensiv_1000"
int <- temp %>% 
  dplyr::rename(
    propcover = Intensiv_1000
  )%>% select(landuse, propcover, fit, se, lower, upper)

# Intensiv_organic_1000
temp <- effectdata$Intensiv_organic_1000
temp$landuse <- "Intensiv_organic_1000"
int_org <- temp %>% 
  dplyr::rename(
    propcover = Intensiv_organic_1000
  )%>% select(landuse, propcover, fit, se, lower, upper)

test <- rbind(ex, ex_org, semi, semi_org, int, int_org)

# Visualization
effectplot_farmland <- test %>% ggplot(aes(x = propcover, y = fit, fill = "#E69F00")) +
  geom_line(aes(color = landuse), size = 2) +
  scale_color_manual(
    values = c("#A67100", "#664600", "#A69A16", "#F2CA27", "#E69D1E", "#E6BF25"),
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
    limits = c(0, 0.40),
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
        x = "Farmland land use cover",
        y = "log Predicted biomass (mg)",
        subtitle = "B",
        colour = "Land use type"
      ) + scale_fill_manual(values = "#E69F00") + guides(colour = guide_legend(nrow = 1))

effectplot <- plot_grid(effectplot_urban, effectplot_farmland)

save_plot("plots/DK_effect_landuse_farmland.png", effectplot_farmland,base_width = 12, base_height = 6)

# correlation plot 
someInsects <- allInsects[,c(12,141,90:94,96:97)]
colnames(someInsects)
colnames(someInsects) <- c("Biomass", "Stops", "Extensive", "Extensive_organic", "Intensive", "Intensive_organic", "Unspecified", "Semi-intensive", "Semi-intensive_organic")

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
         diag = FALSE, title = "Correlation of farmland land use variables", mar=c(0,0,1,0))
