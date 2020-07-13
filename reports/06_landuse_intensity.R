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
table(allInsects$maxLand_use)

g1 <- ggplot(subset(allInsects, maxLand_use = "Urban_1000"),
             aes(x=sqrt(urbGreenPropArea_1000),y=(Biomass+1)))+
  geom_point(col=landuseCols[1])+
  scale_y_log10() +
  theme_bw() +
  geom_smooth(method="lm",color="grey70")+
  xlab("Urban green space cover") +ylab("Biomass")

g2 <- ggplot(subset(allInsects, maxLand_use = "Urban_1000"),
             aes(x=sqrt(byHegnMeterPerHa_1000),y=(Biomass+1)))+
  geom_point(col=landuseCols[1])+
  scale_y_log10() +
  theme_bw() +
  geom_smooth(method="lm",color="grey70")+
  xlab("Hedges m per ha") +ylab(" ")

g3 <- ggplot(subset(allInsects, maxLand_use = "Urban_1000"),
             aes(x=sqrt(Bykerne_1000),y=(Biomass+1)))+
  geom_point(col=landuseCols[1])+
  scale_y_log10() +
  theme_bw() +
  geom_smooth(method="lm",color="grey70")+
  xlab("Inner city cover (large cities)") +ylab(" ")

g4 <- ggplot(subset(allInsects, maxLand_use = "Urban_1000"),
             aes(x=sqrt(Lav.bebyggelse_1000),y=(Biomass+1)))+
  geom_point(col=landuseCols[1])+
  scale_y_log10() +
  theme_bw() +
  geom_smooth(method="lm",color="grey70")+
  xlab("Residential area cover") +ylab("Biomass")

g5 <- ggplot(subset(allInsects, maxLand_use = "Urban_1000"),
             aes(x=sqrt(Høj.bebyggelse_1000),y=(Biomass+1)))+
  geom_point(col=landuseCols[1])+
  scale_y_log10() +
  theme_bw() +
  geom_smooth(method="lm",color="grey70")+
  xlab("Multistory buildings cover (large cities)") +ylab(" ")

g6 <- ggplot(subset(allInsects, maxLand_use = "Urban_1000"),
             aes(x=sqrt(Erhverv_1000),y=(Biomass+1)))+
  geom_point(col=landuseCols[1])+
  scale_y_log10() +
  theme_bw() +
  geom_smooth(method="lm",color="grey70")+
  xlab("Commercial buildings cover") +ylab(" ")

plot_grid(g1,g2, g3, g4, g5, g6)
ggsave("plots/DK_landuseintensity_urban.png", width = 12, height = 6)

### DK urban lmer #######################################
library(lme4)
library(lmerTest)

#full model
mean(allInsects$Urban_1000)
quantile(allInsects$Urban_1000, 0.1) # 0.02915
lmeurban1000 <- lmer(log(Biomass+1) ~ 
                  sqrt(urbGreenPropArea_1000) + 
                  sqrt(byHegnMeterPerHa_1000) +
                  sqrt(Bykerne_1000)+
                  sqrt(Lav.bebyggelse_1000) +
                  sqrt(Høj.bebyggelse_1000) +
                  sqrt(Erhverv_1000) +
                  Time_band + 
                  Time_band:cnumberTime + cStops + cyDay + 
                  (1|RouteID_JB) + (1|PilotID), data=subset(allInsects, maxLand_use = "Urban_1000"))

# as reduced as it can be
lmeurban1000 <- lmer(log(Biomass+1) ~ 
                       #sqrt(urbGreenPropArea_1000) + 
                       sqrt(byHegnMeterPerHa_1000) +
                       #sqrt(Bykerne_1000)+
                       sqrt(Lav.bebyggelse_1000) +
                       sqrt(Høj.bebyggelse_1000) +
                       #sqrt(Erhverv_1000) +
                       Time_band + 
                       Time_band:cnumberTime + cStops + cyDay + 
                       (1|RouteID_JB) + (1|PilotID), data=subset(allInsects, maxLand_use = "Urban_1000"))
summary(lmeurban1000)

library(MuMIn)
r.squaredGLMM(lmeurban1000)

### DK urban spatial model ##############################
library(nlme)

# for DK jitter x and y slightly - fix later
allInsects$x2 <- allInsects$utm_x + rnorm(length(allInsects$utm_x),0,10)
allInsects$y2 <- allInsects$utm_y + rnorm(length(allInsects$utm_y),0,10)

#full model
gls1 <- lme(log(Biomass+1) ~ (urbGreenPropArea_1000) + 
              (byHegnMeterPerHa_1000) +
              (Bykerne_1000)+
              (Lav.bebyggelse_1000) +
              (Høj.bebyggelse_1000) +
              (Erhverv_1000) +
              Time_band + 
              Time_band:cnumberTime + cyDay + Temperature + Wind + cStops,
            random=~1|PilotID/RouteID_JB,
            correlation=corExp(form=~x2+y2|PilotID/RouteID_JB),
            data=subset(allInsects,  maxLand_use = "Urban_1000"))

summary(gls1)

# plot estimated biomass and CIs for urban land use intensity
CIs <- intervals(gls1, which = "fixed")
df <- data.frame(CIs$fixed)
df <- df %>% rownames_to_column(var = "UrbanLandUse")
keep <- c("Høj.bebyggelse_1000", "Lav.bebyggelse_1000", "urbGreenPropArea_1000", "byHegnMeterPerHa_1000", "Bykerne_1000", "Erhverv_1000")
df <- filter(df, UrbanLandUse %in% keep)

p <-
  df %>% mutate(
    UrbanLandUse = fct_relevel(
      UrbanLandUse,
      "Bykerne_1000",
      "Høj.bebyggelse_1000",
      "Erhverv_1000",
      "Lav.bebyggelse_1000",
      "urbGreenPropArea_1000",
      "byHegnMeterPerHa_1000"
    )
  ) %>% ggplot(aes(UrbanLandUse, est.))
finalplot <-
  p + geom_pointrange(aes(ymin = lower, ymax = upper),
                      colour = "#CC79A7",
                      size = 1.5) + theme_minimal_grid() + theme(
                        legend.title = element_blank(),
                        legend.key = element_rect(size = 0.1),
                        legend.key.size = unit(1, 'cm')
                      ) + labs(x = "\nUrban land use", y = "Estimated biomass (mg) and 95% CIs\n", subtitle = "A") + theme(plot.subtitle = element_text(size = 20, face = "bold")) + scale_x_discrete(
                        labels = c(
                          "Høj.bebyggelse_1000" = "Multistory building cover \n(large cities)",
                          "Lav.bebyggelse_1000" = "Residential area cover",
                          "urbGreenPropArea_1000" = "Urban green cover",
                          "byHegnMeterPerHa_1000" = "Hedge per m ha",
                          "Bykerne_1000" = "Inner city cover \n(large cities)",
                          "Erhverv_1000" = "Industrial building cover"
                        )
                      ) + theme(axis.text.x = element_text(size = 12, angle = 90))

save_plot("plots/DK_estimated_biomass_urbanlanduse.png", finalplot, base_width = 8, base_height = 6)

#final model
# removed in order: temperature, Erhverv, urban green area, bykerne, wind 
gls1 <- lme(log(Biomass+1) ~ 
              (Lav.bebyggelse_1000) +
              (Høj.bebyggelse_1000) +
              Time_band + 
              Time_band:cnumberTime + cyDay + cStops,
            random=~1|PilotID/RouteID_JB,
            correlation=corExp(form=~x2+y2|PilotID/RouteID_JB),
            data=subset(allInsects, maxLand_use = "Urban_1000"))

summary(gls1)
r.squaredGLMM(gls1)


#check variance inflation factor
library(car)

lme1000 <- lmer(log(Biomass+1) ~ 
                  sqrt(urbGreenPropArea_1000) + 
                  sqrt(byHegnMeterPerHa_1000) +
                  sqrt(Bykerne_1000)+
                  sqrt(Lav.bebyggelse_1000) +
                  sqrt(Høj.bebyggelse_1000) +
                  sqrt(Erhverv_1000) +
                  Time_band + 
                  Time_band:cnumberTime + cStops + cyDay + 
                  (1|RouteID_JB) + (1|PilotID), data=subset(allInsects, maxLand_use = "Urban_1000"))
#some issue
vif(lme1000)

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

### DK agriculture lmer ######################################

#full model (without weather)
lme1000 <- lmer(log(Biomass+1) ~ 
                       (hegnMeterPerHa_1000) + 
                       (Ekstensiv_organic_1000) +
                       (Ekstensiv_1000)+
                       (Semi.intensiv_1000) +
                  (Semi.intensiv_organic_1000) +
                       Intensiv_1000 +
                  Intensiv_organic_1000 +
                       (Markblok_1000) + 
                  (Markblok_organic_1000) + 
                       Time_band + 
                       Time_band:cnumberTime + cStops + cyDay + 
                       (1|RouteID_JB) + (1|PilotID), data=subset(allInsects, maxLand_use = "Agriculture_1000"))
summary(lme1000)

# as reduced as it can be
lme1000 <- lmer(log(Biomass+1) ~ 
                  (hegnMeterPerHa_1000) + # a structural thing - not a use indicator
                  #(Ekstensiv_organic_1000) +
                  #(Ekstensiv_1000)+
                  #(Semi.intensiv_1000) +
                  #(Semi.intensiv_organic_1000) +
                  #Intensiv_1000 +
                  #Intensiv_organic_1000 +
                  #(Markblok_1000) + 
                  #(Markblok_organic_1000) +  
                  Time_band + 
                  Time_band:cnumberTime + cStops + cyDay + 
                  (1|RouteID_JB) + (1|PilotID), data=subset(allInsects, maxLand_use = "Agriculture_1000"))

summary(lme1000)

r.squaredGLMM(lme1000)

### DK agriculture spatial model ######################################
#full model and final model since stepwise reduction does not lead to significant variables
gls1 <- lme(log(Biomass+1) ~  
              #(hegnMeterPerHa_1000) + # only a structural indicator, not a land use type
              (Ekstensiv_organic_1000) +
              (Ekstensiv_1000)+
              (Semi.intensiv_1000) +
              (Semi.intensiv_organic_1000) +
              Intensiv_1000 +
              Intensiv_organic_1000 +
              (Markblok_1000) + 
              (Markblok_organic_1000) +  
              Time_band + 
              Time_band:cnumberTime + cyDay+ cStops,
            random=~1|PilotID/RouteID_JB,
            correlation=corExp(form=~x2+y2|PilotID/RouteID_JB), na.action = na.omit,
            data=subset(allInsects, maxLand_use = "Agriculture_1000")) # added na omit

summary(gls1)

r.squaredGLMM(gls1)

# plot estimated biomass and CIs for farmlan land use intensity
CIs <- intervals(gls1, which = "fixed")
df <- data.frame(CIs$fixed)
df <- df %>% rownames_to_column(var = "FarmlandLandUse")
keep <- c("Intensiv_1000", "Intensiv_organic_1000", "Semi.intensiv_1000", "Semi.intensiv_organic_1000", "Ekstensiv_1000", "Ekstensiv_organic_1000","Markblok_1000")# , "Markblok_organic_1000"
df <- filter(df, FarmlandLandUse %in% keep)

p <-
  df %>% mutate(
    FarmlandLandUse = fct_relevel(
      FarmlandLandUse,
      "Intensiv_1000",
      "Intensiv_organic_1000",
      "Semi.intensiv_1000",
      "Semi.intensiv_organic_1000",
      "Ekstensiv_1000",
      "Ekstensiv_organic_1000",
      "Markblok_1000",
      #"Markblok_organic_1000"
    )
  ) %>% ggplot(aes(FarmlandLandUse, est.))
finalplot <-
  p + geom_pointrange(aes(ymin = lower, ymax = upper),
                      colour = "#E69F00",
                      size = 1.5) + theme_minimal_grid() + theme(
                        legend.title = element_blank(),
                        legend.key = element_rect(size = 0.1),
                        legend.key.size = unit(1, 'cm')
                      ) + labs(x = "\nFarmland land use", y = "Estimated biomass (mg) and 95% CIs\n", subtitle = "B") + theme(plot.subtitle = element_text(size = 20, face = "bold")) + scale_x_discrete(
                        labels = c(
                          "Ekstensiv_1000" = "Extensive cover",
                          "Ekstensiv_organic_1000" = "Organic extensive cover",
                          "Semi.intensiv_1000" = "Semi-intensive cover",
                          "Semi.intensiv_organic_1000" = "Organic semi-intensive cover",
                          "Intensiv_1000" = "Intensive cover",
                          "Intensiv_organic_1000" = "Organic intensive cover",
                          "Markblok_1000" = "Unspecified field cover"
                          #"Markblok_organic_1000" = "Organic unspecified field cover"
                        )
                      ) + theme(axis.text.x = element_text(size = 12, angle = 90))

save_plot("plots/DK_estimated_biomass_farmlandlanduse.png", finalplot, base_width = 8, base_height = 6)
