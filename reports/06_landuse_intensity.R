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

g1 <- ggplot(subset(allInsects, Urban_1000 > 0.02915),
             aes(x=sqrt(urbGreenPropArea_1000),y=(Biomass+1)))+
  geom_point(col=landuseCols[1])+
  scale_y_log10() +
  theme_bw() +
  geom_smooth(method="lm",color="grey70")+
  xlab("Urban green space cover") +ylab("Biomass")

g2 <- ggplot(subset(allInsects, Urban_1000 > 0.02915),
             aes(x=sqrt(byHegnMeterPerHa_1000),y=(Biomass+1)))+
  geom_point(col=landuseCols[1])+
  scale_y_log10() +
  theme_bw() +
  geom_smooth(method="lm",color="grey70")+
  xlab("Urban hedgerows m/ha") +ylab(" ")

g3 <- ggplot(subset(allInsects, Urban_1000 > 0.02915),
             aes(x=sqrt(Bykerne_1000),y=(Biomass+1)))+
  geom_point(col=landuseCols[1])+
  scale_y_log10() +
  theme_bw() +
  geom_smooth(method="lm",color="grey70")+
  xlab("Inner city cover") +ylab(" ")

g4 <- ggplot(subset(allInsects, Urban_1000 > 0.02915),
             aes(x=sqrt(Lav.bebyggelse_1000),y=(Biomass+1)))+
  geom_point(col=landuseCols[1])+
  scale_y_log10() +
  theme_bw() +
  geom_smooth(method="lm",color="grey70")+
  xlab("Low buildings cover") +ylab("Biomass")

g5 <- ggplot(subset(allInsects, Urban_1000 > 0.02915),
             aes(x=sqrt(Høj.bebyggelse_1000),y=(Biomass+1)))+
  geom_point(col=landuseCols[1])+
  scale_y_log10() +
  theme_bw() +
  geom_smooth(method="lm",color="grey70")+
  xlab("Tall buildings cover") +ylab(" ")

g6 <- ggplot(subset(allInsects, Urban_1000 > 0.02915),
             aes(x=sqrt(Erhverv_1000),y=(Biomass+1)))+
  geom_point(col=landuseCols[1])+
  scale_y_log10() +
  theme_bw() +
  geom_smooth(method="lm",color="grey70")+
  xlab("Commercial buildings cover") +ylab(" ")

plot_grid(g1,g2, g3, g4, g5, g6)

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
                  (1|RouteID_JB) + (1|PilotID), data=subset(allInsects, Urban_1000 > 0.02915))

# as reduced as it can be
lmeurban1000 <- lmer(log(Biomass+1) ~ 
                       sqrt(byHegnMeterPerHa_1000) +
                       sqrt(Lav.bebyggelse_1000) +
                       sqrt(Høj.bebyggelse_1000) +
                       Time_band + 
                       Time_band:cnumberTime + cStops + cyDay + 
                       (1|RouteID_JB) + (1|PilotID), data=subset(allInsects, Urban_1000 > 0.02915))
summary(lmeurban1000)

library(MuMIn)
r.squaredGLMM(lmeurban1000)

### DK urban spatial model ##############################
library(nlme)

# for DK jitter x and y slightly - fix later
allInsects$x2 <- allInsects$utm_x + rnorm(length(allInsects$utm_x),0,10)
allInsects$y2 <- allInsects$utm_y + rnorm(length(allInsects$utm_y),0,10)

#full model
gls1 <- lme(log(Biomass+1) ~ sqrt(urbGreenPropArea_1000) + 
              sqrt(byHegnMeterPerHa_1000) +
              sqrt(Bykerne_1000)+
              sqrt(Lav.bebyggelse_1000) +
              sqrt(Høj.bebyggelse_1000) +
              sqrt(Erhverv_1000) +
              Time_band + 
              Time_band:cnumberTime + cyDay + Temperature + Wind + cStops,
            random=~1|PilotID/RouteID_JB,
            correlation=corExp(form=~x2+y2|PilotID/RouteID_JB),
            data=subset(allInsects, Urban_1000 > 0.02915))

summary(gls1)

#final model
# removed in order: temperature, Erhverv, urban green area, bykerne, wind 
gls1 <- lme(log(Biomass+1) ~ 
              sqrt(byHegnMeterPerHa_1000) +
              sqrt(Lav.bebyggelse_1000) +
              sqrt(Høj.bebyggelse_1000) +
              Time_band + 
              Time_band:cnumberTime + cyDay + cStops,
            random=~1|PilotID/RouteID_JB,
            correlation=corExp(form=~x2+y2|PilotID/RouteID_JB),
            data=subset(allInsects, Urban_1000 > 0.02915))

summary(gls1)
r.squaredGLMM(lmeurban1000)

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
                  (1|RouteID_JB) + (1|PilotID), data=allInsects)
#some issue
vif(lme1000)

### DK farmland##########################################

g1 <- ggplot(subset(allInsects, Agriculture_1000 > 0.1419),
             aes(x=sqrt(hegnMeterPerHa_1000),y=(Biomass+1)))+
  geom_point(col=landuseCols[3])+
  scale_y_log10() +
  theme_bw() +
  geom_smooth(method="lm",color="grey70")+
  xlab("Hedges per m/ha") +ylab("Biomass")

g2 <- ggplot(subset(allInsects, Agriculture_1000 > 0.1419),
             aes(sqrt(x=propOeko_1000),y=(Biomass+1)))+
  geom_point(col=landuseCols[3])+
  scale_y_log10() +
  theme_bw() +
  geom_smooth(method="lm",color="grey70")+
  xlab("Ecological farmland cover") +ylab("Biomass")

g3 <- ggplot(subset(allInsects, Agriculture_1000 > 0.1419),
             aes(sqrt(x=Ekstensiv_1000), y=(Biomass+1)))+
  geom_point(col=landuseCols[3])+
  scale_y_log10() +
  theme_bw() +
  geom_smooth(method="lm",color="grey70")+
  xlab("Extensive farmland cover") +ylab("Biomass")

g4 <- ggplot(subset(allInsects, Agriculture_1000 > 0.1419),
             aes(sqrt(x=Semi.intensiv_1000), y=(Biomass+1)))+
  geom_point(col=landuseCols[3])+
  scale_y_log10() +
  theme_bw() +
  geom_smooth(method="lm",color="grey70")+
  xlab("Semi-intensive farmland cover") +ylab("Biomass")

g5 <- ggplot(subset(allInsects, Agriculture_1000 > 0.1419),
             aes(x=Intensiv_1000, y=(Biomass+1)))+
  geom_point(col=landuseCols[3])+
  scale_y_log10() +
  theme_bw() +
  geom_smooth(method="lm",color="grey70")+
  xlab("Intensive farmland cover") +ylab("Biomass")

g6 <- ggplot(subset(allInsects, Agriculture_1000 > 0.1419),
             aes(sqrt(x=Markblok_1000), y=(Biomass+1)))+
  geom_point(col=landuseCols[3])+
  scale_y_log10() +
  theme_bw() +
  geom_smooth(method="lm",color="grey70")+
  xlab("Field cover") +ylab("Biomass")

plot_grid(g1,g2,g3,g4,g5,g6)

### DK agriculture lmer ######################################

#full model (without weather)
lme1000 <- lmer(log(Biomass+1) ~ 
                       sqrt(hegnMeterPerHa_1000) + 
                       sqrt(propOeko_1000) +
                       sqrt(Ekstensiv_1000)+
                       sqrt(Semi.intensiv_1000) +
                       Intensiv_1000 +
                       sqrt(Markblok_1000) + 
                       Time_band + 
                       Time_band:cnumberTime + cStops + cyDay + 
                       (1|RouteID_JB) + (1|PilotID), data=subset(allInsects, Agriculture_1000 > 0.1419))

# as reduced as it can be
lme1000 <- lmer(log(Biomass+1) ~ 
                  sqrt(hegnMeterPerHa_1000) + 
                  Time_band + 
                  Time_band:cnumberTime + cStops + cyDay + 
                  (1|RouteID_JB) + (1|PilotID), data=subset(allInsects, Agriculture_1000 > 0.1419))


summary(lme1000)

r.squaredGLMM(lme1000)

### DK agriculture spatial model ######################################
#full model
gls1 <- lme(log(Biomass+1) ~  sqrt(hegnMeterPerHa_1000) + 
              sqrt(propOeko_1000) +
              sqrt(Ekstensiv_1000)+
              sqrt(Semi.intensiv_1000) +
              Intensiv_1000 +
              sqrt(Markblok_1000) + 
              Time_band + 
              Time_band:cnumberTime + cyDay + Temperature + Wind + cStops,
            random=~1|PilotID/RouteID_JB,
            correlation=corExp(form=~x2+y2|PilotID/RouteID_JB),
            data=subset(allInsects, Agriculture_1000 > 0.1419))

summary(gls1)

#final model
# removed in order: temperature, Semi.intensiv_1000, wind, Intensiv_1000, Ekstensiv_1000, propOeko_1000, Markblok_1000 
gls1 <- lme(log(Biomass+1) ~  sqrt(hegnMeterPerHa_1000) + 
              Time_band + 
              Time_band:cnumberTime + cyDay + cStops,
            random=~1|PilotID/RouteID_JB,
            correlation=corExp(form=~x2+y2|PilotID/RouteID_JB),
            data=subset(allInsects, Agriculture_1000 > 0.1419))

summary(gls1)
r.squaredGLMM(lmeurban1000)
