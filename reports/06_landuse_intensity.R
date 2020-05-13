#run mergeData script

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

plot_grid(g1,g2)

### DE farmland##########################################

g1 <- ggplot(subset(allInsects,Land_use=="Farmland"),
             aes(x=sqrt(hedges_1000),y=(Biomass+1)))+
  geom_point(col=landuseCols[1])+
  scale_y_log10() +
  theme_bw() +
  geom_smooth(method="lm",color="grey70")+
  xlab("Hedges") +ylab("Biomass")

g2 <- ggplot(subset(allInsects,Land_use=="Farmland"),
             aes(x=exAgr_1000,y=(Biomass+1)))+
  geom_point(col=landuseCols[1])+
  scale_y_log10() +
  theme_bw() +
  geom_smooth(method="lm",color="grey70")+
  xlab("Extensive farmland") +ylab("Biomass")

g3 <- ggplot(subset(allInsects,Land_use=="Farmland"),
             aes(x=intAgr_1000,y=(Biomass+1)))+
  geom_point(col=landuseCols[1])+
  scale_y_log10() +
  theme_bw() +
  geom_smooth(method="lm",color="grey70")+
  xlab("Intensive farmland") +ylab("Biomass")

plot_grid(g1,g2,g3,nrow=1)


