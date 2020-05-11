#run mergeData script

library(cowplot)
library(ggplot2)
library(wesanderson)
library(ggpubr)
library(scales)

####colour################################################################

#decide on common color scheme
landuseCols <- wes_palette('Darjeeling1', 5, type = c("discrete"))

### DE plot land cover##############################################

qU <- ggplot(allInsects)+
              geom_point(aes(x=Urban_1000,y=(Biomass+1)),
                         col=landuseCols[1])+
              scale_y_log10() +
              theme_bw() +
              xlab("Urban cover") +ylab("Biomass")

qF <- ggplot(allInsects)+
  geom_point(aes(x=Agriculture_1000,y=(Biomass+1)),
             col=landuseCols[2])+
  scale_y_log10() +
  theme_bw() +
  xlab("Farmland cover") +ylab("Biomass")

qD <- ggplot(allInsects)+
  geom_point(aes(x=Grassland_1000,y=(Biomass+1)),
             col=landuseCols[3])+
  scale_y_log10() +
  theme_bw() +
  xlab("Dryland cover") +ylab("Biomass")

qW <- ggplot(allInsects)+
  geom_point(aes(x=Wetland_1000,y=(Biomass+1)),
             col=landuseCols[4])+
  scale_y_log10() +
  theme_bw() +
  xlab("Wetland cover") +ylab("Biomass")

qFo <- ggplot(allInsects)+
  geom_point(aes(x=Forest_1000,y=(Biomass+1)),
             col=landuseCols[5])+
  scale_y_log10() +
  theme_bw() +
  xlab("Forest cover") +ylab("Biomass")

plot_grid(qU,qF,qD,qW,qFo)

