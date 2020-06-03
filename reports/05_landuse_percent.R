#run mergeData script
#run the 'sort vars' section in script 04

library(cowplot)
library(ggplot2)
#library(wesanderson)
library(ggpubr)
library(scales)
library(ggpmisc)

####colour################################################################

#decide on common color scheme
#landuseCols <- wes_palette('Darjeeling1', 5, type = c("discrete"))
#landuseCols <- landuseCols[c(1,4,3,5,2)]

landuseCols <- c("#CC79A7", "#E69F00", "#D55E00", "#56B4E9", "#009E73") # colour friendly, ordered by land cover 

landuseOrder <- c("Urban","Farmland","Open uncultivated","Wetland","Forest")

###transformations for land use#####################################

#sqrt to all variables 

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

###DK plot land cover##############################################

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

qD <- ggplot(allInsects,aes(x=sqrt(Open.uncultivated.land_1000),y=(Biomass+1)))+
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
ggsave("plots/DK_Landcover_percent.png",width=3,height=8)

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

### DK plot buffers#################################################

#urban
u50 <- ggplot(allInsects)+
  geom_point(aes(log(x=Urban_50),y=(Biomass+1)),
             col=landuseCols[1])+
  scale_y_log10() + 
  theme_bw() +
  xlab("Urban cover at 50m") +ylab("Biomass")

u50pubr <- ggscatter(allInsects, x = "Urban_50", y = "Biomass",
                     color = landuseCols[1], shape = 19, size = 2, # Points color, shape and size
                     add = "reg.line",  # Add regressin line
                     add.params = list(color = "Darkgrey", fill = "lightgray"), # Customize reg. line
                     conf.int = TRUE, # Add confidence interval
                     cor.coeff.args = list(method = "spearman")
) + stat_cor(aes(label = paste(..rr.label..,
                               if_else(readr::parse_number(..p.label..) < 0.001, 
                                       "p<0.001", ..p.label..), sep = "~`,   `~")), label.x = 5, label.y = 3) + xlab("Urban cover at 50m") + ylab("Biomass") + scale_y_continuous(trans = log10_trans())

u250 <- ggplot(allInsects)+
  geom_point(aes(x=Urban_250,y=(Biomass+1)),
             col=landuseCols[1])+
  scale_y_log10() +
  theme_bw() +
  xlab("Urban cover at 250m") +ylab("Biomass")

u250pubr <- ggscatter(allInsects, x = "Urban_250", y = "Biomass",
                      color = landuseCols[1], shape = 19, size = 2, # Points color, shape and size
                      add = "reg.line",  # Add regressin line
                      add.params = list(color = "Darkgrey", fill = "lightgray"), # Customize reg. line
                      conf.int = TRUE, # Add confidence interval
                      cor.coeff.args = list(method = "spearman")
) + stat_cor(aes(label = paste(..rr.label..,
                               if_else(readr::parse_number(..p.label..) < 0.001, 
                                       "p<0.001", ..p.label..), sep = "~`,   `~")), label.x = 0.5, label.y = 3) + xlab("Urban cover at 250m") + ylab("Biomass") + scale_y_continuous(trans = log10_trans())

u500 <- ggplot(allInsects)+
  geom_point(aes(x=Urban_500,y=(Biomass+1)),
             col=landuseCols[1])+
  scale_y_log10() +
  theme_bw() +
  xlab("Urban cover at 500m") +ylab("Biomass")

u500pubr <- ggscatter(allInsects, x = "Urban_500", y = "Biomass",
                      color = landuseCols[1], shape = 19, size = 2, # Points color, shape and size
                      add = "reg.line",  # Add regressin line
                      add.params = list(color = "Darkgrey", fill = "lightgray"), # Customize reg. line
                      conf.int = TRUE, # Add confidence interval
                      cor.coeff.args = list(method = "spearman")
) + stat_cor(aes(label = paste(..rr.label..,
                               if_else(readr::parse_number(..p.label..) < 0.001, 
                                       "p<0.001", ..p.label..), sep = "~`,   `~")), label.x = 0.5, label.y = 3) + xlab("Urban cover at 500m") + ylab("Biomass") + scale_y_continuous(trans = log10_trans())

u1000 <- ggplot(allInsects)+
  geom_point(aes(x=Urban_1000,y=(Biomass+1)),
             col=landuseCols[1])+
  scale_y_log10() +
  theme_bw() +
  xlab("Urban cover at 1000m") +ylab("Biomass")

u1000pubr <- ggscatter(allInsects, x = "Urban_1000", y = "Biomass",
                       color = landuseCols[1], shape = 19, size = 2, # Points color, shape and size
                       add = "reg.line",  # Add regressin line
                       add.params = list(color = "Darkgrey", fill = "lightgray"), # Customize reg. line
                       conf.int = TRUE, # Add confidence interval
                       cor.coeff.args = list(method = "spearman")
) + font("xy.text", size = 9) + stat_cor(aes(label = paste(..rr.label..,
                               if_else(readr::parse_number(..p.label..) < 0.001, 
                                       "p<0.001", ..p.label..), sep = "~`,   `~")), label.x = 0.5, label.y = 3, size =3) + xlab("Urban cover at 1000m") + ylab("Biomass") + scale_y_continuous(trans = log10_trans())

plot_grid(u50,u250,u500,u1000,ncol=1)
plot_grid(u50pubr,u250pubr,u500pubr,u1000pubr,ncol=1)

#agriculture
a50 <- ggplot(allInsects)+
  geom_point(aes(x=Agriculture_50,y=(Biomass+1)),
             col=landuseCols[2])+
  scale_y_log10() + 
  theme_bw() +
  xlab("Farmland cover at 50m") +ylab("Biomass")

a50pubr <- ggscatter(allInsects, x = "Agriculture_50", y = "Biomass",
                    color = landuseCols[2], shape = 19, size = 2, # Points color, shape and size
                    add = "reg.line",  # Add regressin line
                    add.params = list(color = "Darkgrey", fill = "lightgray"), # Customize reg. line
                    conf.int = TRUE, # Add confidence interval
                    cor.coeff.args = list(method = "spearman")
) + stat_cor(aes(label = paste(..rr.label..,
                               if_else(readr::parse_number(..p.label..) < 0.001, 
                                       "p<0.001", ..p.label..), sep = "~`,   `~")), label.x = 10, label.y = 1) + xlab("Farmland cover at 50m") + ylab("Biomass") + scale_y_continuous(trans = log10_trans())

a250 <- ggplot(allInsects)+
  geom_point(aes(x=Agriculture_250,y=(Biomass+1)),
             col=landuseCols[2])+
  scale_y_log10() +
  theme_bw() +
  xlab("Farmland cover at 250m") +ylab("Biomass")

a250pubr <- ggscatter(allInsects, x = "Agriculture_250", y = "Biomass",
                     color = landuseCols[2], shape = 19, size = 2, # Points color, shape and size
                     add = "reg.line",  # Add regressin line
                     add.params = list(color = "Darkgrey", fill = "lightgray"), # Customize reg. line
                     conf.int = TRUE, # Add confidence interval
                     cor.coeff.args = list(method = "spearman")
) + stat_cor(aes(label = paste(..rr.label..,
                               if_else(readr::parse_number(..p.label..) < 0.001, 
                                       "p<0.001", ..p.label..), sep = "~`,   `~")), label.x = 5, label.y = 1) + xlab("Farmland cover at 250m") + ylab("Biomass") + scale_y_continuous(trans = log10_trans())

a500 <- ggplot(allInsects)+
  geom_point(aes(x=Agriculture_500,y=(Biomass+1)),
             col=landuseCols[2])+
  scale_y_log10() +
  theme_bw() +
  xlab("Farmland cover at 500m") +ylab("Biomass")

a500pubr <- ggscatter(allInsects, x = "Agriculture_500", y = "Biomass",
                      color = landuseCols[2], shape = 19, size = 2, # Points color, shape and size
                      add = "reg.line",  # Add regressin line
                      add.params = list(color = "Darkgrey", fill = "lightgray"), # Customize reg. line
                      conf.int = TRUE, # Add confidence interval
                      cor.coeff.args = list(method = "spearman")
) + stat_cor(aes(label = paste(..rr.label..,
                               if_else(readr::parse_number(..p.label..) < 0.001, 
                                       "p<0.001", ..p.label..), sep = "~`,   `~")), label.x = 1, label.y = 1) + xlab("Farmland cover at 500m") + ylab("Biomass") + scale_y_continuous(trans = log10_trans())

a1000 <- ggplot(allInsects)+
  geom_point(aes(x=Agriculture_1000,y=(Biomass+1)),
             col=landuseCols[2])+
  scale_y_log10() +
  theme_bw() +
  xlab("Farmland cover at 1000m") +ylab("Biomass")

a1000pubr <- ggscatter(allInsects, x = "Agriculture_1000", y = "Biomass",
                      color = landuseCols[2], shape = 19, size = 2, # Points color, shape and size
                      add = "reg.line",  # Add regressin line
                      add.params = list(color = "Darkgrey", fill = "lightgray"), # Customize reg. line
                      conf.int = TRUE, # Add confidence interval
                      cor.coeff.args = list(method = "spearman")
) + font("xy.text", size = 9) + stat_cor(aes(label = paste(..rr.label..,
                               if_else(readr::parse_number(..p.label..) < 0.001, 
                                       "p<0.001", ..p.label..), sep = "~`,   `~")), label.x = 0.5, label.y = 0.8, size =3) + xlab("Farmland cover at 1000m") + ylab("Biomass") + scale_y_continuous(trans = log10_trans())

plot_grid(a50,a250,a500,a1000,ncol=1)
plot_grid(a50pubr,a250pubr,a500pubr,a1000pubr,ncol=1)

# Grassland
g50 <- ggplot(allInsects)+
  geom_point(aes(x=Open.uncultivated.land_50,y=(Biomass+1)),
             col=landuseCols[3])+
  scale_y_log10() + 
  theme_bw() +
  xlab("Grassland cover at 50m") +ylab("Biomass")

g50pubr <- ggscatter(allInsects, x = "Open.uncultivated.land_50", y = "Biomass",
                     color = landuseCols[3], shape = 19, size = 2, # Points color, shape and size
                     add = "reg.line",  # Add regressin line
                     add.params = list(color = "Darkgrey", fill = "lightgray"), # Customize reg. line
                     conf.int = TRUE, # Add confidence interval
                     cor.coeff.args = list(method = "spearman")
) + stat_cor(aes(label = paste(..rr.label..,
                               if_else(readr::parse_number(..p.label..) < 0.001, 
                                       "p<0.001", ..p.label..), sep = "~`,   `~")), label.x = 0.3, label.y = 1) + xlab("Grassland cover at 50m") + ylab("Biomass") + scale_y_continuous(trans = log10_trans())

g250 <- ggplot(allInsects)+
  geom_point(aes(x=Open.uncultivated.land_250,y=(Biomass+1)),
             col=landuseCols[3])+
  scale_y_log10() +
  theme_bw() +
  xlab("Grassland cover at 250m") +ylab("Biomass")

g250pubr <- ggscatter(allInsects, x = "Open.uncultivated.land_250", y = "Biomass",
                      color = landuseCols[3], shape = 19, size = 2, # Points color, shape and size
                      add = "reg.line",  # Add regressin line
                      add.params = list(color = "Darkgrey", fill = "lightgray"), # Customize reg. line
                      conf.int = TRUE, # Add confidence interval
                      cor.coeff.args = list(method = "spearman")
) + stat_cor(aes(label = paste(..rr.label..,
                               if_else(readr::parse_number(..p.label..) < 0.001, 
                                       "p<0.001", ..p.label..), sep = "~`,   `~")), label.x = 0.3, label.y = 1) + xlab("Grassland cover at 250m") + ylab("Biomass") + scale_y_continuous(trans = log10_trans())

g500 <- ggplot(allInsects)+
  geom_point(aes(x=Open.uncultivated.land_500,y=(Biomass+1)),
             col=landuseCols[3])+
  scale_y_log10() +
  theme_bw() +
  xlab("Grassland cover at 500m") +ylab("Biomass")

g500pubr <- ggscatter(allInsects, x = "Open.uncultivated.land_500", y = "Biomass",
                      color = landuseCols[3], shape = 19, size = 2, # Points color, shape and size
                      add = "reg.line",  # Add regressin line
                      add.params = list(color = "Darkgrey", fill = "lightgray"), # Customize reg. line
                      conf.int = TRUE, # Add confidence interval
                      cor.coeff.args = list(method = "spearman")
) + stat_cor(aes(label = paste(..rr.label..,
                               if_else(readr::parse_number(..p.label..) < 0.001, 
                                       "p<0.001", ..p.label..), sep = "~`,   `~")), label.x = 0.3, label.y = 1) + xlab("Grassland cover at 500m") + ylab("Biomass") + scale_y_continuous(trans = log10_trans())

g1000 <- ggplot(allInsects)+
  geom_point(aes(x=Open.uncultivated.land_1000,y=(Biomass+1)),
             col=landuseCols[3])+
  scale_y_log10() +
  theme_bw() +
  xlab("Grassland cover at 1000m") +ylab("Biomass")

g1000pubr <- ggscatter(allInsects, x = "Open.uncultivated.land_1000", y = "Biomass",
                       color = landuseCols[3], shape = 19, size = 2, # Points color, shape and size
                       add = "reg.line",  # Add regressin line
                       add.params = list(color = "Darkgrey", fill = "lightgray"), # Customize reg. line
                       conf.int = TRUE, # Add confidence interval
                       cor.coeff.args = list(method = "spearman")
) + font("xy.text", size = 9) + stat_cor(aes(label = paste(..rr.label..,
                               if_else(readr::parse_number(..p.label..) < 0.001, 
                                       "p<0.001", ..p.label..), sep = "~`,   `~")), label.x = 0.3, label.y = 0.8, size =3) + xlab("Grassland cover at 1000m") + ylab("Biomass") + scale_y_continuous(trans = log10_trans())

plot_grid(g50,g250,g500,g1000,ncol=1)
plot_grid(g50pubr,g250pubr,g500pubr,g1000pubr,ncol=1)

# wetland
w50 <- ggplot(allInsects)+
  geom_point(aes(x=Wetland_50,y=(Biomass+1)),
             col=landuseCols[4])+
  scale_y_log10() + 
  theme_bw() +
  xlab("Wetland cover at 50m") +ylab("Biomass")

w50pubr <- ggscatter(allInsects, x = "Wetland_50", y = "Biomass",
                     color = landuseCols[4], shape = 19, size = 2, # Points color, shape and size
                     add = "reg.line",  # Add regressin line
                     add.params = list(color = "Darkgrey", fill = "lightgray"), # Customize reg. line
                     conf.int = TRUE, # Add confidence interval
                     cor.coeff.args = list(method = "spearman")
) + stat_cor(aes(label = paste(..rr.label..,
                               if_else(readr::parse_number(..p.label..) < 0.001, 
                                       "p<0.001", ..p.label..), sep = "~`,   `~")), label.x = 0.1, label.y = 1) + xlab("Wetland cover at 50m") + ylab("Biomass") + scale_y_continuous(trans = log10_trans())

w250 <- ggplot(allInsects)+
  geom_point(aes(x=Wetland_250,y=(Biomass+1)),
             col=landuseCols[4])+
  scale_y_log10() +
  theme_bw() +
  xlab("Wetland cover at 250m") +ylab("Biomass")

w250pubr <- ggscatter(allInsects, x = "Wetland_250", y = "Biomass",
                      color = landuseCols[4], shape = 19, size = 2, # Points color, shape and size
                      add = "reg.line",  # Add regressin line
                      add.params = list(color = "Darkgrey", fill = "lightgray"), # Customize reg. line
                      conf.int = TRUE, # Add confidence interval
                      cor.coeff.args = list(method = "spearman")
) + stat_cor(aes(label = paste(..rr.label..,
                               if_else(readr::parse_number(..p.label..) < 0.001, 
                                       "p<0.001", ..p.label..), sep = "~`,   `~")), label.x = 0.15, label.y = 1) + xlab("Wetland cover at 250m") + ylab("Biomass") + scale_y_continuous(trans = log10_trans())

w500 <- ggplot(allInsects)+
  geom_point(aes(x=Wetland_500,y=(Biomass+1)),
             col=landuseCols[4])+
  scale_y_log10() +
  theme_bw() +
  xlab("Wetland cover at 500m") +ylab("Biomass")

w500pubr <- ggscatter(allInsects, x = "Wetland_500", y = "Biomass",
                      color = landuseCols[4], shape = 19, size = 2, # Points color, shape and size
                      add = "reg.line",  # Add regressin line
                      add.params = list(color = "Darkgrey", fill = "lightgray"), # Customize reg. line
                      conf.int = TRUE, # Add confidence interval
                      cor.coeff.args = list(method = "spearman")
) + stat_cor(aes(label = paste(..rr.label..,
                               if_else(readr::parse_number(..p.label..) < 0.001, 
                                       "p<0.001", ..p.label..), sep = "~`,   `~")), label.x = 0.22, label.y = 1) + xlab("Wetland cover at 500m") + ylab("Biomass") + scale_y_continuous(trans = log10_trans())

w1000 <- ggplot(allInsects)+
  geom_point(aes(x=Wetland_1000,y=(Biomass+1)),
             col=landuseCols[4])+
  scale_y_log10() +
  theme_bw() +
  xlab("Wetland cover at 1000m") +ylab("Biomass")

w1000pubr <- ggscatter(allInsects, x = "Wetland_1000", y = "Biomass",
                       color = landuseCols[4], shape = 19, size = 2, # Points color, shape and size
                       add = "reg.line",  # Add regressin line
                       add.params = list(color = "Darkgrey", fill = "lightgray"), # Customize reg. line
                       conf.int = TRUE, # Add confidence interval
                       cor.coeff.args = list(method = "spearman")
)+ font("xy.text", size = 9) + stat_cor(aes(label = paste(..rr.label..,
                               if_else(readr::parse_number(..p.label..) < 0.001, 
                                       "p<0.001", ..p.label..), sep = "~`,   `~")), label.x = 0.25, label.y = 0.8, size =3) + xlab("Wetland cover at 1000m") + ylab("Biomass") + scale_y_continuous(trans = log10_trans())

plot_grid(w50,w250,w500,w1000,ncol=1)
plot_grid(w50pubr,w250pubr,w500pubr,w1000pubr,ncol=1)

#forest
f50 <- ggplot(allInsects)+
  geom_point(aes(x=Forest_50,y=(Biomass+1)),
             col=landuseCols[5])+
  scale_y_log10() + 
  theme_bw() +
  xlab("Forest cover at 50m") +ylab("Biomass")

f50pubr <- ggscatter(allInsects, x = "Forest_50", y = "Biomass",
                     color = landuseCols[5], shape = 19, size = 2, # Points color, shape and size
                     add = "reg.line",  # Add regressin line
                     add.params = list(color = "Darkgrey", fill = "lightgray"), # Customize reg. line
                     conf.int = TRUE, # Add confidence interval
                     cor.coeff.args = list(method = "spearman")
) + stat_cor(aes(label = paste(..rr.label..,
                               if_else(readr::parse_number(..p.label..) < 0.001, 
                                       "p<0.001", ..p.label..), sep = "~`,   `~")), label.x = 10, label.y = 1) + xlab("Forest cover at 50m") + ylab("Biomass") + scale_y_continuous(trans = log10_trans())

f250 <- ggplot(allInsects)+
  geom_point(aes(x=Forest_250,y=(Biomass+1)),
             col=landuseCols[5])+
  scale_y_log10() +
  theme_bw() +
  xlab("Forest cover at 250m") +ylab("Biomass")

f250pubr <- ggscatter(allInsects, x = "Forest_250", y = "Biomass",
                      color = landuseCols[5], shape = 19, size = 2, # Points color, shape and size
                      add = "reg.line",  # Add regressin line
                      add.params = list(color = "Darkgrey", fill = "lightgray"), # Customize reg. line
                      conf.int = TRUE, # Add confidence interval
                      cor.coeff.args = list(method = "spearman")
) + stat_cor(aes(label = paste(..rr.label..,
                               if_else(readr::parse_number(..p.label..) < 0.001, 
                                       "p<0.001", ..p.label..), sep = "~`,   `~")), label.x = 5, label.y = 1) + xlab("Forest cover at 250m") + ylab("Biomass") + scale_y_continuous(trans = log10_trans())

f500 <- ggplot(allInsects)+
  geom_point(aes(x=Forest_500,y=(Biomass+1)),
             col=landuseCols[5])+
  scale_y_log10() +
  theme_bw() +
  xlab("Forest cover at 500m") +ylab("Biomass")

f500pubr <- ggscatter(allInsects, x = "Forest_500", y = "Biomass",
                      color = landuseCols[5], shape = 19, size = 2, # Points color, shape and size
                      add = "reg.line",  # Add regressin line
                      add.params = list(color = "Darkgrey", fill = "lightgray"), # Customize reg. line
                      conf.int = TRUE, # Add confidence interval
                      cor.coeff.args = list(method = "spearman")
) + stat_cor(aes(label = paste(..rr.label..,
                               if_else(readr::parse_number(..p.label..) < 0.001, 
                                       "p<0.001", ..p.label..), sep = "~`,   `~")), label.x = 0.5, label.y = 1) + xlab("Forest cover at 500m") + ylab("Biomass") + scale_y_continuous(trans = log10_trans())

f1000 <- ggplot(allInsects)+
  geom_point(aes(x=Forest_1000,y=(Biomass+1)),
             col=landuseCols[5])+
  scale_y_log10() +
  theme_bw() +
  xlab("Forest cover at 1000m") +ylab("Biomass")

f1000pubr <- ggscatter(allInsects, x = "Forest_1000", y = "Biomass",
                       color = landuseCols[5], shape = 19, size = 2, # Points color, shape and size
                       add = "reg.line",  # Add regressin line
                       add.params = list(color = "Darkgrey", fill = "lightgray"), # Customize reg. line
                       conf.int = TRUE, # Add confidence interval
                       cor.coeff.args = list(method = "spearman")
) + font("xy.text", size = 9) + stat_cor(aes(label = paste(..rr.label..,
                               if_else(readr::parse_number(..p.label..) < 0.001, 
                                       "p<0.001", ..p.label..), sep = "~`,   `~")), label.x = 0.5, label.y = 0.8, size =3) + xlab("Forest cover at 1000m") + ylab("Biomass") + scale_y_continuous(trans = log10_trans())

plot_grid(f50,f250,f500,f1000,ncol=1)
plot_grid(f50pubr,f250pubr,f500pubr,f1000pubr,ncol=1)

plot_grid(u1000pubr,a1000pubr,g1000pubr,w1000pubr,f1000pubr,ncol=1)
ggsave("plots/Landcover_percent_with_stats.png",width=6,height=20)

###covariation check#########################################

#check whether explanatory variables are strongly correlated
cor(allInsects[,c("cStops","cTL",names(allInsects)[grepl("_1000",names(allInsects))])])
cor(allInsects[,c("cStops","cTL",names(allInsects)[grepl("_500",names(allInsects))])])
cor(allInsects[,c("cStops","cTL",names(allInsects)[grepl("_250",names(allInsects))])])
cor(allInsects[,c("cStops","cTL",names(allInsects)[grepl("_50",names(allInsects))])])
#correlations between stops and urban cover...

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

###DK simple#########################################################
# NB! changed cTL to cStops since more data for DK 

#agriculture
hist(allInsects$Agriculture_1000)
lme50 <- lmer(log(Biomass+1) ~ sqrt(Agriculture_50) + Time_band + 
                Time_band:cnumberTime + cStops + cyDay + 
                (1|RouteID_JB) + (1|PilotID), data=allInsects)
lme250 <- lmer(log(Biomass+1) ~ sqrt(Agriculture_250) + Time_band + 
                 Time_band:cnumberTime + cStops + cyDay +  
                 (1|RouteID_JB) + (1|PilotID), data=allInsects)
lme500 <- lmer(log(Biomass+1) ~ sqrt(Agriculture_500) + Time_band + 
                 Time_band:cnumberTime + cStops + cyDay +  
                 (1|RouteID_JB) + (1|PilotID), data=allInsects)
lme1000 <- lmer(log(Biomass+1) ~ sqrt(Agriculture_1000) + Time_band + 
                  Time_band:cnumberTime + cStops + cyDay +  
                  (1|RouteID_JB) + (1|PilotID), data=allInsects)
outAgri <- rbind(getEffect(lme50),getEffect(lme250),getEffect(lme500),getEffect(lme1000))
outAgri <- as.data.frame(outAgri)
outAgri$Buffer <- c(50,250,500,1000)
outAgri$Land_use <- "Farmland"

#urban
hist(allInsects$Urban_1000)#should we log it?
lme50 <- lmer(log(Biomass+1) ~ sqrt(Urban_50) + Time_band + 
                Time_band:cnumberTime + cStops + cyDay + 
                (1|RouteID_JB) + (1|PilotID), data=allInsects)
lme250 <- lmer(log(Biomass+1) ~ sqrt(Urban_250) + Time_band + 
                 Time_band:cnumberTime + cStops + cyDay +  
                 (1|RouteID_JB) + (1|PilotID), data=allInsects)
lme500 <- lmer(log(Biomass+1) ~ sqrt(Urban_500) + Time_band + 
                 Time_band:cnumberTime + cStops + cyDay +  
                 (1|RouteID_JB) + (1|PilotID), data=allInsects)
lme1000 <- lmer(log(Biomass+1) ~ sqrt(Urban_1000) + Time_band + 
                  Time_band:cnumberTime + cStops + cyDay +  
                  (1|RouteID_JB) + (1|PilotID), data=allInsects)
outUrban <- rbind(getEffect(lme50),getEffect(lme250),getEffect(lme500),getEffect(lme1000))
outUrban <- as.data.frame(outUrban)
outUrban$Buffer <- c(50,250,500,1000)
outUrban$Land_use <- "Urban"

#Open.uncultivated
hist(allInsects$Open.uncultivated.land_250)#log??
lme50 <- lmer(log(Biomass+1) ~ sqrt(Open.uncultivated.land_50) + Time_band + 
                Time_band:cnumberTime + cStops + cyDay + 
                (1|RouteID_JB) + (1|PilotID), data=allInsects)
lme250 <- lmer(log(Biomass+1) ~ sqrt(Open.uncultivated.land_250) + Time_band + 
                 Time_band:cnumberTime + cStops + cyDay +  
                 (1|RouteID_JB) + (1|PilotID), data=allInsects)
lme500 <- lmer(log(Biomass+1) ~ sqrt(Open.uncultivated.land_500) + Time_band + 
                 Time_band:cnumberTime + cStops + cyDay +  
                 (1|RouteID_JB) + (1|PilotID), data=allInsects)
lme1000 <- lmer(log(Biomass+1) ~ sqrt(Open.uncultivated.land_1000) + Time_band + 
                  Time_band:cnumberTime + cStops + cyDay +  
                  (1|RouteID_JB) + (1|PilotID), data=allInsects)
outOpen.uncultivated <- rbind(getEffect(lme50),getEffect(lme250),getEffect(lme500),getEffect(lme1000))
outOpen.uncultivated <- as.data.frame(outOpen.uncultivated)
outOpen.uncultivated$Buffer <- c(50,250,500,1000)
outOpen.uncultivated$Land_use <- "Open uncultivated"

#forest
hist(allInsects$Forest_250)#log??
lme50 <- lmer(log(Biomass+1) ~ sqrt(Forest_50) + Time_band + 
                Time_band:cnumberTime + cStops + cyDay + 
                (1|RouteID_JB) + (1|PilotID), data=allInsects)
lme250 <- lmer(log(Biomass+1) ~ sqrt(Forest_250) + Time_band + 
                 Time_band:cnumberTime + cStops + cyDay +  
                 (1|RouteID_JB) + (1|PilotID), data=allInsects)
lme500 <- lmer(log(Biomass+1) ~ sqrt(Forest_500) + Time_band + 
                 Time_band:cnumberTime + cStops + cyDay +  
                 (1|RouteID_JB) + (1|PilotID), data=allInsects)
lme1000 <- lmer(log(Biomass+1) ~ sqrt(Forest_1000) + Time_band + 
                  Time_band:cnumberTime + cStops + cyDay +  
                  (1|RouteID_JB) + (1|PilotID), data=allInsects)
outForest<- rbind(getEffect(lme50),getEffect(lme250),getEffect(lme500),getEffect(lme1000))
outForest <- as.data.frame(outForest)
outForest$Buffer <- c(50,250,500,1000)
outForest$Land_use <- "Forest"

#wetland
hist(allInsects$Wetland_1000)#log??
lme50 <- lmer(log(Biomass+1) ~ sqrt(Wetland_50) + Time_band + 
                Time_band:cnumberTime + cStops + cyDay +  
                (1|RouteID_JB) + (1|PilotID), data=allInsects)
lme250 <- lmer(log(Biomass+1) ~ sqrt(Wetland_250) + Time_band + 
                 Time_band:cnumberTime + cStops + cyDay +  
                 (1|RouteID_JB) + (1|PilotID), data=allInsects)
lme500 <- lmer(log(Biomass+1) ~ sqrt(Wetland_500) + Time_band + 
                 Time_band:cnumberTime + cStops + cyDay +  
                 (1|RouteID_JB) + (1|PilotID), data=allInsects)
lme1000 <- lmer(log(Biomass+1) ~ sqrt(Wetland_1000) + Time_band + 
                  Time_band:cnumberTime + cStops + cyDay +  
                  (1|RouteID_JB) + (1|PilotID), data=allInsects)
outWetland<- rbind(getEffect(lme50),getEffect(lme250),getEffect(lme500),getEffect(lme1000))
outWetland <- as.data.frame(outWetland)
outWetland$Buffer <- c(50,250,500,1000)
outWetland$Land_use <- "Wetland"

###DK multiple regression########################################

# used cStops instead of cTL for DK data

#full model
lme1000 <- lmer(log(Biomass+1) ~ 
                  sqrt(Agriculture_1000) + 
                  sqrt(Urban_1000) +
                  sqrt(Open.uncultivated.land_250)+
                  sqrt(Wetland_50) +
                  sqrt(Forest_500) +
                  Time_band + 
                  Land_use:Time_band +
                  Time_band:cnumberTime + cStops + cyDay + 
                  (1|RouteID_JB) + (1|PilotID), data=allInsects)

#final
lme1000 <- lmer(log(Biomass+1) ~ 
                  sqrt(Agriculture_1000) +
                  sqrt(Wetland_50) + 
                  sqrt(Forest_500) +
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
                  sqrt(Forest_500) +
                  sqrt(Wetland_50) +
                  sqrt(Urban_1000) +
                  Time_band + 
                  Time_band:cnumberTime + cStops + cyDay + 
                  (1|RouteID_JB) + (1|PilotID), data=allInsects)
#some issue
vif(lme1000)

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
  xlab("Buffer size (m)") + ylab("Effect of land cover on biomass")

ggsave("plots/DK_Landcover_buffer.png",width=3,height=8)

#DK plus spatial models################################################################

library(nlme)

# for DK jitter x and y slightly - fix later
allInsects$x2 <- allInsects$utm_x + rnorm(length(allInsects$utm_x),0,10)
allInsects$y2 <- allInsects$utm_y + rnorm(length(allInsects$utm_y),0,10)

#DK
gls1 <- lme(log(Biomass+1) ~  sqrt(Agriculture_1000) + 
              sqrt(Urban_1000) +
              sqrt(Open.uncultivated.land_250)+
              sqrt(Wetland_50) +
              sqrt(Forest_500) + Time_band + 
              Time_band:cnumberTime + cyDay + Temperature + Wind + cStops,
            random=~1|PilotID/RouteID_JB,
            correlation=corExp(form=~x2+y2|PilotID/RouteID_JB),
            data=allInsects,na.action=na.omit)
summary(gls1)

#final model DK - use cStops instead of cTL - sam story as without spatial correlation
gls1 <- lme(log(Biomass+1) ~ sqrt(Agriculture_1000) + 
              sqrt(Wetland_50) +
              sqrt(Forest_500) + Time_band + 
              Time_band:cnumberTime + cyDay + cStops,
            random=~1|PilotID/RouteID_JB,
            correlation=corExp(form=~x2+y2|PilotID/RouteID_JB),
            data=allInsects,na.action=na.omit)
summary(gls1)
#keep in TL even if not significant

r.squaredGLMM(gls1)

summary(gls1) %>% intervals(which = "fixed")

#test timeband interactions
lme1000 <- lmer(log(Biomass+1) ~ 
                  Time_band*sqrt(Agriculture_1000) + 
                  sqrt(Forest_500) +
                  sqrt(Wetland_50) +
                  Time_band + 
                  Time_band:cnumberTime + cStops + cyDay + 
                  (1|RouteID_JB) + (1|PilotID), data=allInsects)
summary(lme1000)

lme1000 <- lmer(log(Biomass+1) ~ 
                  sqrt(Agriculture_1000) + 
                  Time_band*sqrt(Forest_500) +
                  sqrt(Wetland_50) +
                  Time_band:cnumberTime + cStops + cyDay + 
                  (1|RouteID_JB) + (1|PilotID), data=allInsects)
summary(lme1000)

lme1000 <- lmer(log(Biomass+1) ~ 
                  sqrt(Agriculture_1000) + 
                  sqrt(Forest_500) +
                  sqrt(Wetland_50) +
                  Time_band*sqrt(Urban_1000)+
                  Time_band + 
                  Time_band:cnumberTime + cStops + cyDay + 
                  (1|RouteID_JB) + (1|PilotID), data=allInsects)
summary(lme1000)

lme1000 <- lmer(log(Biomass+1) ~ 
                  sqrt(Agriculture_1000) + 
                  sqrt(Forest_500) +
                  Time_band*sqrt(Wetland_50) +
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

lme1000 <- lmer(log(Biomass+1) ~ 
                  sqrt(Wetland_1000) + 
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
exp(predict(lme1000,newdata=newData,re.form=NA))

predFun <- function(fit) {
  predict(fit,newData,re.form=NA)
}

bb <- bootMer(lme1000,nsim=1000,FUN=predFun,seed=101)
exp(quantile(bb$t,c(0.025,0.975)))

###DK stop correlation plot #############################
data1000 <-
  allInsects %>% select(
    Biomass,
    stops,
    Agriculture_1000,
    Forest_1000,
    Wetland_1000,
    Open.uncultivated.land_1000,
    Urban_1000,
    propOeko_1000,
    hegnLength_1000
  )

cor1000 <- cor(data1000)

corrplot.mixed(
  cor1000,
  lower.col = "black",
  upper = "color",
  number.cex = .7,
  tl.col = "black",
  tl.cex = 0.5
)