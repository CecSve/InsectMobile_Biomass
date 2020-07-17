# Supplementary material analysis for InsectMobile biomass paper

# run script 03 before the analysis

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

### Denmark ####################

###DK simple regression plot land cover##############################################

fit = lm(((allInsects$Biomass)+1) ~ sqrt(allInsects$Urban_1000))
coef(summary(fit))[2,4] # significant

qU <- ggplot(allInsects,aes(x=sqrt(Urban_1000),y=(Biomass+1)))+
  geom_point(col=landuseCols[1])+
  scale_y_log10() +
  theme_bw() +
  geom_smooth(method="lm",color="grey18", linetype = "solid")+
  xlab("Urban cover") +ylab("") + labs(subtitle = "A") + scale_x_continuous(limits = c(0, 1), labels = function(x) paste0(x*100, "%"))

fit = lm(((allInsects$Biomass)+1) ~ sqrt(allInsects$Agriculture_1000))
coef(summary(fit))[2,4] # significant

qF <- ggplot(allInsects,aes(x=sqrt(Agriculture_1000),y=(Biomass+1)))+
  geom_point(col=landuseCols[2])+
  scale_y_log10() +
  theme_bw() +
  geom_smooth(method="lm",color="grey18", linetype = "solid")+
  xlab("Farmland cover") +ylab("") + labs(subtitle = "B")+ scale_x_continuous(limits = c(0, 1), labels = function(x) paste0(x*100, "%"))

fit = lm(((allInsects$Biomass)+1) ~ sqrt(allInsects$Open.uncultivated.land_1000))
coef(summary(fit))[2,4] # unsignificant

qD <- ggplot(allInsects,aes(x=sqrt(Open.uncultivated.land_1000),y=(Biomass+1)))+
  geom_point(col=landuseCols[3])+
  scale_y_log10() +
  theme_bw() +
  geom_smooth(method="lm",color="grey18", linetype = "dashed")+
  xlab("Grassland cover") +ylab("")+ labs(subtitle = "C")+ scale_x_continuous(limits = c(0, 1), labels = function(x) paste0(x*100, "%"))

fit = lm(((allInsects$Biomass)+1) ~ sqrt(allInsects$Wetland_1000))
coef(summary(fit))[2,4] # unsignificant

qW <- ggplot(allInsects,aes(x=sqrt(Wetland_1000),y=(Biomass+1)))+
  geom_point(col=landuseCols[4])+
  scale_y_log10() +
  theme_bw() +
  geom_smooth(method="lm",color="grey18", linetype = "dashed")+
  xlab("Wetland cover") +ylab("") + labs(subtitle = "D")+ scale_x_continuous(limits = c(0, 1), labels = function(x) paste0(x*100, "%"))

fit = lm(((allInsects$Biomass)+1) ~ sqrt(allInsects$Forest_1000))
coef(summary(fit))[2,4] # unsignificant

qFo <- ggplot(allInsects,aes(x=sqrt(Forest_1000),y=(Biomass+1)))+
  geom_point(col=landuseCols[5])+
  scale_y_log10() +
  theme_bw() +
  geom_smooth(method="lm",color="grey18", linetype = "dashed")+
  xlab("Forest cover") +ylab("") + labs(subtitle = "E")+ scale_x_continuous(limits = c(0, 1), labels = function(x) paste0(x*100, "%"))

# will not include unspecified land cover in the plot
#qUns <- ggplot(allInsects,aes(x=sqrt(Unspecified.land.cover_1000),y=(Biomass+1)))+geom_point(col="darkgrey")+scale_y_log10() +theme_bw() +geom_smooth(method="lm",color="grey70")+xlab("Unspecified land cover") +ylab("") + labs(subtitle = "F")+ scale_x_continuous(labels = function(x) paste0(x*100, "%"))

y.grob <- textGrob("Biomass (mg)", 
                   gp=gpar(fontface="bold", col="black", fontsize=10), rot=90)

plot <- plot_grid(qU,qF,qD,qW,qFo,ncol=1)
plot <- grid.arrange(arrangeGrob(plot, left = y.grob))
#ggsave("plots/DK_Landcover_percent.png",width=3,height=8)
save_plot("plots/DK_Landcover_percent.png", plot, base_width = 3, base_height = 8)

### DK plot buffers#################################################

#urban
u50 <- ggplot(allInsects)+
  geom_point(aes(x=Urban_50,y=(Biomass+1)),
             col=landuseCols[1])+
  scale_y_log10() + 
  theme_bw() +
  xlab("Urban cover at 50m") +ylab("Biomass (mg)")

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
  xlab("Urban cover at 250m") +ylab("Biomass (mg)")

u250pubr <- ggscatter(allInsects, x = "Urban_250", y = "Biomass",
                      color = landuseCols[1], shape = 19, size = 2, # Points color, shape and size
                      add = "reg.line",  # Add regressin line
                      add.params = list(color = "Darkgrey", fill = "lightgray"), # Customize reg. line
                      conf.int = TRUE, # Add confidence interval
                      cor.coeff.args = list(method = "spearman")
) + stat_cor(aes(label = paste(..rr.label..,
                               if_else(readr::parse_number(..p.label..) < 0.001, 
                                       "p<0.001", ..p.label..), sep = "~`,   `~")), label.x = 0.5, label.y = 3) + xlab("Urban cover at 250m") + ylab("Biomass (mg)") + scale_y_continuous(trans = log10_trans())

u500 <- ggplot(allInsects)+
  geom_point(aes(x=Urban_500,y=(Biomass+1)),
             col=landuseCols[1])+
  scale_y_log10() +
  theme_bw() +
  xlab("Urban cover at 500m") +ylab("Biomass ()mg")

u500pubr <- ggscatter(allInsects, x = "Urban_500", y = "Biomass",
                      color = landuseCols[1], shape = 19, size = 2, # Points color, shape and size
                      add = "reg.line",  # Add regressin line
                      add.params = list(color = "Darkgrey", fill = "lightgray"), # Customize reg. line
                      conf.int = TRUE, # Add confidence interval
                      cor.coeff.args = list(method = "spearman")
) + stat_cor(aes(label = paste(..rr.label..,
                               if_else(readr::parse_number(..p.label..) < 0.001, 
                                       "p<0.001", ..p.label..), sep = "~`,   `~")), label.x = 0.5, label.y = 3) + xlab("Urban cover at 500m") + ylab("Biomass (mg)") + scale_y_continuous(trans = log10_trans())

u1000 <- ggplot(allInsects, aes(x=Urban_1000,y=(Biomass+1)))+
  geom_point(col=landuseCols[1])+
  geom_smooth(method = "lm", formula = y ~ x, colour = "darkgrey", fill = "lightgrey") +
  scale_y_log10() +
  theme_bw() +
  xlab("Urban cover at 1000m") +ylab("Biomass (mg)")

u1000pubr <- ggscatter(allInsects, x = "Urban_1000", y = "Biomass",
                       color = landuseCols[1], shape = 19, size = 2, # Points color, shape and size
                       add = "reg.line",  # Add regressin line
                       add.params = list(color = "Darkgrey", fill = "lightgray"), # Customize reg. line
                       conf.int = TRUE, # Add confidence interval
                       cor.coeff.args = list(method = "spearman")
) + font("xy.text", size = 9) + stat_cor(aes(label = paste(..rr.label..,
                                                           if_else(readr::parse_number(..p.label..) < 0.001, 
                                                                   "p<0.001", ..p.label..), sep = "~`,   `~")), label.x = 0.5, label.y = 3, size =3) + xlab("Urban cover at 1000m") + ylab("Biomass (mg)") + scale_y_continuous(trans = log10_trans()) + xscale(.scale = "percent")

plot_grid(u50,u250,u500,u1000,ncol=1)
plot_grid(u50pubr,u250pubr,u500pubr,u1000pubr,ncol=1)

#agriculture
a50 <- ggplot(allInsects)+
  geom_point(aes(x=Agriculture_50,y=(Biomass+1)),
             col=landuseCols[2])+
  scale_y_log10() + 
  theme_bw() +
  xlab("Farmland cover at 50m") +ylab("Biomass (mg)")

a50pubr <- ggscatter(allInsects, x = "Agriculture_50", y = "Biomass",
                     color = landuseCols[2], shape = 19, size = 2, # Points color, shape and size
                     add = "reg.line",  # Add regressin line
                     add.params = list(color = "Darkgrey", fill = "lightgray"), # Customize reg. line
                     conf.int = TRUE, # Add confidence interval
                     cor.coeff.args = list(method = "spearman")
) + stat_cor(aes(label = paste(..rr.label..,
                               if_else(readr::parse_number(..p.label..) < 0.001, 
                                       "p<0.001", ..p.label..), sep = "~`,   `~")), label.x = 10, label.y = 1) + xlab("Farmland cover at 50m") + ylab("Biomass (mg)") + scale_y_continuous(trans = log10_trans())

a250 <- ggplot(allInsects)+
  geom_point(aes(x=Agriculture_250,y=(Biomass+1)),
             col=landuseCols[2])+
  scale_y_log10() +
  theme_bw() +
  xlab("Farmland cover at 250m") +ylab("Biomass (mg)")

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
  xlab("Farmland cover at 500m") +ylab("Biomass (mg)")

a500pubr <- ggscatter(allInsects, x = "Agriculture_500", y = "Biomass",
                      color = landuseCols[2], shape = 19, size = 2, # Points color, shape and size
                      add = "reg.line",  # Add regressin line
                      add.params = list(color = "Darkgrey", fill = "lightgray"), # Customize reg. line
                      conf.int = TRUE, # Add confidence interval
                      cor.coeff.args = list(method = "spearman")
) + stat_cor(aes(label = paste(..rr.label..,
                               if_else(readr::parse_number(..p.label..) < 0.001, 
                                       "p<0.001", ..p.label..), sep = "~`,   `~")), label.x = 1, label.y = 1) + xlab("Farmland cover at 500m") + ylab("Biomass (mg)") + scale_y_continuous(trans = log10_trans())

a1000 <- ggplot(allInsects, aes(x=Agriculture_1000,y=(Biomass+1)))+
  geom_point(col=landuseCols[2])+
  #geom_smooth(method = "lm", formula = y ~ x, colour = "darkgrey", fill = "lightgrey") +
  scale_y_log10() +
  theme_bw() +
  xlab("Farmland cover at 1000m") +ylab("Biomass (mg)")

a1000pubr <- ggscatter(allInsects, x = "Agriculture_1000", y = "Biomass",
                       color = landuseCols[2], shape = 19, size = 2, # Points color, shape and size
                       add = "reg.line",  # Add regressin line
                       add.params = list(color = "Darkgrey", fill = "lightgray"), # Customize reg. line
                       conf.int = TRUE, # Add confidence interval
                       cor.coeff.args = list(method = "spearman")
) + font("xy.text", size = 9) + stat_cor(aes(label = paste(..rr.label..,
                                                           if_else(readr::parse_number(..p.label..) < 0.001, 
                                                                   "p<0.001", ..p.label..), sep = "~`,   `~")), label.x = 0.5, label.y = 0.8, size =3) + xlab("Farmland cover at 1000m") + ylab("Biomass (mg)") + scale_y_continuous(trans = log10_trans()) + xscale(.scale = "percent")

plot_grid(a50,a250,a500,a1000,ncol=1)
plot_grid(a50pubr,a250pubr,a500pubr,a1000pubr,ncol=1)

# Grassland
g50 <- ggplot(allInsects)+
  geom_point(aes(x=Open.uncultivated.land_50,y=(Biomass+1)),
             col=landuseCols[3])+
  scale_y_log10() + 
  theme_bw() +
  xlab("Grassland cover at 50m") +ylab("Biomass (mg)")

g50pubr <- ggscatter(allInsects, x = "Open.uncultivated.land_50", y = "Biomass",
                     color = landuseCols[3], shape = 19, size = 2, # Points color, shape and size
                     add = "reg.line",  # Add regressin line
                     add.params = list(color = "Darkgrey", fill = "lightgray"), # Customize reg. line
                     conf.int = TRUE, # Add confidence interval
                     cor.coeff.args = list(method = "spearman")
) + stat_cor(aes(label = paste(..rr.label..,
                               if_else(readr::parse_number(..p.label..) < 0.001, 
                                       "p<0.001", ..p.label..), sep = "~`,   `~")), label.x = 0.3, label.y = 1) + xlab("Grassland cover at 50m") + ylab("Biomass (mg)") + scale_y_continuous(trans = log10_trans())

g250 <- ggplot(allInsects)+
  geom_point(aes(x=Open.uncultivated.land_250,y=(Biomass+1)),
             col=landuseCols[3])+
  scale_y_log10() +
  theme_bw() +
  xlab("Grassland cover at 250m") +ylab("Biomass (mg)")

g250pubr <- ggscatter(allInsects, x = "Open.uncultivated.land_250", y = "Biomass",
                      color = landuseCols[3], shape = 19, size = 2, # Points color, shape and size
                      add = "reg.line",  # Add regressin line
                      add.params = list(color = "Darkgrey", fill = "lightgray"), # Customize reg. line
                      conf.int = TRUE, # Add confidence interval
                      cor.coeff.args = list(method = "spearman")
) + stat_cor(aes(label = paste(..rr.label..,
                               if_else(readr::parse_number(..p.label..) < 0.001, 
                                       "p<0.001", ..p.label..), sep = "~`,   `~")), label.x = 0.3, label.y = 1) + xlab("Grassland cover at 250m") + ylab("Biomass (mg)") + scale_y_continuous(trans = log10_trans())

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
                                       "p<0.001", ..p.label..), sep = "~`,   `~")), label.x = 0.3, label.y = 1) + xlab("Grassland cover at 500m") + ylab("Biomass (mg)") + scale_y_continuous(trans = log10_trans())

g1000 <- ggplot(allInsects, aes(x=Open.uncultivated.land_1000,y=(Biomass+1)))+
  geom_point(col=landuseCols[3])+
  #geom_smooth(method = "lm", formula = y ~ x, colour = "darkgrey", fill = "lightgrey") +
  scale_y_log10() +
  theme_bw() +
  xlab("Grassland cover at 1000m") +ylab("Biomass (mg)")

g1000pubr <- ggscatter(allInsects, x = "Open.uncultivated.land_1000", y = "Biomass",
                       color = landuseCols[3], shape = 19, size = 2, # Points color, shape and size
                       add = "reg.line",  # Add regressin line
                       add.params = list(color = "Darkgrey", fill = "lightgray"), # Customize reg. line
                       conf.int = TRUE, # Add confidence interval
                       cor.coeff.args = list(method = "spearman")
) + font("xy.text", size = 9) + stat_cor(aes(label = paste(..rr.label..,
                                                           if_else(readr::parse_number(..p.label..) < 0.001, 
                                                                   "p<0.001", ..p.label..), sep = "~`,   `~")), label.x = 0.3, label.y = 0.8, size =3) + xlab("Grassland cover at 1000m") + ylab("Biomass (mg)") + scale_y_continuous(trans = log10_trans()) + xscale(.scale = "percent")

plot_grid(g50,g250,g500,g1000,ncol=1)
plot_grid(g50pubr,g250pubr,g500pubr,g1000pubr,ncol=1)

# wetland
w50 <- ggplot(allInsects)+
  geom_point(aes(x=Wetland_50,y=(Biomass+1)),
             col=landuseCols[4])+
  scale_y_log10() + 
  theme_bw() +
  xlab("Wetland cover at 50m") +ylab("Biomass (mg)")

w50pubr <- ggscatter(allInsects, x = "Wetland_50", y = "Biomass",
                     color = landuseCols[4], shape = 19, size = 2, # Points color, shape and size
                     add = "reg.line",  # Add regressin line
                     add.params = list(color = "Darkgrey", fill = "lightgray"), # Customize reg. line
                     conf.int = TRUE, # Add confidence interval
                     cor.coeff.args = list(method = "spearman")
) + stat_cor(aes(label = paste(..rr.label..,
                               if_else(readr::parse_number(..p.label..) < 0.001, 
                                       "p<0.001", ..p.label..), sep = "~`,   `~")), label.x = 0.1, label.y = 1) + xlab("Wetland cover at 50m") + ylab("Biomass (mg)") + scale_y_continuous(trans = log10_trans())

w250 <- ggplot(allInsects)+
  geom_point(aes(x=Wetland_250,y=(Biomass+1)),
             col=landuseCols[4])+
  scale_y_log10() +
  theme_bw() +
  xlab("Wetland cover at 250m") +ylab("Biomass (mg)")

w250pubr <- ggscatter(allInsects, x = "Wetland_250", y = "Biomass",
                      color = landuseCols[4], shape = 19, size = 2, # Points color, shape and size
                      add = "reg.line",  # Add regressin line
                      add.params = list(color = "Darkgrey", fill = "lightgray"), # Customize reg. line
                      conf.int = TRUE, # Add confidence interval
                      cor.coeff.args = list(method = "spearman")
) + stat_cor(aes(label = paste(..rr.label..,
                               if_else(readr::parse_number(..p.label..) < 0.001, 
                                       "p<0.001", ..p.label..), sep = "~`,   `~")), label.x = 0.15, label.y = 1) + xlab("Wetland cover at 250m") + ylab("Biomass (mg)") + scale_y_continuous(trans = log10_trans())

w500 <- ggplot(allInsects)+
  geom_point(aes(x=Wetland_500,y=(Biomass+1)),
             col=landuseCols[4])+
  scale_y_log10() +
  theme_bw() +
  xlab("Wetland cover at 500m") +ylab("Biomass (mg)")

w500pubr <- ggscatter(allInsects, x = "Wetland_500", y = "Biomass",
                      color = landuseCols[4], shape = 19, size = 2, # Points color, shape and size
                      add = "reg.line",  # Add regressin line
                      add.params = list(color = "Darkgrey", fill = "lightgray"), # Customize reg. line
                      conf.int = TRUE, # Add confidence interval
                      cor.coeff.args = list(method = "spearman")
) + stat_cor(aes(label = paste(..rr.label..,
                               if_else(readr::parse_number(..p.label..) < 0.001, 
                                       "p<0.001", ..p.label..), sep = "~`,   `~")), label.x = 0.22, label.y = 1) + xlab("Wetland cover at 500m") + ylab("Biomass (mg)") + scale_y_continuous(trans = log10_trans())

w1000 <- ggplot(allInsects, aes(x=Wetland_1000,y=(Biomass+1))) +
  geom_point(col=landuseCols[4])+
  #geom_smooth(method = "lm", formula = y ~ x, colour = "darkgrey", fill = "lightgrey") +
  scale_y_log10() +
  theme_bw() +
  xlab("Wetland cover at 1000m") +ylab("Biomass (mg)")

w1000pubr <- ggscatter(allInsects, x = "Wetland_1000", y = "Biomass",
                       color = landuseCols[4], shape = 19, size = 2, # Points color, shape and size
                       add = "reg.line",  # Add regressin line
                       add.params = list(color = "Darkgrey", fill = "lightgray"), # Customize reg. line
                       conf.int = TRUE, # Add confidence interval
                       cor.coeff.args = list(method = "spearman")
)+ font("xy.text", size = 9) + stat_cor(aes(label = paste(..rr.label..,
                                                          if_else(readr::parse_number(..p.label..) < 0.001, 
                                                                  "p<0.001", ..p.label..), sep = "~`,   `~")), label.x = 0.25, label.y = 0.8, size =3) + xlab("Wetland cover at 1000m") + ylab("Biomass (mg)") + scale_y_continuous(trans = log10_trans()) + xscale(.scale = "percent")

plot_grid(w50,w250,w500,w1000,ncol=1)
plot_grid(w50pubr,w250pubr,w500pubr,w1000pubr,ncol=1)

#forest
f50 <- ggplot(allInsects)+
  geom_point(aes(x=Forest_50,y=(Biomass+1)),
             col=landuseCols[5])+
  scale_y_log10() + 
  theme_bw() +
  xlab("Forest cover at 50m") +ylab("Biomass (mg)")

f50pubr <- ggscatter(allInsects, x = "Forest_50", y = "Biomass",
                     color = landuseCols[5], shape = 19, size = 2, # Points color, shape and size
                     add = "reg.line",  # Add regressin line
                     add.params = list(color = "Darkgrey", fill = "lightgray"), # Customize reg. line
                     conf.int = TRUE, # Add confidence interval
                     cor.coeff.args = list(method = "spearman")
) + stat_cor(aes(label = paste(..rr.label..,
                               if_else(readr::parse_number(..p.label..) < 0.001, 
                                       "p<0.001", ..p.label..), sep = "~`,   `~")), label.x = 10, label.y = 1) + xlab("Forest cover at 50m") + ylab("Biomass (mg)") + scale_y_continuous(trans = log10_trans())

f250 <- ggplot(allInsects)+
  geom_point(aes(x=Forest_250,y=(Biomass+1)),
             col=landuseCols[5])+
  scale_y_log10() +
  theme_bw() +
  xlab("Forest cover at 250m") +ylab("Biomass (mg)")

f250pubr <- ggscatter(allInsects, x = "Forest_250", y = "Biomass",
                      color = landuseCols[5], shape = 19, size = 2, # Points color, shape and size
                      add = "reg.line",  # Add regressin line
                      add.params = list(color = "Darkgrey", fill = "lightgray"), # Customize reg. line
                      conf.int = TRUE, # Add confidence interval
                      cor.coeff.args = list(method = "spearman")
) + stat_cor(aes(label = paste(..rr.label..,
                               if_else(readr::parse_number(..p.label..) < 0.001, 
                                       "p<0.001", ..p.label..), sep = "~`,   `~")), label.x = 5, label.y = 1) + xlab("Forest cover at 250m") + ylab("Biomass (mg)") + scale_y_continuous(trans = log10_trans())

f500 <- ggplot(allInsects)+
  geom_point(aes(x=Forest_500,y=(Biomass+1)),
             col=landuseCols[5])+
  scale_y_log10() +
  theme_bw() +
  xlab("Forest cover at 500m") +ylab("Biomass (mg)")

f500pubr <- ggscatter(allInsects, x = "Forest_500", y = "Biomass",
                      color = landuseCols[5], shape = 19, size = 2, # Points color, shape and size
                      add = "reg.line",  # Add regressin line
                      add.params = list(color = "Darkgrey", fill = "lightgray"), # Customize reg. line
                      conf.int = TRUE, # Add confidence interval
                      cor.coeff.args = list(method = "spearman")
) + stat_cor(aes(label = paste(..rr.label..,
                               if_else(readr::parse_number(..p.label..) < 0.001, 
                                       "p<0.001", ..p.label..), sep = "~`,   `~")), label.x = 0.5, label.y = 1) + xlab("Forest cover at 500m") + ylab("Biomass (mg)") + scale_y_continuous(trans = log10_trans())

f1000 <- ggplot(allInsects, aes(x=Forest_1000,y=(Biomass+1)))+
  geom_point(col=landuseCols[5])+
  #geom_smooth(method = "lm", formula = y ~ x, colour = "darkgrey", fill = "lightgrey") +
  scale_y_log10() +
  theme_bw() +
  xlab("Forest cover at 1000m") +ylab("Biomass (mg)")

f1000pubr <- ggscatter(allInsects, x = "Forest_1000", y = "Biomass",
                       color = landuseCols[5], shape = 19, size = 2, # Points color, shape and size
                       add = "reg.line",  # Add regressin line
                       add.params = list(color = "Darkgrey", fill = "lightgray"), # Customize reg. line
                       conf.int = TRUE, # Add confidence interval
                       cor.coeff.args = list(method = "spearman")
) + font("xy.text", size = 9) + stat_cor(aes(label = paste(..rr.label..,
                                                           if_else(readr::parse_number(..p.label..) < 0.001, 
                                                                   "p<0.001", ..p.label..), sep = "~`,   `~")), label.x = 0.5, label.y = 0.8, size =3) + xlab("Forest cover at 1000m") + ylab("Biomass (mg)") + scale_y_continuous(trans = log10_trans()) + xscale(.scale = "percent")

plot_grid(f50,f250,f500,f1000,ncol=1)
plot_grid(f50pubr,f250pubr,f500pubr,f1000pubr,ncol=1)

plot_grid(u1000,a1000,g1000,w1000,f1000,ncol=1, labels = c("A", "B", "C", "D", "E"))
ggsave("plots/Landcover_percent_with_regline.png",width=5,height=15)
plot_grid(u1000pubr,a1000pubr,g1000pubr,w1000pubr,f1000pubr,ncol=1, labels = c("A", "B", "C", "D", "E"))
ggsave("plots/Landcover_percent_with_stats.png",width=6,height=20)

### intensive vs organic farming predited biomass ###############################
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

