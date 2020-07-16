#run mergeData script
#run the 'sort vars' section in script 04

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

####colour################################################################

#decide on common color scheme
#landuseCols <- wes_palette('Darjeeling1', 5, type = c("discrete"))
#landuseCols <- landuseCols[c(1,4,3,5,2)]

landuseCols <- c("#CC79A7", "#E69F00", "#D55E00", "#56B4E9", "#009E73", "darkgrey") # colour friendly, ordered by land cover 

landuseOrder <- c("Urban","Farmland","Grassland","Wetland","Forest")
landuseOrder <- c("Urban","Farmland","Grassland","Wetland","Forest", "Unspecified")


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

### DK avPlots ##################################################
#full model - without taking random effect into account
lm1000 <- lm(log(Biomass+1) ~ 
                    sqrt(Urban_1000) +
               (Agriculture_1000) + 
                    sqrt(Open.uncultivated.land_1000)+
                    sqrt(Wetland_1000) +
                    sqrt(Forest_1000), data=allInsects)
summary(lm1000)

avPlots(lm1000, col.lines = "gray18", layout = c(5, 1), main = "", ylab = "Biomass (mg)")


# ggplot version
library(purrr)
library(broom)
mod_vars = all.vars( formula(lm1000) )[-1]

preddat_fun = function(data, allvars, var) {
  sums = summarise_at(data, 
                      vars( one_of(allvars), -one_of(var) ), 
                      median) 
  cbind( select_at(data, var), sums)
}

head( preddat_fun(allInsects, mod_vars, "cStops") )

pred_dats = mod_vars %>%
  set_names() %>%
  map( ~preddat_fun(allInsects, mod_vars, .x) )
str(pred_dats)

preds = pred_dats %>%
  map(~augment(lm1000, newdata = .x) ) %>%
  map(~mutate(.x, 
              lower = exp(.fitted - 2*.se.fit),
              upper = exp(.fitted + 2*.se.fit),
              pred = exp(.fitted) ) )
str(preds$Agriculture_1000)

#testing with one plot
ggplot(data = preds$Agriculture_1000, aes(x = Agriculture_1000, y = pred)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .25) +
  theme_bw(base_size = 14) +
  labs(x = "Farmland",
       y = "Biomass (mg)") +
  ylim(10, 300) + scale_x_continuous(limits = c(0, 1), labels = function(x) paste0(x*100, "%"))

xlabs = c("Urban cover", 
          "Farmland cover",
          "Grassland cover", 
          "Wetland cover",
          "Forest cover")

pred_plot = function(data, variable, xlab) {
  ggplot(data, aes(x = .data[[variable]], y = pred) ) +
    geom_line(size = 1) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .25) +
    #geom_rug(sides = "b") +
    theme_bw() +
    labs(x = xlab,
         y = "Biomass (mg)") +
    ylim(10, 300) + scale_x_continuous(limits = c(0, 1), labels = function(x) paste0(x*100, "%"))
}

# stat_smooth(method = "lm", se = F, colour = "grey18") + # if linear line is needed - NB does not fit with geom_ribbon

pred_plot(preds[[1]], mod_vars[1], xlabs[1])

all_plots = pmap( list(preds, mod_vars, xlabs), pred_plot)
all_plots

cowplot::plot_grid(plotlist = all_plots,
                   labels = "AUTO",
                   ncol = 1)

plot <- cowplot::plot_grid(plotlist = all_plots,
                   labels = "AUTO",
                   ncol = 1)

save_plot("plots/DK_avPlot.png", plot, base_width = 4, base_height = 10)

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

###land use check###################################################

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
           

###covariation check#########################################

#check whether explanatory variables are strongly correlated
cor(allInsects[,c("cStops","cTL",names(allInsects)[grepl("_1000",names(allInsects))])])
cor(allInsects[,c("cStops","cTL",names(allInsects)[grepl("_500",names(allInsects))])])
cor(allInsects[,c("cStops","cTL",names(allInsects)[grepl("_250",names(allInsects))])])
cor(allInsects[,c("cStops","cTL",names(allInsects)[grepl("_50",names(allInsects))])])
#correlations between stops and urban cover...

###pca analysis###############################################
#taken from the Quick R website

mydata <- allInsects[,c("cStops","cTL",names(allInsects)[grepl("_1000",names(allInsects))])]
names(mydata)
mydata <- mydata[,2:9]

fit <- princomp(mydata, cor=TRUE)
summary(fit) # print variance accounted for
loadings(fit) # pc loadings
plot(fit,type="lines") # scree plot
fit$scores # the principal components
biplot(fit)

#with ggplot
library(ggfortify)
autoplot(fit)
autoplot(fit, data = allInsects, colour = 'Land_use',
         loadings = TRUE, 
         loadings.colour = 'black',
         loadings.label = TRUE, 
         loadings.label.size = 2) + scale_colour_manual(values = landuseCols)

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

fitModels <- function(variable){
  myformula()

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

# spatial models
gls1 <- lme(log(Biomass+1) ~ Agriculture_1000 + 
              Forest_250 +
              Time_band + 
              Time_band:cnumberTime + 
              cTL + 
              cyDay,
            random=~1|PilotID/RouteID,
            correlation=corExp(form=~x2+y2|PilotID/RouteID,nugget=TRUE),
            data=allInsects)
#0.0003768015
#     range     nugget 
#45.8854417  0.148841
AICc(gls1)#451.1499

gls1 <- lme(log(Biomass+1) ~ Agriculture_1000 + 
              Forest_250 +
              Time_band + 
              Time_band:cnumberTime + 
              cTL + 
              cyDay,
            random=~1|PilotID,
            correlation=corExp(form=~x2+y2|PilotID,nugget=TRUE),
            data=allInsects)
#Parameter estimate(s):
#  range     nugget 
#45.8854582  0.1488418
#AIC 448.687

gls1 <- lme(log(Biomass+1) ~ Agriculture_1000 + 
              Forest_250 +
              Time_band + 
              Time_band:cnumberTime + 
              cTL + 
              cyDay,
            random=~1|PilotID/RouteID,
            correlation=corExp(form=~x2+y2|PilotID/RouteID,nugget=FALSE),
            data=allInsects)
#range 
#6.984794
#451.5497

gls1 <- lme(log(Biomass+1) ~ Agriculture_1000 + 
              Forest_250 +
              Time_band + 
              Time_band:cnumberTime + 
              cTL + 
              cyDay,
            random=~1|PilotID,
            correlation=corExp(form=~x2+y2|PilotID,nugget=FALSE),
            data=allInsects)
#range 
#24.58072 
#AICc 450.6248

gls1 <- lme(log(Biomass+1) ~ Agriculture_1000 + 
              Forest_250 +
              Time_band + 
              Time_band:cnumberTime + 
              cTL + 
              cyDay,
            random=~1|PilotID/RouteID,
            data=allInsects)
#AICc(gls1)449.7846

#spatial models
gls1 <- lme(log(Biomass+1) ~ Agriculture_1000 + 
              Forest_250 +
              Time_band + 
              Time_band:cnumberTime + 
              cTL + 
              cyDay,
            random=~1|PilotID/RouteID,
            correlation=corExp(form=~x2+y2|PilotID/RouteID,nugget=TRUE),
            data=allInsects)
#0.0003768015
#     range     nugget 
#45.8854417  0.148841
AICc(gls1)#451.1499

gls1 <- lme(log(Biomass+1) ~ Agriculture_1000 + 
              Forest_250 +
              Time_band + 
              Time_band:cnumberTime + 
              cTL + 
              cyDay,
            random=~1|PilotID,
            correlation=corExp(form=~x2+y2|PilotID,nugget=TRUE),
            data=allInsects)
#Parameter estimate(s):
#  range     nugget 
#45.8854582  0.1488418
#AIC 448.687

gls1 <- lme(log(Biomass+1) ~ Agriculture_1000 + 
              Forest_250 +
              Time_band + 
              Time_band:cnumberTime + 
              cTL + 
              cyDay,
            random=~1|PilotID/RouteID,
            correlation=corExp(form=~x2+y2|PilotID/RouteID,nugget=FALSE),
            data=allInsects)
#range 
#6.984794
#451.5497

gls1 <- lme(log(Biomass+1) ~ Agriculture_1000 + 
              Forest_250 +
              Time_band + 
              Time_band:cnumberTime + 
              cTL + 
              cyDay,
            random=~1|PilotID,
            correlation=corExp(form=~x2+y2|PilotID,nugget=FALSE),
            data=allInsects)
#range 
#24.58072 
#AICc 450.6248

gls1 <- lme(log(Biomass+1) ~ Agriculture_1000 + 
              Forest_250 +
              Time_band + 
              Time_band:cnumberTime + 
              cTL + 
              cyDay,
            random=~1|PilotID/RouteID,
            data=allInsects)
#AICc(gls1)449.7846


###check DE spatial autocorrelation#################################

#DE jitter x and y slightly - fix later
allInsects$x2 <- allInsects$x + rnorm(length(allInsects$x),0,10)
allInsects$y2 <- allInsects$y + rnorm(length(allInsects$y),0,10)

#plot residuals
#full model
lme1000 <- lme4::lmer(log(Biomass+1) ~ 
                  sqrt(Agriculture_1000) + 
                  sqrt(Urban_1000) +
                  sqrt(Open.uncultivated_1000)+
                  sqrt(Wetland_1000) +
                  sqrt(Forest_250) +
                  Time_band + 
                  Time_band:cnumberTime + cTL + cyDay + 
                    (1|PilotID), data=allInsects)

allInsects$resids <- as.numeric(residuals(lme1000))
allInsects$resids_binary <- as.factor(ifelse(allInsects$resids>0,1,-1))
qplot(x,y,data=allInsects,colour=resids)+
  scale_colour_viridis_c()
qplot(x,y,data=allInsects,colour=resids_binary)+
  scale_colour_viridis_d()


#plot autocorrelation
library(ncf)
correlog1 <- correlog(allInsects$x2, allInsects$y2, residuals(lme1000),
                        na.rm=T, increment=1000, resamp=0)
plot(correlog1$correlation, type="b", pch=16, cex=1.5, lwd=1.5,
     xlab="distance", ylab="Moran's I", cex.lab=2, cex.axis=1.5); abline(h=0)


spline.autocorrelation_glm = spline.correlog(allInsects$x2, allInsects$y2, 
                                             residuals(lme1000), 
                                             latlon=F, resamp=100)
plot(spline.autocorrelation_glm)
summary(spline.autocorrelation_glm)

#test it
library(DHARMa)
res = simulateResiduals(lme1000)
testSpatialAutocorrelation(res, x =  allInsects$x2, y = allInsects$y2)

###DK simple#########################################################
# NB! changed cTL to cStops since more data for DK 
str(allInsects)
#agriculture
hist(allInsects$Agriculture_500)
hist(sqrt(allInsects$Agriculture_500)) # does not help
lme50 <- lmer(log(Biomass+1) ~ (Agriculture_50) + Time_band + 
                Time_band:cnumberTime + cStops + cyDay + 
                (1|RouteID_JB) + (1|PilotID), data=allInsects)
lme250 <- lmer(log(Biomass+1) ~ (Agriculture_250) + Time_band + 
                 Time_band:cnumberTime + cStops + cyDay +  
                 (1|RouteID_JB) + (1|PilotID), data=allInsects)
lme500 <- lmer(log(Biomass+1) ~ (Agriculture_500) + Time_band + 
                 Time_band:cnumberTime + cStops + cyDay +  
                 (1|RouteID_JB) + (1|PilotID), data=allInsects)
lme1000 <- lmer(log(Biomass+1) ~ (Agriculture_1000) + Time_band + 
                  Time_band:cnumberTime + cStops + cyDay +  
                  (1|RouteID_JB) + (1|PilotID), data=allInsects)
outAgri <- rbind(getEffect(lme50),getEffect(lme250),getEffect(lme500),getEffect(lme1000))
outAgri <- as.data.frame(outAgri)
outAgri$Buffer <- c(50,250,500,1000)
outAgri$Land_use <- "Farmland"

#urban
hist(allInsects$Urban_1000)#should we log it?
hist(sqrt(allInsects$Urban_1000)) #sqrt is bettwe
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
outOpen.uncultivated$Land_use <- "Grassland"

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

#unspecified
hist(allInsects$Unspecified.land.cover_1000)#log??
hist(log(allInsects$Unspecified.land.cover_1000))
lme50 <- lmer(log(Biomass+1) ~ log(Unspecified.land.cover_50) + Time_band + 
                Time_band:cnumberTime + cStops + cyDay +  
                (1|RouteID_JB) + (1|PilotID), data=allInsects)
lme250 <- lmer(log(Biomass+1) ~ log(Unspecified.land.cover_250) + Time_band + 
                 Time_band:cnumberTime + cStops + cyDay +  
                 (1|RouteID_JB) + (1|PilotID), data=allInsects)
lme500 <- lmer(log(Biomass+1) ~ log(Unspecified.land.cover_500) + Time_band + 
                 Time_band:cnumberTime + cStops + cyDay +  
                 (1|RouteID_JB) + (1|PilotID), data=allInsects)
lme1000 <- lmer(log(Biomass+1) ~ log(Unspecified.land.cover_1000) + Time_band + 
                  Time_band:cnumberTime + cStops + cyDay +  
                  (1|RouteID_JB) + (1|PilotID), data=allInsects)
outUnspecified<- rbind(getEffect(lme50),getEffect(lme250),getEffect(lme500),getEffect(lme1000))
outUnspecified <- as.data.frame(outUnspecified)
outUnspecified$Buffer <- c(50,250,500,1000)
outUnspecified$Land_use <- "Unspecified"

###DK multiple regression########################################

# used cStops instead of cTL for DK data

#full model
lme1000 <- lmer(log(Biomass+1) ~ 
                  (Agriculture_1000) + 
                  sqrt(Urban_1000) +
                  sqrt(Open.uncultivated.land_50)+
                  sqrt(Wetland_50) +
                  sqrt(Forest_250) +
                  #sqrt(Unspecified.land.cover_500) +
                  Time_band + 
                  Land_use:Time_band +
                  Time_band:cnumberTime + cStops + cyDay + 
                  (1|RouteID_JB) + (1|PilotID), data=allInsects)
summary(lme1000)

#final
lme1000 <- lmer(log(Biomass+1) ~ 
                  (Agriculture_1000) + 
                  #sqrt(Urban_1000) +
                  #sqrt(Open.uncultivated.land_50)+
                  sqrt(Wetland_50) +
                  sqrt(Forest_250) +
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
                  sqrt(Forest_250) +
                  sqrt(Open.uncultivated.land_50)+
                  sqrt(Wetland_50) +
                  sqrt(Urban_1000) +
                  Time_band + 
                  Time_band:cnumberTime + cStops + cyDay + 
                  (1|RouteID_JB) + (1|PilotID), data=allInsects)
#some issue
vif(lme1000)

#as spatial model


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
  xlab("Buffer size (m)") + ylab("Effect of land cover on biomass") + labs(subtitle = "A")+ theme(plot.subtitle=element_text(size=18, face="bold", color="black"))

ggsave("plots/DK_Landcover_buffer.png",width=3,height=8)

#DK plus spatial models################################################################

library(nlme)

# for DK jitter x and y slightly - fix later
allInsects$x2 <- allInsects$utm_x + rnorm(length(allInsects$utm_x),0,10)
allInsects$y2 <- allInsects$utm_y + rnorm(length(allInsects$utm_y),0,10)

### spatial autocorrelation ###########
#plot residuals
#full model
unique_coords <- distinct(allInsects, RouteID_JB, .keep_all = TRUE) # make dataframe with only one coordinate per row to test spatial autocorrelation for unique coordinates
unique_coords <- droplevels(unique_coords)

lme1000 <- lme4::lmer(log(Biomass+1) ~ 
                        (Agriculture_1000) + 
                        (Urban_1000) +
                        (Open.uncultivated.land_1000)+
                        (Wetland_1000) +
                        (Forest_250) +
                        Time_band + 
                        Time_band:cnumberTime + cTL + cyDay + 
                        (1|RouteID) + (1|PilotID), data=unique_coords)
summary(lme1000)

unique_coords$resids <- as.numeric(residuals(lme1000))
unique_coords$resids_binary <- as.factor(ifelse(unique_coords$resids>0,1,-1))
qplot(x,y,data=unique_coords,colour=resids)+
  scale_colour_viridis_c()
qplot(x,y,data=allInsects,colour=resids_binary)+
  scale_colour_viridis_d()

# visualise it 
library(ncf)
spline.autocorrelation_glm = spline.correlog(unique_coords$x2, unique_coords$y2, residuals(lme1000), latlon=T, resamp=100)
plot(spline.autocorrelation_glm)
summary(spline.autocorrelation_glm)

autocorrelation_glm = correlog(unique_coords$x2, unique_coords$y2, residuals(lme1000), increment = 1000, latlon=T, resamp=100)
plot(autocorrelation_glm)
autocorrelation_glm$correlation
mantel.test(unique_coords$x2, unique_coords$y2, residuals(lme1000), resamp = 100, latlon = T) #does not work

#test it
library(DHARMa)
simulationOutput <- simulateResiduals(fittedModel = lme1000, plot = T)
#residuals(simulationOutput)
#residuals(simulationOutput, quantileFunction = qnorm, outlierValues = c(-7,7))
testDispersion(lme1000)
testOutliers(simulationOutput)

res = simulateResiduals(lme1000)
testSpatialAutocorrelation(res, x =  unique_coords$x2, y = unique_coords$y2)

# calculating summaries per group since we have several observations per location
#simulationOutput = recalculateResiduals(simulationOutput, group = allInsects$RouteID_JB)

# spatial models (DK) - model selection
gls1 <- lme(log(Biomass+1) ~ Agriculture_1000 + 
              Forest_250 +
              Wetland_50 +
              Time_band + 
              Time_band:cnumberTime + 
              cStops + 
              cyDay,
            random=~1|PilotID/RouteID,
            correlation=corExp(form=~x2+y2|PilotID/RouteID,nugget=TRUE),
            data=allInsects)
summary(gls1)

#range     nugget 
#0.1668    0.1198  
AICc(gls1)#925.4

gls1 <- lme(log(Biomass+1) ~ Agriculture_1000 + 
              Forest_250 +
              Wetland_50 +
              Time_band + 
              Time_band:cnumberTime + 
              cStops + 
              cyDay,
            random=~1|PilotID,
            correlation=corExp(form=~x2+y2|PilotID,nugget=TRUE),
            data=allInsects)

summary(gls1)
#Parameter estimate(s):
#  range     nugget 
#0.1109   0.2492 
#AIC 925.5

gls1 <- lme(log(Biomass+1) ~ Agriculture_1000 + 
              Forest_250 +
              Wetland_50 +
              Time_band + 
              Time_band:cnumberTime + 
              cStops + 
              cyDay,
            random=~1|PilotID/RouteID,
            correlation=corExp(form=~x2+y2|PilotID/RouteID,nugget=FALSE),
            data=allInsects)
summary(gls1)

#range     nugget 
#0.165       
AICc(gls1)#923.2

gls1 <- lme(log(Biomass+1) ~ Agriculture_1000 + 
              Forest_250 +
              Wetland_50 +
              Time_band + 
              Time_band:cnumberTime + 
              cStops + 
              cyDay,
            random=~1|PilotID,
            correlation=corExp(form=~x2+y2|PilotID,nugget=FALSE),
            data=allInsects)
summary(gls1)
#range 
#0.1311   
AICc(gls1) # 923.3

gls1 <- lme(log(Biomass+1) ~ Agriculture_1000 + 
              Forest_250 +
              Wetland_50 +
              Time_band + 
              Time_band:cnumberTime + 
              cStops + 
              cyDay,
            random=~1|PilotID/RouteID,
            data=allInsects)
summary(gls1)
AICc(gls1) # 921

#final model DK - use cStops instead of cTL - sam story as without spatial correlation
gls1 <- lme(log(Biomass+1) ~ (Agriculture_1000) + 
              #sqrt(Urban_1000) +
              #sqrt(Open.uncultivated.land_50)+
              sqrt(Wetland_50) +
              sqrt(Forest_250) + Time_band + 
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

# urban
lme100 <- lmer(log(Biomass+1) ~ 
                  sqrt(Urban_1000) + 
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


#make predictions
Urban1 <- t(as_tibble(exp(predict(lme1000,newdata=newData,re.form=NA))))

predFun <- function(fit) {
  predict(fit,newData,re.form=NA)
}

bb <- bootMer(lme1000,nsim=1000,FUN=predFun,seed=101)
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
                  (Agriculture_1000) + 
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
                  sqrt(Open.uncultivated.land_1000) + 
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
wetland1 <- t(as_tibble(exp(predict(lme1000,newdata=newData,re.form=NA))))

predFun <- function(fit) {
  predict(fit,newData,re.form=NA)
}

bb <- bootMer(lme1000,nsim=1000,FUN=predFun,seed=101)
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
                  sqrt(Forest_1000) + 
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
Forest2 <- t(as_tibble(exp(quantile(bb$t,c(0.025,0.975)))))

Forest <- cbind(Forest1, Forest2)
Forest <- as.data.frame(Forest)
colnames(Forest)
names(Forest)[1] <- "predBiomass"
names(Forest)[2] <- "lowCI"
names(Forest)[3] <- "highCI"
row.names(Forest) <- "Forest"

# Unspecified
lme1000 <- lmer(log(Biomass+1) ~ 
                  sqrt(Unspecified.land.cover_1000) + 
                  Time_band + 
                  Time_band:cnumberTime +
                  log(cStops+1) + cyDay + 
                  (1|RouteID_JB) + (1|PilotID), 
                data=allInsects)
summary(lme1000)
newData = data.frame(Unspecified.land.cover_1000=0.5,
                     cStops=0,
                     cyDay = 0,
                     Time_band = "midday",
                     cnumberTime = 0)


#make predictions
Unspecified1 <- t(as_tibble(exp(predict(lme1000,newdata=newData,re.form=NA))))

predFun <- function(fit) {
  predict(fit,newData,re.form=NA)
}

bb <- bootMer(lme1000,nsim=1000,FUN=predFun,seed=101)
Unspecified2 <- t(as_tibble(exp(quantile(bb$t,c(0.025,0.975)))))

Unspecified <- cbind(Unspecified1, Unspecified2)
Unspecified <- as.data.frame(Unspecified)
colnames(Unspecified)
names(Unspecified)[1] <- "predBiomass"
names(Unspecified)[2] <- "lowCI"
names(Unspecified)[3] <- "highCI"
row.names(Unspecified) <- "Unspecified"

predConfData <- rbind(Urban, Farmland)
predConfData <- rbind(predConfData, Grassland)
predConfData <- rbind(predConfData, Wetland)
predConfData <- rbind(predConfData, Forest)
predConfData <- rbind(predConfData, Unspecified)

predConfData <- rownames_to_column(predConfData, var = "landcover")

# plot

p <- predConfData %>%
  mutate(landcover = fct_relevel(landcover, "Urban", "Farmland", "Grassland", "Wetland", "Forest")) %>% ggplot(aes(landcover, predBiomass, colour = landcover))

finalplot <- p + geom_pointrange(aes(ymin = lowCI, ymax = highCI), size =1.5, show.legend = F) + scale_colour_manual(values = landuseCols) + theme_minimal_grid() + theme(legend.title = element_blank(), legend.key = element_rect(size = 0.1), legend.key.size = unit(1, 'cm')) + labs(x = "\nLand cover", y = "Predicted biomass (mg) and 95% CIs\n", subtitle = "A") + theme(plot.subtitle = element_text(size = 20, face = "bold"))

save_plot("plots/DK_predicted_biomass.png", finalplot, base_width = 8, base_height = 5)

# intensive vs organic farming
# Intensive
lme1000 <- lmer(log(Biomass+1) ~ 
                  sqrt(Intensiv_1000) + 
                  Time_band + 
                  Time_band:cnumberTime +
                  log(cStops+1) + cyDay + 
                  (1|RouteID_JB) + (1|PilotID), 
                data=allInsects)
summary(lme1000)
newData = data.frame(Intensiv_1000=0.5,
                     cStops=0,
                     cyDay = 0,
                     Time_band = "midday",
                     cnumberTime = 0)

#make predictions
Intensiv1 <- t(as_tibble(exp(predict(lme1000,newdata=newData,re.form=NA))))

predFun <- function(fit) {
  predict(fit,newData,re.form=NA)
}

bb <- bootMer(lme1000,nsim=1000,FUN=predFun,seed=101)
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
                  sqrt(propOeko_1000) + 
                  Time_band + 
                  Time_band:cnumberTime +
                  log(cStops+1) + cyDay + 
                  (1|RouteID_JB) + (1|PilotID), 
                data=allInsects)
summary(lme1000)
newData = data.frame(propOeko_1000=0.5,
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
finalplot <- p + geom_pointrange(aes(ymin = lowCI, ymax = highCI), size =1.5) + scale_colour_manual(values = c("#F09018", "#E3B622" )) + theme_minimal_grid() + theme(legend.title = element_blank(), legend.key = element_rect(size = 0.1), legend.key.size = unit(1, 'cm')) + labs(x = "\nFarming system", y = "Predicted biomass (mg) and 95% CIs\n", subtitle = "A") + theme(plot.subtitle = element_text(size = 20, face = "bold"))

save_plot("plots/DK_predicted_biomass_farmtype.png", finalplot, base_width = 8, base_height = 5)

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
