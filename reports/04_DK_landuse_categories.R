### Landuse intensity analysis for Danish data. First part incorporates parts of the 03_mergeData, 04_landuse_categories script  

library(lubridate)
library(ggplot2)
library(plyr)
library(ggpirate)
library(cowplot)
library(wesanderson)

# load data
# from report 03
insectsDK <-
  read.delim("cleaned-data/DK_rough_landuse_biomass.txt", sep = "\t")
str(insectsDK)

#format Date
insectsDK$Date <- as.Date(insectsDK$Date, "%d-%m-%Y")
insectsDK$yDay <- yday(insectsDK$Date)

#order time band
insectsDK$Time_band <- factor(insectsDK$Time_band,levels=c("midday","evening"))
#order habitat
insectsDK$Land_use <- factor(insectsDK$Land_use,levels=c("Urban","Farmland",
                                                           "Dryland","Wetland","Forest"))

####################################################################

#decide on common color scheme
landuseCols <- wes_palette('Darjeeling1', 5, type = c("discrete"))

###Fig 2############################################################

# from report 04, added/changed some of the explanatory variables in the models compared to the merged data

#total biomass
ggplot(insectsDK,aes(x=Land_use, y=log(Biomass+1)))+
  geom_pirate(aes(colour=Time_band,fill=Time_band),show.legend=TRUE)+
  theme_bw()+
  #facet_wrap(~Temperature)+
  xlab("Land cover")+ylab("Total insect biomass")

ggplot(insectsDK,aes(x=Land_use, y=log(Biomass+1)))+
  geom_boxplot(aes(colour=Time_band,fill=Time_band),show.legend=TRUE)+
  theme_bw()+
  #facet_wrap(~Wind)+
  xlab("Land cover")+ylab("Total insect biomass")

#ignoring time-band
ggplot(insectsDK,aes(x=Land_use, y=log10(Biomass+1)))+
  geom_boxplot(fill=landuseCols)+
  theme_bw()+
  #facet_wrap(~Country)+
  xlab("Land cover")+ylab("Insect biomass")

#split by size --> did not include +1 to log biomass
g1 <- ggplot(insectsDK,aes(x=Land_use, y=log(Biomass_large),colour=Land_use))+
  geom_boxplot(fill="white")+
  scale_color_manual(values=landuseCols)+
  theme_bw()+
  #facet_wrap(~Country)+
  theme(legend.position = "none")+
  xlab("Land cover")+ylab("Large insect biomass")

g2 <- ggplot(insectsDK,aes(x=Land_use, y=log(Biomass_small),colour=Land_use))+
  geom_boxplot(fill="white")+
  scale_color_manual(values=landuseCols)+
  theme_bw()+
  #facet_wrap(~Country)+
  theme(legend.position = "none")+
  xlab("Land cover")+ylab("Small insect biomass")

plot_grid(g1,g2,ncol=1)

###lm##############################################################

hist(log(insectsDK$Biomass+1))

lm1 <- lm(log(Biomass+1) ~ Land_use + Time_band + Wind * Temperature + Time_driven + Velocity, data=insectsDK) # including interaction between wind and temperature, since it seemed important for mosquito study
summary(lm1)
drop1(lm1)

#plus random effects##############################################################

library(lme4)
library(lmerTest)
lme1 <-
  lmer(
    log(Biomass + 1) ~ Land_use + Time_band +
      Time_band:as.numeric(StartTime) + yDay +
      (1 | RouteID) + (1 | PilotID),
    data = insectsDK
  )

summary(lme1)

#plus spatial models################################################################
# add x and y coordinates for each route
#jitter x and y slightly - fix later
insectsDK <-
  read.delim("cleaned-data/DK_mergedData.txt", sep = "\t") # data with utm coordinates included
str(insectsDK)

#format Date
insectsDK$Date <- as.Date(insectsDK$Date, "%d-%m-%Y")
insectsDK$yDay <- yday(insectsDK$Date)

#order time band
insectsDK$Time_band <- factor(insectsDK$Time_band,levels=c("midday","evening"))
#order habitat
insectsDK$Land_use <- factor(insectsDK$Land_use,levels=c("Urban","Farmland",
                                                         "Dryland","Wetland","Forest"))

# first replace commas with points for decimals
insectsDK$utm_x <- sapply(insectsDK$utm_x, gsub, pattern = ",", replacement= ".")
insectsDK$utm_y <- sapply(insectsDK$utm_y, gsub, pattern = ",", replacement= ".")
str(insectsDK)

# change from character to numeric
insectsDK$utm_x <- as.numeric(insectsDK$utm_x)
insectsDK$utm_y <- as.numeric(insectsDK$utm_y)

library(nlme)

insectsDK$x2 <- insectsDK$utm_x + rnorm(length(insectsDK$utm_x),0,100)
insectsDK$y2 <- insectsDK$utm_y + rnorm(length(insectsDK$utm_y),0,100)

gls1 <- lme(log(Biomass+1) ~ Land_use + Time_band + 
              Time_band:as.numeric(StartTime) + yDay,
            random=~1|RouteID,
            correlation=corExp(form=~x2+y2|RouteID),
            data=insectsDK,na.action=na.omit)

summary(gls1)
