#run report 03_mergeData

#devtools::install_github("mikabr/ggpirate")
library(ggplot2)
library(plyr)
library(ggpirate)
library(cowplot)
library(wesanderson)

####################################################################

#decide on common color scheme
landuseCols <- wes_palette('Darjeeling1', 5, type = c("discrete"))

###Fig 2############################################################

#total biomass

ggplot(allInsects,aes(x=Land_use, y=log(Biomass+1)))+
  geom_pirate(aes(colour=Time_band,fill=Time_band),show.legend=TRUE)+
  theme_bw()+
  facet_wrap(~Country)+
  xlab("Land cover")+ylab("Total insect biomass")

ggplot(allInsects,aes(x=Land_use, y=log(Biomass+1)))+
  geom_boxplot(aes(colour=Time_band,fill=Time_band),show.legend=TRUE)+
  theme_bw()+
  facet_wrap(~Country)+
  xlab("Land cover")+ylab("Total insect biomass")

#ignoring time-band
ggplot(allInsects,aes(x=Land_use, y=log10(Biomass+1)))+
  geom_boxplot(fill="red")+
  theme_bw()+
  facet_wrap(~Country)+
  xlab("Land cover")+ylab("Insect biomass")

#split by size
g1 <- ggplot(allInsects,aes(x=Land_use, y=log(Biomass_large+1),colour=Land_use))+
  geom_boxplot(fill="white")+
  scale_color_manual(values=landuseCols)+
  theme_bw()+
  facet_wrap(~Country)+
  theme(legend.position = "none")+
  xlab("Land cover")+ylab("Large insect biomass")

g2 <- ggplot(allInsects,aes(x=Land_use, y=log(Biomass_small+1),colour=Land_use))+
  geom_boxplot(fill="white")+
  scale_color_manual(values=landuseCols)+
  theme_bw()+
  facet_wrap(~Country)+
  theme(legend.position = "none")+
  xlab("Land cover")+ylab("Small insect biomass")

plot_grid(g1,g2,ncol=1)

###lm##############################################################

hist(log(allInsects$Biomass+1))

lm1 <- lm(log(Biomass+1) ~ Land_use + Time_band + StartTime + , data=allInsects)
summary(lm1)


#plus random effects##############################################################

library(lme4)
library(lmerTest)
lme1 <- lmer(log(Biomass+1) ~ Land_use + Time_band + 
               Time_band:as.numeric(StartTime) + yDay + 
               (1|RouteID) + (1|PilotID), data=allInsects)

summary(lme1)

#plus spatial models################################################################

library(nlme)

#jitter x and y slightly - fix leter
allInsects$x2 <- allInsects$x + rnorm(length(allInsects$x),0,100)
allInsects$y2 <- allInsects$y + rnorm(length(allInsects$y),0,100)

gls1 <- lme(log(Biomass+1) ~ Land_use + Time_band + 
              Time_band:as.numeric(StartTime) + yDay,
            random=~1|RouteID,
            correlation=corExp(form=~x2+y2|RouteID),
            data=allInsects,na.action=na.omit)

