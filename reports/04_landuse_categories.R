#run report 03_mergeData

#devtools::install_github("mikabr/ggpirate")
library(ggplot2)
library(plyr)
library(ggpirate)
library(cowplot)

#Fig 2

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


#split by size
g1 <- ggplot(allInsects,aes(x=Land_use, y=log(Biomass_large+1)))+
  geom_boxplot(aes(colour=Time_band,fill=Time_band),show.legend=TRUE)+
  theme_bw()+
  facet_wrap(~Country)+
  xlab("Land cover")+ylab("Large insect biomass")

g2 <- ggplot(allInsects,aes(x=Land_use, y=log(Biomass_small+1)))+
  geom_boxplot(aes(colour=Time_band,fill=Time_band),show.legend=TRUE)+
  theme_bw()+
  facet_wrap(~Country)+
  xlab("Land cover")+ylab("Small insect biomass")

plot_grid(g1,g2)

#ignoring time-band
ggplot(allInsects,aes(x=Land_use, y=log10(Biomass+1)))+
  geom_boxplot(fill="red")+
  theme_bw()+
  facet_wrap(~Country)+
  xlab("Land cover")+ylab("Insect biomass")

temp <- ddply(allInsects,.(Land_use,Country),summarise,
              meanInsects=mean(log10(Biomass+1)),
              sdInects=sd(log(Biomass+1)))

ggplot(temp,aes(x=Land_use, y=meanInsects))+
  geom_bar(fill="red",stat="identity")+
  theme_bw()+
  facet_wrap(~Country)+
  xlab("Land cover")+ylab("Insect biomass")

g1 <- ggplot(allInsects,aes(x=Land_use, y=log(Biomass_large+1)))+
  geom_boxplot(colour="darkblue",fill="white")+
  theme_bw()+
  facet_wrap(~Country)+
  xlab("Land cover")+ylab("Large insect biomass")

g2 <- ggplot(allInsects,aes(x=Land_use, y=log(Biomass_small+1)))+
  geom_boxplot(colour="darkblue",fill="white")+
  theme_bw()+
  facet_wrap(~Country)+
  xlab("Land cover")+ylab("Small insect biomass")

plot_grid(g1,g2,ncol=1)

#relationship between small and large biomass
ggplot(allInsects)+
  geom_point(aes(x=log(Biomass_small+1),y=log(Biomass_large+1),colour=Land_use))+
  facet_wrap(Time_band~Country)+
  geom_abline(intercept=0,slope=1)


###quick analysis
lm1 <- lm(log(Biomass+1) ~ Land_use + Time_band + Country, data=allInsects)
summary(lm1)


#more detailed analysis - with random effects
library(lme4)
lme1 <- lmer(log(Biomass+1) ~ Land_use + Time_band + Country +
               Country:StartTime + Country:yDay + 
               (1|RouteID) + (1|PilotID) + offset(log(Distance_driven)),
             data=insectsDE)

#spatial model
library(nlme)
gls1 <- lme(log(Biomass+1) ~ Land_use + Time_band + Country +
              Country:StartTime + Country:yDay,
            offset=log(Distance_driven),
            random=~1|PilotID/RouteID,
            correlation=corExp(form=~x+y),
            data=insectsDE)

