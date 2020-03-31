### Landuse intensity analysis for Danish data. First part incorporates parts of the 03_mergeData, 04_landuse_categories script  

library(lubridate)
library(ggplot2)
library(plyr)
library(ggpirate)
library(cowplot)
library(wesanderson)

# load data
# from report 03
insectsDK <- read.delim("cleaned-data/DK_rough_landuse_biomass.txt",sep=" ")
str(insectsDK)

#format Date
insectsDK$Date <- as.Date(insectsDK$Date, "%d-%m-%Y")
insectsDK$yDay <- yday(insectsDK$Date)

#order time band
insectsDK$Time_band <- factor(insectsDK$Time_band,levels=c("midday","evening"))
#order habitat
insectsDK$Land_use <- factor(insectsDK$Land_use,levels=c("Urban","Farmland",
                                                           "Dryland","Wetland","Forest"))
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
  geom_boxplot(fill="red")+
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

lm1 <- lm(log(Biomass+1) ~ Land_use + Time_band + Wind + Temperature + Time_driven + Velocity, data=insectsDK)
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

# incorporating spatial data #######################################################
#get land use data
land_use <- read.delim("cleaned-data/")

# plotting
library(tidyverse)
library(ggpubr)
library(lme4)
library(lmerTest)
library(nlme)

# correlation plot of number of trafic lights and insect biomass
ggscatter(mergedData, x = "Num_trafficLights", y = "Biomass",
          color = "black", shape = 21, size = 3, # Points color, shape and size
          add = "reg.line",  # Add regressin line
          add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
          conf.int = TRUE, # Add confidence interval
          cor.coeff.args = list(method = "spearman"), facet.by = "Land_use"
) + stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), label.x = 0.5, label.y = 20) + yscale("log2", .format = T) + xscale("log2", .format = T) # but adjust for more traffic lights in urban. 33 % is not high correlation!

# biomass by landuse intensity at 1000m buffer
ggscatter(data_1000m, x = "Biomass", y = "areaProportion",
          color = "black", shape = 1, size = 1, # Points color, shape and size
          add = "reg.line",  # Add regressin line
          add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
          conf.int = TRUE, # Add confidence interval
          cor.coeff.args = list(method = "spearman"), facet.by = "type", font.label = c("bold")
) + stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), size = 3, show.legend = T) +xscale("log2", .format = T) + yscale("log2", .format = T) 

# urban data and traffic lights 1000 m --> only samples wth rough land_use category urban
urban_data <- data_1000m %>% filter(Land_use == "Urban")
urban_data$Num_trafficLights <- as.factor(urban_data$Num_trafficLights)

# group by number of traffic lights, colour by number of traffic lights 
ggscatter(urban_data, x = "areaProportion", y = "Biomass",
          add = "reg.line",                         
          conf.int = TRUE,                          
          color = "Num_trafficLights", palette = "jco",           
          shape = "Num_trafficLights", facet.by = "Num_trafficLights") + stat_cor(method = "spearman", aes(color = Num_trafficLights, label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), label.y = 15, show.legend = F) + xscale("log2", .format = T) + yscale("log2", .format = T)   

# biomass by landuse intensity for urban samples only --> proportional data for the four landuse categories with most urban
# urban data and traffic lights 50 m
urban_data <- data_1000m %>% filter(type == c("Bykerne", "Høj bebyggelse", "Erhverv", "Lav bebyggelse"))
urban_data$Num_trafficLights <- as.factor(urban_data$Num_trafficLights)

ggscatter(urban_data, x = "areaProportion", y = "Biomass",
          add = "reg.line",                         
          conf.int = TRUE, 
          shape = "Num_trafficLights",
          color = "Num_trafficLights", palette = "jco",           
          facet.by = "Num_trafficLights") + stat_cor(method = "spearman", aes(color = Num_trafficLights, label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), label.y = 30, show.legend = F) +xscale("log2", .format = T) + yscale("log2", .format = T) + rremove("legend")

# all data facetted by traffic lights
data_1000m$Num_trafficLights <- as.factor(data_1000m$Num_trafficLights)

ggscatter(data_1000m, x = "areaProportion", y = "Biomass",
          add = "reg.line",                         
          conf.int = TRUE, 
          shape = "Num_trafficLights",
          color = "Num_trafficLights", palette = "jco",           
          facet.by = "Num_trafficLights") + stat_cor(method = "spearman", aes(color = Num_trafficLights, label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), label.y = 30, show.legend = F) +xscale("log2", .format = T) + yscale("log2", .format = T) + rremove("legend")

# all data correlation between propoortional area nd traffic lights facetted by proportional land use types
data_1000m$Num_trafficLights <- as.double(data_1000m$Num_trafficLights)

ggscatter(data_1000m, x = "Num_trafficLights", y = "areaProportion",
          add = "reg.line",                         
          conf.int = TRUE, 
          color = "type", palette = "rickandmorty", facet.by = "type"           
          ) + stat_cor(method = "spearman", aes(color = Num_trafficLights, label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), label.y = 30, show.legend = F) + yscale("log2", .format = T) + rremove("legend")

# invesitgating urban cover n relation to traffic lights and biomass
urbancover <- c("Bykerne", "Erhverv", "Høj bebyggelse", "Lav bebyggelse")

urban <- data_1000m %>% 
     filter(type %in% urbancover) %>% group_by(type, Biomass, Num_trafficLights) %>% summarise(propUrbancover = sum(areaProportion)) 
  
cor.test(urban$propUrbancover, urban$Num_trafficLights)
cor.test(log(urban$propUrbancover+1),log(urban$Num_trafficLights+1))

summary(urban$propUrbancover)
summary(urban$Num_trafficLights)

summary(lm(log(data_1000m$Biomass+1)~data_1000m$areaProportion))
summary(lm(log(data_1000m$Biomass+1)~log(data_1000m$Num_trafficLights+1)))
        
summary(urban$propUrbancover)
urban <- data_50m %>% 
  filter(type %in% urbancover) %>% group_by(type, Biomass, Num_trafficLights) %>% summarise(propUrbancover = sum(areaProportion)) 
summary(urban$propUrbancover)

# So we can compare the effect sizes also d

summary(lm(log(data_1000m$Biomass+1)~scale(data_1000m$areaProportion)))
summary(lm(log(data_1000m$Biomass+1)~scale(log(data_1000m$Num_trafficLights+1))))

summary(lm(log(urban$Biomass+1)~scale(urban$propUrbancover)))
summary(lm(log(urban$Biomass+1)~scale(log(urban$Num_trafficLights+1))))

# trying with random effect
model <- lmer(log(Biomass+1) ~  log(areaProportion+1) +
               Land_use:log(Num_trafficLights+1)+
               (1|RouteID_JB) + (1|PilotID), data=data_1000m)

summary(model)
ranef(model) # estimated deviation
biomass_route <- fixef(model) + ranef(model)$RouteID_JB
biomass_route$route <-rownames(biomass_route)
names(biomass_route)[1]<-"Intercept"
biomass_route <- biomass_route[,c(2,1)]

ggplot(biomass_route,aes(x=route,y=Intercept))+geom_point()

# not facetted by landuse type
with_stops %>% mutate(Biomass = replace(Biomass,is.na(Biomass),0)) %>% ggplot(aes(with_stops))+
  geom_point(aes(x=log(Num_trafficLights+1),y=log(Biomass+1)))+
  #facet_wrap(~Land_use)+
  stat_smooth(method="lm",aes(x=log(Num_trafficLights+1),y=log(Biomass+1)))

# by landuse
with_stops %>% mutate(Biomass = replace(Biomass,is.na(Biomass),0)) %>% ggplot(aes(with_stops))+
  geom_point(aes(x=log(Num_trafficLights+1),y=log(Biomass+1)))+
  facet_wrap(~Land_use)+
  stat_smooth(method="lm",aes(x=log(Num_trafficLights+1),y=log(Biomass+1)))

# by landuse - small size fraction
with_stops %>% mutate(Biomass_small = replace(Biomass_small,is.na(Biomass_small),0)) %>% ggplot(aes(with_stops))+
  geom_point(aes(x=log(Num_trafficLights+1),y=log(Biomass_small+1)))+
  facet_wrap(~Land_use)+
  stat_smooth(method="lm",aes(x=log(Num_trafficLights+1),y=log(Biomass_small+1)))

# by landuse - large size fraction
with_stops %>% mutate(Biomass_large = replace(Biomass_large,is.na(Biomass_large),0)) %>% ggplot(aes(with_stops))+
  geom_point(aes(x=log(Num_trafficLights+1),y=log(Biomass_large+1)))+
  facet_wrap(~Land_use)+
  stat_smooth(method="lm",aes(x=log(Num_trafficLights+1),y=log(Biomass_large+1)))


#plus spatial models################################################################
# add x and y coordinates for each route
#jitter x and y slightly - fix later
insectsDK$x <- 0
insectsDK$y <- 0

insectsDK$x2 <- insectsDK$x + rnorm(length(insectsDK$x),0,100)
insectsDK$y2 <- insectsDK$y + rnorm(length(insectsDK$y),0,100)

gls1 <- lme(log(Biomass+1) ~ Land_use + Time_band + 
              Time_band:as.numeric(StartTime) + yDay,
            random=~1|RouteID,
            correlation=corExp(form=~x2+y2|RouteID),
            data=insectsDK,na.action=na.omit)