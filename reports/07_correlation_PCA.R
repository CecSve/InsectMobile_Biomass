# run script 03 first

### Load required libraries ###########################

#library(ggbiplot)
library(corrplot)
library(Hmisc)
library(ggfortify) # to plot PCA

### Denmark ###########################

### Correlation plot ##################

# prepare plotting
#par(mfrow = c(2, 2))

#names(allInsects)

# select variables for PCA - we will only use land cover at 1000 m and cStops
#biomass.pca <- prcomp(allInsects[,c(12,142,44:49)], center = TRUE,scale. = TRUE) #choose biomass, stops and land covers
#summary(biomass.pca)
#str(biomass.pca)

#ggbiplot(biomass.pca)

# correlation plot for 1000 m buffer
someInsects <- allInsects[,c(12,142,44:45,47:49)]
colnames(someInsects)
colnames(someInsects) <- c("Biomass", "Stops", "Farmland", "Forest", "Grassland", "Urban", "Wetland")

p <- cor(someInsects)

# add significance
res1 <- cor.mtest(someInsects, conf.level = .95)
res2 <- cor.mtest(someInsects, conf.level = .99)

#png(file = "plots/DK_correlation_buffers.png")
tiff("plots/DK_correlation_buffers.tiff", units="in", width=7, height=7, res=300)

# prepare plotting
par(mfrow = c(2, 2))

# with correlation coefficient instead of p-values, coloured boxes = significant at a 0.05 level
corrplot(p, method = "color",
         type = "upper", order = "AOE", number.cex = .7,
         addCoef.col = "black", # Add coefficient of correlation
         tl.col = "black", tl.srt = 90, # Text label color and rotation
         # Combine with significance
         p.mat = res1$p, sig.level = 0.05, insig = "blank", 
         # hide correlation coefficient on the principal diagonal 
         diag = FALSE, title = "Correlation at 1000 m", mar=c(0,0,1,0))

# correlation plot for 500 m buffer
someInsects <- allInsects[,c(12,142,38:39,42:44)]
colnames(someInsects)
colnames(someInsects) <- c("Biomass", "Stops", "Farmland", "Forest", "Grassland", "Urban", "Wetland")

p <- cor(someInsects)

# add significance
res1 <- cor.mtest(someInsects, conf.level = .95)
res2 <- cor.mtest(someInsects, conf.level = .99)

# with correlation coefficient instead of p-values, coloured boxes = significant at a 0.05 level
corrplot(p, method = "color",
         type = "upper", order = "AOE", number.cex = .7,
         addCoef.col = "black", # Add coefficient of correlation
         tl.col = "black", tl.srt = 90, # Text label color and rotation
         # Combine with significance
         p.mat = res1$p, sig.level = 0.05, insig = "blank", 
         # hide correlation coefficient on the principal diagonal 
         diag = FALSE, title = "Correlation at 500 m", mar=c(0,0,1,0))

# correlation plot for 250 m buffer
someInsects <- allInsects[,c(12,142,32:33,35:37)]
colnames(someInsects)
colnames(someInsects) <- c("Biomass", "Stops", "Farmland", "Forest", "Grassland", "Urban", "Wetland")

p <- cor(someInsects)

# add significance
res1 <- cor.mtest(someInsects, conf.level = .95)
res2 <- cor.mtest(someInsects, conf.level = .99)

# with correlation coefficient instead of p-values, coloured boxes = significant at a 0.05 level
corrplot(p, method = "color",
         type = "upper", order = "AOE", number.cex = .7,
         addCoef.col = "black", # Add coefficient of correlation
         tl.col = "black", tl.srt = 90, # Text label color and rotation
         # Combine with significance
         p.mat = res1$p, sig.level = 0.05, insig = "blank", 
         # hide correlation coefficient on the principal diagonal 
         diag = FALSE, title = "Correlation at 250 m", mar=c(0,0,1,0))

# correlation plot for 50 m buffer
someInsects <- allInsects[,c(12,142,26:27,29:31)]
colnames(someInsects)
colnames(someInsects) <- c("Biomass", "Stops", "Farmland", "Forest", "Grassland", "Urban", "Wetland")

p <- cor(someInsects)

# add significance
res1 <- cor.mtest(someInsects, conf.level = .95)
res2 <- cor.mtest(someInsects, conf.level = .99)

# with correlation coefficient instead of p-values, coloured boxes = significant at a 0.05 level
corrplot(p, method = "color",
         type = "upper", order = "AOE", number.cex = .7,
         addCoef.col = "black", # Add coefficient of correlation
         tl.col = "black", tl.srt = 90, # Text label color and rotation
         # Combine with significance
         p.mat = res1$p, sig.level = 0.05, insig = "blank", 
         # hide correlation coefficient on the principal diagonal 
         diag = FALSE, title = "Correlation at 50 m", mar=c(0,0,1,0))

dev.off()

### PCA #########

# subset data for each land cover buffer prior to analysis
names(allInsects)
cor50 <- allInsects[,c(1,26:31)]
cor250 <- allInsects[,c(1,32:37)]
cor500 <- allInsects[,c(1,38:43)]
cor1000 <- allInsects[,c(1,44:49)]

# 50 m buffer correlation of land cover variables
fit <- princomp(cor50, cor=TRUE)
summary(fit) # print variance accounted for 
loadings(fit) # pc loadings 
plot(fit,type="lines") # scree plot 
fit$scores # the principal components
biplot(fit)

# 250 m buffer correlation of land cover variables
fit <- princomp(cor250, cor=TRUE)
summary(fit) # print variance accounted for 
loadings(fit) # pc loadings 
plot(fit,type="lines") # scree plot 
fit$scores # the principal components
biplot(fit)

# 250 m buffer correlation of land cover variables
fit <- princomp(cor500, cor=TRUE)
summary(fit) # print variance accounted for 
loadings(fit) # pc loadings 
plot(fit,type="lines") # scree plot 
fit$scores # the principal components
biplot(fit)

# 250 m buffer correlation of land cover variables
fit <- princomp(cor1000, cor=TRUE)
summary(fit) # print variance accounted for 
loadings(fit) # pc loadings 
plot(fit,type="lines") # scree plot 
fit$scores # the principal components
biplot(fit)

mydata <- allInsects[,c("cStops",names(allInsects)[grepl("_1000",names(allInsects))])]
names(mydata)
mydata <- mydata[,c(2:3, 5:7)]
names(mydata)[names(mydata)=="Open.uncultivated.land_1000"] <- "Grassland_1000"
names(mydata)[names(mydata)=="Agriculture_1000"] <- "Farmland_1000"
names(mydata) <- gsub("_1000","",names(mydata))
allInsects$Land_use <- as.character(allInsects$Land_use)
allInsects$Land_use[allInsects$Land_use=="Dryland"] <- "Grassland"
allInsects$Land_use[allInsects$Land_use=="Open uncultivated land"] <- "Grassland"
landuseOrder
allInsects$Land_use <- factor(allInsects$Land_use, levels=landuseOrder)

fit <- princomp(mydata, cor=TRUE)

#pca with rotation
library(psych)
#packageurl <- "https://cran.r-project.org/src/contrib/Archive/mnormt/mnormt_1.5-7.tar.gz"
#install.packages(packageurl, repos=NULL, type="source")
pca_rotated <- psych::principal(mydata, rotate="varimax", nfactors=2, scores=TRUE)
biplot(pca_rotated, main = "A: Denmark")
print(pca_rotated)

ggsave("plots/pca_with_rotation_1000_DK.png")

#add PCA axes scores to the dataset
allInsects$Urbanization_gradient <- pca_rotated$scores[,1]
allInsects$Forest_gradient <- pca_rotated$scores[,2]

#with ggplot
autoplot(fit)
dk_autoplot <- autoplot(fit, data = allInsects, colour = 'Land_use', 
         loadings = TRUE, 
         loadings.colour = 'black',
         loadings.label = TRUE, 
         loadings.label.size = 5) + 
  scale_colour_manual(values = landuseCols[1:6])+
  theme_bw() + labs(colour = "Land cover")

ggsave("plots/pca_1000_DK.png")
dk_autoplot <- plot_grid(dk_autoplot, labels = "AUTO")
save_plot("plots/pca_1000_DK_numbered.png", dk_autoplot, base_height = 8, base_width = 12)

lme1000 <- lmer(log(Biomass+1) ~ 
                  Urbanization_gradient + 
                  Forest_gradient +
                  Time_band + 
                  Time_band:cnumberTime + cStops + cyDay + 
                  (1|RouteID_JB) + (1|PilotID), data=allInsects)
summary(lme1000)

library(MuMIn)
r.squaredGLMM(lme1000)

### Germany ###########################

### Correlation plot ##################
names(allInsects)

#1000m plot
someInsects <- allInsects[,c(5,24,28,32,36,40,65)]

colnames(someInsects)
colnames(someInsects) <- c("Biomass", "Farmland", "Forest","Grassland", "Urban", "Wetland","Stops")

p <- cor(someInsects)

# add significance
res1 <- cor.mtest(someInsects, conf.level = .95)
res2 <- cor.mtest(someInsects, conf.level = .99)

#png(file = "plots/DK_correlation_buffers.png")
tiff("plots/DE_correlation_buffers.tiff", units="in", width=7, height=7, res=300)

# prepare plotting
par(mfrow = c(2, 2))

# with correlation coefficient instead of p-values, coloured boxes = significant at a 0.05 level
corrplot(p, method = "color",
         type = "upper", order = "AOE", number.cex = .7,
         addCoef.col = "black", # Add coefficient of correlation
         tl.col = "black", tl.srt = 90, # Text label color and rotation
         # Combine with significance
         p.mat = res1$p, sig.level = 0.05, insig = "blank", 
         # hide correlation coefficient on the principal diagonal 
         diag = FALSE, title = "Correlation at 1000 m", mar=c(0,0,1,0))

# correlation plot for 500 m buffer
someInsects <- allInsects[,c(5,23,27,31,35,39,65)]
colnames(someInsects)
colnames(someInsects) <- c("Biomass", "Farmland", "Forest","Grassland", "Urban", "Wetland","Stops")
p <- cor(someInsects)

# with correlation coefficient instead of p-values, coloured boxes = significant at a 0.05 level
corrplot(p, method = "color",
         type = "upper", order = "AOE", number.cex = .7,
         addCoef.col = "black", # Add coefficient of correlation
         tl.col = "black", tl.srt = 90, # Text label color and rotation
         # Combine with significance
         p.mat = res1$p, sig.level = 0.05, insig = "blank", 
         # hide correlation coefficient on the principal diagonal 
         diag = FALSE, title = "Correlation at 500 m", mar=c(0,0,1,0))

# correlation plot for 250 m buffer
someInsects <- allInsects[,c(5,22,26,30,34,38,65)]
colnames(someInsects)
colnames(someInsects) <- c("Biomass", "Farmland", "Forest","Grassland", "Urban", "Wetland","Stops")
p <- cor(someInsects)

# with correlation coefficient instead of p-values, coloured boxes = significant at a 0.05 level
corrplot(p, method = "color",
         type = "upper", order = "AOE", number.cex = .7,
         addCoef.col = "black", # Add coefficient of correlation
         tl.col = "black", tl.srt = 90, # Text label color and rotation
         # Combine with significance
         p.mat = res1$p, sig.level = 0.05, insig = "blank", 
         # hide correlation coefficient on the principal diagonal 
         diag = FALSE, title = "Correlation at 250 m", mar=c(0,0,1,0))

# correlation plot for 50 m buffer
someInsects <- allInsects[,c(5,21,25,29,33,37,65)]
colnames(someInsects)
colnames(someInsects) <- c("Biomass", "Farmland", "Forest","Grassland", "Urban", "Wetland","Stops")
p <- cor(someInsects)

# with correlation coefficient instead of p-values, coloured boxes = significant at a 0.05 level
corrplot(p, method = "color",
         type = "upper", order = "AOE", number.cex = .7,
         addCoef.col = "black", # Add coefficient of correlation
         tl.col = "black", tl.srt = 90, # Text label color and rotation
         # Combine with significance
         p.mat = res1$p, sig.level = 0.05, insig = "blank", 
         # hide correlation coefficient on the principal diagonal 
         diag = FALSE, title = "Correlation at 50 m", mar=c(0,0,1,0))

dev.off()

###pca analysis###############################################
#taken from the Quick R website

mydata <- allInsects[,c("cStops",names(allInsects)[grepl("_1000",names(allInsects))])]
names(mydata)
mydata <- mydata[,2:6]
names(mydata)[names(mydata)=="Open.uncultivated_1000"] <- "Grassland_1000"
names(mydata)[names(mydata)=="Agriculture_1000"] <- "Farmland_1000"
names(mydata) <- gsub("_1000","",names(mydata))
allInsects$Land_use <- as.character(allInsects$Land_use)
allInsects$Land_use[allInsects$Land_use=="Dryland"] <- "Grassland"
allInsects$Land_use[allInsects$Land_use=="Open uncultivated"] <- "Grassland"
landuseOrder
allInsects$Land_use <- factor(allInsects$Land_use, levels=landuseOrder)

fit <- princomp(mydata, cor=TRUE)
summary(fit) # print variance accounted for
loadings(fit) # pc loadings
plot(fit,type="lines") # scree plot
fit$scores # the principal components
biplot(fit)

#Maximum Likelihood Factor Analysis - from Quick R
# with varimax rotation
fit <- factanal(mydata, 2, rotation="varimax")
print(fit, digits=2, sort=TRUE)
load <- fit$loadings[,1:2]
plot(load,type="n")
text(load,labels=names(mydata),cex=.7)  

#pca with rotation
library(psych)
#packageurl <- "https://cran.r-project.org/src/contrib/Archive/mnormt/mnormt_1.5-7.tar.gz"
#install.packages(packageurl, repos=NULL, type="source")
pca_rotated <- psych::principal(mydata, rotate="varimax", nfactors=2, scores=TRUE)
biplot(pca_rotated,main="B: Germany")
print(pca_rotated)
#                       RC1  RC2
#SS loadings           1.49 1.46
#Proportion Var        0.30 0.29


ggsave("plots/pca_with_rotation_1000_DE.png",width=8,height=12)

#add PCA axes scores to the dataset
allInsects$Urbanization_gradient <- pca_rotated$scores[,1]
allInsects$Forest_gradient <- pca_rotated$scores[,2]

#with ggplot
autoplot(fit)
autoplot(fit, data = allInsects, colour = 'Land_use',
         loadings = TRUE, 
         loadings.colour = 'black',
         loadings.label = TRUE, 
         loadings.label.size = 2.5) + 
  scale_colour_manual(values = landuseCols[1:5])+
theme_bw()

ggsave("plots/pca_1000_DE.png")