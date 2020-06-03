# run script 03 and some of 04 to get all variables

library(ggbiplot)
library(corrplot)
library(Hmisc)

### Correlation plot ##################

names(allInsects)

# select variables for PCA - we will only use land cover at 1000 m and cStops
biomass.pca <- prcomp(allInsects[,c(12,121,41:45)], center = TRUE,scale. = TRUE)
summary(biomass.pca)
str(biomass.pca)

ggbiplot(biomass.pca)

# correlation plot for 1000 m buffer
someInsects <- allInsects[,c(12,121,41:45)]
colnames(someInsects)
colnames(someInsects) <- c("Biomass", "Stops", "Farmland", "Forest", "Grassland", "Urban", "Wetland")

p <- cor(someInsects)

# add significance
res1 <- cor.mtest(someInsects, conf.level = .95)
res2 <- cor.mtest(someInsects, conf.level = .99)

## add p-values on no significant coefficient
corrplot(p, p.mat = res1$p, method = "color", type = "upper",
         sig.level = c(.001, .01, .05), pch.cex = .9,
         insig = "label_sig", pch.col = "white", order = "AOE", tl.cex = 0.6, tl.col = "black", col = landuseCols)

# with correlation coefficient as well
corrplot.mixed(p, p.mat = res1$p, lower.col = "black", upper = "color",
         sig.level = c(.001, .01, .05), pch.cex = .9,
         insig = "label_sig", pch.col = "white", order = "AOE", tl.cex = 0.7, tl.col = "black", upper.col = landuseCols, number.cex = 0.7)

corrplot(p, p.mat = res1$p, method = "color", type = "upper",
         insig = "label_sig", pch.col = "white",
         pch = "p<.05", pch.cex = .8, order = "AOE", tl.cex = 0.6, tl.col = "black", col = landuseCols) # "AOE" is for the angular order of the eigenvectors

### PCA #########

# subset data for each land cover buffer prior to analysis
names(allInsects)
cor50 <- allInsects[,c(1,27:31)]
cor250 <- allInsects[,c(1,32:36)]
cor500 <- allInsects[,c(1,37:41)]
cor1000 <- allInsects[,c(1,42:46)]

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
