# run script 03 and some of 04 to get all variables

library(ggbiplot)
library(corrplot)
library(Hmisc)

### Correlation plot ##################

# prepare plotting
par(mfrow = c(2, 2))

names(allInsects)

# select variables for PCA - we will only use land cover at 1000 m and cStops
biomass.pca <- prcomp(allInsects[,c(12,141,44:49)], center = TRUE,scale. = TRUE)
summary(biomass.pca)
str(biomass.pca)

ggbiplot(biomass.pca)

# correlation plot for 1000 m buffer
someInsects <- allInsects[,c(12,141,44:49)]
colnames(someInsects)
colnames(someInsects) <- c("Biomass", "Stops", "Farmland", "Forest", "Heathland", "Grassland", "Urban", "Wetland")

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

# with correlation coefficient instead of p-values, coloured boxes = significant at a 0.05 level
corrplot(p, method = "color", col = landuseCols,
         type = "upper", order = "AOE", number.cex = .7,
         addCoef.col = "black", # Add coefficient of correlation
         tl.col = "black", tl.srt = 90, # Text label color and rotation
         # Combine with significance
         p.mat = res1$p, sig.level = 0.05, insig = "blank", 
         # hide correlation coefficient on the principal diagonal 
         diag = FALSE, title = "Correlation at 1000 m", mar=c(0,0,1,0))

# correlation plot for 500 m buffer

someInsects <- allInsects[,c(12,141,38:43)]
colnames(someInsects)
colnames(someInsects) <- c("Biomass", "Stops", "Farmland", "Forest", "Heathland", "Grassland", "Urban", "Wetland")

p <- cor(someInsects)

# add significance
res1 <- cor.mtest(someInsects, conf.level = .95)
res2 <- cor.mtest(someInsects, conf.level = .99)

# with correlation coefficient instead of p-values, coloured boxes = significant at a 0.05 level
corrplot(p, method = "color", col = landuseCols,
         type = "upper", order = "AOE", number.cex = .7,
         addCoef.col = "black", # Add coefficient of correlation
         tl.col = "black", tl.srt = 90, # Text label color and rotation
         # Combine with significance
         p.mat = res1$p, sig.level = 0.05, insig = "blank", 
         # hide correlation coefficient on the principal diagonal 
         diag = FALSE, title = "Correlation at 500 m", mar=c(0,0,1,0))

# correlation plot for 250 m buffer
someInsects <- allInsects[,c(12,141,32:37)]
colnames(someInsects)
colnames(someInsects) <- c("Biomass", "Stops", "Farmland", "Forest", "Heathland", "Grassland", "Urban", "Wetland")

p <- cor(someInsects)

# add significance
res1 <- cor.mtest(someInsects, conf.level = .95)
res2 <- cor.mtest(someInsects, conf.level = .99)

# with correlation coefficient instead of p-values, coloured boxes = significant at a 0.05 level
corrplot(p, method = "color", col = landuseCols,
         type = "upper", order = "AOE", number.cex = .7,
         addCoef.col = "black", # Add coefficient of correlation
         tl.col = "black", tl.srt = 90, # Text label color and rotation
         # Combine with significance
         p.mat = res1$p, sig.level = 0.05, insig = "blank", 
         # hide correlation coefficient on the principal diagonal 
         diag = FALSE, title = "Correlation at 250 m", mar=c(0,0,1,0))

# correlation plot for 50 m buffer
someInsects <- allInsects[,c(12,141,26:31)]
colnames(someInsects)
colnames(someInsects) <- c("Biomass", "Stops", "Farmland", "Forest", "Heathland", "Grassland", "Urban", "Wetland")

p <- cor(someInsects)

# add significance
res1 <- cor.mtest(someInsects, conf.level = .95)
res2 <- cor.mtest(someInsects, conf.level = .99)

# with correlation coefficient instead of p-values, coloured boxes = significant at a 0.05 level
corrplot(p, method = "color", col = landuseCols,
         type = "upper", order = "AOE", number.cex = .7,
         addCoef.col = "black", # Add coefficient of correlation
         tl.col = "black", tl.srt = 90, # Text label color and rotation
         # Combine with significance
         p.mat = res1$p, sig.level = 0.05, insig = "blank", 
         # hide correlation coefficient on the principal diagonal 
         diag = FALSE, title = "Correlation at 50 m", mar=c(0,0,1,0))


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
