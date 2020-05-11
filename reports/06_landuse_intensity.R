#run mergeData script

library(cowplot)
library(ggplot2)
library(wesanderson)
library(ggpubr)
library(scales)

####colour################################################################

#decide on common color scheme
landuseCols <- wes_palette('Darjeeling1', 5, type = c("discrete"))

### DE plot land cover##############################################

qU <- ggplot(allInsects)+
              geom_point(aes(x=Roads_1000,y=(Biomass+1)),
                         col=landuseCols[1])+
              scale_y_log10() +
              theme_bw() +
              xlab("Road density") +ylab("Biomass")



