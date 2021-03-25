## ******************************************************************** ##
## VegetationOrdination.R
##
## Henry Frye
## Created: June 27, 2019
##
## Purpose: Ordination of tree/shrub species from Packera sites
##
## ******************************************************************** ##

## ******************************************************************** ##
####Set Up####
## ******************************************************************** ##

#Remove objects
rm(list= ls())

#load libraries
library(tidyverse)
library(ggthemes)
library(vegan)
library(ade4)
library(adespatial)
library(gclus) 
library(cluster) 
library(FD)
library(ggordiplots)
library(ggrepel)

#read in data
spe <- read.csv('spe.csv') #species presence/absence matrix
env<- read.csv('env.csv')

## ******************************************************************** ##
####PCoA of binary data####
## ******************************************************************** ##

# Distance matrices are based on Sorensen distances
spe.ds <- dist.ldc(spe, "sorensen")

#Run PCoA on distance matrix
spe.ds.pcoa <- cmdscale(spe.ds, k = (nrow(spe) - 1), eig = TRUE)

#Species weighted abundance scores
spe.wa <- wascores(spe.ds.pcoa$points[, 1:2], spe)

#Create dataframe for plotting out of wa scores
WaPoints <- as.data.frame(spe.wa)
WaPoints$Names = rownames(spe.wa)

#set seed for envfit permutations
set.seed(6123)

#run a posteriori env fit on pcoa and make plotting object
VegPlot <- gg_envfit(spe.ds.pcoa, env[,1:6], groups = env$SiteName, perm = 9999)

#Main plot
VegPlot <- VegPlot$plot
WaPoints
MainPCoA <- VegPlot + 
  geom_point(data = WaPoints, aes(x = V1, y  = V2),inherit.aes = FALSE) +
  geom_text_repel(data = WaPoints, aes(x = V1, y  = V2, label = Names),max.overlaps = 15, size =4, inherit.aes = FALSE) +
  theme_tufte() +
  theme(legend.direction = "horizontal",
        legend.background = element_rect(fill = "darkgray"),
        legend.key.size = unit(0.5, "cm"),
        legend.position = c(0.87, 0.05)) 

#save plot
ggsave('GGPCoAVegetationOrdination.png', MainPCoA, units =  "cm", height = 18, width = 24)
