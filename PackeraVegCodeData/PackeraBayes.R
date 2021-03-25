## ******************************************************************** ##
## PackeraBayesBetaAnalysis.R 
##
## Author: Henry Frye
##
## Purpose:
## Modelling of Packera population data
## ******************************************************************** ##


####FINAL PROJECT CODE####
rm(list= ls())
####SETUP####

#load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggthemes)
library(reshape2)
library(broom)
library(R2jags)
library(MCMCvis)

#load data
transect<- read.csv('TransectClean.csv')
transect$pack_presence <- ifelse(transect$PACK_COV == 0,0,1)

jags.data <- list(n.sites = nrow(transect),
                     pres = transect$pack_presence,
                      aspect = transect$ASPECT,
                      comp = transect$COMP_COV)
                      #Y = transect$PACK_COV/100)
#let's try a full model with heirarchical processes


#presence only data
transects.pres <- transect[which(transect$PACK_COV > 0),]

jags.data.full <- list(n.sites = nrow(transect),
                       n.cont = nrow(transects.pres),
                       y = transect$pack_presence,
                       #slope.dens = scale(transects.pres$SLOPE_DEG)[,1],
                       canopy.dens = scale(transects.pres$CANOPY_OPENNESS.)[,1],
                       aspect.dens = scale(transects.pres$ASPECT)[,1],
                       comp.dens = scale(transects.pres$COMP_COV/100)[,1],
                       canopy.abs = scale(transect$CANOPY_OPENNESS.)[,1],
                       comp.abs = scale(transect$COMP_COV/100)[,1],
                       hemlock.abs = transect$HEMLOCK,
                       cedar.abs = transect$CEDAR,
                       open_cover.abs = scale(transect$OPENSOIL_COV/100)[,1],
                       y.c = transects.pres$PACK_COV/100)

pack.jags.model.full <- jags(model.file = 'packera_full_jags.R',
                        data = jags.data.full, 
                        parameters.to.save = c("a0","a1","a2","a3","a4", "a5", "b0", "b1","b2","b3", "tau"),
                        n.chains = 3,
                        n.iter = 10000,
                        n.burnin = 1000,
                        #inits= inits,
                        n.thin = 2)

pack.jags.model.full

jpeg('MCMCcoef.jpg')
MCMCplot(pack.jags.model.full,params = c("a0","a1","a2","a3","a4","a5", "b0", "b1","b2","b3"), ref_ovl=TRUE, rank=TRUE,
         labels = c("Establishment intercept","Competitor establishment","Canopy establishment","Hemlock establishment","Cedar establishment","Bare ground establishment",
                    "Abundance intercept","Canopy abundance","Competitor abundance","Aspect abundance"))
dev.off()


MCMCtrace(pack.jags.model.full, params = c("a0","a1","a2","a3","a4","a5", "b0", "b1","b2","b3","tau"), ind=TRUE)

plot(pack.jags.model.full)
traceplot(pack.jags.model.full)

prop.table(table(pack.jags.model.full$BUGSoutput$sims.list$b3 < 0))[2]

#probability that b3 is less than 0?
prop.table(table(pack.jags.model.full$BUGSoutput$sims.list$b3 > 0))[2]

x <- seq(0, 10, length.out = 11)
y <- plogis(1.135 - 0.841*x)

y.dens <- plogis(-1.333  - 0.205*x)
y.dens <- plogis(-1.333)




