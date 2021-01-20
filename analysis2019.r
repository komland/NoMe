library(data.table)
library(RMark)

source("MARKformulas.r")

## declare which year this analysis is for (2017, 2018, or 2019)
focalYr <- 2019

## load the prepared inp file
dat7 <- readRDS(paste0("Data/inpFile", focalYr, ".RDS"))
## sex as factor
dat7[,sex := factor(sex, levels = c("F", "M"))]
dat7[, .N, sex] # n.b. some marked butterflies did not have sex determined

## also load observer file
observer <- setDT(fread("observer.csv"))
observer[,DATE := as.IDate(DATE)]
(obsY <- observer[year(DATE) == focalYr])
obs2 <- data.frame(time = as.numeric(obsY[,DATE] - obsY[1, DATE]) + 1,
                   obs = obsY[,OBSERVER])

## process data and make design
## CJS without sex as a covariate
NoMe.CJS1 <- process.data(dat7, groups = "sex")
NoMe.CJS1.ddl <- make.design.data(NoMe.CJS1)

NoMe.phidot.pdot <- mark(NoMe.CJS1,
                         NoMe.CJS1.ddl,
                         model.parameters = list(Phi = Phidot, p = pdot))
NoMe.phidot.ptime <- mark(NoMe.CJS1,
                          NoMe.CJS1.ddl,
                          model.parameters = list(Phi = Phidot, p = ptime))
## three observers
NoMe.CJS1.ddl$p <- merge_design.covariates(NoMe.CJS1.ddl$p, obs2)
NoMe.phidot.pobs3 <- mark(NoMe.CJS1,
                          NoMe.CJS1.ddl,
                          model.parameters = list(Phi = Phidot, p = pobs))
NoMe.phidot.pSxOb3 <- mark(NoMe.CJS1,
                           NoMe.CJS1.ddl,
                           model.parameters = list(Phi = Phidot, p = pSxOb))
## collapse D, E -> F
NoMe.CJS1.ddl$p$obs <- factor(ifelse(NoMe.CJS1.ddl$p$obs == "A", "A", "F"))
NoMe.phidot.pobs2 <- mark(NoMe.CJS1,
                          NoMe.CJS1.ddl,
                          model.parameters = list(Phi = Phidot, p = pobs))
NoMe.phidot.pSxOb2 <- mark(NoMe.CJS1,
                           NoMe.CJS1.ddl,
                           model.parameters = list(Phi = Phidot, p = pSxOb))
# sex
NoMe.phisex.pdot <- mark(NoMe.CJS1,
                         NoMe.CJS1.ddl,
                         model.parameters = list(Phi = Phisex, p = pdot))
NoMe.phidot.psex <- mark(NoMe.CJS1,
                         NoMe.CJS1.ddl,
                         model.parameters = list(Phi = Phidot, p = psex))

## build model selection table
if(exists("msCJS")) rm(msCJS)
if(exists("markModelInt")) rm(markModelInt)
(NoMe.list <- ls()[sapply(ls(), function(f) class(get(f))[1] == "mark")])
for(i in NoMe.list){
    if(exists("markModelInt")) rm(markModelInt)
    markModelInt <- get(i)
    msInt <- data.frame(object = i,
                        model = markModelInt$model.name,
                        npar = markModelInt$results$npar,
                        deviance = markModelInt$results$deviance,
                        AICc = markModelInt$results$AICc)
    if(!exists("msCJS")){
        msCJS <- msInt
    }else{
        msCJS <- rbind(msCJS, msInt)
    }
}
msCJS$deltaAICc <- msCJS$AICc - min(msCJS$AICc)
(msCJS <- msCJS[order(msCJS$AICc),])

## weather, possibly, but it looks more like observer ...
plot(NoMe.phidot.ptime$results$real$estimate[2:(nrow(obsY)-1)],
     type = "o", pch = 16,
     xaxt = "n", xlab = "",
     ylab = "p(detection)", ylim = c(0, 1), yaxs = "i")
axis(1, at = 1:(nrow(obsY)-1), las = 2, labels = obsY[2:.N,DATE])
points(NoMe.phidot.ptime$results$real$ucl[2:(nrow(obsY)-1)], pch = 2, type = "o")
points(NoMe.phidot.ptime$results$real$lcl[2:(nrow(obsY)-1)], pch = 6, type = "o")

## different detection by sex
NoMe.phidot.pSxOb2$results$real

## POPAN
NoMe.POPAN <- process.data(dat7, model = "POPAN", groups = "sex")
NoMe.POPAN.ddl <- make.design.data(NoMe.POPAN)
NoMe.POPAN.ddl$p <- merge_design.covariates(NoMe.POPAN.ddl$p, obs2)
## collapse D, E -> F
NoMe.POPAN.ddl$p$obs <- factor(ifelse(NoMe.POPAN.ddl$p$obs == "A", "A", "F"))

NoMe.fmP <- mark(NoMe.POPAN,
                 NoMe.POPAN.ddl,
                 model.parameters = list(Phi = Phidot, p = pdot,
                                         pent = pentdot, N = Ndot))
NoMe.fmPsx <- mark(NoMe.POPAN,
                   NoMe.POPAN.ddl,
                   model.parameters = list(Phi = Phidot, p = psex,
                                           pent = pentdot, N = Ndot))
NoMe.fmPob <- mark(NoMe.POPAN,
                   NoMe.POPAN.ddl,
                   model.parameters = list(Phi = Phidot, p = pobs,
                                           pent = pentdot, N = Ndot))
NoMe.fmPSxOb <- mark(NoMe.POPAN,
                     NoMe.POPAN.ddl,
                     model.parameters = list(Phi = Phidot, p = pSxOb,
                                             pent = pentdot, N = Ndot))
## model selection among fmP
if(exists("msPPN")) rm(msPPN)
if(exists("markModelInt")) rm(markModelInt)
(NoMe.l2st <- setdiff(ls()[sapply(ls(), function(f) class(get(f))[1] == "mark")], NoMe.list))
for(i in NoMe.l2st){
    if(exists("markModelInt")) rm(markModelInt)
    markModelInt <- get(i)
    msInt <- data.frame(object = i,
                        model = markModelInt$model.name,
                        npar = markModelInt$results$npar,
                        deviance = markModelInt$results$deviance,
                        AICc = markModelInt$results$AICc)
    if(!exists("msPPN")){
        msPPN <- msInt
    }else{
        msPPN <- rbind(msPPN, msInt)
    }
}
msPPN$deltaAICc <- msPPN$AICc - min(msPPN$AICc)
(msPPN <- msPPN[order(msPPN$AICc),])

## population estimate
NoMe.fmPSxOb$results$real[1:7,]
NoMe.fmPSxOb$results$derived$`Gross N* Population Size`
sapply(NoMe.fmPSxOb$results$derived$`Gross N* Population Size`, sum)

## observer A/first 10 days only
dat7W <- dat7
dat7W$ch <- substr(dat7W$ch, 1, 10)
## only 12 butterflies marked after he finished working
nrow(dat7W[dat7W$ch == "0000000000",])
## keep the 99 Weston marked
dat7W <- dat7W[dat7W$ch != "0000000000",]
nrow(dat7W)

NoMe.CJS2 <- process.data(dat7W, groups = "sex")
NoMe.CJS2.ddl <- make.design.data(NoMe.CJS2)

NoMe.phidot.pdotW <- mark(NoMe.CJS2,
                         NoMe.CJS2.ddl,
                         model.parameters = list(Phi = Phidot, p = pdot))
NoMe.phidot.ptimeW <- mark(NoMe.CJS2,
                          NoMe.CJS2.ddl,
                          model.parameters = list(Phi = Phidot, p = ptime))
NoMe.phisex.pdotW <- mark(NoMe.CJS2,
                         NoMe.CJS2.ddl,
                         model.parameters = list(Phi = Phisex, p = pdot))
NoMe.phidot.psexW <- mark(NoMe.CJS2,
                         NoMe.CJS2.ddl,
                         model.parameters = list(Phi = Phidot, p = psex))

## build model selection table
if(exists("msCJS2")) rm(msCJS2)
if(exists("markModelInt")) rm(markModelInt)
(NoMe.l3st <- setdiff(ls()[sapply(ls(), function(f) class(get(f))[1] == "mark")],
                      union(NoMe.list, NoMe.l2st)))
for(i in NoMe.l3st){
    if(exists("markModelInt")) rm(markModelInt)
    markModelInt <- get(i)
    msInt <- data.frame(object = i,
                        model = markModelInt$model.name,
                        npar = markModelInt$results$npar,
                        deviance = markModelInt$results$deviance,
                        AICc = markModelInt$results$AICc)
    if(!exists("msCJS2")){
        msCJS2 <- msInt
    }else{
        msCJS2 <- rbind(msCJS2, msInt)
    }
}
msCJS2$deltaAICc <- msCJS2$AICc - min(msCJS2$AICc)
(msCJS2 <- msCJS2[order(msCJS2$AICc),])

## different detection by sex
NoMe.phidot.psexW$results$real

## POPAN
NoMe.POPANW <- process.data(dat7W, model = "POPAN", groups = "sex")
NoMe.POPANW.ddl <- make.design.data(NoMe.POPANW)

NoMe.fmPW <- mark(NoMe.POPAN,
                 NoMe.POPAN.ddl,
                 model.parameters = list(Phi = Phidot, p = pdot,
                                         pent = pentdot, N = Ndot))
NoMe.fmPsxW <- mark(NoMe.POPAN,
                   NoMe.POPAN.ddl,
                   model.parameters = list(Phi = Phidot, p = psex,
                                           pent = pentdot, N = Ndot))
## model selection among fmP
for(i in c("NoMe.fmPW", "NoMe.fmPsxW")){
    if(exists("markModelInt")) rm(markModelInt)
    markModelInt <- get(i)
    msInt <- data.frame(object = i,
                        model = markModelInt$model.name,
                        npar = markModelInt$results$npar,
                        deviance = markModelInt$results$deviance,
                        AICc = markModelInt$results$AICc)
    if(!exists("msPPNW")){
        msPPNW <- msInt
    }else{
        msPPNW <- rbind(msPPNW, msInt)
    }
}
msPPNW$deltaAICc <- msPPNW$AICc - min(msPPNW$AICc)
(msPPNW <- msPPNW[order(msPPNW$AICc),])

## population estimate
NoMe.fmPsxW$results$real[1:5,]
NoMe.fmPsxW$results$derived$`Gross N* Population Size`
sapply(NoMe.fmPsxW$results$derived$`Gross N* Population Size`, sum)
