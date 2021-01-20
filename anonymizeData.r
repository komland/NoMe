installedPackages <- attr(installed.packages(), "dimnames")[1][[1]]
if(!"data.table" %in% installedPackages) install.packages("data.table")
library(data.table)

load("../dat4.RData")

dat5 <- data.table(dat4)
dat5[,provenance:=NULL]
dat5[,SAMPLER:=NULL]
dat5[,PATCH := factor(ifelse(PATCH == "BBL", "A",
                            ifelse(PATCH == "BD-HG-HF", "B",
                                   ifelse(PATCH == "PC", "C", NA))))]
fwrite(dat5, "resight.csv")

effort <- data.table(unique(dat4[,c("DATE", "SAMPLER")]))
# 2017 gloss over blip that only Roy got there 07-10
effort[year(DATE)==2017, OBSERVER:="A"]
# 2018 gloss over blip that I don't know who was there 07-15
effort[DATE %between% c("2018-06-29", "2018-07-07"), OBSERVER:="B"]
effort[DATE == "2018-07-08", OBSERVER:="BC"]
effort[DATE %between% c("2018-07-09", "2018-07-21"), OBSERVER:="C"]
# 2019
effort[DATE %between% c("2019-06-28", "2019-07-07"), OBSERVER:="A"]
effort[DATE %between% c("2019-07-08", "2019-07-14"), OBSERVER:="D"]
effort[DATE %between% c("2019-07-15", "2019-07-18"), OBSERVER:="E"]
effort[,SAMPLER:=NULL]
fwrite(effort, "observer.csv")
