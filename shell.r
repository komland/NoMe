rm(list = ls())
source("anonymizeData.r", echo = TRUE)

rm(list = ls())
source("prepareinp.r", echo = TRUE)

rm(list = ls())
sink("run2017.txt")
source("analysis2017.r", echo = TRUE)
sink()

rm(list = ls())
sink("run2018.txt")
source("analysis2018.r", echo = TRUE)
sink()

rm(list = ls())
sink("run2019.txt")
source("analysis2019.r", echo = TRUE)
sink()
