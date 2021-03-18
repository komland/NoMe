library(data.table)
library(lattice)

load("../dat4.RData")

dat5 <- data.table(dat4)

dat6 <- dat5[!is.na(SEX) & STATUS %in% c("Marked", "Resighted"),
             .N,
             .(year(DATE), BUGNO, SEX)]

png("Fig4.png", width = 6, height = 4, units = "in", res = 600)
trellis.par.set(strip.background = list(col = gray(c(0.9, 0.75))),
                plot.polygon = list(col = "white"))
histogram(~ N | as.factor(year) * SEX, data = dat6,
          xlab = "Sightings", breaks = 0:12,
          type = "density", as.table = TRUE, between = list(x = 0.5, y = 0))
dev.off()

# also table
dat6[,mean(N), .(year, SEX)]

fm <- glm(N ~ -1 + as.factor(year):SEX, dat6, family = "poisson")
summary(fm)
resightCI <- confint(fm)
cbind(exp(coef(fm)),
      exp(resightCI[,1]),
      exp(resightCI[,2]))

# also using the script to refresh my memory of movers
for(p in unique(dat5[,provenance])){
  yr <- sub("Kent", "", p)
  assign(paste0("m", yr),
         dat5[provenance == p & !is.na(BUGNO),
                     .(mover = length(unique(PATCH))),
                     BUGNO][mover > 1, BUGNO])
  print(list(year = yr,
             numberMovers = length(get(paste0("m", yr)))))
  for(m in get(paste0("m", yr))){
    print(dat5[provenance == p & !is.na(BUGNO) & BUGNO == m])
    scan()
  }
}

# I should have included pMovers
c(9, 6, 1)/dat5[!is.na(BUGNO), .(y = length(unique(BUGNO))), provenance][,y]
# across all three seasons
sum(c(9, 6, 1))/sum(dat5[!is.na(BUGNO), .(y = length(unique(BUGNO))), provenance][,y])
