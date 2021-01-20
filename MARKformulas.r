## model formulas
## survival: uniform or differing between sexes
Phidot <- list(formula = ~ 1)
Phisex <- list(formula = ~ sex)
## resighting: uniform, differing between sexes, or differing by observer
pdot <- list(formula = ~ 1)
psex <- list(formula = ~ sex)
ptime <- list(formula = ~ time)
pobs <- list(formula = ~ obs)
## observer-sex interaction
pSxOb <- list(formula = ~ obs:sex)
## probability of entry: constant
pentdot <- list(formula = ~ 1)
## population: combined or by sex
Ndot <- list(formula = ~ 1)
Nsex <- list(formula = ~ sex)
