library(data.table)

## load the effort table (date, observer(s))
effort <- setDT(fread("observer.csv"))
effort[,DATE := as.IDate(DATE)]

## load the raw data in field form format (record ID, date, patch, bug number, sex, status)
## status in {Marked, Resighted, Unmarked}
dat4 <- setDT(fread("resight.csv"))
## format date and bug number
dat4[,DATE := as.IDate(DATE)]
dat4[,BUGNO := as.numeric(BUGNO)]

## develop encounter history file for each year
for(i in 1:3){
    y <- c(2017:2019)[i]
    ## sub-tables for the year
    effortY <- effort[year(DATE) == y, DATE]
    dat5 <- dat4[year(DATE) == y &
                     is.finite(BUGNO) &
                     STATUS %in% c("Marked", "Resighted")]
    ## set up array with rows for individuals and columns for days, initially populated with zeros
    dat6 <- array(data = 0,
                  dim = c(length(unique(dat5[,BUGNO])),
                          length(effortY)),
                  dimnames = list(unique(dat5[,BUGNO]),
                                  sub(paste("201", y, "-", sep = ""), "", effortY)))
    ## fill in 1 for individuals on days they were marked or resighted
    for(j in 1:nrow(dat6)){
        ## records for each individual
        dat5Int <- dat5[BUGNO == as.numeric(dimnames(dat6)[[1]][j])]
        dat6[j, match(dat5Int[,DATE], effortY)] <- 1
    }
    # ## daily counts, two ways (can verify processing to array)
    # dat5[, .N, DATE]
    # apply(dat6, 2, sum)
    ## format as encounter history as expected in Program MARK
    dat7 <- data.table(BUGNO = as.numeric(dimnames(dat6)[[1]]),
                       ch = apply(dat6, 1, function(f){
                           paste(f, collapse = "")}))
    ## attach sex
    coVariates <- unique(dat5[, c("BUGNO", "SEX")])
    dat7 <- merge(dat7, coVariates)
    names(dat7) <- tolower(names(dat7))
    saveRDS(dat7, paste0("Data/inpFile", y, ".RDS"))
}
