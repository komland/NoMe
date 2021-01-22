library(data.table)

## load the effort table (date, observer(s))
effort <- setDT(fread("observer.csv"))
effort[,fDate := as.IDate(DATE)]

## load the raw data in field form format (record ID, date, patch, bug number, sex, status)
## status in {Marked, Resighted, Unmarked}
dat4 <- setDT(fread("resight.csv"))
dat4[,fDate := as.IDate(DATE)]

## some simple data descriptions
datDesc <- merge(dat4[,.N,year(fDate)],
                 dat4[STATUS == "Unmarked",.(unm = .N),year(fDate)])
datDesc[,pUnm := unm/N]
datDesc

dat4[STATUS == "Marked", .N, .(year(fDate), PATCH)][order(PATCH, year)]

## develop encounter history file for each year
for(i in 1:3){
    y <- c(2017:2019)[i]
    
    ## effort for the year
    effortY <- effort[year(fDate) == y, fDate]
    ## ensure it is continuous
    if(!all(diff(effortY) == 1)) effortY <- seq(min(effortY), max(effortY), by = "day")
    
    ## mark-resight for the year
    dat5 <- dat4[year(fDate) == y & STATUS %in% c("Marked", "Resighted")]
    ## set up array with rows for individuals and columns for days, initially populated with zeros
    dat6 <- array(data = 0,
                  dim = c(length(unique(dat5[,BUGNO])),
                          length(effortY)),
                  dimnames = list(unique(dat5[,BUGNO]),
                                  sub(paste("201", y, "-", sep = ""), "", effortY)))
    ## fill in 1 for individuals on days they were marked or resighted
    for(j in 1:nrow(dat6)){
        ## records for each individual
        dat5Int <- dat5[BUGNO == dimnames(dat6)[[1]][j]]
        dat6[j, match(dat5Int[,fDate], effortY)] <- 1
    }
    # ## daily counts, two ways (can verify processing to array)
    # dat5[, .N, fDate]
    # apply(dat6, 2, sum)
    ## format as encounter history as expected in Program MARK
    dat7 <- data.table(BUGNO = dimnames(dat6)[[1]],
                       ch = apply(dat6, 1, function(f){
                           paste(f, collapse = "")}))
    ## attach sex
    coVariates <- unique(dat5[, c("BUGNO", "SEX")])
    dat7 <- merge(dat7, coVariates)
    names(dat7) <- tolower(names(dat7))
    saveRDS(dat7, paste0("Data/inpFile", y, ".RDS"))
}
