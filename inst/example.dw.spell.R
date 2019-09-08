rm(list=ls())

library(RGENERATEPREC)
source('/home/ecor/Dropbox/R-packages/RGENERATEPREC/R/dw.spell.R') 

data(trentino)

year_min <- 1961
year_max <- 1990

period <- PRECIPITATION$year>=year_min & PRECIPITATION$year<=year_max
station <- names(PRECIPITATION)[!(names(PRECIPITATION) %in% c("day","month","year"))]
prec_mes <- PRECIPITATION[period,station]  

## removing nonworking stations (e.g. time series with NA)
accepted <- array(TRUE,length(names(prec_mes)))
names(accepted) <- names(prec_mes)
for (it in names(prec_mes)) {
	accepted[it]  <- (length(which(!is.na(prec_mes[,it])))==length(prec_mes[,it]))
}

prec_mes <- prec_mes[,accepted]
## the dateset is reduced!!! 
prec_mes <- prec_mes[,1:3]

origin <- paste(year_min,1,1,sep="-")
dw_spell <- dw.spell(prec_mes,origin=origin)
dw_spell.dry <- dw.spell(prec_mes,origin=origin,extract="dry")

hist(dw_spell.dry$T0001$spell_length)

## One Station

prec_mes <- prec_mes[,1]

origin <- paste(year_min,1,1,sep="-")

dw_spell <- dw.spell(prec_mes,origin=origin)
dw_spell.dry <- dw.spell(prec_mes,origin=origin,extract="dry")

hist(dw_spell.dry[[1]]$spell_length)


