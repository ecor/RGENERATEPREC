# TODO: Add comment
# 
# Author: ecor
###############################################################################

rm(list=ls())


library(ggplot2)
library(reshape2)
library(RMAWGEN)
library(stringr)
library(RGENERATEPREC)
library(RMAWGENplotAlpha)

data(trentino)




###download.file("https://github.com/ecor/RMAWGENCodeCorner/blob/master/data/results_uncoupled_temperature_generator_P06.rda")
####

year_min <- 1961
year_max <- 1990
origin <- paste(year_min,1,1,sep="-")


period <- PRECIPITATION$year>=year_min & PRECIPITATION$year<=year_max
station_prec <- names(PRECIPITATION)[!(names(PRECIPITATION) %in% c("day","month","year"))]
station_temp <- names(TEMPERATURE_MAX)[!(names(TEMPERATURE_MAX) %in% c("day","month","year"))]
station <- intersect(station_prec,station_temp)
prec_mes <- PRECIPITATION[period,station]
Tx_mes <- TEMPERATURE_MAX[period,station]
Tn_mes <- TEMPERATURE_MIN[period,station]
## removing nonworking stations (e.g. time series with NA)
accepted <- array(TRUE,length(names(prec_mes)))
names(accepted) <- names(prec_mes)
for (it in station) {
	acceptedprec  <- (length(which(!is.na(prec_mes[,it])))==length(prec_mes[,it]))
	accepted[it]  <- acceptedprec & (length(which(!is.na(Tx_mes[,it])))==length(Tx_mes[,it]))
	
}

station <- station[accepted]

prec_mes <- prec_mes[,station]
Tn_mes <- Tn_mes[,station]
Tx_mes <- Tx_mes[,station]

## MARGINAL GAUSSIANIZATION 

prec_mes_gauss  <- normalizeGaussian_severalstations(x=prec_mes, data=prec_mes,step=0,sample="monthly",extremes=TRUE,origin_x = origin,origin_data = origin)
Tn_mes_gauss  <- normalizeGaussian_severalstations(x=Tn_mes, data=Tn_mes,sample="monthly",extremes=TRUE,origin_x = origin, origin_data = origin)

Tx_mes_gauss  <- normalizeGaussian_severalstations(x=Tx_mes, data=Tx_mes,sample="monthly",extremes=TRUE,origin_x = origin, origin_data = origin)
dt_mes_gauss <- normalizeGaussian_severalstations(x=Tx_mes-Tn_mes, data=Tx_mes-Tn_mes,sample="monthly",extremes=TRUE,origin_x = origin, origin_data = origin)
## LM 

month <- 12
monhs <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
month <- months[month]

monthdays <- extractmonths(1:nrow(prec_mes_gauss),origin=origin,when=month)



lmm <- lm(formula = prec_mes_gauss[monthdays, 1] ~ Tn_mes_gauss[monthdays, 1] + Tx_mes_gauss[monthdays, 1])
lmd <- lm(formula = prec_mes_gauss[monthdays, 1] ~ dt_mes_gauss[monthdays, 1])