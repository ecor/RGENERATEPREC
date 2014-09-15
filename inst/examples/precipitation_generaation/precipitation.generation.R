# TODO: Add comment
# 
# Author: ecor
###############################################################################


library(RGENERATEPREC)


data(trentino)

year_min <- 1961
year_max <- 1990
origin <- paste(year_min,1,1,sep="-")

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
prec_mes <- prec_mes[,1:5]

valmin <- 0.5
prec_mes_gaussWilks <- WilksGaussianization(x=prec_mes, data=prec_mes,valmin=valmin,sample="monthly",extremes=TRUE,origin_x = origin, origin_data = origin,force.precipitation.value="both")

prec_mes_ginv  <- normalizeGaussian_severalstations(x=prec_mes_gaussWilks, data=prec_mes,step=0,sample="monthly",extremes=TRUE,origin_x = origin, origin_data = origin,inverse=TRUE)
ccgamma <- CCGamma(data=prec_mes, lag = 0,valmin=valmin,only.matrix=TRUE,tolerance=0.001)

