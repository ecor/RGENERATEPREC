NULL
#' Extract randomly precipitetion amounts from precipitation occuence time-series
#' 
#' @param gen generated ocurrence time series (data frame)
#' @param obs observed precipitation amount time series (data frame)
#' @param station string vector containing station identification codes
#' @param valmin maximum admitted value of precipitation depth 
#' @param method see \code{\link{cor}}
#' @param origin date of the day referred by he first row of \code{x}. 
#' @param sample character string. If it is \code{"monthly"} (Default), the corralaton matrix is calculated per each month.  
#' @param use.gaussianized.precipitation.value.correlation logical value. 
#' If it is \code{TRUE} (Default), precipitation generation takes into account the correlations given by \code{\link{gaussianized.precipitation.value.correlation}}.
#' @param prec,... further agruments for \code{\link{normalizeGaussian_severalstations}}
#' 
#' @seealso \code{\link{gaussianized.precipitation.value.correlation}}. 
#'@export
#'@examples 
#' library(RGENERATEPREC)
#' 
#' set.seed(1245)
#' 
#' data(trentino)
#' 
#' year_min <- 1961
#' year_max <- 1990
#' 
#' origin <- paste(year_min,1,1,sep="-")
#' end <- paste(year_max,12,31,sep="-")
#' 
#' period <- PRECIPITATION$year>=year_min & PRECIPITATION$year<=year_max
#' period_temp <- TEMPERATURE_MAX$year>=year_min & TEMPERATURE_MAX$year<=year_max
#' 
#' prec_mes <- PRECIPITATION[period,]
#' Tx_mes <- TEMPERATURE_MAX[period_temp,]
#' Tn_mes <- TEMPERATURE_MIN[period_temp,]
## removing nonworking stations (e.g. time series with NA)
#' accepted <- array(TRUE,length(names(prec_mes)))
#' names(accepted) <- names(prec_mes)
#' for (it in names(prec_mes)) {
#' 	acc <- TRUE
#' 	acc <- (length(which(!is.na(Tx_mes[,it])))==length(Tx_mes[,it]))
#' 	acc <- (length(which(!is.na(Tn_mes[,it])))==length(Tn_mes[,it])) & acc
#' 	accepted[it]  <- (length(which(!is.na(prec_mes[,it])))==length(prec_mes[,it])) & acc
#' 	
#' }
#' 
#' valmin <- 1.0
###station <- names(PRECIPITATION)[!(names(PRECIPITATION) %in% c("day","month","year"))]
#' prec_mes <- prec_mes[,accepted]
#' 
#' 
#' 
#' Tx_mes <- Tx_mes[,accepted]
#' Tn_mes <- Tn_mes[,accepted]
#' prec_occurence_mes <- prec_mes>=valmin
#' 
#' station <- names(prec_mes)[!(names(prec_mes) %in% c("day","month","year"))]
#' ### MultiSite Generation 
#' 
#' 
#' station <- station[1:2]
#' exogen <- Tx_mes[,station]-Tn_mes[,station]
#' 
#' months <- factor(prec_mes$month)
#' 
#' model_multisite <- PrecipitationOccurenceMultiSiteModel(x=prec_mes[,station],exogen=exogen,origin=origin,multisite_type="wilks")
#'
#' 
#' obs_multisite <- prec_mes[,station]
#' 
#' gen_multisite_occ <- generate(model_multisite,exogen=exogen,origin=origin,end=end)
#' 
#' 
#' gen_multisite <- random.precipitation.values(gen=gen_multisite_occ,obs=obs_multisite,valmin=valmin,station=station)
#' 



random.precipitation.values <- function(gen,obs,valmin=1,prec = 10^-4,station=names(gen),sample="monthly",method = c("pearson", "kendall", "spearman"),origin="1961-1-1",use.gaussianized.precipitation.value.correlation=TRUE,...) {
	
	if (!is.null(station)) {
		
		gen <- gen[,station]
		obs <- obs[,station]
		
	}
	
	out <- as.matrix(gen*0)
	
	na.index     <- which(is.na(gen))
	false.index  <- which(gen==FALSE)
	true.index   <- which(gen==TRUE)
	

	
	out[na.index] <- NA
	
	out[false.index] <- 0
	out[true.index] <- valmin+3*prec
		
	out <- as.data.frame(out)
	
	gauss <- normalizeGaussian_severalstations(x=out,data=obs,step=valmin,prec=prec,mean=0,sd=1,inverse=FALSE,sample=sample,origin_x=origin,origin_data=origin,...)
	gaussm <- as.matrix(gauss)
	pgauss <- pnorm(gaussm)
	
	if (use.gaussianized.precipitation.value.correlation==TRUE) {
		
		corm <- gaussianized.precipitation.value.correlation(x=obs,valmin=valmin,station=station,sample=sample,origin=origin,method = c("pearson", "kendall", "spearman"),...)
		#print(corm)
		##stop("ECCOCI!!!")
		gausscr <- generate(corm,origin=origin,type="covariance",n=nrow(obs),FUN=rnorm)
		names(gausscr) <- names(gauss)
		gausscrm <- as.matrix(gausscr)
		pgausscrm <- pnorm(gausscrm)
		pgauss[true.index] <- pgauss[true.index]+pgausscrm[true.index]*(1.0-pgauss[true.index])
		newgauss <- qnorm(pgauss)
		newgauss <- as.data.frame(newgauss)
		names(newgauss) <- names(out)
		
		
	}  else { 
		
		
		pgauss[true.index] <- runif(length(true.index),min=pgauss[true.index],max=1)
		
		newgauss <- qnorm(pgauss)
		newgauss <- as.data.frame(newgauss)
		names(newgauss) <- names(out)
	} 
	
	
	out <- normalizeGaussian_severalstations(x=newgauss,data=obs,mean=0,sd=1,inverse=TRUE,sample=sample,origin_x=origin,origin_data=origin,...)
	
	
	
	
	return(out)
	
}