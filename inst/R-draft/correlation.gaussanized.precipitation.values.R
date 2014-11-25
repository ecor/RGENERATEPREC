NULL
#' ....
#' 
#' @param x observed precipitation amount time series (data frame)
#' @param station string vector containing station identification codes
#' @param valmin maximum admitted value of precipitation depth 
#' @param method see \code{\link{cor}}
#' @param origin date of the day referred by he first row of \code{x}. 
#' @param sample character string. If it is \code{"monthly"} (Default), the corralaton matrix is calculeted per each month.  
#' @param ... further agruments for \code{\link{normalizeGaussian_severalstations}}
#' 
#' 
#' @return The function returns the correlation matrix of precipitation amount values (excluding the zeros). 
#' In case \code{sample=="monthly"} the runction return a \code{MonlthyList} S3 object.
#' 
#' @seealso \code{\link{generate}},\code{\link{random.precipitation.values}},\code{\link{cor}}
#' 
#' @export
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
#' 
#' cor <- gaussianized.precipitation.value.correlation(prec_mes,station=station,origin=origin)
#' 






gaussianized.precipitation.value.correlation <- function(x,valmin=1,station=names(gen),sample="monthly",origin="1961-1-1",method = c("pearson", "kendall", "spearman"),...) {
	
	if (!is.null(station)) {
		
		x <- x[,station]

		
	}
	
	
	xm <- as.matrix(x)
	na.index     <- which(is.na(xm))
	dry.index  <- which(xm<valmin)
	
	

	
	xm[dry.index] <- NA
	
	x[,] <- xm
	
	gauss <- normalizeGaussian_severalstations(x=x,data=x,mean=0,sd=1,inverse=FALSE,sample=sample,origin_x=origin,origin_data=origin,...)
	
	#### FARE UN LAPPLY CON I MESI!!!!!
	
	
	
	
	if (sample=="monthly") {
		names <- names(gauss)
		gauss <- adddate(gauss,origin=origin)
		
		months <- unique(gauss$month)
		out <- list()
		for (m in months) {
			
			out[[m]] <- cor(gauss[gauss$month==m,names],use="pairwise.complete.obs",method=method)
			
		}
		
		class(out) <- "MonthlyList"
	} else {
		
		out <- cor(gauss,use="pairwise.complete.obs",method=method)
	
	}
	
	 
	return(out)
	
}