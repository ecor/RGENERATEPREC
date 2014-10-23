# TODO: Add comment
# 
# Author: ecor
###############################################################################

NULL
#' precipitation.probability
#' 
#' This function produces time series (data frames) of daily precipititaton quantiles from simulated continous gaussian variables
#' 
#' @param x gaussianized variable
#' @param data referred variable. e.g. data frame of observation
#' @param valmim minimum value acceptable of precipitation, e.g. 0.5 millimeters
#' @param sample see \code{\link{normalizeGaussian_severalstations}}
#' @param ... 
#' 
#' @seealso \code{\link{pnorm}}
#' 
#' @export
#' 
#' @examples
#' 
#' library(RGENERATEPREC)
#' #
#' set.seed(1233)
#' data(trentino)
#' #
#' year_min <- 1987
#' year_max <- 1990
#' #
#' period <- PRECIPITATION$year>=year_min & PRECIPITATION$year<=year_max
#' station <- names(PRECIPITATION)[!(names(PRECIPITATION) %in% c("day","month","year"))]
#' prec_mes <- PRECIPITATION[period,station]  
#' #
#' ### removing nonworking stations (e.g. time series with NA)
#' accepted <- array(TRUE,length(names(prec_mes)))
#' names(accepted) <- names(prec_mes)
#' for (it in names(prec_mes)) {
#' 		 accepted[it]  <- (length(which(!is.na(prec_mes[,it])))==length(prec_mes[,it]))
#' }
#'
#' prec_mes <- prec_mes[,accepted]
#' ### the dateset is reduced!!! 
#' 
#' prec_mes <- prec_mes[,1:2]
#' 
#' origin_data <- paste(year_min,1,1,sep="-")
#' origin_x <- origin_data
#' 
#' x <- prec_mes*NA
#' x[,] <- rnorm(length(as.matrix(prec_mes)))
#' 
#' out <- precipitation.probability(x=x,data=prec_mes,valmin=0.5,origin_x=origin_x,origin_data=origin_data,sample="monthly")
#' 
#' 
#' 
#' #
#' 
#' 


precipitation.probability <- function (x,data,valmin=0.5,sample=NULL,origin_x=origin,origin_data=origin,origin=NULL,...) {
	
	out <- NULL
	
	if (!is.null(sample)) sample <- "all"

	if (sample=="monthly") {
		
		names <- names(out)
		out <- x*NA
		x <- adddate(x,origin=origin_x)
		data <- adddate(data,origin_data=origin_data)
		
		months <- 1:12
		
		
		
		
		for (m in months) {
			
			out[x$month==m,names] <- precipitation.probability(x=x[x$month==m,names],data=data[data$month==m,names],sample="all",origin_x=NULL,origin_data=NULL,...) 
			
			
		}
		
		
		return(out)
		
	}
	
	
	
	if (is.data.frame(x)) {
		
		out <- x*NA
		for (i in 1:ncol(x)) {
			
			out[,i] <- precipitation.probability(x=x[,i],data=data[,i],valmin=valmin,sample=sample,origin_x=origin_x,origin_data=origin_data,...)
			
			
		}
		
		return(out)
		
	}
	
	
	
	
	out <- pnorm(x,...)
	
	data <- data[!is.na(data)]
	
	len <- length(data[data<valmin])/length(data) 
	

	
	out  <- (out-len)/(1-len)
	
	
	out[out<0] <- 0 
	
	
	return(out)
	
	
}

