
#
NULL
#' PrecipitationOccurenceMultiSiteModel
#' 
#' PrecipitationOccurenceMultiSiteModel Description
#' 
## @param x 
## @param 
#' 
#' 
#' 
#'  @export
#' 
#' 
#' @examples
#' 
#' library(RGENERATEPREC)
#' 
#' data(trentino)
#' 
#' year_min <- 1961
#' year_max <- 1990
#' origin <- paste(year_min,1,1,sep="-")
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

#' station <- station[1:4]
#' exogen <- Tx_mes-Tn_mes
#' months <- factor(prec_mes$month)

#' model_multisite <- PrecipitationOccurenceMultiSiteModel(x=prec_mes,exogen=exogen,origin=origin)
#'
PrecipitationOccurenceMultiSiteModel <- function(x,exogen=NULL,station=names(x),origin=origin,valmin=0.5,multisite_type="wilks",tolerance_wilks=0.001,...) {
	
	x <- adddate(x,origin=origin)
	monthly.factor <- factor(x$month)
	
	station <- station[!(station %in% c("day","month","year"))]
	
	x <- x[,station]
	
	if (multisite_type=="wilks") {
		
	
		if (is.null(exogen)) {
			
			exogen <- lapply(X=station,FUN=function(x){ NULL })
			names(exogen) <- station
			
			
		}
	
		out <- list()
	
		
		for (it in station) {
			
			if (is.data.frame(exogen)) {
				
				exogen_loc <- exogen[,it]
				
			} else {
				
				exogen_loc <- exogen[[it]]
			}
			
			out[[it]] <- PrecipitationOccurenceModel(x=x[,it],exogen=exogen_loc,monthly.factor=monthly.factor,valmin=valmin,...)
			
		}
		
#		CCGamma(data, lag = 0, p0_v1 = NULL, p = NA, valmin = 0.5,
# 				nearPD = (lag >= 0), interval = c(-1, 1),
# 				tolerance = .Machine$double.eps, only.matrix = FALSE,
# 				return.value = NULL, null.gcorrelation = 1e-05, sample = NULL,
# 				origin = "1961-1-1", ...)
#
#
#		
#
		
		out$ccgamma <- CCGamma(x,lag=0,tolerance=tolerance_wilks,sample="monthly",only.matrix=TRUE,origin=origin)
		
		names(out$ccgamma) <- sprintf("month%02d",1:length(out$ccgamma))
	} else if (type=="logit"){
		
		
		
		
		
		out <- NULL
		
		
		
	}
	
	out$type <- multisite_type
	out$station <- station 
	
	class(out) <- "PrecipitationOccurenceMultiSiteModel"
	return(out)
	
	
	
}

