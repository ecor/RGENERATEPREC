
NULL

#' predict method 
#' 
#' 
#' @param object model returned by \code{\link{PrecipitationOccurenceModel}}
#' @param newdata predictor or exogenous variables
#' @param type see \code{\link{predict.glm}}. DEfault is \code{"response"}
#' @param previous logical vector containing previously occurred states
#' @param ... further arguments 
#' 
#' @seealso \code{\link{predict.glm}},\code{\link{PrecipitationOccurenceModel}}
#' @export 
#' @method predict PrecipitationOccurenceModel
#' @S3method predict PrecipitationOccurenceModel
#' @aliases predict generate.PrecipitationOccurenceModelx 
##### @importFrom predict stats
#' 
#' @examples
#' 
#' library(RGENERATEPREC)
#' 
#' data(trentino)
#' 
#' year_min <- 1961
#' year_max <- 1990
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

#' it <- station[2]
#' vect <- Tx_mes[,it]-Tn_mes[,it]
#' months <- factor(prec_mes$month)

#' model <- PrecipitationOccurenceModel(x=prec_mes[,it],exogen=vect,monthly.factor=months)
#' 
#' probs <- predict(model)
#' 
#' nday <- 3.0
#' vect_new <- array(1.0,nday)
#' months_new <- array(1,nday)
#' row_test <- 2000:2007
#' newdata <- model$predictor[row_test,]
#' probs2 <- predict(model,newdata=newdata)
#' 
#' probs[row_test]==probs2
#' ###
#' 
#' 
#' 
#' 


predict.PrecipitationOccurenceModel <- function(object,newdata=NULL,type="response",previous=NULL,...) {
	
	
	if (!is.null(previous)) {
		
		previous <- as.vector(previous)
		
		previous[!is.logical(previous)] <- previous[!is.logical(previous)]>=object$valmin
		
		if(is.null(newdata)) newdata <- object$predictor
		
		
		for (l in 1:object$p) {
			
			label <- sprintf("x_l%02d",l)
			
			newdata[,label] <- previous[l]
			
			
		}
		
		
		
		
		
		
	}
	
	out <- NULL
	out <- predict(object$glm,newdata=newdata,type=type,...)
	if (is.null(newdata) & (object$p>0)) {
		
		out <- c(array(NA,object$p),out)
		
	}
	return(out)
}




