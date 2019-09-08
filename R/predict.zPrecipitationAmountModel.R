NULL
#'
#' 
#' @param origin_newdata character string containing the date corresponding the first row of \code{newdata} 
#' @param precipitation.value.random.generation logical value. 
#' If it is \code{FALSE} (Default) the method \code{predict.PrecipitationAmountModel} returns conditioned random values, 
#' otherwise these values are converted to precipitation values  through their observed non-parametric distributions.
#' 
#' @export 
#' @method predict PrecipitationAmountModel
#' @aliases predict predict.PrecipitationAmountModel
#' @rdname predict
#'
#' 
#' 
predict.PrecipitationAmountModel <- function(object,newdata=NULL,origin_newdata=NA,precipitation.value.random.generation=FALSE,...) {
	
	if (is.null(origin_newdata)) origin_newdata <- NA
	
	if (is.na(origin_newdata)) origin_newdata <- object$origin
	
	###str(object$x)
	###str(object$valmin)
	
	if (is.null(newdata)) {
		newdata <- as.data.frame(as.matrix(object$x[,object$station]>=object$valmin))
	} else {
		
		newdata <- as.data.frame(newdata)
	}
		
		###
	if (length(object$station)==1) {
		
		names(newdata) <- object$station
		###
	} else {
		
		newdata <- newdata[,object$station]
	}
	

	sample <- object$sample
	
	if (is.null(sample)) sample <- NA
	
	if (sample=="monthly") {
		
		names <- names(newdata)
		
		newdata <- adddate(newdata,origin=origin_newdata)
		month <- factor(newdata$month)
		newdata <- as.data.frame(as.matrix(newdata[,names]))
		
		
		names(newdata) <- names
		
		newdata$month <- month
	}
	
	
	###newdata <- as.list(newdata)
	#str(newdata) ## ADDED EC20190410
	
	out <- lapply(X=object[object$station],FUN=function(x,nd=NULL,...) {
				
				id <- attr(x,"station")
				
				if (!is.null(nd)) {
				###	print(nrow(nd))
					out <- array(NA,nrow(nd))
				
					rows <- which(nd[,id]==TRUE)
					
					nd <- nd[rows,]
					nnd <- attr(x$terms,"term.labels")
					nd <- data.frame(nd[,nnd])
					names(nd) <- nnd
					
				    ####
					
					
					####
					
					
					out[rows] <- predict(x,newdata=nd,...)
					
					
					
				} else {
					
					out <- predict(x,newdata=nd,...)
				}
				
				
				
				
				
				
				
				return(out)
				
				
			},nd=newdata,...)
	
	
	
	
	if (precipitation.value.random.generation==TRUE)  {
		
		
		resid <- lapply(X=object[object$station],FUN=function(x) {sd(residuals(x),na.rm=TRUE)})
		names(resid) <- object$station
		
		out_generated <- lapply(X=resid,FUN=function(x,n) {rnorm(n,mean=0,sd=x)},n=length(out[[1]]))
		
		out <- as.data.frame(out)
		out_generated <- as.data.frame(out_generated)
		out_generated <- out_generated[,names(out)]
		
		outg <- out+out_generated
		
		xm <- object$x[,names(outg)]
		xm <- as.matrix(xm)
		drydays <- which(xm<object$valmin)
		xm[drydays] <- NA
		xm <- as.data.frame(xm)
		
		out <- normalizeGaussian_severalstations(x=outg,data=xm,mean=0,sd=1,inverse=TRUE,sample=sample,origin_x=origin_newdata,origin_data=object$origin)
		
		out_m <- as.matrix(out)
		out_m[is.na(out_m)] <- 0
		out <- as.data.frame(out_m)
		
		
		
		
	} else{
		
		out <- as.data.frame(out)
	}
	
	return(out)
	
}