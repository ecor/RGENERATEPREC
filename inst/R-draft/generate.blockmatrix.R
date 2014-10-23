NULL
#' generate
#' 
#' Implementation of \code{generate} method for \code{blockmatrix} S3 object.  
#' 
#' @param x \code{blockmatrix} S3 object. See \code{\link{CoeffYWeq}}
#' @param noise noise data frame. Default is \code{NULL}.
#' @param x.noise.gen R object to be used for noise generation with \code{\link{generate}} method  It is used if \code{noise} is set \code{NULL}. Default is \code{NULL}.
#' @param n number of generations requested. It is used if \code{noise} is set \code{NULL}. 
#' @param xprev null object or initial condition of the multivariate random process to be generated. Default is \code{NULL}. 
#' @param ... further arguments 
#' 
#' 
#' @title generate
# @name generate
# @rdname generate
#' @rdname generate.blockmatrix
#' @method generate blockmatrix
#' @S3method generate blockmatrix
#' @aliases generate generate.blockmatrix 
#' @importFrom RGENERATE generate
#' @export
# @import methods
#' @seealso \code{\link{generate}},\code{\link{CCGammaToBlockmatrix}},\code{\link{blockmatrix}}
#' 
#' @examples 
#' 
#' li
#' 
#' set.seed(125)
#' data(trentino)
#' 
#' year_min <- 1961
#' year_max <- 1990
#' 
#' period <- PRECIPITATION$year>=year_min & PRECIPITATION$year<=year_max
#' station <- names(PRECIPITATION)[!(names(PRECIPITATION) %in% c("day","month","year"))]
#' prec_mes <- PRECIPITATION[period,station]  
#' 
#' ## removing nonworking stations (e.g. time series with NA)
#' accepted <- array(TRUE,length(names(prec_mes)))
#' names(accepted) <- names(prec_mes)
#' for (it in names(prec_mes)) {
#' 		 accepted[it]  <- (length(which(!is.na(prec_mes[,it])))==length(prec_mes[,it]))
#' }
#'
#' prec_mes <- prec_mes[,accepted]
#' ## the dateset is reduced!!! 
#' prec_mes <- prec_mes[,1:2]
#' 
#' ## Not Run in the examples, uncomment to run the following lines 
#'  coeff <- CoeffYWeq(data=prec_mes,p=2,tolerance=0.001)
#' 
#' generation <- generate(coeff$A)
#' 
#' gp <- generation 
#' 
#' generation_test <- generate(coeff$A,gap.filling=gp,n=100)
#' 
#' 



generate.blockmatrix <- function(x,xprev=NULL,noise=NULL,n=10,x.noise.gen=NULL,is.VAR=TRUE,gap.filling=NULL,...) {
	
	
	out <- NULL
	p <- nrow(x)
	K <- nrow(x[1,1])
	#print(p)
	
	if (is.null(xprev)) xprev <- generate(x.noise.gen,n=p,K=K,...)
	if (!is.null(xprev)) { 
		
		
		if (nrow(xprev)>1) {
			xprevv <- as.vector(xprev[1,])
			names(xprevv) <- names(xprev)
			for (r in 2:nrow(xprev)) {
				temp <- as.vector(xprev[r,])
				
				names(temp) <- paste(names(temp),(r-1),sep=".l")
				xprevv <- cbind(xprevv,temp)
				
				
				
			}
			xprev <- as.vector(xprevv)
			xprev <- t(xprev)
		}	else if (nrow(xprev)==1) {
			
			xprev <- t(xprev)
		}
		
	
	} 
	
	if (length(xprev)!=p*K) { stop("Error in generate.blockamatix: xprev not consistent!!!")}
	
	
	
	if (p>1) count <- 1:p 
	
	
	if (!is.null(noise)) n <- nrow(noise)
	
	if (is.null(noise))  noise <- generate(x.noise.gen,n=n,K=K,...)
	
	
	if (!is.null(gap.filling)) {
		
		if (nrow(noise)!=nrow(gap.filling)) { 
			
					warning("Gap.filling and noise must have different number of rows!!! ")
		} 
		if (nrow(noise)>nrow(gap.filling))  {
			
					nn <- nrow(noise)
					ng <- nrow(gap.filling)
					gap.filling[(ng+1):nn,] <-  NA
					
					warning("Missing rows in gap.filling were filled with NAs")
			
		} else if (nrow(noise)<nrow(gap.filling)) {
			
					nn <- nrow(noise)
					ng <- nrow(gap.filling)
					gap.filling <-  gap.filling[1:nn,]
					warning("Exceeding rows in gap.filling were omitted")
			
			
		}
 		
		
		
	}
	
	
	
	noisep <- noise	
	gap.filling.p <- gap.filling
	if (p>1) {
		
		temp <- noisep
		
		temp_gp <- gap.filling.p
		for (l in 1:(p-1)) {
			### Inserire gap.filling 	
			###count <- (p+1):n-l
			temp  <- noise
			if (l>0) names(temp) <- paste(names(temp),l,sep=".l")
			if (l>0) temp <- temp*0
			noisep <- cbind(noisep,temp)
			
			if (!is.null(gap.filling)) {
				nrows <- nrow(temp_gp)
				if (l>0) temp_gp <- rbind(NA,temp_gp[-nrows,])
				if (l>0) names(temp_gp) <- names(temp)
				gap.filling.p <- cbind(gap.filling.p,temp_gp)
				
				
			}
			
				
		}
			
			####print(noisep) str
			###### TO DOOO
			### GENERARE DA P+1 IN POI ......
			
	}
	
	out <- generate(as.matrix(x),noise=noisep,names=names(noisep),xprev=xprev,gap.filling=gap.filling.p)

	
	
	###out <- rbind(out[1:p,]*NA,out)
	rownames(out) <- rownames(noise)
	if (is.VAR) out <- out[,1:K]
		
	return(out)
} 

