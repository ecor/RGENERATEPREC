# TODO: Add comment
# 
# Author: ecor
###############################################################################
NULL
#' Rearrange ...
#' 
#' Rearrange generated precipitation value according to a given probability distribution  
#' 
#' @param gen generated sample
#' @param obs observation data
#' @param qfun quantile probability function, e. g. \code{\link{qnorm}}. Default is \code{NULL}
#' @param valmin minimum admitted value for precipitation
#' @param sample character string. It is \code{"monthly"}, the rearrangment is made separately per each month.
#' @param origin,origin_obs,origin_gen character string containg the date of the first row
#' @param random.generation logical value ...
#' @param extremes logical value
#' @param ... furher argument for \code{qfun} or \code{quantile}
#' 
#' @details The rearrangement is made according to  \code{qfun} if it not \code{NULL}. Otherwise the quantiles are calculeted from \code{obs} through \code{\link{quantile}}
#' 
#' 
#' @export 
#' 
#' 
#' @examples 
#' set.seed(122)
#' N <- 100
#' valmin=0.5
#' obs <- runif(N,min=-1,max=1)
#' indexo <- which(obs<0)
#' obs[-indexo] <- 0
#' obs[indexo] <- rexp(length(indexo),rate=1.5)
#' 
#' gen <- runif(N,min=-1,max=1)
#' indexg <- which(gen<0)
#' gen[-indexg] <- 0
#' gen[indexg] <- rexp(length(indexg),rate=2)
#' 

#' qqplot(obs[obs>valmin],gen[gen>valmin])
#' abline(0,1)
#' 
#' out <- rearrange.precipitation.generation(gen=gen,obs=obs,valmin=0.5)
#' 
#' qqplot(obs[obs>valmin],out[out>valmin])
#' abline(0,1)
#' 

rearrange.precipitation.generation <- function(gen,obs=NULL,qfun=NULL,valmin=0.5,sample=NULL,origin_obs=origin,origin_gen=origin,origin="1961-01-01",random.generation=TRUE,extremes=TRUE,...) {
	
	if (is.null(sample)) sample="all"
	
	

	if (sample=="monthly") {
			##str(gen)
			
			months <- 1:12
			gen <- as.data.frame(gen)
			obs <- as.data.frame(obs)
			out <- gen
			names(out) <- names(gen)
			
			obs <- adddate(as.data.frame(obs),origin=origin_obs)
			gen <- adddate(as.data.frame(gen),origin=origin_gen)
			names <- names(out)[names(out) %in% names(obs)]
			for (m in months){
			#	str(out)
			#	str(gen)
			#	print(m)str(gen$month==m)
				out[gen$month==m,names] <- rearrange.precipitation.generation(gen=gen[gen$month==m,names],obs=obs[obs$month==m,names],valmin=valmin,random.generation=random.generation,extremes=extremes,sample="all",...)
			#	str(out)
			#	str(gen)
				
			}
				
				
		##	for ()
			
			
			return(out)
		
	}
	
	if (is.data.frame(gen)) {
		if (ncol(gen)>1) {
			
			out <- gen 
			
			
			names <- names(out)[names(out) %in% names(obs)]
			
			for (it in names) {
				
				out[,it] <- rearrange.precipitation.generation(gen=gen[,it],obs=obs[,it],valmin=valmin,random.generation=random.generation,extremes=extremes,sample="all",...)
				
			}
		
			
			return(out)
			
		} else {
			
			gen <- as.vector(gen)
		}	
		
		
		
	}
	
	out <- gen 
	
	genv <- gen[gen>valmin]
	obsv <- obs[obs>valmin]
	outv <- genv
	
	n <- length(genv)
	
	if (random.generation==FALSE) {
		
		qgenv <- ecdf(genv)(genv)
		
		
	} else {
		
		qgenv <- runif(n)
	}
	
	if (extremes==TRUE) qgenv <- qgenv*(n/(n+1))
	
	if (is.null(qfun)) {
		
		
		 outv <- quantile(x=obsv,prob=qgenv,...)
		 
	} else {
		
		
		outv <- qfun(qgenv,...)
	}
	
	out[out>valmin] <- outv
	return(out)
}


