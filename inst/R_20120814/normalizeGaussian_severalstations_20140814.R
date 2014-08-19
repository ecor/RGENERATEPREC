# TODO: Add comment
# 
# Author: ecor
###############################################################################
NULL
#' Wilsks Correctio for Precipitation Marginal Gausianization
#' 
#' Wrapper Gaussianization of \code{\link{normalizeGaussian_severalstations}} 
#' 
#' 
#' 
#' @param x see  \code{\link{normalizeGaussian_severalstations}}, e. g. precipitation depth values
#' @param data see  \code{\link{normalizeGaussian_severalstations}}
#' 
#' @param prec_tolerance tolerance used for precipitation value
#' @param iterations  number of iteration proposed for 'Wilks Gaussianization' 
#' @param force.precipitation.value logical value. If it is \code{TRUE} (Default) gaussianized values corresponding to precipitation days are forced to fit the observed precipitation values.    
#' @param seed seed used for random generation. 
#' 
#' @param valmin,tolerance see \code{\link{CCGamma}}
#' @param ... further arguments for \code{\link{normalizeGaussian_severalstations}}
#' 
#' 
#' @export 
#' 
#' 
#' 
#' 
#' 
#' 
#' @seealso \code{\link{normalizeGaussian_severalstations}},\code{\link{CCGamma}}
#' 
#' @examples
#' 
#'
#'library(RMRAINGEN2)
#'
#'
#'data(trentino)
#'
#'year_min <- 1961
#'year_max <- 1990
#'origin <- paste(year_min,1,1,sep="-")
#'
#'period <- PRECIPITATION$year>=year_min & PRECIPITATION$year<=year_max
#'station <- names(PRECIPITATION)[!(names(PRECIPITATION) %in% c("day","month","year"))]
#'prec_mes <- PRECIPITATION[period,station]
#'
#'
#'
#' ## removing nonworking stations (e.g. time series with NA)
#'accepted <- array(TRUE,length(names(prec_mes)))
#' names(accepted) <- names(prec_mes)
#'for (it in names(prec_mes)) {
#'	accepted[it]  <- (length(which(!is.na(prec_mes[,it])))==length(prec_mes[,it]))
#'}
#'
#'prec_mes <- prec_mes[,accepted]
#'valmin <- 0.5
#'prec_mes_gaussWilks <- WilksGaussianization(x=prec_mes, data=prec_mes,valmin=valmin,sample="monthly",extremes=TRUE,origin_x = origin, origin_data = origin,force.precipitation.value="both") 
#' prec_mes_gauss <- normalizeGaussian_severalstations(x=prec_mes, data=prec_mes,step=0,sample="monthly",extremes=TRUE,origin_x = origin, origin_data = origin)  
#### prec_mes_gauss_nullstep <- normalizeGaussian_severalstations(x=prec_mes, data=prec_mes,step=NULL,sample="monthly",extremes=TRUE,origin_x = origin, origin_data = origin)  
# CCGamma <- CCGamma(data=prec_mes, lag = 0,valmin=valmin,only.matrix=TRUE,tolerance=0.001)
#' prec_mes_ginv <- list()
#' prec_mes_ginv$unforced <- normalizeGaussian_severalstations(x=prec_mes_gaussWilks$unforced, data=prec_mes,step=0,sample="monthly",extremes=TRUE,origin_x = origin, origin_data = origin,inverse=TRUE)
#' prec_mes_ginv$forced <- normalizeGaussian_severalstations(x=prec_mes_gaussWilks$forced, data=prec_mes,step=0,sample="monthly",extremes=TRUE,origin_x = origin, origin_data = origin,inverse=TRUE)
#' 
#' str(prec_mes_ginv)
#' 
#'  plot(prec_mes[,1],prec_mes_ginv$unforced[,1])
#'  plot(prec_mes[,19],prec_mes_ginv$unforced[,19])
#' 
#' CCGamma <- CCGamma(data=prec_mes, lag = 0,valmin=valmin,only.matrix=TRUE,tolerance=0.001)
#' 
#' 
# plot(prec_mes[,5],prec_mes_ginv[,5])

# plot(prec_mes_gaussWilks[,1],prec_mes_gauss[,1])
# plot(prec_mes_gaussWilks[,2],prec_mes_gauss[,2])
# plot(prec_mes_gaussWilks[,19],prec_mes_gauss[,19])
# plot(prec_mes_gaussWilks[,10],prec_mes_gauss[,10])
#'  plot(cor(prec_mes_gaussWilks$unforced),cor(prec_mes_gaussWilks$forced))
#' abline(0,1)
#' 
#' 
#' VARselect(prec_mes_gaussWilks$forced)
#'
#' 
#' # u <- apply(prec_mes_ginv$forced,2,rank)/(nrow(prec_mes_ginv$forced)+1)
#' #copula <- normalCopula(dim=ncol(u), disp = "un",param=P2p(CCGamma))
#' #out <- fitCopula(copula, data=u, method = "ml" )
#'      #     start = NULL, lower = NULL, upper = NULL,
#'      #     optim.method = "BFGS", optim.control = list(maxit=1000),
#'      #     estimate.variance = TRUE, hideWarnings = TRUE)
# #### c("mpl", "ml", "itau", "irho"),
#
#' 
# ### ,step = valmin, prec = 10^-4, type = 3,
#		extremes = TRUE, sample = "monthly", origin_x = origin, origin_data = origin)
#
#
#
#' 
#
#
WilksGaussianization <- function (x,data=x,gauss=NULL,valmin=0.5,tolerance=0.001,prec_tolerance=c(0.05,2.5),iterations=20,force.precipitation.value=TRUE,seed=1234,shuffle=list(e1=1:ncol(x)),p=1,args_var=NULL,...) {
	
	if (!is.list(shuffle) | is.data.frame(shuffle)) shuffle <- list(v1=shuffle)
	
	if (length(shuffle)>1) {
		cc <- length(shuffle)
		for (i in 1:(cc-1)) { 
			
			gauss <- WilksGaussianization(x=x,data=data,gauss=gauss,valmin=valmin,tolerance=tolerance,iterations=iterations,force.precipitation.value=TRUE,seed=seed,shuffle=shuffle[i],...)
		
		}
		
		shuffle <- shuffle[cc]
		### TO FINISH .....
	}
	
	if (!is.list(shuffle)) shuffle <- list(v1=shuffle)
	
	set.seed(seed)
	lag=0
	names <- names(x)
	x <- x[,shuffle[[1]]]
	
	x[x<valmin] <- 0 
	data[data<valmin] <- 0
	
	
	
	
	if (is.null(gauss)) { 
		gauss <- normalizeGaussian_severalstations(x=x,data=data,step=0,...)
	} else if (!all.equal(names,names(gauss))) {
		
		stop("Error in WilksGaussianization: different names between gauss and x!!!")
	}
	
	
	ccgamma <- CCGamma(data=x, lag = lag,valmin=valmin,only.matrix=TRUE,tolerance=tolerance)
	### change ccgamma
	
	ccgamma_block <- CCGammaToBlockmatrix(data=x,lag=0,valmin=valmin,tolerance=tolerance,p=p+1)
	
	CCGamma0 <- as.blockmatrix(ccgamma_block[1:p,1:p],nrow=p,ncol=p)
	CCGamma1 <- as.blockmatrix(ccgamma_block[(1:p),(1:p)+1],nrow=p,ncol=p)
	CCGamma_1t <- t(CCGamma1)
	CCGamma_0t <- t(CCGamma0)
	A <- t(solve(CCGamma_0t,CCGamma_1t,symm.precond=TRUE)) ### A blockmatrix from Yule-Walker Equation
	
	
	eigenvs <- abs(eigen(as.matrix(A))$values)
	stable <- eigenvs<=1
	stable <- length(which(stable))==length(stable)
	ccgamma <- ccgamma_block[1,1]
	### END  change ccgamma
	chol_ccg <- t(as.matrix(chol(ccgamma)))
	
	out <- gauss
	for (iter in 1:iterations) {
		##print(iter)
		
		
		cor <- nearPD(cor(gauss),cor=TRUE)$mat
		chol_cor <- t(as.matrix(chol(cor))) 
		inv_chol_cor <- solve(chol_cor)
		
		
		
		for (r in 1:nrow(out)) {
			
			
			temp <- as.vector(gauss[r,])
			temp <- t(as.matrix(temp))
			
	
			xv <- inv_chol_cor %*% temp
			
			out[r,] <- chol_ccg %*% as.matrix(xv)
			
		}
	
			
			message <- sprintf("Iteration: %d on %d",iter,iterations)
			
			x_rec <- normalizeGaussian_severalstations(x=out, data=data,step=0,inverse=TRUE,...)
			if (length(prec_tolerance)==1) prec_tolerance[2] <- 50*prec_tolerance[2]
			cond <- abs(x-x_rec)<prec_tolerance[1]
			condhigh <- abs(x-x_rec)<prec_tolerance[2]
			points <- length(cond)
			adjusted <- length(which(!cond))
			adjustedhigh <- length(which((!condhigh) & (x>valmin)))
			message2 <- sprintf("Points adjusted: %d (%d) on %d",adjusted,adjustedhigh,points)
			
			print(message)  
			print(message2)
			gauss[cond] <- out[cond]
			

			#### INSERT AUTOREGRESSION WITH VARs 
#			if (p>0) { 
#				
#				###
#				args_var_used <- c(list(y=gauss,p=p),args_var)
#				str(gauss)
#				var <- do.call("VAR",args=args_var_used)
#				res <- as.data.frame(residuals(var))
#				res <- res[c(array(1,var$p),1:nrow(res)),]
#				res[1:var$p,] <- 0
#				##str(res)
#				
#				
#				if (!stable) {
#					print(eigenves)
#					stop("Error A eigenvalues greater than 1 in modulus!!!")
#					
#				}
#				
#			#	print(A)
#			#	print(eigen(as.matrix(A))$values)
#				xprev <- NULL
#				out <- generate(A,noise=res,names=names(out))
#				names(res) <- names(out) 
#				out <- out+res
#				#str(out)
#				#str(data)
#				x_rec <- normalizeGaussian_severalstations(x=out, data=data,step=0,inverse=TRUE,...)
#				
#				cond <- abs(x-x_rec)<prec_tolerance[1]
#				condhigh <- abs(x-x_rec)<prec_tolerance[2]
#				points <- length(cond)
#				adjusted <- length(which(!cond))
#				adjustedhigh <- length(which((!condhigh) & (x>valmin)))
#				message2 <- sprintf("AR correction: Points adjusted: %d (%d) on %d",adjusted,adjustedhigh,points)
#				
#				print(message)  
#				print(message2)
#				gauss[cond] <- out[cond]
#				
#			#	stop()
#			
#			}
			
		
	}
	
	
	
	
	
	
	
	
	if (force.precipitation.value==TRUE) {
		
	   cond <- x>valmin
	   out[cond] <- gauss[cond]
	   out <- out[,names]
		
	} else  if  (force.precipitation.value=="both") {
					
		gauss_unforced <- out
		
		out <- list()
		out$forced <- gauss_unforced
		out$unforced <- gauss_unforced
		
		cond <- x>valmin
		out$forced[cond] <- gauss[cond]
		
		out$forced <- out$forced[,names]
		out$unforced <- out$unforced[,names]
					
					
	}
		
###	}  
	return(out)
	
	
}


