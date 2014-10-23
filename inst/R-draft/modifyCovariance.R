NULL
#' modifyCovariance 
#' 
#' It transforms the covariance matrix through Cholevsky trasformation.
#' 
#' @param x 
#' @param covmatrix covariance matrix for function output
#' @param covchol triangular matrix obtained by Cholevsky Decomposition of \code{covmatrix} argument
#' @param ...  further arguments for function \code{cov}
#' 
#' @export 
#' 
#' @examples 
#' 
#' cov <- array(0.5,c(3,3))
#' diag(cov) <- 1 
#' 
#' print(cov)
#' 
#' df <- generate(NULL,n=1000,cov=cov)
#' 
#' cov2 <- cov
#' cov2[3,1] <- 0.9
#' cov2 <- (cov2+t(cov2))/2
#'
#' 
#' df2 <- modifyCovariance(df,cov.matrix=cov2)
#' 
#' str(df2)
#' 
#' print(cov(df2))
#' print(cov(df))
#' 
#' 
#' 
#' 
modifyCovariance <- function(x,covmatrix=diag(3),covchol=t(as.matrix(chol(covmatrix))),preserve.names=TRUE,...) {
	
	
	names <- names(x)
	
	cov_x <- cov(x,...)

	cov_x <- nearPD(cov_x,keepDiag=TRUE,doSym=TRUE)$mat

	chol_cov_x <- t(as.matrix(chol(cov_x))) 
	
	inv_chol_cov <- solve(chol_cov_x)
	
	x <- t(as.matrix(x)) 
	
	
	xv <- inv_chol_cov %*% x
	
	out  <- covchol %*% as.matrix(xv)
	
	####out[r,] <- chol_ccg %*% as.matrix(xv)
	
	out <- as.data.frame(t(out))
	print(names)
	if (preserve.names==TRUE) names(out) <- names 
	###str(out)
	return(out)
	
}

#
#
###
###
###
#
#stable <- eigenvs<=1
#stable <- length(which(stable))==length(stable)
#ccgamma <- ccgamma_block[1,1]
#### END  change ccgamma
#chol_ccg <- t(as.matrix(chol(ccgamma)))
#
#out <- gauss
#for (iter in 1:iterations) {
#	##print(iter)
#	
#	
#	cor <- nearPD(cor(gauss),cor=TRUE)$mat
#	chol_cor <- t(as.matrix(chol(cor))) 
#	inv_chol_cor <- solve(chol_cor)
#	
#	
#	
#	for (r in 1:nrow(out)) {
#		
#		
#		temp <- as.vector(gauss[r,])
#		temp <- t(as.matrix(temp))
#		
#		
#		xv <- inv_chol_cor %*% temp
#		
#		out[r,] <- chol_ccg %*% as.matrix(xv)
#		
#	}
#	
#	