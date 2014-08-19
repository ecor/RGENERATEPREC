\name{modifyCovariance}
\alias{modifyCovariance}
\title{modifyCovariance}
\usage{
modifyCovariance(x, covmatrix = diag(3),
  covchol = t(as.matrix(chol(covmatrix))), preserve.names = TRUE, ...)
}
\arguments{
  \item{x}{}

  \item{covmatrix}{covariance matrix for function output}

  \item{covchol}{triangular matrix obtained by Cholevsky
  Decomposition of \code{covmatrix} argument}

  \item{...}{further arguments for function \code{cov}}
}
\description{
It transforms the covariance matrix through Cholevsky
trasformation.
}
\examples{
cov <- array(0.5,c(3,3))
diag(cov) <- 1

print(cov)

df <- generate(NULL,n=1000,cov=cov)

cov2 <- cov
cov2[3,1] <- 0.9
cov2 <- (cov2+t(cov2))/2


df2 <- modifyCovariance(df,cov.matrix=cov2)

str(df2)

print(cov(df2))
print(cov(df))
}

