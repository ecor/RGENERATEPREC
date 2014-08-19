\name{omega}
\alias{omega}
\title{This function finds the bivariate joint probability or the binary correlation from the corresponding Gaussian correlation \code{x}}
\usage{
omega(x = 0.5, p0_v1 = 0.5, p0_v2 = NA, correlation = FALSE)
}
\arguments{
  \item{x}{value of expected correlation between the
  corresponding Gaussian-distributed variables}

  \item{p0_v1,p0_v2}{probability of no precipitation
  occurences for the v1 and v2 time series respectively.
  See \code{Notes}.}

  \item{correlation}{logical numeric value. Default is
  \code{FALSE}. If \code{TRUE} the function returns the
  binary correlation like eq. 6 of Mhanna, et al.,2011.}
}
\value{
probability of no precipitation occurence in both v1 and v2
simultaneously. It is a matrix if \code{x} is a matrix.
}
\description{
This function finds the bivariate joint probability or the
binary correlation from the corresponding Gaussian
correlation \code{x}
}
\note{
This function makes use of normal copula. A graphical
introduction to this function (with its inverse) makes is
present in the following URL references:
\url{http://onlinelibrary.wiley.com/doi/10.1002/joc.2305/abstract}
and
\url{http://www.sciencedirect.com/science/article/pii/S0022169498001863}
(See fig. 1 and par. 3.2) If the argument \code{p0_v2}, the
two marginal probabily values must be given as a vector
through the argument \code{p0_v1}:
\code{p0_v1=c(p0_v1,p0_v2)} . In case \code{x} is a
correlation/covariance matrix the marginal probabilities
are given as a vector through the argument \code{p0_v1}.
}
\examples{
rho <- 0.4
p00 <- omega(x=rho,p0_v1=0.5,p0_v2=0.5)
cor00 <- omega(x=rho,p0_v1=0.5,p0_v2=0.5,correlation=TRUE)
}
\author{
Emanuele Cordano
}
\seealso{
\code{\link{normalCopula}},\code{\link{pcopula}}
}

