\name{omega_root}
\alias{omega_root}
\title{This is the target function whose zero is searched to crete the inverse function of  \code{\link{omega}}.}
\usage{
omega_root(x = 0.5, p0_v1 = 0.5, p0_v2 = 0.5, p00 = p0_v1 * p0_v2,
  correlation = NA)
}
\arguments{
  \item{x}{value of expected correlation between the
  corresponding Gaussian-distributed variables}

  \item{p0_v1,p0_v2}{probablity of no precipitatin
  occurences for the v1 and v2 time series respectively.}

  \item{p00}{probability of no precipitation occurence in
  both v1 and v2 simultanously returned by
  \code{\link{omega}}}

  \item{correlation}{numerical value. DEfault is \code{NA}.
  Binary correlation retured by \code{\link{omega}} when
  the argumet \code{correlation=TRUE}}
}
\value{
the value \code{p00-omega(x=x,p0_v1=p0_v1,p0_v2=p0_v2)} or
\code{correlation-omega(x=x,p0_v1=p0_v1,p0_v2=p0_v2)} (if
\code{correlation} is not \code{NA})
}
\description{
This is the target function whose zero is searched to crete
the inverse function of \code{\link{omega}}.
}
\note{
This function makes use of normal copula
}
\examples{
rho <- 0.4
p00 <- omega(x=rho,p0_v1=0.5,p0_v2=0.5)
omega_root(x=rho,p0_v1=0.5,p0_v2=0.5,p00=p00)
}
\author{
Emanuele Cordano
}
\seealso{
\code{\link{normalCopula}},\code{\link{pcopula}},\code{\link{omega}},\code{\link{omega_inv}}
}

