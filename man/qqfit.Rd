\name{qqfit}
\alias{qqfit}
\title{Quantile-Quantile Fit between observed data and a parametric probability distribution.}
\usage{
qqfit(x, FUN = qexp, par = list(), use.x = FALSE)
}
\arguments{
  \item{x}{observed data}

  \item{FUN}{quantile probability function, e.g.
  \code{\link{qexp}} or \code{"exp"}}

  \item{par}{list of parameters}

  \item{use.x}{logical. Default is \code{FALSE}. If it is
  \code{TRUE}, the quantiles correspond to \code{x} and
  function \code{\link{ecdf}} is not used.}
}
\description{
Quantile-Quantile Fit between observed data and a
parametric probability distribution.
}
\examples{
x <- rexp(100,rate=2)
y2 <- qqfit(x,FUN=qexp,list(rate=2))
y4 <- qqfit(x,FUN=qexp,list(rate=4))

qqplot(x,y2)
abline(0,1)

qqplot(x,y4)
abline(0,1)
}

