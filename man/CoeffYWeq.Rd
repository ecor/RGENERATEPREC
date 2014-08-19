\name{CoeffYWeq}
\alias{CoeffYWeq}
\title{This function calculates  ...}
\usage{
CoeffYWeq(data = NULL, CCGamma0 = NULL, CCGamma1 = NULL, p = 1,
  sample = NULL, origin = "1961-01-01", ...)
}
\arguments{
  \item{data}{data frame or 'zoo' R object containing daily
  precipitation time series for several gauges (one gauge
  time series per column). See \code{\link{CCGamma}}.}

  \item{CCGamma0}{correlation block-matrix with lag of 0
  days. Object returned by
  \code{\link{CCGammaToBlockmatrix}}. If omitted,default is
  \code{NULL}, it is internally calculated.}

  \item{CCGamma1}{correlation block-matrix with lag of 1
  days. Object returned by
  \code{\link{CCGammaToBlockmatrix}}. If omitted,default is
  \code{NULL}, it is internally calculated.}

  \item{p}{numeric order $p$ of the auto-regeression, see
  \code{\link{CCGammaToBlockmatrix}}}

  \item{sample}{character string indicated if the
  coefficients must be estimated differently for subset of
  the year, e.g. monthly. Admitted values are \code{NULL}
  (Default), \code{"all"} or \code{"monthly"}.}

  \item{origin}{character string (yyyy-dd-mm) indicated the
  date of the first row of \code{"data"}. It is used if
  \code{data} and \code{sample} are not \code{NULL}.}

  \item{...}{other arguments of
  \code{\link{CCGammaToBlockmatrix}}}
}
\value{
A S3 object of class
\code{"YuleWalkerCoefficientBlockmatrices"} (or
\code{"YuleWalkerCoefficientBlockmatricesPerEachMonth"} in
case \code{sample="monthly"}) which is a list containing
the block matrices \code{A},\code{Sigma_u} of the
Yule-Walker Equation and the object \code{CCGammaInfo}
containing probabilities of no precipitation occurence and
returned by function \code{\link{CCGamma}} applied with
\code{lag=0}. In case \code{sample="monthly"}) functioion
return a
\code{"YuleWalkerCoefficientBlockmatricesPerEachMonth"}, i.
e. a list of \code{"YuleWalkerCoefficientBlockmatrices"}
for each month.
}
\description{
This function calculates ...
}
\note{
This function uses Yule-Walker equations for VAR to
estimate the coefficient block-matrices blockmatrix
\code{A} and \code{Sigma_u}. The input of this function are
the correletion block-matrices \code{CCGamma0} and
\code{CCGamma1}. If they are missing (and then \code{NULL})
, they are also calculated from the original dataset
(argument \code{data}). In this last case, the coefficients
can be estiomated differently for each monthly setting
\code{sample} equal to \code{"monthly"}.
}
\examples{
library(RMRAINGEN2)


data(trentino)

year_min <- 1961
year_max <- 1990

period <- PRECIPITATION$year>=year_min & PRECIPITATION$year<=year_max
station <- names(PRECIPITATION)[!(names(PRECIPITATION) \%in\% c("day","month","year"))]
prec_mes <- PRECIPITATION[period,station]

## removing nonworking stations (e.g. time series with NA)
accepted <- array(TRUE,length(names(prec_mes)))
names(accepted) <- names(prec_mes)
for (it in names(prec_mes)) {
		 accepted[it]  <- (length(which(!is.na(prec_mes[,it])))==length(prec_mes[,it]))
}

prec_mes <- prec_mes[,accepted]
## the dateset is reduced!!!
prec_mes <- prec_mes[,1:2]

# ## Not Run in the examples, uncomment to run the following line
# coeff <- CoeffYWeq(data=prec_mes,p=1,tolerance=0.001)

#
#
# Alternatively the coefficients of Vector Auto-Regressive Model
# can be separately calculated for each month

# ## Not Run in the examples, uncomment to run the following line
#origin <- paste(year_min,1,1,sep="-")
#
#

#coeff_monthly <- CoeffYWeq(data=prec_mes,p=1,tolerance=0.001,sample="monthly",origin=origin)
}
\author{
Emanuele Cordano
}
\seealso{
\code{\link{CCGammaToBlockmatrix}}
}

