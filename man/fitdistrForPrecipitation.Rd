\name{fitdistrForPrecipitation}
\alias{fitdistrForPrecipitation}
\title{This function calculates the parameters of a parametric probability distribution by fitting daily precipitation data for each station and each month. It is a wrapper function for \code{\link{fitdistr}}}
\usage{
fitdistrForPrecipitation(data, dname = "exp", start = NULL,
  sample = "all", origin = "1961-1-1", valmin = 0.5, ...)
}
\arguments{
  \item{data}{dataset}

  \item{dname}{name of the pobability distribution to be
  fitted, e.g. \code{"exp"}.}

  \item{start}{initialization configuration of probability
  distribution parameters for \code{\link{fitdistr}}.}

  \item{sample}{character string indicated if the
  parameters must be estimated differently for subset of
  the year, e.g. monthly. Admitted values are \code{NULL}
  (Default), \code{"all"} or \code{"monthly"}.}

  \item{origin}{character string containing the date
  releted to the first row of \code{data}. Default is
  \code{"1961-1-1"}.}

  \item{valmin}{threshold precipitation value [mm] for
  wet/dry day indicator. If precipitation is lower than
  \code{valmin}, day is considered dry. Default is 0.5 mm.
  See
  \code{\link{continuity_ratio}},\code{\link{CCGamma}}.}

  \item{...}{further arguments for \code{\link{fitdistr}}}
}
\value{
a list containig the fitting parameters: S3
\code{fitdistrForEachStation-class} or
\code{fitdistrForEachStationForEachMonth-class}
(\code{sample=="monthly"}) object
}
\description{
This function calculates the parameters of a parametric
probability distribution by fitting daily precipitation
data for each station and each month. It is a wrapper
function for \code{\link{fitdistr}}
}
\note{
This function is taken by the previous package
\code{RMAWGEN}.
}
\examples{
library(RMRAINGEN)


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
prec_mes <- prec_mes[,1:3]

fit  <- fitdistrForPrecipitation(data=prec_mes,dname="exp",start=NULL,sample=NULL)

origin <- paste(year_min,1,1,sep="-")


fit_monthly  <- fitdistrForPrecipitation(data=prec_mes,dname="exp",
					start=NULL,sample="monthly",origin=origin)
fit_monthly_gamma  <- fitdistrForPrecipitation(data=prec_mes,dname="gamma",
						 start=NULL,sample="monthly",origin=origin)
}
\seealso{
\code{\link{fitdistr}},\code{\link{adddate}}
}

