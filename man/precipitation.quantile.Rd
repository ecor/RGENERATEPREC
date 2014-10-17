\name{precipitation.quantile}
\alias{precipitation.quantile}
\title{precipitation.quantile}
\usage{
precipitation.quantile(x, data, valmin = 0.5, sample = NULL,
  origin_x = origin, origin_data = origin, origin = NULL, ...)
}
\arguments{
  \item{x}{gaussianized variable}

  \item{data}{referred variable. e.g. data frame of
  observation}

  \item{valmim}{minimum value acceptable of precipitation,
  e.g. 0.5 millimeters}

  \item{sample}{see
  \code{\link{normalizeGaussian_severalstations}}}

  \item{...}{}
}
\description{
This function produces time series (data frames) of daily
precipititaton quantiles from simulated continous gaussian
variables
}
\examples{
library(RGENERATEPREC)
#
set.seed(1233)
data(trentino)
#
year_min <- 1987
year_max <- 1990
#
period <- PRECIPITATION$year>=year_min & PRECIPITATION$year<=year_max
station <- names(PRECIPITATION)[!(names(PRECIPITATION) \%in\% c("day","month","year"))]
prec_mes <- PRECIPITATION[period,station]
#
### removing nonworking stations (e.g. time series with NA)
accepted <- array(TRUE,length(names(prec_mes)))
names(accepted) <- names(prec_mes)
for (it in names(prec_mes)) {
		 accepted[it]  <- (length(which(!is.na(prec_mes[,it])))==length(prec_mes[,it]))
}

prec_mes <- prec_mes[,accepted]
### the dateset is reduced!!!

prec_mes <- prec_mes[,1:2]

origin_data <- paste(year_min,1,1,sep="-")
origin_x <- origin_data

x <- prec_mes*NA
x[,] <- rnorm(length(as.matrix(prec_mes)))

out <- precipitation.quantile(x=x,data=prec_mes,valmin=0.5,origin_x=origin_x,origin_data=origin_data,sample="monthly")



#
}
\seealso{
\code{\link{pnorm}}
}

