\name{random.precipitation.values}
\alias{random.precipitation.values}
\title{Extract randomly precipitetion amounts from precipitation occuence time-series}
\usage{
random.precipitation.values(gen, obs, valmin = 1, prec = 10^-4,
  station = names(gen), sample = "monthly", method = c("pearson",
  "kendall", "spearman"), origin = "1961-1-1",
  use.gaussianized.precipitation.value.correlation = TRUE, ...)
}
\arguments{
  \item{gen}{generated ocurrence time series (data frame)}

  \item{obs}{observed precipitation amount time series
  (data frame)}

  \item{station}{string vector containing station
  identification codes}

  \item{valmin}{maximum admitted value of precipitation
  depth}

  \item{method}{see \code{\link{cor}}}

  \item{origin}{date of the day referred by he first row of
  \code{x}.}

  \item{sample}{character string. If it is \code{"monthly"}
  (Default), the corralaton matrix is calculated per each
  month.}

  \item{use.gaussianized.precipitation.value.correlation}{logical
  value. If it is \code{TRUE} (Default), precipitation
  generation takes into account the correlations given by
  \code{\link{gaussianized.precipitation.value.correlation}}.}

  \item{prec,...}{further agruments for
  \code{\link{normalizeGaussian_severalstations}}}
}
\description{
Extract randomly precipitetion amounts from precipitation
occuence time-series
}
\examples{
library(RGENERATEPREC)

set.seed(1245)

data(trentino)

year_min <- 1961
year_max <- 1990

origin <- paste(year_min,1,1,sep="-")
end <- paste(year_max,12,31,sep="-")

period <- PRECIPITATION$year>=year_min & PRECIPITATION$year<=year_max
period_temp <- TEMPERATURE_MAX$year>=year_min & TEMPERATURE_MAX$year<=year_max

prec_mes <- PRECIPITATION[period,]
Tx_mes <- TEMPERATURE_MAX[period_temp,]
Tn_mes <- TEMPERATURE_MIN[period_temp,]
accepted <- array(TRUE,length(names(prec_mes)))
names(accepted) <- names(prec_mes)
for (it in names(prec_mes)) {
	acc <- TRUE
	acc <- (length(which(!is.na(Tx_mes[,it])))==length(Tx_mes[,it]))
	acc <- (length(which(!is.na(Tn_mes[,it])))==length(Tn_mes[,it])) & acc
	accepted[it]  <- (length(which(!is.na(prec_mes[,it])))==length(prec_mes[,it])) & acc

}

valmin <- 1.0
prec_mes <- prec_mes[,accepted]



Tx_mes <- Tx_mes[,accepted]
Tn_mes <- Tn_mes[,accepted]
prec_occurence_mes <- prec_mes>=valmin

station <- names(prec_mes)[!(names(prec_mes) \%in\% c("day","month","year"))]
### MultiSite Generation


station <- station[1:2]
exogen <- Tx_mes[,station]-Tn_mes[,station]

months <- factor(prec_mes$month)

model_multisite <- PrecipitationOccurenceMultiSiteModel(x=prec_mes[,station],exogen=exogen,origin=origin,multisite_type="wilks")


obs_multisite <- prec_mes[,station]

gen_multisite_occ <- generate(model_multisite,exogen=exogen,origin=origin,end=end)


gen_multisite <- random.precipitation.values(gen=gen_multisite_occ,obs=obs_multisite,valmin=valmin,station=station)
}
\seealso{
\code{\link{gaussianized.precipitation.value.correlation}}.
}

