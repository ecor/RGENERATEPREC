\name{dw.spell}
\alias{dw.spell}
\title{It calculates dry/wet spell duration.}
\usage{
dw.spell(data, valmin = 0.5, origin = "1961-1-1", extract = NULL,
  month = 1:12)
}
\arguments{
  \item{data}{data frame R object containing daily
  precipitation time series for several gauges (one gauge
  time series per column).}

  \item{valmin}{threshold precipitation value [mm] for
  wet/dry day indicator.}

  \item{origin}{character string \code{"yyyy-mm-dd"}
  indicated the date of the first row of \code{"data"}.}

  \item{extract}{string charecter referred to the state to
  be extracted, eg. \code{"dry"} or \code{"wet"}}

  \item{month}{integer vectors containing the considered
  months. Default is \code{1:12} (all the year).}
}
\value{
Function returns a list of data frames containing the spell
length expressed in days
}
\description{
It calculates dry/wet spell duration.
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

origin <- paste(year_min,1,1,sep="-")
dw.spell <- dw.spell(prec_mes,origin=origin)
dw.spell.dry <- dw.spell(prec_mes,origin=origin,extract="dry")

hist(dw.spell.dry$T0001$spell_length)
}

