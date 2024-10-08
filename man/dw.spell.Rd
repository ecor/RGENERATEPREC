% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dw.spell.R
\name{dw.spell}
\alias{dw.spell}
\title{It calculates dry/wet spell duration.}
\usage{
dw.spell(
  data,
  valmin = 0.5,
  origin = "1961-1-1",
  extract = NULL,
  month = 1:12,
  melting.df = FALSE,
  from.start = FALSE,
  only.inner = FALSE
)
}
\arguments{
\item{data}{data frame R object containing daily precipitation time series for several gauges (one gauge time series per column).}

\item{valmin}{threshold precipitation value [mm] for wet/dry day indicator.}

\item{origin}{character string \code{"yyyy-mm-dd"} indicated the date of the first row of \code{"data"}.}

\item{extract}{string character referred to the state to be extracted, eg. \code{"dry"} or \code{"wet"}}

\item{month}{integer vectors containing the considered months. Default is \code{1:12} (all the year).}

\item{melting.df}{logical value. If it \code{TRUE} the output is melted into a data frame. Default is \code{FALSE}.}

\item{from.start}{logical value. If is \code{TRUE} the spell is referenced to its first day, if it is \code{FALSE} (default) the spell is referenced to its last date.}

\item{only.inner}{logical value. It is used in case \code{extract} is not \code{NULL}, if the value is \code{TRUE}, it extracts dry/wet spells completely inside the selected \code{month} period. Default is \code{FALSE}.}
}
\value{
Function returns a list of data frames containing the spell length expressed in days
}
\description{
It calculates dry/wet spell duration.
}
\examples{
 


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
dw_spell <- dw.spell(prec_mes,origin=origin)
dw_spell_dry <- dw.spell(prec_mes,origin=origin,extract="dry")

hist(dw_spell_dry$T0001$spell_length)


## Single Gauging Station

prec_mes <- prec_mes[,1]

origin <- paste(year_min,1,1,sep="-")
dw_spell <- dw.spell(prec_mes,origin=origin)
dw_spell_dry <- dw.spell(prec_mes,origin=origin,extract="dry")
dw_spell_dry_start <- dw.spell(prec_mes,origin=origin,extract="dry",
	month=5:8,from.start=TRUE) ## dry spell 
dw_spell_dry_start_2 <- dw.spell(prec_mes,origin=origin,extract="dry",
month=5:8,from.start=TRUE,only.inner=TRUE) ## dry spell 
## is referenced to the first day instead of the latest one as default. 

hist(dw_spell_dry[[1]]$spell_length)


}
