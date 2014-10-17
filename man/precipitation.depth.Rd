\name{precipitation.depth}
\alias{precipitation.depth}
\title{Generation of Precipitation Amount}
\usage{
precipitation.depth(x, origin = "1961-1-1", par = NULL, ...)
}
\arguments{
  \item{x}{object of precipitation probability returned by
  \code{\link{precipitation.probability}}}

  \item{origin}{character string containing the date
  releted to the first row of \code{x}. Default is
  \code{"1961-1-1"}.}

  \item{par}{\code{fitdistrForEachStationForEachMonth-class}
  or \code{fitdistrForEachStation-class} object returned by
  \code{\link{fitdistrForPrecipitation}}}

  \item{...}{further arguments}
}
\description{
Generetion of daily precipitation amount taking into accont
a previous generation of precipitation occurence with
\code{\link{generate}}
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

probs <- precipitation.probability(x=x,data=prec_mes,valmin=0.5,origin_x=origin_x,origin_data=origin_data,sample="monthly")






fit_monthly   <- fitdistrForPrecipitation(data=prec_mes,dname="gamma",
                 start=NULL,sample="monthly",origin=origin)


prec_gen_2 <- precipitation.depth(x=probs,origin=origin,par=fit_monthly)


 when <- c("Dec", "Jan", "Feb")
 prec_mes_when <- extractmonths(prec_mes, when = when, year = NULL,origin = origin_data)
 prec_gen_when <- extractmonths(prec_gen_2, when = when, year = NULL,origin = origin_x)

 qqplot(prec_mes_when[,1],prec_gen_when[,1])
 abline(0,1)


## TO DO
}
\seealso{
\code{\link{qqfit}},\code{\link{fitdistrForPrecipitation}},\code{\link{generate}},\code{\link{precipitation.probability}}
}

