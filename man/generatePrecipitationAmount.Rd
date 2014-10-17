\name{generatePrecipitationAmount}
\alias{generatePrecipitationAmount}
\title{Generation of Precipitation Amount}
\usage{
generatePrecipitationAmount(x, origin = "1961-1-1", par = NULL, ...)
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
## TO DO
}
\seealso{
\code{\link{qqfit}},\code{\link{fitdistrForPrecipitation}},\code{\link{generate}}
}

