\name{WilksGaussianization}
\alias{WilksGaussianization}
\title{Wilsks Correctio for Precipitation Marginal Gausianization}
\usage{
WilksGaussianization(x, data = x, gauss = NULL, valmin = 0.5,
  tolerance = 0.001, prec_tolerance = c(0.05, 2.5), iterations = 20,
  force.precipitation.value = TRUE, seed = 1234, shuffle = list(e1 =
  1:ncol(x)), p = 1, args_var = NULL, ...)
}
\arguments{
  \item{x}{see
  \code{\link{normalizeGaussian_severalstations}}, e. g.
  precipitation depth values}

  \item{data}{see
  \code{\link{normalizeGaussian_severalstations}}}

  \item{prec_tolerance}{tolerance used for precipitation
  value}

  \item{iterations}{number of iteration proposed for 'Wilks
  Gaussianization'}

  \item{force.precipitation.value}{logical value. If it is
  \code{TRUE} (Default) gaussianized values corresponding
  to precipitation days are forced to fit the observed
  precipitation values.}

  \item{seed}{seed used for random generation.}

  \item{valmin,tolerance}{see \code{\link{CCGamma}}}

  \item{...}{further arguments for
  \code{\link{normalizeGaussian_severalstations}}}
}
\description{
Wrapper Gaussianization of
\code{\link{normalizeGaussian_severalstations}}
}
\examples{
library(RGENERATEPREC)


data(trentino)

year_min <- 1961
year_max <- 1990
origin <- paste(year_min,1,1,sep="-")

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
prec_mes <- prec_mes[,1:5]

valmin <- 0.5
prec_mes_gaussWilks <- WilksGaussianization(x=prec_mes, data=prec_mes,valmin=valmin,sample="monthly",extremes=TRUE,origin_x = origin, origin_data = origin,force.precipitation.value="both")

##prec_mes_ginv <- list()
prec_mes_ginv  <- normalizeGaussian_severalstations(x=prec_mes_gaussWilks, data=prec_mes,step=0,sample="monthly",extremes=TRUE,origin_x = origin, origin_data = origin,inverse=TRUE)
ccgamma <- CCGamma(data=prec_mes, lag = 0,valmin=valmin,only.matrix=TRUE,tolerance=0.001)
ccgamma_wilks <- cor(prec_mes_gaussWilks,use="pairwise.complete.obs")


plot(prec_mes[,2],prec_mes_ginv[,2])

plot(ccgamma,ccgamma_wilks)
abline(0,1)
}
\seealso{
\code{\link{normalizeGaussian_severalstations}},\code{\link{CCGamma}}
}

