\name{generate.blockmatrix}
\alias{generate}
\alias{generate.blockmatrix}
\title{generate}
\usage{
\method{generate}{blockmatrix}(x, xprev = NULL, noise = NULL, n = 10,
  x.noise.gen = NULL, is.VAR = TRUE, gap.filling = NULL, ...)
}
\arguments{
  \item{x}{\code{blockmatrix} S3 object. See
  \code{\link{CoeffYWeq}}}

  \item{noise}{noise data frame. Default is \code{NULL}.}

  \item{x.noise.gen}{R object to be used for noise
  generation with \code{\link{generate}} method It is used
  if \code{noise} is set \code{NULL}. Default is
  \code{NULL}.}

  \item{n}{number of generations requested. It is used if
  \code{noise} is set \code{NULL}.}

  \item{xprev}{null object or initial condition of the
  multivariate random process to be generated. Default is
  \code{NULL}.}

  \item{...}{further arguments}
}
\description{
generate
}
\details{
Implementation of \code{generate} method for
\code{blockmatrix} S3 object.
}
\examples{
li

set.seed(125)
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

## Not Run in the examples, uncomment to run the following lines
 coeff <- CoeffYWeq(data=prec_mes,p=2,tolerance=0.001)

generation <- generate(coeff$A)

gp <- generation

generation_test <- generate(coeff$A,gap.filling=gp,n=100)
}
\seealso{
\code{\link{generate}},\code{\link{CCGammaToBlockmatrix}},\code{\link{blockmatrix}}
}

