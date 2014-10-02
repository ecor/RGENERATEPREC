\name{rearrange.precipitation.generation}
\alias{rearrange.precipitation.generation}
\title{Rearrange ...}
\usage{
rearrange.precipitation.generation(gen, obs, qfun = NULL, valmin = 0.5,
  sample = NULL, origin_obs = origin, origin_gen = origin,
  origin = "1961-01-01", random.generation = TRUE, extremes = TRUE, ...)
}
\arguments{
  \item{gen}{generated sample}

  \item{obs}{observation data}

  \item{qfun}{quantile probability function, e. g.
  \code{\link{qnorm}}. Default is \code{NULL}}

  \item{valmin}{minimum admitted value for precipitation}

  \item{sample}{character string. It is \code{"monthly"},
  the rearrangment is made separately per each month.}

  \item{origin,origin_obs,origin_gen}{character string
  containg the date of the first row}

  \item{random.generation}{logical value ...}

  \item{extremes}{logical value}

  \item{...}{furher argument for \code{qfun} or
  \code{quantile}}
}
\description{
Rearrange generated precipitation value according to a
given probability distribution
}
\details{
The rearrangement is made according to \code{qfun} if it
not \code{NULL}. Otherwise the quantiles are calculeted
from \code{obs} through \code{\link{quantile}}
}
\examples{
set.seed(122)
N <- 100
valmin=0.5
obs <- runif(N,min=-1,max=1)
indexo <- which(obs<0)
obs[-indexo] <- 0
obs[indexo] <- rexp(length(indexo),rate=1.5)

gen <- runif(N,min=-1,max=1)
indexg <- which(gen<0)
gen[-indexg] <- 0
gen[indexg] <- rexp(length(indexg),rate=2)

qqplot(obs[obs>valmin],gen[gen>valmin])
abline(0,1)

out <- rearrange.precipitation.generation(gen=gen,obs=obs,valmin=0.5)

qqplot(obs[obs>valmin],out[out>valmin])
abline(0,1)
}

