\name{rerrange.precipitation.generation}
\alias{rerrange.precipitation.generation}
\title{Rearrange generated precipitation value according to a given probability distribution}
\usage{
rerrange.precipitation.generation(gen, obs, qfun = NULL, valmin = 0.5,
  sample = NULL, origin = "1961-01-01", random.generation = TRUE,
  extremes = TRUE, ...)
}
\description{
Rearrange generated precipitation value according to a
given probability distribution
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
}

