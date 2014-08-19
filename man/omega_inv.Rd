\name{omega_inv}
\alias{omega_inv}
\title{This function is the inverse of \code{\link{omega}} function}
\usage{
omega_inv(p0 = NULL, p0_v1 = 0.5, p0_v2 = p0_v1, p00 = p0_v1 * p0_v2,
  correlation = NA, only.value = TRUE, interval = c(-1, 1),
  tolerance = 0.001, nearPD = TRUE, force.independence = TRUE, ...)
}
\arguments{
  \item{p0}{matrix of joint probabilities. Default is
  \code{NULL}, otherwise functions returns a matrix with
  values}

  \item{p0_v1,p0_v2}{probablity of no precipitatin
  occurences for the v1 and v2 time series respectively.}

  \item{p00}{probability of no precipitation occurence in
  both v1 and v2 simultanously returned by
  \code{\link{omega}}}

  \item{only.value}{logical value. If \code{TRUE} (Default)
  the only Gaussian correletion (\code{x} input variable of
  \code{\link{omega}}) is returned, otherwise the complete
  output of \code{\link{uniroot}} is returned.}

  \item{correlation}{numerical value. DEfault is \code{NA}.
  Binary correlation retured by \code{\link{omega}} when
  the argumet \code{correlation=TRUE} (see
  \code{\link{omega_root}})}

  \item{interval}{see \code{interval} option of
  \code{\link{uniroot}}. Default is \code{c(-1,1)}.}

  \item{tolerance}{tolerance (numeric) parameter used for
  comparisons with the extreme value of marginal
  probabilities. Default is 0.001.}

  \item{nearPD}{logical. If \code{TRUE} (Default) a
  positive-definite correlation matrix is returned by
  applying \code{\link{nearPD}} in case \code{p0} is a
  matrix and not \code{NULL}.}

  \item{force.independence}{logical value. Default is
  \code{TRUE}. If it is \code{TRUE}, no negative corelation
  is considered and negative values of correletion are
  forced to be 0 (independence).}

  \item{...}{further arguments for \code{\link{uniroot}}}
}
\value{
value of expected correlation between the corresponding
Gaussian-distributed variables (see \code{x} input argument
of \code{\link{omega}}.
}
\description{
This function is the inverse of \code{\link{omega}}
function
}
\note{
This function find the zero of the \code{\link{omega_root}}
function by calling \code{\link{uniroot}}. If the argument
\code{p0} is not \code{NULL} and is a matrix of joint
probabilities, the function returns a correlation matrix by
using the elements of \code{p0} ass joint probabilities for
each couple and \code{p0_v1} as a vector of marginal
probability of each occurence/no-occurence (In this case if
the length of \code{p0_v1} does not correspond to the
number of columns of \code{p0}, the marginal probabilities
are taken from the diagonal of \code{p0}). See the R code
for major details
}
\examples{
x <- omega_inv(p0_v1=0.5,p0_v2=0.5,p00=1.1*0.5*0.5)
omega(x,p0_v1=0.5,p0_v2=0.5)
}
\author{
Emanuele Cordano
}
\seealso{
\code{\link{normalCopula}},\code{\link{pcopula}},\code{\link{omega}}(and
reference URLs therein)
}

