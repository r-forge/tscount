\name{ingarch.se}
\alias{ingarch.se}
\title{
Standard Errors of a Fitted INGARCH Model
}
\description{
Computes the standard errors for the parameters of a fitted INGARCH model.
}
\usage{
ingarch.se(fit, B, parallel, ...)
}
\arguments{
  \item{fit}{
an object of class \code{"ingarch"}. Usually the result of a call to \code{\link{ingarch}}.
}
  \item{B}{
positive integer value giving the number of bootstrap samples to use for estimation of the standard errors. If missing the standard errors are based on a normal approximation.
  }
  \item{parallel}{
  logical value. If \code{parallel=TRUE}, the bootstrap is distributed to multiple cores parallely. Requires a computing cluster to be initialised by \code{\link[snowfall:sfInit]{sfInit}} from package \code{snowfall}. 
}
  \item{...}{
  additional arguments passed to the fitting function \code{\link{ingarch}}.
}
}

\details{
By default the standard errors are based on a normal approximation of the maximum likelihood estimator. They are the square roots of the diagonal elements of the inverse of the information matrix.

If the number of bootstrap samples \code{B} is given, the standard errors are computed by a parametric bootstrap. The standard errors are the empirical standard deviation of the parameter estimations of \code{B} random samples drawn from the fitted model \code{fit}.
}

\value{
A list with the following components:

\item{est}{a vector of the maximum likelihood estimated coefficients.}

\item{se}{a vector with the standard error of each estimated coefficient.}

\item{type}{a character value \code{"normapprox"} or \code{"bootstrap"} saying how the standard errors are computed.}

If the standard errors are computed by a parametric bootstrap procedure, the following component is additionally returned:

\item{B}{positive integer value giving the number of bootstrap samples used for estimation of the standard errors.}
}

\author{
Tobias Liboschik and Philipp Probst
}

\seealso{
 \code{\link{ingarch}} for fitting an INGARCH model.
}

\examples{
seatbelts_fit <- ingarch(ts=Seatbelts[, "DriversKilled"],
  model=list(past_obs=1, past_mean=c(1, 12), xreg=Seatbelts[, "PetrolPrice"]), distr="poisson")
ingarch.se(fit=seatbelts_fit) #by normal approximation
ingarch.se(fit=seatbelts_fit, B=10) #by bootstrap
}

\keyword{INGARCH}
\keyword{standard error}
\keyword{standard deviation}
