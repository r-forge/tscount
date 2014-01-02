\name{summary.ingarch}
\alias{summary.ingarch}
\alias{print.summary.ingarch}
\title{
Summarising INGARCH Model Fits
}
\description{
\code{summary} method for class \code{"ingarch"}.
}
\usage{
\method{summary}{ingarch}(object, ...)
}
\arguments{
  \item{object}{
an object of class \code{"ingarch"}. Usually the result of a call to \code{\link{ingarch}}.
}
  \item{...}{
further arguments are currently ignored. Only for compatibility with generic function.
}
}
\details{
Computes and returns a list of summary statistics of the fitted INGARCH model given in argument \code{object}.
}
\value{
A named list with the following elements:

  \item{call}{see \code{\link{ingarch}}.}
  
  \item{residuals}{see \code{\link{ingarch}}.}
  
  \item{coefficients}{data frame with estimated parameters and their standard errors (based on a normal approximation, see \code{\link{ingarch.se}}).}

  \item{number.coef}{number of coefficients.}
  
  \item{logLik}{value of the maximised log-likelihood function.}

  \item{AIC}{Akaike's information criteria (AIC).}

  \item{corrected.AIC}{AIC corrected for finite sample size.}

  \item{BIC}{Bayesian information criteria (BIC).}  

  \item{pearson.resid}{Pearson residuals, see \code{\link{residuals.ingarch}}.}
}

\author{
Tobias Liboschik and Philipp Probst
}

\seealso{
S3 method \code{\link{print}}.

\code{\link{ingarch}} for fitting an INGARCH model and \code{\link{ingarch.se}} for standard errors of the coefficients.
}
\examples{
seatbelts_fit <- ingarch(ts=Seatbelts[, "DriversKilled"],
  model=list(past_obs=1, past_mean=c(1, 12), xreg=Seatbelts[, "PetrolPrice"]))
summary(seatbelts_fit)
}

\keyword{INGARCH}
\keyword{standard errors}

