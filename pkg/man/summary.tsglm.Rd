\name{summary.tsglm}
\alias{summary.tsglm}
\alias{print.summary.tsglm}
\title{
Summarising Fits of Count Time Series following Generalised Linear Models
}
\description{
\code{summary} method for class \code{"tsglm"}.
}
\usage{
\method{summary}{tsglm}(object, B, parallel=FALSE, ...)
}
\arguments{
  \item{object}{
an object of class \code{"tsglm"}. Usually the result of a call to \code{\link{tsglm}}.
}
  \item{B}{
controls the computation of standard errors. Is passed to \code{\link[=se.tsglm]{se}}.
  }
  \item{parallel}{
controls the computation of standard errors. Is passed to \code{\link[=se.tsglm]{se}}.
}
  \item{...}{
further arguments are currently ignored. Only for compatibility with generic function.
}
}
\details{
Computes and returns a list of summary statistics of the fitted model given in argument \code{object}.
}
\value{
A named list with the following elements:

  \item{call}{see \code{\link{tsglm}}.}
 
  \item{link}{see \code{\link{tsglm}}.}
  
  \item{distr}{see \code{\link{tsglm}}.}
 
  \item{residuals}{see \code{\link{tsglm}}.}
 
  \item{coefficients}{data frame with estimated parameters and their standard errors (based on a normal approximation, see \code{\link[=se.tsglm]{se}}).}
 
  \item{number.coef}{number of coefficients.}
  
  \item{logLik}{value of the maximised log-likelihood function.}
 
  \item{AIC}{Akaike's information criterion (AIC).}
 
  \item{BIC}{Bayesian information criterion (BIC).}  
 
  \item{pearson.resid}{Pearson residuals, see \code{\link{residuals.tsglm}}.}
}

\author{
Tobias Liboschik and Philipp Probst
}

\seealso{
S3 method \code{\link{print}}.

\code{\link{tsglm}} for fitting a GLM for time series of counts.
}
\examples{
seatbelts_fit <- tsglm(ts=Seatbelts[, "DriversKilled"],
  model=list(past_obs=1, past_mean=c(1, 12)), xreg=Seatbelts[, "PetrolPrice"],
  link="identity", distr="poisson")
summary(seatbelts_fit)
}

\keyword{Generalised linear model}
\keyword{Model assessment}
\keyword{Inference}
