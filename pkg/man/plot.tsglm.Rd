\name{plot.tsglm}
\alias{plot.tsglm}
\title{
Diagnostic Plots for a Fitted GLM-type Model for Time Series of Counts
}
\description{
Produces several diagnostic plots to asses the fit of a GLM-type model for time series of counts.
}
\usage{
\method{plot}{tsglm}(x, ask = TRUE, ...)
}
\arguments{
  \item{x}{
an object of class \code{"tsglm"}. Usually the result of a call to \code{\link{tsglm}}.
}
  \item{ask}{
logical value. If \code{TRUE} (and the R session is interactive) the user is asked for input, before a new figure is drawn (see \code{\link{devAskNewPage}}).
}
  \item{...}{
further arguments are currently ignored. Only for compatibility with generic function.
}
}
\details{
Produces plots of the acf of the Pearson residuals, the Pearson residuals plotted against time, a cumulative periodogramm of the Pearson residuals, a probability integral transform (PIT) histogram (see function \code{\link{pit}}) and a marginal calibration plot (see function \code{\link{marcal}}). The cumulative periodogramm is plotted with the function \code{\link{cpgram}} from package \code{MASS} and is omitted if this package is not available.
}
\author{
Tobias Liboschik and Philipp Probst
}

\seealso{
\code{\link{tsglm}} for fitting a GLM for time series of counts.
}

\examples{
seatbelts_fit <- tsglm(ts=Seatbelts[, "DriversKilled"],
  model=list(past_obs=1, past_mean=c(1, 12)), xreg=Seatbelts[, "PetrolPrice"],
  link="identity", distr="nbinom")
op <- par(mfrow=c(2,2))
plot(seatbelts_fit, ask=FALSE)
par(op)
}

\keyword{Generalised linear model}
\keyword{Model assessment}
