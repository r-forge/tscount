\name{plot.tsglm}
\alias{plot.tsglm}

\title{
Plot Diagnostics for an \code{ingarch} Object
}
\description{
\code{plot.tsglm} provides four plots for diagnostical analysis.
}
\usage{
\method{plot}{tsglm}(x, ask = TRUE, ...)
}
\arguments{
  \item{x}{
an object of class \code{"tsglm"}. Usually the result of a call to \code{\link{tsglm}}.
}
  \item{ask}{
logical value. If \code{TRUE} (and the R session is interactive) the user is asked for input, before a new figure is drawn (see \code{\link{par}}).
}
  \item{...}{
further arguments are currently ignored. Only for compatibility with generic function.
}
}
\details{
Produces plots of the acf and the histogram of the Pearson residuals, the Pearson residuals plotted against time and a cumulative periodogramm of the Pearson residuals. The cumulative periodogramm is plotted with the function \code{\link{cpgram}} from package \code{MASS} and is omitted if this package is not available.
}
\author{
Tobias Liboschik and Philipp Probst
}

\seealso{
\code{\link{tsglm}} for fitting a GLM for time series of counts.
}

\examples{
seatbelts_fit <- ingarch(ts=Seatbelts[, "DriversKilled"],
  model=list(past_obs=1, past_mean=c(1, 12), xreg=Seatbelts[, "PetrolPrice"]))
op <- par(mfrow=c(2,2))
plot(seatbelts_fit, ask=FALSE)
par(op)
}

\keyword{INGARCH}
\keyword{plot}

