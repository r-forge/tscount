\name{plot.interv_multiple}
\alias{plot.interv_multiple}
\title{
Plot for Iterative Intervention Detection Procedure for Count Time Series following Generalised Linear Models
}
\description{
Provides a plot with the detected intervention effects and the time series cleaned from these intervention effects.
}
\usage{
\method{plot}{interv_multiple}(x, ...)
}
\arguments{
  \item{x}{
an object of class \code{"interv_multiple"}, usually a result of a call to \code{\link[=interv_detect.tsglm]{interv_detect}}.
}
  \item{...}{
  additional arguments to be passed to function \code{\link{plot}}.
}
}
\details{
The vertical red lines indicate where possible interventions were found and the dotted line is the time series cleaned from all detected intervention effects.
}
\author{
Tobias Liboschik and Philipp Probst
}

\seealso{
\code{\link[=interv_multiple.tsglm]{interv_multiple}} for detecting multiple intervention effects in GLM-type count time series and \code{\link{tsglm}} for fitting such a model.
}

\examples{
\dontrun{
campy_fit <- tsglm(ts=campy, model=list(past_obs=1, past_mean=c(7,13)))
campy_intervmultiple <- interv_multiple(fit=campy_fit, taus=80:120,
                              deltas=c(0,0.8,1), external=FALSE, B=2,
                              signif_level=0.05) #runs several hours!
plot(campy_intervmultiple)
}
}

\keyword{INGARCH}
\keyword{intervention}
\keyword{plot}