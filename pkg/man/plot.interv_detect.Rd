\name{plot.interv_detect}
\alias{plot.interv_detect}

\title{
Plot Test Statistic of Intervention Detection Procedure for INGARCH Processes
}

\description{
Provides a plot of the test statistics of a test on an intervention in an INGARCH process (as returned by \code{interv_detect.ingarch}) against time.
}
\usage{
\method{plot}{interv_detect}(x, ...)
}
\arguments{
  \item{x}{
  an object of class \code{"interv_detect"}, usually a result of a call to \code{\link{interv_detect.ingarch}}.
}
  \item{...}{
  additional arguments to be passed to function \code{\link{plot}}.
}
}

\author{
Tobias Liboschik and Philipp Probst
}

\seealso{
\code{\link[=interv_detect.ingarch]{interv_detect}} for detecting an intervention effect in an INGARCH process and \code{\link{tsglm}} for fitting an INGARCH model.
}

\examples{
#campy_fit <- tsglm(ts=campy, model=list(past_obs=1, past_mean=c(7,13)))
#campy_intervdetect <- interv_detect(fit=campy_fit, taus=80:120,
#                        delta=1, external=FALSE)
#plot(campy_intervdetect)
}

\keyword{INGARCH}
\keyword{intervention}
\keyword{plot}