\name{residuals.tsglm}
\alias{residuals.tsglm}
\title{
Residuals of a GLM for time series of counts
}
\description{
Returns the residuals of a fitted GLM-type model for time series of counts.
}
\usage{
\method{residuals}{tsglm}(object, type = c("response", "pearson", "anscombe"), ...)
}

\arguments{
  \item{object}{
an object of class \code{"tsglm"}. Usually the result of a call to \code{\link{tsglm}}.
}
  \item{type}{
character value giving the type of residuals which should be returned. Choose \code{type="response"} for raw residuals, \code{type="pearson"} for Pearson residuals and \code{type="anscombe"} for Anscombe residuals, which are symmetric.
}
  \item{...}{
further arguments are currently ignored. Only for compatibility with generic function.
}
}
\details{
Computes a vector with the respective residuals of the INGARCH fit given in argument \code{object}.
}

\value{
Numerical vector with the residuals.
}

\author{
Tobias Liboschik and Philipp Probst
}

\seealso{
\code{\link{tsglm}} for fitting a GLM for time series of counts.
}

\keyword{INGARCH}
\keyword{residuals}
\keyword{Pearson}
\keyword{Anscombe}
