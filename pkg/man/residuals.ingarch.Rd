\name{residuals.ingarch}
\alias{residuals.ingarch}
\title{
Residuals of an INGARCH Model
}
\description{
Returns the residuals of a fitted INGARCH model.
}
\usage{
\method{residuals}{ingarch}(object, type = c("response", "pearson", "anscombe"), ...)
}

\arguments{
  \item{object}{
an object of class \code{"ingarch"}. Usually the result of a call to \code{\link{ingarch}}.
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
\code{\link{ingarch}} for fitting an INGARCH model.
}

\keyword{INGARCH}
\keyword{residuals}
\keyword{Pearson}
\keyword{Anscombe}
