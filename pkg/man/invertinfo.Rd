\name{invertinfo}
\alias{invertinfo}
\title{
Compute a Covariance Matrix from a Fisher Information Matrix
}
\description{
Stable function for computing a covariance matrix from a given Fisher Information Matrix by inversion.
}
\usage{
invertinfo(mat, silent=TRUE, stopOnError=FALSE)
}
\arguments{
  \item{mat}{
  a Fisher Information Matrix.
}
  \item{silent}{
  logical value. If set to \code{FALSE} errors in the computation of the inverse while using the Cholesky decomposition algorithm are printed.
  If set to \code{TRUE} errors can be seen only in the value \code{error_message}.
}
  \item{stopOnError}{
  logical value. If set to \code{TRUE} in case of error only an error message is printed. 
}
}

\details{
A Cholesky decomposition is used to obtain the covariance matrix. This can be done because the Fisher Information Matrix is symmetrical and positive definite.
}

\value{
A list containing the following components:

\item{vcov}{the covariance matrix.}
\item{error_message}{possible error messages that occured while inverting the Fisher Information Matrix.}
}

\author{
Tobias Liboschik and Philipp Probst
}

\seealso{
 \code{\link{chol}} and \code{\link{chol2inv}}.
}

\examples{
library(Matrix)
invertinfo(Hilbert(5), stopOnError=TRUE)
invertinfo(Hilbert(100))
invertinfo(Hilbert(100), silent=FALSE)
\dontrun{invertinfo(Hilbert(100), stopOnError=TRUE)}
}
