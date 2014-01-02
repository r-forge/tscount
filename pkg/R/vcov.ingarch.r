vcov.ingarch <- function(object, ...){
  if(is.null(object$info.matrix)) stop("No information matrix provided. Argument 'object' must be the output of a call to the function 'ingarch' with argument 'info' not set to \"none\"")
  invertedinfo <- invertinfo(object$info.matrix, stopOnError=TRUE)$vcov
  result <- invertedinfo %*% object$info.matrix_corrected %*% invertedinfo #sandwich-type formula (equals invertedinfo in case of a distribution other than Poisson)
  return(result)
}
