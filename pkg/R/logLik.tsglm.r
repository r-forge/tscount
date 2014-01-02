logLik.tsglm <- function(object, ...){
  result <- object$logLik
  attr(result, "df") <- length(coef(object)) + length(object$distrcoefs)
  attr(result, "nobs") <- object$n_obs
  class(result) <- "logLik"
  return(result)
}
