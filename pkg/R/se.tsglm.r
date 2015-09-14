se <- function(object, ...) UseMethod("se")

se.tsglm <- function(object, B, parallel=FALSE, ...){
  tsglm.check(object)
  est <- c(coef(object), sigmasq=if(object$distr=="poisson") NULL else object$sigmasq)
  if(missing(B)){
    covmatrix <- vcov(object)
    variances <- diag(covmatrix)
    stderrors <- c(sqrt(variances), sigmasq=if(object$distr=="poisson") NULL else NA)
    result <- list(est=est, se=stderrors, type="normapprox")
  }else{
    bootstrap_coefs <- simcoefs(object, method="bootstrap", B=B, parallel=parallel, ...)$coefs[, seq(along=est), drop=FALSE]
    n_invalid <- sum(bootstrap_coefs[, "sigmasq"]==1e-9)
    if(object$distr!="poisson" && n_invalid>0) warning(paste("The overdispersion coefficient 'sigmasq' could not be estimated\nin", n_invalid, "of the", B, "replications. It is set to zero for these\nreplications. This might to some extent result in an overestimation\nof its true variability."))
    stderrors <- apply(bootstrap_coefs, 2, sd, na.rm=TRUE)
    result <- list(est=est, se=stderrors, type="bootstrap", B=B)
  }
  return(result)
}
