se <- function(object, ...) UseMethod("se")

se.tsglm <- function(object, B, parallel=FALSE, ...){
  tsglm.check(object)
  sigmasq <- if(is.null(object$distrcoefs)) NULL else object$sigmasq
  est <- c(coef(object), sigmasq=sigmasq)
  if(missing(B)){
    vcov <- vcov(object)
    var <- diag(vcov)
    stderrors <- c(sqrt(var), sigmasq=NA)
    result <- list(est=est, se=stderrors, type="normapprox")
  }else{
    stopifnot(B>=2, B%%1==0)
    simfit <- function(seed, fit, ...){
      set.seed(seed)
      ts_sim <- tsglm.sim(fit=fit)$ts
      fit_sim <- tsglm(ts=ts_sim, model=fit$model, xreg=fit$xreg, link=fit$link, distr=fit$distr, score=FALSE, info="none", ...)
      sigmasq_sim <- if(is.null(fit_sim$distrcoefs)) NULL else fit_sim$sigmasq
      result <- c(coef(fit_sim), sigmasq=sigmasq_sim)
      return(result)
    }
    seeds <- sample(1e+9, size=B)
    if(parallel){
      library(parallel)
      Sapply <- function(X, FUN, ...) parSapply(cl=NULL, X=X, FUN=FUN, ...)
    }else{
      Sapply <- sapply
    }
    bootstrap_coefs <- Sapply(seeds, simfit, fit=object, ..., simplify=TRUE)
    if(object$distr=="nbinom" && anyNA(bootstrap_coefs["sigmasq",])) warning(paste("The overdispersion coefficient 'sigmasq' could not be estimated in\n", sum(is.na(bootstrap_coefs["sigmasq",])), "of the", B, "replications. It is set to zero for this replications.\nThis might result in an overestimation of the true variability."))
    stderrors <- apply(bootstrap_coefs, 1, sd, na.rm=TRUE)
    result <- list(est=est, se=stderrors, type="bootstrap", B=B)
  }
  return(result)
}
