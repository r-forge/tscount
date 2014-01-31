se <- function(object, ...) UseMethod("se")

se.tsglm <- function(object, B, parallel=FALSE, ...){
  tsglm.check(object)
  est <- c(coef(object), object$distrcoefs)
  if(missing(B)){
    vcov <- vcov(object)
    var <- diag(vcov)
    stderrors <- c(sqrt(var), rep(NA, length(object$distrcoefs)))
    result <- list(est=est, se=stderrors, type="normapprox")
  }else{
    stopifnot(B>=2, B%%1==0)
    simfit <- function(seed, fit, ...){
      set.seed(seed)
      ts_sim <- tsglm.sim(fit=fit)$ts
      fit_sim <- tsglm(ts=ts_sim, model=fit$model, link=fit$link, distr=fit$distr, score=FALSE, info="none", ...)
      result <- c(coef(fit_sim), fit_sim$distrcoefs)
      return(result)
    }
    seeds <- sample(1e+9, size=B)
    if(parallel){
      cluster_running <- try(sfIsRunning(), silent=TRUE)
      snowfall_loaded <- !class(cluster_running)=="try-error"
      if(snowfall_loaded){
        if(cluster_running){
          sfExport("simfit")
          Sapply <- sfSapply
        }else{
          stop("No cluster initialised; initialise cluster with function 'sfInit' or set argument 'parallel=FALSE'")
        }
      }else{   
        stop("Package 'snowfall' not loaded; load package with 'library(snowfall)' and initialise cluster with function 'sfInit' or set argument 'parallel=FALSE'")
      }
    }else{
      Sapply <- sapply
    }
    bootstrap_coefs <- Sapply(seeds, simfit, fit=object, ..., simplify=TRUE)
    stderrors <- apply(bootstrap_coefs, 1, sd)
    result <- list(est=est, se=stderrors, type="bootstrap", B=B)
  }
  return(result)
}
