ingarch.se <- function(fit, B, parallel=FALSE, ...){
  ingarch.check(fit)
  est <- c(coef(fit), fit$distrcoefs)
  if(missing(B)){
    vcov <- vcov(fit)
    var <- diag(vcov)
    se <- c(sqrt(var), rep(NA, length(fit$distrcoefs)))
    result <- list(est=est, se=se, type="normapprox")
  }else{
    stopifnot(B>=2, B%%1==0)
    simfit <- function(seed, fit, ...){
      set.seed(seed)
      ts_sim <- ingarch.sim(fit=fit)$ts
      fit_sim <- ingarch(ts=ts_sim, model=fit$model, distr=fit$distr, score=FALSE, info="none", ...)
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
    bootstrap_coefs <- Sapply(seeds, simfit, fit=fit, ..., simplify=TRUE)
    se <- apply(bootstrap_coefs, 1, sd)
    result <- list(est=est, se=se, type="bootstrap", B=B)
  }
  return(result)
}
