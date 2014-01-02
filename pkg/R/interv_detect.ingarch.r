interv_detect <- function(...) UseMethod("interv_detect")

interv_detect.ingarch <- function(fit, taus=2:length(ts), delta, external=FALSE, B=NULL, info=c("score", "hessian"), init.control_bootstrap, final.control_bootstrap, inter.control_bootstrap, parallel=FALSE, est_interv=TRUE, ...){
  #Test on a single intervention of known type at a UNknown point in time
  #depends on function invert() in file matrix_inversion.r
  #B: Integer (>0). Number of bootstrap samples for erstimation of the p-value, for B=NULL no p-value is returned.
  #taus: Integer vector (>=0). Times which should be considered for an intervention.
  ##############################
  
  #Check and modify arguments:
  ingarch.check(fit)
  info <- match.arg(info)
  taus <- sort(unique(taus)) #ensure, that vector of considered time points is sorted and does not include any duplicates
  
  #Function to compute the test statistic:
  compute_test_statistic <- function(model, ts, distr, fit_H0=NULL, taus, delta, external, est_interv=FALSE, ...){
    n <- length(ts)
    if(all(ts==0)) return(list(error_message="Time series is constantly zero"))
    if(is.null(fit_H0)){ #do not estimate the parameter under the null hypothesis of no intervention if it is given in the argument fit_H0
      #ML estimation for the model without intervention:
      op <- options(show.error.messages=FALSE) #suppress error messages for the next call
      fit_H0 <- try(ingarch(ts=ts, model=model, distr=distr, score=FALSE, info="none", ...))
      options(op)
      if("try-error" %in% class(fit_H0)) return(list(error_message=fit_H0[[1]]))
    }
    #Add information about intervention effect:
    param_H0_extended <- c(fit_H0$coefficients, 0)
    model_extended <- model
    model_extended$xreg <- cbind(model$xreg, numeric(n))
    model_extended$external <- c(model$external, external) 
    condmean_H0 <- ingarch.condmean(paramvec=param_H0_extended, model=model_extended, ts=ts, derivatives=ifelse(info=="hessian", "second", "first"))
    #Compute test statistic for known time for all tau:
    test_statistic_tau <- as.numeric(rep(NA, length(taus)))
    names(test_statistic_tau) <- taus
    for(j in seq(along=taus)){
      model_extended$xreg <- cbind(model$xreg, interv_covariate(n=n, tau=taus[j], delta=delta)) #is overwritten for each tau
      loglik <- ingarch.loglik(paramvec=param_H0_extended, model=model_extended, ts=ts, score=TRUE, info=info, condmean=condmean_H0, from=taus[j])
      
      infomat_corrected <- apply((1/loglik$kappa + fit$sigmasq)*loglik$outerscoreprod, c(2,3), sum)
      vcov <- try(vcov.ingarch(list(info.matrix=loglik$info, info.matrix_corrected=infomat_corrected)) )
      if(class(vcov)=="try-error"){
        return(list(error_message=paste("Error in invertinfo(mat) : \n", vcov[[1]], sep="")))
      }
      test_statistic_tau[j] <- (t(loglik$score) %*% vcov %*% loglik$score)[1,1]
    }
    index_tau_max <- which.max(test_statistic_tau)
    tau_max <- taus[index_tau_max]
    test_statistic <- test_statistic_tau[index_tau_max]
    covariate <- interv_covariate(n=n, tau=tau_max, delta=delta)
    model_extended$xreg <- cbind(model$xreg, covariate) #add intervention with maximum test statistic to the model
    result <- list(
      test_statistic=test_statistic,
      test_statistic_tau=test_statistic_tau,
      tau_max=tau_max,
      fit_H0=fit_H0
    )
    if(est_interv){ #ML estimation for the model with intervention at the point in time where the test statistic has its maximum
      fit_interv <- try(ingarch(ts=ts, model=model_extended, distr=distr, score=FALSE, info="none", ...))
      result <- c(result, list(fit_interv=fit_interv))  
    }
    result <- c(result, list(model_interv=model_extended)) 
    return(result)
  }
  
  result <- compute_test_statistic(model=fit$model, ts=fit$ts, distr=fit$distr, fit_H0=fit, taus=taus, delta=delta, external=external, est_interv=est_interv, ...)
  
  #Bootstrap to compute p-value:
  if(!is.null(B)){
    #Set arguments controlling the estimation in the bootstrap:
    if(missing(init.control_bootstrap)) init.control_bootstrap <- if(hasArg(init.control)) init.control else list()
    bootstrap_noest <- !missing(final.control_bootstrap) && is.null(final.control_bootstrap) #is argument final.control_bootstrap is NULL, then the parameters are not re-estimated for each bootstrap sample but the true parameters used for simulation are used
    if(missing(final.control_bootstrap) || is.null(final.control_bootstrap)) final.control_bootstrap <- if(hasArg(final.control)) final.control else list()        
    if(missing(inter.control_bootstrap)) inter.control_bootstrap <- if(hasArg(final.control)) final.control else NULL    
    if(parallel){
      cluster_running <- try(sfIsRunning(), silent=TRUE)
      snowfall_loaded <- !class(cluster_running)=="try-error"
      if(snowfall_loaded){
        if(cluster_running){
          sfExport("compute_test_statistic")
          Lapply <- sfLapply
        }else{
          stop("No cluster initialised; initialise cluster with function 'sfInit' or set argument 'parallel=FALSE'")
        }
      }else{   
        stop("Package 'snowfall' not loaded; load package with 'library(snowfall)' and initialise cluster with function 'sfInit' or set argument 'parallel=FALSE'")
      }
    }else{
      Lapply <- lapply
    }
    bootstrap <- function(seed=NULL, fit_H0, n, model, distr, taus, delta, external, ...){
      if(!is.null(seed)) set.seed(seed)
      ts.bootstrap <- ingarch.sim(fit=fit_H0)$ts
      fit_H0.bootstrap <- if(bootstrap_noest) fit_H0 else NULL
      dotdotdot <- list(...)
      dotdotdot[names(dotdotdot) %in% c("init.control", "final.control", "inter.control")] <- NULL #remove these arguments to avoid matching multiple arguments in the following call
      result.bootstrap <- do.call(compute_test_statistic, args=c(list(model=model, ts=ts.bootstrap, distr=distr, fit_H0=fit_H0.bootstrap, taus=taus, delta=delta, external=external, est_interv=TRUE, init.control=init.control_bootstrap, final.control=final.control_bootstrap, inter.control=inter.control_bootstrap), dotdotdot))
      result <- ifelse(is.null(result.bootstrap$error_message), result.bootstrap["test_statistic"], result.bootstrap["error_message"])
      return(result)
    }
    bootstrap_test_statistics <- NULL
    bootstrap_errors <- NULL
    B_left <- B
    while(B_left > 0){
      seeds <- sample(1e+8, size=B_left)
      if(B_left==1) Lapply <- lapply #temporary solution for the problem, that sfLapply does give an error (Error in cut.default(i, breaks) : 'breaks' are not unique) if applied to a vector of length one (see https://bugs.r-project.org/bugzilla3/show_bug.cgi?id=14898 for a similar error)   
      output.bootstrap <- Lapply(seeds, bootstrap, fit_H0=fit, n=fit$n_obs, model=fit$model, distr=fit$distr, taus=taus, delta=delta, external=external, ...)
      index_errors <- sapply(output.bootstrap, function(x) is.character(x[[1]]))
      bootstrap_test_statistics <- c(bootstrap_test_statistics, unlist(output.bootstrap[!index_errors]))
      B_left <- B - length(bootstrap_test_statistics)
      bootstrap_errors <- c(bootstrap_errors, unlist(output.bootstrap[index_errors]))
    }
    if(length(bootstrap_errors)>0) warning(paste("For", length(bootstrap_errors), "bootstrapped time series no test statistic could be computed and new time series was drawn, see error messages in list element '$bootstrap_errors'"))   
    p_value <- sum(bootstrap_test_statistics > result$test_statistic)/(B+1)
    result <- c(result, list(
      p_value=p_value,
      bootstrap_test_statistics=bootstrap_test_statistics,
      bootstrap_errors=bootstrap_errors
    ))
  }
  
  class(result) <- "interv_detect"
  return(result)
}