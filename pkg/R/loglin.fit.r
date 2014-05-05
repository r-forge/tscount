loglin.fit <- function(ts, model=list(past_obs=NULL, past_mean=NULL, xreg=NULL, external=NULL), score=TRUE, info=c("score", "none"), init=c("marginal", "iid", "firstobs"), epsilon=1e-06, slackvar=1e-06, init.control=list(), final.control=list(), inter.control=NULL){
  ##############
  #Checks and preparations: 
  cl <- match.call()
  durations <- c(init=NA, inter=NA, final=NA, total=NA)
  begin_total <- proc.time()["elapsed"]
  model_names <- c("past_obs", "past_mean", "xreg", "external")
  stopifnot( #Are the arguments valid?
    all(names(model) %in% model_names)
  )
  model <- model[model_names]
  names(model) <- model_names
  if(is.null(model$xreg)) model$xreg <- matrix(0, nrow=length(ts), ncol=0) else model$xreg <- as.matrix(model$xreg)
  if(length(model$external)==0) model$external <- rep(FALSE, ncol(model$xreg)) else model$external <- as.logical(model$external) #the default value for model$external is FALSE (i.e. an internal covariate effect)
  if(length(model$external)==1) model$external <-  rep(model$external, ncol(model$xreg)) else model$external <- as.logical(model$external) #if only one value for model$external is provided, this is used for all covariates
  if(any(is.na(ts)) || any(is.na(model$xreg))) stop("Cannot make estimation with missing values in time series or covariates")
  stopifnot( #Are the arguments valid?
    model$past_obs%%1==0,
    model$past_mean%%1==0,
    length(ts)==nrow(model$xreg),    
    length(model$external)==ncol(model$xreg)
  )    
  stopifnot( #Are the arguments valid?
    is.list(init.control),
    is.null(final.control) || is.list(final.control)
  )
  info <- match.arg(info)
  if(is.null(final.control) || is.null(final.control$constrained)) slackvar <- 0
  n <- length(ts)
  p <- length(model$past_obs)
  P <- seq(along=numeric(p)) #sequence 1:p if p>0 and NULL otherwise
  p_max <- max(model$past_obs, 0)
  q <- length(model$past_mean)
  Q <- seq(along=numeric(q)) #sequence 1:q if q>0 and NULL otherwise
  q_max <- max(model$past_mean, 0)
  r <- ncol(model$xreg)
  R <- seq(along=numeric(r)) #sequence 1:r if r>0 and NULL otherwise
  parameternames <- tsglm.parameternames(model)
  init_default <- list(method="GLM", use=Inf)
  if(!all(names(init.control)%in%c(names(init_default), "intercept", "past_obs", "past_mean", "xreg"))) stop("There are unknown list elements in argument 'init'")
  init_default[names(init.control)] <- init.control #options given by user override the default
  init.control <- init_default #use these options in the following
  if(!is.null(final.control)){
    final_default <- list(constrained=list(outer.iterations=100, outer.eps=1e-05), optim.method="BFGS", optim.control=list(maxit=100, reltol=1e-11))
    final_default[names(final.control)] <- final.control  
    final.control <- final_default
    if(!all(names(final.control)%in%names(final_default))) stop("There are unknown list elements in argument 'final.control'")
  }
  if(!is.null(inter.control)){
    inter_default <- list(constrained=list(outer.iterations=5, outer.eps=1e-05), optim.method="Nelder-Mead", optim.control=list(maxit=20, reltol=1e-8))
    inter_default[names(inter.control)] <- inter.control  
    inter.control <- inter_default
    if(!all(names(inter.control)%in%names(inter_default))) stop("There are unknown list elements in argument 'inter.control'")
  }
  
  ##############
  #Initial estimation:
  begin_init <- proc.time()["elapsed"]
  param_init <- do.call(init.fit, args=list(allobj=mget(ls()), linkfunc="log"))
  
  # # # # # # #
  #Transformation to a stationary solution of a autoregressive log-linear process process:
  total <- sum(abs(param_init$past_obs))+sum(abs(param_init$past_mean))
  if(total > 1-epsilon-slackvar){ #Shrink the parameters to fulfill the stationarity condition if necessary:
    shrinkage_factor <- (1-slackvar-epsilon)/total #chosen, such that total_new = 1-slackvar-epsilon for total_new the sum of the alpha's and beta's after shrinkage
    param_init$past_mean <- param_init$past_mean*shrinkage_factor
    param_init$past_obs <- param_init$past_obs*shrinkage_factor
#########correct the initial estimation of the intercept for this possible shrinkage?
  }
  # # # # # # #
  
  
  paramvec_init <- unlist(param_init)
  names(paramvec_init) <- parameternames
  # # # # # # #
  durations["init"] <- proc.time()["elapsed"] - begin_init
  if(is.null(final.control)){
      durations["total"] <- proc.time()["elapsed"] - begin_total
      result <- list(init=paramvec_init, call=cl, n_obs=n, durations=durations, ts=ts, model=model) 
  return(result)      
  }
  ##############
  
  ##############
  #Final estimation:
  
  # # # # # # #
  #Create some functions as wrappers:
    f <- function(paramvec, model) loglin.loglik(paramvec=paramvec, model=model, ts=ts, score=FALSE, info="none")$loglik    
    grad <- function(paramvec, model) loglin.loglik(paramvec=paramvec, model=model, ts=ts, score=TRUE, info="none")$score
    optimisation <- function(starting_value, model, arguments){  
      if(!is.null(arguments$constrained)){
        ui <- -cbind(rep(0,2^(p+q)), as.matrix(expand.grid(lapply(c(P,Q), function(x) c(-1,+1)))), matrix(0, ncol=r, nrow=2^(p+q)))
        ci <- rep(-1+slackvar, 2^(p+q)) 
        optim_result <- do.call(constrOptim, args=c(list(theta=starting_value, f=f, grad=grad, ui=ui, ci=ci, method=arguments$optim.method, control=c(list(fnscale=-1), arguments$optim.control), model=model), arguments$constrained))
      }else{
        optim_result <- optim(par=starting_value, fn=f, gr=grad, model=model, method=arguments$optim.method, control=c(list(fnscale=-1), arguments$optim.control))
      }
      return(optim_result)
    }
  # # # # # # #
  if(is.null(inter.control)){ #no additional optimisation step is done
    inter_optim <- NULL
    durations["final"] <- system.time(final_optim <- optimisation(starting_value=paramvec_init, model=model, arguments=final.control))["elapsed"]
  }else{ #an additional optimisation step between initial estimation and final optimisation is introduced
    durations["inter"] <- system.time(inter_optim <- optimisation(starting_value=paramvec_init, model=model, arguments=inter.control))["elapsed"]
    durations["final"] <- system.time(final_optim <- optimisation(starting_value=inter_optim$par, model=model, arguments=final.control))["elapsed"]
  }
  paramvec_inter <- as.numeric(inter_optim$par)
  paramvec_final <- as.numeric(final_optim$par)
  if(p+q+r>0 && all(abs((paramvec_final-paramvec_init)/paramvec_final) < 0.01)) warning("Final estimation is still very close to initial estimation. This might indicate a problem with the optimisation but could also have happended by chance. Please check results carefully.")
  if(p+q>0 && mean(abs(paramvec_final[c(1+P,1+p+Q)])) < 1e-04) warning("There is almost no serial dependence estimated in the data. This might be appropriate but could just as likely indicate a problem with the optimisation. Please check results carefully.")
  if(abs(paramvec_final[1]) < 0.01) warning("Estimated absolute intercept is very small (< 0.01). This might indicate a problem with the optimisation unless the observed marginal mean is very low or the observed serial dependence is very strong. Please check results carefully.")
  ##############  
  
  ##############
  #Score vector and information matrix:
  #If score==FALSE and info=="none" the computation in the following two lines would not be necessary. However, the extra time needed to re-calculate the log-likelihood function which is already available in final_optim$value is negligable in comparison to the total duration of the function. This avoids some additional if-statements and the code is more readable.
  condmean <- loglin.condmean(paramvec=paramvec_final, model=model, ts=ts, derivatives={if(!score & info=="none") "none" else "first"}, init=init)
  loglik <- loglin.loglik(paramvec=paramvec_final, model=model, ts=ts, score=score, info=info, condmean=condmean, from=Inf) #because of argument from=Inf no re-calculation of the recursion is done, instead the calculations from object condmean are used
  if(is.ts(ts)) loglik$kappa <- ts(loglik$kappa, start=start(ts), frequency=frequency(ts)) #give the linear predictors the same time series structure as the input time series
  ##############
  
  durations["total"] <- proc.time()["elapsed"] - begin_total 
#  if(all(abs((final_optim$par-paramvec_init)/final_optim$par) < 0.01)){warning("Final estimation is still very close to initial estimation.")}
  result <- c(list(coefficients=final_optim$par, init=paramvec_init, inter=inter_optim, final=final_optim, residuals=ts-exp(loglik$kappa), fitted.values=exp(loglik$kappa), linear.predictors=loglik$kappa, logLik=loglik$loglik, score=loglik$score, info.matrix=loglik$info, outerscoreprod=loglik$outerscoreprod, call=cl, n_obs=n, durations=durations, ts=ts, model=model))
  return(result)
}
