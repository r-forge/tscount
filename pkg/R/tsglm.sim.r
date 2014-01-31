tsglm.sim <- function(n, param=list(intercept=1, past_obs=NULL, past_mean=NULL, xreg=NULL), model=list(past_obs=NULL, past_mean=NULL, xreg=NULL, external=FALSE), link=c("identity", "log"), distr=c("poisson", "nbinom"), distrcoefs, fit, n_start=50){

  #Use information of a previous model fit if provided:
  if(!missing(fit)){
    tsglm.check(fit)    
    if(!missing(n)){
      warning("Number of observations provided in argument 'n' is used, the number of observations in argument 'fit$n_obs' is ignored")
    }else{
      n <- fit$n
    }
    if(!missing(param)) warning("Parameters provided in argument 'fit$coefficients' is used, the parameters in argument 'param' are ignored")
    paramvec <- fit$coefficients
    if(!missing(model)) warning("Model specification provided in argument 'fit$model' is used, the model specification in argument 'model' is ignored")
    model <- fit$model
    if(!missing(distr)){
      warning("Link function provided in argument 'link' is used, the link function in argument 'fit$distr' is ignored")
    }else{
      link <- fit$link
    }
    if(!missing(distr)){
      warning("Conditional distribution provided in argument 'distr' with its additional coefficients in argument 'distrcoefs' is used, the distribution in argument 'fit$distr' with its additional coefficients in argument 'fit$distrcoefs' is ignored")
    }else{
      distr <- fit$distr
      distrcoefs <- fit$distrcoefs
    }
  }else{
    if(missing(n)) stop("Argument 'n' is missing. Either provide this argument or an argument 'fit' with the result of a previous model fit")
    if(missing(param)) stop("Argument 'param' is missing. Either provide this argument or an argument 'fit' with the result of a previous model fit")
    if(missing(model)) stop("Argument 'model' is missing. Either provide this argument or an argument 'fit' with the result of a previous model fit")
  }
  
  #Check and modify arguments:
  link <- match.arg(link)
  distr <- match.arg(distr)
  if(distr=="nbinom"){
    if(missing(distrcoefs) || length(distrcoefs)!=1) stop("For the negative binomial parameter (only) the dispersion parameter 'size' has to be provided in argument 'distrcoefs'")
    if(distrcoefs<=0) stop("The additional dispersion parameter for the negative binomial distribution has to be greater than zero")
  }
  model_names <- c("past_obs", "past_mean", "xreg", "external")
  stopifnot( #Are the arguments valid?
    n%%1==0 & n>=0,
    n_start%%1==0 & n_start>=0,
    names(model) %in% model_names,
    model$past_obs%%1==0,
    model$past_mean%%1==0
  )
  model <- model[model_names]
  names(model) <- model_names
  model$xreg <- if(is.null(model$xreg)) matrix(0, nrow=n, ncol=0) else as.matrix(model$xreg)
  p <- length(model$past_obs)
  P <- seq(along=numeric(p)) #sequence 1:p if p>0 and NULL otherwise
  p_max <- max(model$past_obs, 0)
  q <- length(model$past_mean)
  Q <- seq(along=numeric(q)) #sequence 1:q if q>0 and NULL otherwise
  q_max <- max(model$past_mean, 0)
  r <- max(ncol(model$xreg), 0)
  R <- seq(along=numeric(r)) #sequence 1:r if r>0 and NULL otherwise
  stopifnot(
    nrow(model$xreg)==n,
    length(model$external)%in%c(r,1,0)
  )
  model$external <- as.logical(model$external)
  if(any(is.na(model$external))) stop("Argument 'model$external' could not be coerced to logical")
  if(length(model$external)==0) model$external <- rep(FALSE, r) #by default covariate has internal effect
  if(length(model$external)==1 && r!=1) rep(model$external, r) #if only one value is given this is used for all covariates
  if(!missing(fit)){ #use information of previous model fit if provided:
    param <- list( #transform parameter vector to a list
      intercept=paramvec[1],
      past_obs=paramvec[1+P],
      past_mean=paramvec[1+p+Q],
      xreg=paramvec[1+p+q+R]
    )
  }else{ #Check argument param for validity and modify it whenever necessary:
    param_names <- c("intercept", "past_obs", "past_mean", "xreg")
    stopifnot(
      names(param) %in% param_names,
      "intercept" %in% names(param),
      length(param$intercept)==1,
      length(param$past_obs)==p,
      length(param$past_mean)==q,
      length(param$xreg)==r
    )
    param <- param[param_names]
    names(param) <- param_names
    if(is.null(model$past_obs)) model$past_obs <- seq(along=param$past_obs) #by default the parameters are for regression on the first few past observations
    if(is.null(model$past_mean)) model$past_mean <- seq(along=param$past_mean) #by default the parameters are for regression on the first few past means
  }

  #Use information of a previous model fit if provided:
  if(!missing(fit)){
    tsglm.check(fit)    
    if(!missing(n)){
      warning("Number of observations provided in argument 'n' is used, the number of observations in argument 'fit$n_obs' is ignored")
    }else{
      n <- fit$n
    }
    if(!missing(param)) warning("Parameters provided in argument 'fit$coefficients' is used, the parameters in argument 'param' are ignored")
    paramvec <- fit$coefficients
    if(!missing(model)) warning("Model specification provided in argument 'fit$model' is used, the model specification in argument 'model' is ignored")
    model <- fit$model
    if(!missing(distr)){
      warning("Conditional distribution provided in argument 'distr' with its additional coefficients in argument 'distrcoefs' is used, the distribution in argument 'fit$distr' with its additional coefficients in argument 'fit$distrcoefs' is ignored")
    }else{
      distr <- fit$distr
      distrcoefs <- fit$distrcoefs
    }
  }else{
    if(missing(n)) stop("Argument 'n' is missing. Either provide this argument or an argument 'fit' with the result of a previous model fit")
    if(missing(param)) stop("Argument 'param' is missing. Either provide this argument or an argument 'fit' with the result of a previous model fit")
    if(missing(model)) stop("Argument 'model' is missing. Either provide this argument or an argument 'fit' with the result of a previous model fit")
  }
  
  #Check and modify arguments:
  distr <- match.arg(distr)
  if(distr=="nbinom"){
    if(missing(distrcoefs) || length(distrcoefs)!=1) stop("For the negative binomial parameter (only) the dispersion parameter 'size' has to be provided in argument 'distrcoefs'")
    if(distrcoefs<=0) stop("The additional dispersion parameter for the negative binomial distribution has to be greater than zero")
  }
  model_names <- c("past_obs", "past_mean", "xreg", "external")
  stopifnot( #Are the arguments valid?
    n%%1==0 & n>=0,
    n_start%%1==0 & n_start>=0,
    names(model) %in% model_names,
    model$past_obs%%1==0,
    model$past_mean%%1==0
  )
  model <- model[model_names]
  names(model) <- model_names
  model$xreg <- if(is.null(model$xreg)) matrix(0, nrow=n, ncol=0) else as.matrix(model$xreg)
  p <- length(model$past_obs)
  P <- seq(along=numeric(p)) #sequence 1:p if p>0 and NULL otherwise
  p_max <- max(model$past_obs, 0)
  q <- length(model$past_mean)
  Q <- seq(along=numeric(q)) #sequence 1:q if q>0 and NULL otherwise
  q_max <- max(model$past_mean, 0)
  r <- max(ncol(model$xreg), 0)
  R <- seq(along=numeric(r)) #sequence 1:r if r>0 and NULL otherwise
  stopifnot(
    nrow(model$xreg)==n,
    length(model$external)%in%c(r,1,0)
  )
  model$external <- as.logical(model$external)
  if(any(is.na(model$external))) stop("Argument 'model$external' could not be coerced to logical")
  if(length(model$external)==0) model$external <- rep(FALSE, r) #by default covariate has internal effect
  if(length(model$external)==1 && r!=1) rep(model$external, r) #if only one value is given this is used for all covariates
  if(!missing(fit)){ #use information of previous model fit if provided:
    param <- list( #transform parameter vector to a list
      intercept=paramvec[1],
      past_obs=paramvec[1+P],
      past_mean=paramvec[1+p+Q],
      xreg=paramvec[1+p+q+R]
    )
  }else{ #Check argument param for validity and modify it whenever necessary:
    param_names <- c("intercept", "past_obs", "past_mean", "xreg")
    stopifnot(
      names(param) %in% param_names,
      "intercept" %in% names(param),
      length(param$intercept)==1,
      length(param$past_obs)==p,
      length(param$past_mean)==q,
      length(param$xreg)==r
    )
    param <- param[param_names]
    names(param) <- param_names
    if(is.null(model$past_obs)) model$past_obs <- seq(along=param$past_obs) #by default the parameters are for regression on the first few past observations
    if(is.null(model$past_mean)) model$past_mean <- seq(along=param$past_mean) #by default the parameters are for regression on the first few past means
  }
  
  #Check parameter restrictions:
  if(link=="identity"){
    stopifnot(     
      param$intercept>0,
      param$past_obs>=0,
      param$past_mean>=0,
      param$xreg>=0
    )
    sum_param_past <- sum(param$past_obs)+sum(param$past_mean)
    if(sum_param_past>=1) stop(paste("Parameters are outside the stationary region, sum of parameters for regression on past observations and on past conditional means is", sum_param_past, "> 1")) 
    kappa_stationary <- param$intercept/(1-sum_param_past)
    if(kappa_stationary>1e+09) stop("Too large mean to simulate from Poisson distribution, sum of parameters for regression on past observations and on past conditional means might be too close to one and/or intercept is too large")
  }
  if(link=="log"){
    sum_param_past <- sum(abs(param$past_obs))+sum(abs(param$past_mean))
    if(sum_param_past>=1) stop(paste("Parameters are outside the stationary region, sum of absolite parameters for regression on past observations and on past conditional means is", sum_param_past, "> 1")) 
  }
    
  n_total <- n_start + n #total number of observations to be simulated (including burn-in)
  
  #Random number generation from conditional distribution:
  if(distr=="poisson") rdistr <- function(n, meanvalue, distrcoefs) rpois(n, lambda=meanvalue)
  if(distr=="nbinom") rdistr <- function(n, meanvalue, distrcoefs) rnbinom(n, mu=meanvalue, size=distrcoefs)

  if(link=="identity"){  
    #Initialisation:
    X <- rbind(matrix(0, nrow=q_max+n_start, ncol=r), model$xreg)    
    kappa <- c(rep(kappa_stationary, q_max), numeric(n_total))  
    z <- c(rdistr(p_max, meanvalue=kappa_stationary, distrcoefs=distrcoefs), integer(n_total))
    
    #Recursion:  
    for(t in 1:n_total){
      kappa[t+q_max] <- param$intercept + sum(param$past_obs*z[(t-model$past_obs)+p_max]) + sum(param$past_mean*kappa[(t-model$past_mean)+q_max]) + if(r>0){sum(param$xreg*X[t+q_max,]) - if(q>0){sum(param$past_mean*colSums(model$external*param$xreg*t(X[(t-model$past_mean)+q_max, , drop=FALSE])))}else{0}}else{0}
      z[t+p_max] <- rdistr(1, meanvalue=kappa[t+q_max], distrcoefs=distrcoefs)
    }
  }
  if(link=="log"){
    #Initialisation:
    X <- rbind(matrix(0, nrow=q_max+n_start, ncol=r), model$xreg)    
    kappa <- c(rep(0, q_max), numeric(n_total))  
    z <- c(rep(0, p_max), integer(n_total))
    
    #Recursion:  
    for(t in 1:n_total){
      kappa[t+q_max] <- param$intercept + sum(param$past_obs*log(z[(t-model$past_obs)+p_max]+1)) + sum(param$past_mean*kappa[(t-model$past_mean)+q_max]) + if(r>0){sum(param$xreg*X[t+q_max,]) - if(q>0){sum(param$past_mean*colSums(model$external*param$xreg*t(X[(t-model$past_mean)+q_max, , drop=FALSE])))}else{0}}else{0}
      z[t+p_max] <- rdistr(1, meanvalue=exp(kappa[t+q_max]), distrcoefs=distrcoefs)
    }    
  }
  
  #Remove initialisation:
  z <- z[p_max+n_start+(1:n)]
  kappa <- kappa[q_max+n_start+(1:n)]
    
  z <- as.ts(z)
  kappa <- as.ts(kappa)
  xreg_effect <- as.ts(colSums(param$xreg*t(model$xreg)))
  result <- list(ts=z, linear.predictors=kappa, xreg.effects=xreg_effect)
  return(result)
}
