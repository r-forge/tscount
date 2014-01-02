ingarch.fit <- function(ts, model=list(past_obs=NULL, past_mean=NULL, xreg=NULL, external=NULL), epsilon=1e-06, slackvar=1e-06, init.control=list(), final.control=list(), inter.control=NULL, score=TRUE, info=c("score", "none", "hessian")){
  cl <- match.call()
  durations <- c(init=NA, inter=NA, final=NA, total=NA)
  begin_total <- proc.time()["elapsed"]
  #Check arguments:  
  model_names <- c("past_obs", "past_mean", "xreg", "external")
  stopifnot( #Are the arguments valid?
    all(names(model) %in% model_names)
  )
  model <- model[model_names]
  names(model) <- model_names
  if(is.null(model$xreg)) model$xreg <- matrix(0, nrow=length(ts), ncol=0) else model$xreg <- as.matrix(model$xreg)
  if(length(model$external)==0) model$external <- rep(FALSE, ncol(model$xreg)) else model$external <- as.logical(model$external) #the default value for model$external is FALSE (i.e. an internal covariate effect)
  if(length(model$external)==1) model$external <-  rep(model$external, ncol(model$xreg)) else model$external <- as.logical(model$external) #if only one value for model$external is provided, this is used for all covariates
  if(any(is.na(ts)) || any(is.na(model$xreg))) stop("Cannot make estimation with missing values in time series or regressor")
  stopifnot( #Are the arguments valid?
    model$past_obs%%1==0,
    model$past_mean%%1==0,
    length(ts)==nrow(model$xreg),    
    length(model$external)==ncol(model$xreg),
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
  parameternames <- ingarch.parameternames(model)
  init_default <- list(method="CSS", use=Inf, optim.method="BFGS", optim.control=list(maxit=25))
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
  param_init <- list(intercept=NULL, past_obs=NULL, past_mean=NULL, xreg=NULL)
   if(init.control$method=="GLM"){
    delayed_ts <- function(x,ts){
      c(rep(0,x),ts[(x:n)-x])}
    glm_fit <- glm(ts ~ cbind(sapply(model$past_obs,delayed_ts,ts=ts)) + model$xreg,family=poisson())$coefficients # has to be specified
    intercept <- param_init$intercept <- glm_fit[1]
    param_init$past_obs <- glm_fit[1+P] 
    param_init$past_mean <- rep(0, q)
    param_init$xreg <- glm_fit[1+p+R]
   }else{
  if(init.control$method == "fixed"){ #fixed values, use given ones where available
    param_init$intercept <- if(!is.null(init.control$intercept)) init.control$beta_0 else 1
    param_init$past_obs <- if(!is.null(init.control$past_obs)) init.control$past_obs else rep(0, p)
    param_init$past_mean <- if(!is.null(init.control$past_mean)) init.control$past_mean else rep(0, q)
    param_init$xreg <- if(!is.null(init.control$xreg)) init.control$xreg else rep(0, r)
  }
  if(init.control$method %in% c("MM","CSS", "ML", "CSS-ML")){
  # # # # # # #
  #Which observations to use for initial estimation?
      if(is.null(init.control$use)) init.control$use <- n
      if(length(init.control$use)<1 | length(init.control$use)>2) stop("Argument 'init.control$use' must be of length 1 or 2")
      if(length(init.control$use)==1){
        if(init.control$use==Inf) init.control$use <- n
        if(init.control$use<p+q+1) stop(paste("Too few observations for initial estimation, argument 'init.control$use' must be greater than p+q+1=", p+q+1, sep=""))
        if(init.control$use>n){ init.control$use <- n; warning(paste("Argument 'init.control$use' is out of range and set to the largest possible value n=", n, sep="")) }
        init_use <- 1:init.control$use
      }else{
        if(init.control$use[2]-init.control$use[1]<=p+q+1) stop(paste("Too few observations for initial estimation, for argument 'init.control$use' the difference init.control$use[2]-init.control$use[1] must be greater than p+q+1=", p+q+1, sep=""))
        if(init.control$use[2]>n | init.control$use[1]<1) stop(paste("Argument 'init.control$use' is out of range, init.control$use[1] must be greater than 1 and init.control$use[2] lower than n=", n, sep=""))
        init_use <- init.control$use[1]:init.control$use[2]
      }
      ts_init <- ts[init_use]
      # # # # # # #
    k <- max(p_max, q_max)
    if(k > 0){ #non-trivial case for q>0 and p>0
      if(init.control$method == "MM"){ #moment estimator via ARMA(1,1) representation, assume parameters for higher order to be zero
        momest <- momest_arma11(ts_init)
        ma <- c(momest["ma1"], rep(0,k-1)) #set higher order parameters to zero
        ar <- c(momest["ar1"], rep(0,k-1)) #see above
        intercept <- momest["intercept"]  
      }
      if(init.control$method %in% c("CSS", "ML", "CSS-ML")){ #least squares or maximum likelihood estimator via ARMA(k,k) representation
        arma_fit <- as.numeric(suppressWarnings(arima(ts_init, order=c(k,0,k), transform.pars=TRUE, method=init.control$method, optim.method=init.control$optim.method, optim.control=init.control$optim.control)$coef)) #Supress warning messages, which occur quite frequently and are not very relevant to the user, as this is only an initial estimation. However, the interested user can find detailed information on this optimisation in the output.                     
        ma <- arma_fit[(1:k)+k]
        ar <- arma_fit[1:k]
        intercept <- arma_fit[k+k+1] 
      }
         param_init$past_obs <- ar[model$past_obs]+ma[model$past_obs]
      param_init$past_mean <- -ma[model$past_mean] 
     }else{
      param_init$past_obs <- param_init$past_mean <- NULL
      intercept <- mean(ts_init)
     }
     param_init$intercept <- intercept*(1-sum(param_init$past_obs)-sum(param_init$past_mean))
     param_init$xreg <- if(!is.null(init.control$xreg)) init.control$xreg else rep(0, r)
  }
  }
  # # # # # # #
  #Transformation to a stationary solution of an INGARCH process:
  param_init$past_mean <- pmax(param_init$past_mean, rep(epsilon, q)) #alpha_i in [0+epsilon,Inf)
  param_init$past_obs <- pmax(param_init$past_obs, rep(epsilon, p)) #beta_i in [0+epsilon,Inf)
  total <- sum(param_init$past_obs)+sum(param_init$past_mean)
  if(total > 1-epsilon-slackvar){ #Shrink the parameters to fulfill the stationarity condition if necessary:
    shrinkage_factor <- (1-slackvar-epsilon)/total #chosen, such that total_new = 1-slackvar-epsilon for total_new the sum of the alpha's and beta's after shrinkage
    param_init$past_mean <- param_init$past_mean*shrinkage_factor
    param_init$past_obs <- param_init$past_obs*shrinkage_factor
    #Note: This way former negative values, which have been set to epsilon before, become lower than epsilon!
  }
  if(init.control$method %in% c("MM", "CSS", "ML", "CSS-ML","GLM")) param_init$intercept <- intercept*(1-sum(param_init$past_obs)-sum(param_init$past_mean)) #replaces the previous definition of beta_0_init by a one which depends on the new alpha and beta and has the same mean 
  param_init$intercept <- max(param_init$intercept, slackvar+epsilon)
  param_init$xreg <- pmax(param_init$xreg, epsilon)
  # # # # # # #
  
  
  paramvec_init <- unlist(param_init)
  names(paramvec_init) <- parameternames
  # # # # # # #
  durations["init"] <- proc.time()["elapsed"] - begin_init
  if(is.null(final.control)){
      durations["total"] <- proc.time()["elapsed"] - begin_total
      result <- list(init=paramvec_init, call=cl, n_obs=n, durations=durations, ts=ts, model=model) 
  class(result) <- "ingarch"  
  return(result)      
  }
  ##############
  
  ##############
  #Final estimation:
  
  # # # # # # #
  #Create some functions as wrappers:
    f <- function(paramvec, model) ingarch.loglik(paramvec=paramvec, model=model, ts=ts, score=FALSE, info="none")$loglik    
    grad <- function(paramvec, model) ingarch.loglik(paramvec=paramvec, model=model, ts=ts, score=TRUE, info="none")$score
    optimisation <- function(starting_value, model, arguments){  
      if(!is.null(arguments$constrained)){
        ui <- rbind(diag(1+p+q+r), c(0,rep(-1,p+q),rep(0, r)))
        ci <- c(slackvar, rep(0, p+q+r), -1+slackvar) 
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
  ##############  
  
  ##############
  #Score vector and information matrix:
  #If score==FALSE and info=="none" the computation in the following two lines would not be necessary. However, the extra time needed to re-calculate the log-likelihood function which is already available in final_optim$value is negligable in comparison to the total duration of the function. This avoids some additional if-statements and the code is more readable.
  condmean <- ingarch.condmean(paramvec=paramvec_final, model=model, ts=ts, derivatives={if(!score & info=="none") "none" else if(info=="hessian") "second" else "first"}, condmean=NULL)
  loglik <- ingarch.loglik(paramvec=paramvec_final, model=model, ts=ts, score=score, info=info, condmean=condmean, from=Inf) #because of argument from=Inf no re-calculation of the recursion is done, instead the calculations from object condmean are used
  ##############
  durations["total"] <- proc.time()["elapsed"] - begin_total 
#  if(all(abs((final_optim$par-paramvec_init)/final_optim$par) < 0.01)){warning("Final estimation is still very close to initial estimation.")}
  result <- c(list(coefficients=final_optim$par, init=paramvec_init, inter=inter_optim, final=final_optim, residuals=ts-loglik$kappa, fitted.values=loglik$kappa, linear.predictors=loglik$kappa, logLik=loglik$loglik, score=loglik$score, info.matrix=loglik$info, outerscoreprod=loglik$outerscoreprod, call=cl, n_obs=n, durations=durations, ts=ts, model=model))
  return(result)
}
