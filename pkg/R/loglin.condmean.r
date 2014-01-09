loglin.condmean <- function(paramvec, model, ts, derivatives=c("none", "first"), condmean=NULL, from=1){
  #Recursion for the conditional mean and its derivatives of a log-linear autoregressive process (with intervention)
  #derivatives. Character. "none" does only the recursion for the conditional mean, "first" additionally computes first partial derivatives.
  #condmean: List. Output of a previous call of this function with all arguments except tau identical to this call and tau of this call <= tau of the previous call. The recursion up to tau is taken from condmean, so that only the recursion from time point tau on has to be computed. If NULL the complete recursion is computed. For not computing anything of the recursion set argument tau=Inf.

  ##############
  #Checks and preparations:
  n <- length(ts)
  p <- length(model$past_obs)
  P <- seq(along=numeric(p)) #sequence 1:p if p>0 and NULL otherwise
  p_max <- max(model$past_obs, 0)
  q <- length(model$past_mean)
  Q <- seq(along=numeric(q)) #sequence 1:q if q>0 and NULL otherwise
  q_max <- max(model$past_mean, 0)
  Q_max <- seq(along=numeric(q_max))
  r <- max(ncol(model$xreg), 0)
  R <- seq(along=numeric(r)) #sequence 1:r if r>0 and NULL otherwise
  parameternames <- tsglm.parameternames(model)
  derivatives <- match.arg(derivatives)
  param <- list( #transform parameter vector to a list
    intercept=paramvec[1],
    past_obs=paramvec[1+P],
    past_mean=paramvec[1+p+Q],
    xreg=paramvec[1+p+q+R]
  )    
  if(!is.null(condmean)){ #If the output of a previous call is provided, the recursion starts from t=from. Else initialisation of all objects is necessary and the recursion starts from t=1.
    times <- if(from <= n) from:n else NULL
    #Load objects:
    z <- condmean$z
    kappa <- condmean$kappa
    if(derivatives=="first") partial_kappa <- condmean$partial_kappa
#########include checks if argument condmean is sufficient for further calculations
  }else{  
    times <- 1:n
  ##############

  ##############
  #Initialisation:   
    #Initialisation by arbritary zero:
    kappa <- c(rep(0, q_max), numeric(n))
    z <- c(as.integer(rep(0, p_max)), ts)
    if(derivatives == "first"){
      partial_kappa <- matrix(0, nrow=n+q_max, ncol=1+p+q+r)
    }   
  }
  X <- matrix(0, nrow=q_max+n, ncol=r)
  X[q_max+(1:n), ] <- model$xreg
  ##############

  ##############
  #Recursion:
  
  # # # # # # #
  #Conditional mean:
  for(t in times){
    kappa[t+q_max] <- param$intercept + sum(param$past_obs*log(z[(t-model$past_obs)+p_max]+1)) + sum(param$past_mean*kappa[(t-model$past_mean)+q_max]) + if(r>0){sum(param$xreg*X[t+q_max, ]) - if(q>0){sum(param$past_mean*colSums(model$external*param$xreg*t(X[(t-model$past_mean)+q_max, , drop=FALSE])))}else{0}}else{0}   
  }
  result <- list(z=z, kappa=kappa)
  # # # # # # #
  
  # # # # # # #
  #Conditional mean:    
  if(derivatives == "first"){
    for(t in times){
      partial_kappa[t+q_max, 1] <- 1 + sum(param$past_mean*partial_kappa[(t-model$past_mean)+q_max, 1]) #intercept
      if(p>0) partial_kappa[t+q_max, 1+P] <- log(z[t-model$past_obs+p_max]+1) + (if(q>0){t(param$past_mean) %*% partial_kappa[(t-model$past_mean)+q_max, 1+P, drop=FALSE]}else{numeric(p)}) #past_obs    
      if(q>0) partial_kappa[t+q_max, 1+p+Q] <- kappa[t-model$past_mean+q_max] + t(param$past_mean) %*% partial_kappa[(t-model$past_mean)+q_max, 1+p+Q, drop=FALSE] - (if(r>0){param$past_mean*colSums(model$external*param$xreg*t(X[(t-model$past_mean)+q_max, , drop=FALSE]))}else{numeric(q)}) #past_mean
      if(r>0) partial_kappa[t+q_max, 1+p+q+R] <- (if(q>0){colSums(param$past_mean*partial_kappa[(t-model$past_mean)+q_max, 1+p+q+R, drop=FALSE]) - model$external*colSums(param$past_mean*X[(t-model$past_mean)+q_max, , drop=FALSE])}else{numeric(r)}) + X[t+q_max, ] #covariates
    }
    dimnames(partial_kappa)[[2]] <- if(p==0 & q==0) list(parameternames) else parameternames
    result <- c(result, list(partial_kappa=partial_kappa))
  }
  # # # # # # #
  ##############
  
  return(result)
} 
