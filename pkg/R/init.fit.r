init.fit <- function(allobj, linkfunc){
  envi <- environment()
  allobj <- c(allobj, list(linkfunc=linkfunc))
  with(allobj, { #run the following code in an environment with all elemnts of the named list 'allobj' are available as objects of this name
    if(linkfunc=="identity") trafo <- function(x) x
    if(linkfunc=="log") trafo <- function(x) if(!is.null(x)) log(x+1) else NULL  
    param_init <- list(intercept=NULL, past_obs=NULL, past_mean=NULL, xreg=NULL)
    if(init.control$method == "fixed"){ #fixed values, use given ones where available
      param_init$intercept <- if(!is.null(init.control$intercept)) init.control$beta_0 else 1
      param_init$past_obs <- if(!is.null(init.control$past_obs)) init.control$past_obs else rep(0, p)
      param_init$past_mean <- if(!is.null(init.control$past_mean)) init.control$past_mean else rep(0, q)
      param_init$xreg <- if(!is.null(init.control$xreg)) init.control$xreg else rep(0, r)
    }else{
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
    }
    if(init.control$method == "GLM"){
      delayed_ts <- function(x, timser) c(rep(0,x), timser[(x:length(timser))-x])
      dataset <- data.frame(timser=ts_init, trafo(sapply(model$past_obs, delayed_ts, timser=ts_init)), model$xreg[init_use,])
      startingvalues <- c(trafo(mean(ts_init)), rep(0, ncol(dataset)-1))
      glm_fit <- suppressWarnings(glm(timser ~ ., family=poisson(link=linkfunc), data=dataset, start=startingvalues)$coefficients)
      param_init$intercept <- intercept <- glm_fit[1]
      param_init$past_obs <- glm_fit[1+P] 
      param_init$past_mean <- rep(0, q)
      param_init$xreg <- glm_fit[1+p+R]
    }
    if(init.control$method %in% c("MM", "CSS", "ML", "CSS-ML")){ #approaches via an ARMA representation of the process, which differ only in the method to fit the ARMA process
      ts_init <- trafo(ts_init)
      k <- max(p_max, q_max)
      K <- seq(along=numeric(k)) #sequence 1:k if k>0 and NULL otherwise    
      if(init.control$method == "MM"){ #moment estimator via ARMA(1,1) representation, assume parameters for higher order to be zero
        if(k > 0){ #non-trivial case for q>0 and p>0
          momest <- momest_arma11(ts_init)
          ma <- c(momest["ma1"], rep(0,k-1)) #set higher order parameters to zero
          ar <- c(momest["ar1"], rep(0,k-1)) #see above
          intercept <- momest["intercept"]
        }else{
          ar <- ma <- NULL
          intercept <- mean(ts_init)
        }  
        regressors <- if(!is.null(init.control$xreg)) init.control$xreg else rep(0, r)  
      }
      if(init.control$method %in% c("CSS", "ML", "CSS-ML")){ #least squares or maximum likelihood estimator via ARMA(k,k) representation
        arma_fit <- as.numeric(suppressWarnings(arima(ts_init, order=c(k,0,k), xreg=model$xreg, transform.pars=TRUE, method=init.control$method, optim.method=init.control$optim.method, optim.control=init.control$optim.control)$coef)) #Supress warning messages, which occur quite frequently and are not very relevant to the user, as this is only an initial estimation. However, the interested user can find detailed information on this optimisation in the output.                     
        ma <- arma_fit[k+K]
        ar <- arma_fit[K]
        intercept <- arma_fit[k+k+1]
        regressors <- arma_fit[k+k+1+R] 
      }
      param_init$past_obs <- ar[model$past_obs]+ma[model$past_obs]
      param_init$past_mean <- -ma[model$past_mean] 
      param_init$intercept <- intercept*(1-sum(param_init$past_obs)-sum(param_init$past_mean))
      param_init$xreg <- regressors
    }
    assign("result", param_init, envir=envi)
  })
  return(result)
}

momest_arma11 <- function(ts){
  #Moment estimators for ARMA(1,1) process
  #ts: Numeric vector. Time series assumed to be a relaisation of an ARMA(1,1) process.
  m <- mean(ts)
  a <- acf(ts, lag.max=2, plot=FALSE)$acf[-1,,1]
  psi1 <- a[2]/a[1]
  #The MA parameter is the solution of a quadratic equation in monic form (i.e. the first coefficient is 1) which is solved using the pq formula:
  Qe <- (psi1^2-2*a[1]*psi1+1)/(a[1]-psi1)
  Pu <- 1
  discriminant <- Qe^2-4*Pu
  if(discriminant>=0){ #the solutions are real
    solutions <- (-Qe + c(+1,-1)*sqrt(discriminant))/2
  }else{ #the soulutions are complex (namely -Qe/2 + c(+1,-1)*i*sqrt(-discriminant)), use their orthogonal projection on the real axis instead
    solutions <- -Qe/2
  }
  theta1 <- solutions[abs(solutions)<=1][1] #choose a solution for which the resulting ARMA(1,1) process can be stationary (although this is not yet guaranteed only by fulfilling abs(.)<=1)
  result <- c(ar1=psi1, ma1=-theta1, intercept=m) #different parametrisation
  return(result)
}
