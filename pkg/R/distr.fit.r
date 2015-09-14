distr.fit <- function(object, distr){ 
  if(distr=="poisson"){
    distrcoefs <- NULL
    sigmasq <- 0
  }
  if(distr=="nbinom"){  
    fitval <- object$fitted.values
    ts <- object$response
    n <- object$n_eff
    m <- length(object$coefficients)  
    #Pearson type estimator:
    find_root <- function(v) sum((ts-fitval)^2/(fitval*(1+fitval/v))) - n + m
    root <- try(uniroot(f=find_root, interval=c(0, 1e100)), silent=TRUE)  
    if(class(root)=="try-error"){
      size <- 1e+9
      warning("The dispersion parameter of the negative binomial distribution cannot be\nestimated. This indicates that there is no or only very little overdispersion\nin the data. The dispersion parameter was set to a value of 1e+9, which\nvirtually corresponds to a Poisson distribution. Try to use a Poisson\ndistribution with argument 'distr' set to \"poisson\" instead.")
    }else{
      size <- root$root
    }
    distrcoefs <- c(size=size)
    sigmasq <- 1/size
  }
  result <- list(distr=distr, distrcoefs=distrcoefs, sigmasq=sigmasq)
  return(result)
}
