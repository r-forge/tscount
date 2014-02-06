scoring <- function(object) UseMethod("scoring")

scoring.tsglm <- function(object){
  #Density functton of the conditional distribution:
  if(object$distr=="poisson") ddistr <- function(x, meanvalue, distrcoefs) dpois(x, lambda=meanvalue)
  if(object$distr=="nbinom") ddistr <- function(x, meanvalue, distrcoefs) dnbinom(x, mu=meanvalue, size=distrcoefs[["size"]])  
  #Cumulative distribution function of the conditional distribution (cf. marcal.tsglm):
  if(object$distr=="poisson") pdistr <- function(q, meanvalue, distrcoefs) ppois(q, lambda=meanvalue)
  if(object$distr=="nbinom") pdistr <- function(q, meanvalue, distrcoefs) pnbinom(q, mu=meanvalue, size=distrcoefs[["size"]])
  #Standard deviation of the conditional distribution (cf. marcal.tsglm):
  if(object$distr=="poisson") sddistr <- function(meanvalue, distrcoefs) sqrt(meanvalue)
  if(object$distr=="nbinom") sddistr <- function(meanvalue, distrcoefs) sqrt(meanvalue + meanvalue^2/distrcoefs[["size"]])
  n <- object$n_obs 
  p_y <- quadrat_p <- 0 #auxiliary objects
  logarithmic <- quadratic <- spherical <- rankprob <- dawseb <- normsq <- sqerror <- 0 #scores
  for(t in 1:n){
    y <- object$ts[t]
    mu <- fitted(object)[t]
    sigma <- sddistr(meanvalue=mu, distrcoefs=object$distrcoefs)
    p_y <- ddistr(y, meanvalue=mu, distrcoefs=object$distrcoefs)
    quadrat_p <- sum(ddistr(0:(1000+round(mu*10,0)), meanvalue=mu, distrcoefs=object$distrcoefs)^2)
    logarithmic <- logarithmic + (- log(p_y)/n)
    quadratic <- quadratic + (- 2*p_y + quadrat_p)
    spherical <- spherical + (- p_y/sqrt(quadrat_p))
    rankprob <- sum((pdistr(0:(1000+round(mu*10,0)), meanvalue=mu, distrcoefs=object$distrcoefs) - as.integer(y <= 0:(1000+round(mu*10,0))))^2)
    normsq <- normsq + ((y-mu)/sigma)^2 
    dawseb <- dawseb + ((y-mu)/sigma)^2 + 2*log(sigma)
    sqerror <- sqerror + (y-mu)^2
  }
  result <- c(
    logarithmic=logarithmic,
    quadratic=quadratic,
    spherical=spherical,
    rankprob=rankprob,
    dawseb=dawseb,
    normsq=normsq,
    sqerror=sqerror
  )
  return(result)
}
