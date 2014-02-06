pit <- function(object, ...) UseMethod("pit")

pit.tsglm <- function(object, bins=10, ...){
  #Cumulative distribution function of the conditional distribution (cf. marcal.tsglm):
  if(object$distr=="poisson") pdistr <- function(q, meanvalue, distrcoefs) ppois(q, lambda=meanvalue)
  if(object$distr=="nbinom") pdistr <- function(q, meanvalue, distrcoefs) pnbinom(q, mu=meanvalue, size=distrcoefs)
  n <- object$n_obs
  x <- seq(from=0, to=1, by=0.0001)
  pit <- numeric(length(x))
  for(i in 1:length(object$ts)){
    P_x <- pdistr(object$ts[i], meanvalue=fitted(object)[i], distrcoefs=object$distrcoefs)
    if(object$ts[i]!=0){
      P_x_1 <- pdistr(object$ts[i]-1, meanvalue=fitted(object)[i], distrcoefs=object$distrcoefs)
    }else{
      P_x_1 <- 0
    }
    pit <- pit + punif(x, P_x_1, P_x)/n
  }
  hist_args <- modifyList(list(main="PIT Histogram", xlab="Probability Integral Transform", ylab="Relative Frequency"), list(...)) #the default arguments can be overriden by those provided in the ... argument
  do.call("hist", args=c(list(pit, breaks=seq(0, 1, length=bins+1), freq=FALSE, include.lowest=TRUE), hist_args))
}
