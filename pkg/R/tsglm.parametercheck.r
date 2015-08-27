tsglm.parametercheck <- function(param, link=c("identity", "log"), stopOnError=TRUE, silent=TRUE){
  #Check parameter vector of a count time series following GLMs

  ##############
  #Checks and preparations:
  link <- match.arg(link)
  if(link == "log") parametercheck <-  loglin.parametercheck
  if(link == "identity") parametercheck <-  ingarch.parametercheck
  
  if(stopOnError){  
    result <- parametercheck(param)  
  }else{
    result <- try(parametercheck(param), silent=silent)
    if(class(result)=="try-error") result <- FALSE
  } 
  return(result)
}
