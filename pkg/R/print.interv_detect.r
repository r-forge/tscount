print.interv_detect <- function(x, ...){
  if(!is.null(x$error_message)){
    warning(x$error_message)
    cat("\n")
  }else{
    cat("\nMaximum test statistic: ", x$test_statistic, " at time ", x$tau_max, sep = "")
    cat("\n")
    if(!is.null(x$fit_interv)){ 
      print(x$fit_interv, ...)
    }else{
      cat("\n")
    }
    invisible(x)
  }
}
