print.interv_test <- function(x, ...){
  if(!is.null(x$error_message)){
    warning(x$error_message)
    cat("\n")
  }else{
    cat("\nChisq-Statistic: ", x$test_statistic, " on ", x$df, " degrees of freedom, p-value: ", x$p_value, sep = "")
    cat("\n")
    if(!is.null(x$fit_interv)){ 
      print(x$fit_interv, ...)
    }else{
      cat("\n")
    }
  invisible(x)
  }
}
