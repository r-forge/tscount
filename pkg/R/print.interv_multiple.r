print.interv_multiple <- function(x, ...){
  if(!is.null(x$error_message)){
    warning(x$error_message)
    cat("\n")
  }else{
    cat("\nFound intervention(s): ", sep = "")
    cat("\n")
    print(x$interventions, ...)
    cat("\n")
    if(!is.null(x$fit_interv)){ 
      print(x$fit_interv, ...)
    }else{
      cat("\n")
    }
  invisible(x)
  }
}
