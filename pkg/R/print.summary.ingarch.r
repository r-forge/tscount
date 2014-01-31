print.summary.tsglm <- function(x, ...){ 
  if(length(coef(x)) > 0){
    cat("\nCall:\n", paste(deparse(x$call), sep="\n", collapse = "\n"), "\n\n", sep="")
    cat("Coefficients:\n")
    print(format.data.frame(as.data.frame(coef(x)), digits=3), print.gap=2, quote = FALSE)
    cat(
      "\nNumber of coefficients: ", x$number.coef,
      "\nLog-likelihood: ", x$logLik,
      "\nAIC: ", x$AIC,
      "\nBIC: ", x$BIC,
      "\ncorrected AIC: ", x$corrected.AIC,
    sep = "")
    cat("\n\n")
  }else{ 
    if(length(x$init)>0){
      print(x, ...)
    }else{
      cat("No coefficients\n")
    }
  }
  invisible(x)  
}
