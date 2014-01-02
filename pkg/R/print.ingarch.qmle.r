print.ingarch.qmle <- function(x, digits=max(3, getOption("digits") - 3), ...){
  cat("\nCall:\n", paste(deparse(x$call), sep="\n", collapse="\n"), 
      "\n\n", sep="")
  if(length(coef(x))){
    cat("Coefficients:\n")
    print.default(format(coef(x), digits=4), print.gap=2, quote=FALSE)
  }else{ 
    if(length(x$init)){
      cat("Initial Coefficients:\n")
      print.default(format(x$init, digits=4), print.gap=2, quote=FALSE)
      warning("only initial estimation was made")
    }else{
      cat("No coefficients\n")
    }
  }
  cat("\n")
  invisible(x)
}
