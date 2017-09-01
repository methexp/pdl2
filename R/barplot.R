################################################################
##
## this is a copy from http://monkeysuncle.stanford.edu/?p=485
##
################################################################
#' @export
error.bar <- function(x, y, upper, lower=upper, length=0.1,...){
  if(length(x) != length(y) | length(y) !=length(lower) | length(lower) != length(upper))
    stop("vectors must be same length")
  arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...)
}

# returns a short character string for printing of number of non-missing values
#' @export
n.obs<-function(x, ...){
  y<-paste(c("n=",sum(x, ...)),collapse="")
  return(y)
}

# returns number of non-missing values
#' @export
individual.n<-function(x,na.rm){
  y<-sum(!is.na(x))
  return(y)
}

#' @export
#' 
apa.barplot <- papaja::apa_barplot