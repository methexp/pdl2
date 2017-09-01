
dic_waic <- function(pointLiks, deviance=NULL){
  # computes model comparison stats DIC & WAIC
  # for DIC, it takes as input a vector of the deviances computed in each sample 
  # for WAIC, it takes as input a samples X participants matrix of the point likelihoods (i.e., the likelihood for each person) computed in each sample
  # code taken from Avraham Adler via Stan User Group, https://groups.google.com/d/msg/stan-users/X2VO3JqyUlk/e7T9Mv0jyL0J

  if(!is.null(deviance)){
    ## compute DIC from deviance, if deviances are given
    DIC <- mean(deviance) + .5*var(deviance)
  } else {
    ## compute DIC from point Likelihoods
    # first, compute sum of pointLiks over participants
    mlik <- apply(pointLiks, 1, FUN=sum)
    dev <- -2*mlik
    # all.equal(dev, mlik) # TRUE
    m_dev <- mean(dev)
    pD <- .5*var(dev)
    
    DIC <- m_dev + pD
  }
  
## compute WAIC
  ##WAIC <- function(pointMatrix) {
  pointLiks <- exp(pointLiks) # needs Likelihoods, not logliks
  # from Avraham Adler via Stan User Group, https://groups.google.com/d/msg/stan-users/X2VO3JqyUlk/e7T9Mv0jyL0J
  # Using the definition of population variance E(X^2) - E(X)^2, colMeans, and the bias correction was faster than using apply and FUN = var.
  lppd <- sum(log(colMeans(pointLiks)))
  LogMatrix <- log(pointLiks)
  n <- dim(LogMatrix)[1]
  pWAIC2 <- sum((colMeans(LogMatrix^2) - colMeans(LogMatrix)^2)) * n / (n - 1)
  WAIC <- -2 * (lppd - pWAIC2) # return(-2 * (lppd - pWAIC2))
  
  output <-     list(
    "DIC" = DIC
    , "WAIC" = WAIC
    , "pD" = pD
  )
  
  class(output) <- "dic_waic"
  
  return(output)
}



apa_print.dic_waic <- function(x, which = "DIC"){
  
  x <- lapply(x, FUN = papaja::printnum, big.mark = "{,}", digits = 2)
  names_x <- paste0("\\mathit{", names(x), "}")
  
  string <- paste0(
    "$\\mathrm{DIC} = "
    , x$DIC
    , "$, $\\mathrm{WAIC} = "
    , x$WAIC
    , "$, $p_D = "
    , x$pD
    , "$"
  )
  
  return(string)
}
  