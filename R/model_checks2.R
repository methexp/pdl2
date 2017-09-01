#' Calculate Klauer's fit diagnostics
#'
#' This function calculates Klauer's fit statistics T_A and T_B
#' 
#' T_A: How well can the model describe the mean category frequencies (aggregated over both participants and items)?
#' T_A1: How well can the model desribe the mean category frequencies (per participant, aggregated over items)?
#' T_A2: How well can the model desribe the mean category frequencies (per item, aggregated over participants)?
#' T_A3: How well can the model desribe the mean category frequencies (per participant-item comnbination)?
#' T_B1: How well can the model account for the variances and covariances introduced by participants?
#' T_B2: How well can the model account for the variances and covariances introduced by items?
#'
#' @export
fit.diagnostics <- function(theta, data, full = FALSE, cpus = 4) {
  
  if(!all(c("ci", "ce", "ai", "ae") %in% dimnames(theta)[[4]]))
    stop("You did not provide parameter names in your array. dimnames(theta)[[4]] must be c(\"ai\", \"ae\", \"ci\", \"ce\")")


  snowfall::sfInit(parallel = TRUE, cpus = cpus)

  draws <- snowfall::sfApply(x = data, margin = c(1:2, 4), fun = sum)

  if(cpus>1) {
    value <- as.data.frame(t(snowfall::sfApply(x = theta, margin = 1, fun = T1, data = data, draws = draws)))
  } else {
    value <- as.data.frame(t(apply(X = theta, MARGIN = 1, FUN = T1, data = data, draws = draws)))
  }
  snowfall::sfStop()

  p.statistics <- colMeans(value[, 1:6] > value[, 7:12])

  observed <- apply(X = value[, 7:12], MARGIN = 2, FUN = median)
  expected <- apply(X = value[, 1:6], MARGIN = 2, FUN = median)


  tmp <- data.frame(
    "statistic" = gsub(colnames(value)[1:6], pattern = "exp.", replacement = "")
    , "observed" = as.numeric(observed)
    , "expected" = as.numeric(expected)
    , "p" = p.statistics
    )
  rownames(tmp) <- NULL

  class(tmp) <- c("mptfit", "data.frame")

  if(full ==TRUE) {
    tmp <- list("fit" = tmp, "values" = value)
  }

  return(tmp)
  # return(draws)
}

## Calculate T statistics for one single MCMC draw
#' @export
T1 <- function(theta, data, draws){

  K <- dim(data)[4]/2
  dims <- dim(theta)
  I <- dims[1]    # number of participants
  J <- dims[2]    # number of items
  if(J!=1){
    if(K==1){data <- abind::abind(data[,,,1], data[,,,2], along = 3)}
    if(K==2){data <- abind::abind(data[,,,1], data[,,,2], data[,,,3], data[,,,4], along = 3)}
  }
  if(J==1){
   if(K==1){data <-abind::abind(data[,1,,1], data[,1,,2], along = 2)}
   if(K==2){data <-abind::abind(data[,1,,1], data[,1,,2], data[,1,,3], data[,1,,4], along = 2)}
   data <- array(data, dim = c(dim(data)[1], 1, dim(data)[2]))
  }
  # print(dim(data))
  predicted.data <- array(NA, dim = dim(data))
  expected.data <- predicted.data
  
  # number of within Ss conditions
  K <- dim(predicted.data)[3]/4

  for (i in 1:I){
    for (j in 1:J){
      for (k in 1:K){
        sel <- (k-1) * 4 + 1:4
        seld <- (k-1) * 2 + 1:2
        tmp <- predict.data(theta = theta[i, j, sel], draws = draws[i, j, seld])
        predicted.data[i, j, sel] <- tmp$data
        expected.data[i, j, sel] <- tmp$expected
      }
    }
  }

  fit.sim <- statistics(predicted.data = predicted.data, expected.data = expected.data)
  names(fit.sim) <- sapply(X = names(fit.sim), FUN = function(x){paste0("exp.", x)})
  fit.emp <- statistics(predicted.data = data, expected.data = expected.data)
  names(fit.emp) <- sapply(X = names(fit.emp), FUN = function(x){paste0("obs.", x)})

  return(c(fit.sim, fit.emp))


}






#' @export
print_fit <- function(fit) {
  p_fit <- lapply(X = fit, FUN = function(fit){
    format(fit, digits = 2, nsmall = 2, zero.print = TRUE, big.mark = ",")
  })
  p_fit$p_T_A <- papaja::printp(fit$p_T_A)
  p_fit$p_T_B1 <- papaja::printp(fit$p_T_B1)
  p_fit$p_T_B2 <- papaja::printp(fit$p_T_B2)

  ## Add 'equals' where necessary

  p_fit <- unlist(p_fit)
  ps <- c("p_T_A", "p_T_B1", "p_T_B2")
  eq <- ps[!grepl(p_fit[ps], pattern = "<|>|=")]

  for (i in eq) {
    p_fit[i] <- paste0("= ", p_fit[i])
  }
  # return(p_fit)




  string <- paste0(
    "$$T_{A}^{observed} = "
    , p_fit["T_A"]
    , ", T_{A}^{expected} = "
    , p_fit["exp.T_A"]
    , ", p "
    , p_fit["p_T_A"]
    , "\\\\"
    , "T_{B1}^{observed} = "
    , p_fit["T_B1"]
    , ", T_{B1}^{expected} = "
    , p_fit["exp.T_B1"]
    , ", p "
    , p_fit["p_T_B1"]
    , "\\\\"
    , "T_{B2}^{observed} = "
    , p_fit["T_B2"]
    , ", T_{B2}^{expected} = "
    , p_fit["exp.T_B2"]
    , ", p "
    , p_fit["p_T_B2"]
    , "$$"
  )
  return(string)
}

# fit <- list()
# fit$T_A <- 1
# fit$T_B1 <- 2
# fit$T_B2 <- 3
# # Bayesian p values
# fit$p_T_A <- .05
# fit$p_T_B1 <- .99999999
# fit$p_T_B2 <- .0000001
# # expected statistics
# fit$exp.T_A <- 1
# fit$exp.T_B1 <- 2
# fit$exp.T_B2 <- 3
#
# unlist(fit)
# unlist(fit)
#
#
# print_fit(fit)

statistics <- function(predicted.data, expected.data) {

  # Observed category frequencies (overall)
  observed.means <- apply(X = predicted.data, MARGIN = c(3), FUN = mean, na.rm = TRUE)
  
  # Expected category frequencies (overall)
  expected.means <- apply(X = expected.data, MARGIN = c(3), FUN = mean, na.rm = TRUE)

  # Observed category Means (per participant)
  observed.means.I <- apply(X = predicted.data, MARGIN = c(1, 3), FUN = mean, na.rm = TRUE)
  
  # Expected Category Means (per participant)
  expected.means.I <- apply(X = expected.data, MARGIN = c(1, 3), FUN = mean, na.rm = TRUE)

  # Observed category Means (per item)
  observed.means.J <- apply(X = predicted.data, MARGIN = c(2, 3), FUN = mean, na.rm = TRUE)

  # Expected Category Means (per item)
  expected.means.J <- apply(X = expected.data, MARGIN = c(2, 3), FUN = mean, na.rm = TRUE)

  # Observed Category Means (per participant/item combination)
  observed.means.IJ <- apply(X = predicted.data, MARGIN = c(1, 2, 3), FUN = mean, na.rm = TRUE)

  # Expected Category Means (per participant/item combination)
  expected.means.IJ <- apply(X = expected.data, MARGIN = c(1, 2, 3), FUN = mean, na.rm = TRUE)


  # Observed Variances and Covariances across Participants
  aggregated.I <- apply(X = predicted.data, MARGIN = c(1,3), FUN = sum, na.rm = TRUE)
  observed.I <- var(aggregated.I, na.rm = TRUE)

  # Expected Variances and Covariances across Participants
  aggregated.I <- apply(X = expected.data, MARGIN = c(1,3), FUN = sum, na.rm = TRUE)
  expected.I <- var(aggregated.I, na.rm = TRUE)

  # Observed Variances and Covariances across Items
  aggregated.J <- apply(X = predicted.data, MARGIN = 2:3, FUN = sum, na.rm = TRUE)
  observed.J <- var(aggregated.J, na.rm = TRUE)

  # Expected Variances and Covariances across Items
  aggregated.J <- apply(X = expected.data, MARGIN = 2:3, FUN = sum, na.rm = TRUE)
  expected.J <- var(aggregated.J, na.rm = TRUE)

  T_A <- sum((observed.means -  expected.means)^2 / expected.means, na.rm = TRUE)
  T_A1 <- sum((observed.means.I -  expected.means.I)^2 / expected.means.I, na.rm = TRUE)
  T_A2 <- sum((observed.means.J -  expected.means.J)^2 / expected.means.J, na.rm = TRUE)
  T_A3 <- sum((observed.means.IJ -  expected.means.IJ)^2 / expected.means.IJ, na.rm = TRUE)

  
  ###############################################################################################
  ##
  ## Correction term
  ##
  ###############################################################################################
  N <- nrow(aggregated.I)
  
  # Calculate the average of the expected variances and covariances at the person level
  # for each tree
  n_tree <- length(observed.means)/2

  for (tree in 1:n_tree){
    cats <- (tree-1)*2 + 1:2
    # print(cats)
    sel <- 1:(n_tree * 2) %in% cats
    # print(sel)
    tmp <- as.matrix(aggregated.I)[, cats, drop = FALSE]
    # print(tmp)
    n <- rowSums(tmp)
    # variance for multinomial: n*p_i*(1-p_i)
    mean.ind.cov <-  diag(colMeans(tmp*(1-tmp/n), na.rm = TRUE))
    
    # covariance for multinomial: -n*p1*p2
    combs <- combn(1:sum(sel),2)
    for(c in 1:ncol(combs)){
      mean.ind.cov[combs[1,c],combs[2,c]] <- mean(-tmp[,combs[1,c]]*tmp[,combs[2,c]]/n, na.rm = TRUE)
    }
    mean.ind.cov[lower.tri(mean.ind.cov)] <- mean.ind.cov[upper.tri(mean.ind.cov)]
    expected.I[sel, sel] <- expected.I[sel,sel] + (N-1)/N*mean.ind.cov
  }
  # print(observed.I)
  ###############################################################################################
  ##
  ## Correction term
  ##
  ###############################################################################################
  N <- nrow(aggregated.J)
  
  # Calculate the average of the expected variances and covariances at the item level
  # for each tree
  # print(aggregated.J)
  for (tree in 1:n_tree){
    cats <- (tree-1)*2 + 1:2
    sel <- 1:(n_tree * 2) %in% cats
    # print(as.matrix(aggregated.J))
    tmp <- as.matrix(aggregated.J)[, cats, drop = FALSE]
    
    
    n <- rowSums(tmp)
    # variance for multinomial: n*p_i*(1-p_i)
    mean.ind.cov <-  diag(colMeans(tmp*(1-tmp/n), na.rm = TRUE))
    
    # covariance for multinomial: -n*p1*p2
    combs <- combn(1:sum(sel),2)
    for(c in 1:ncol(combs)){
      mean.ind.cov[combs[1,c],combs[2,c]] <- mean(-tmp[,combs[1,c]]*tmp[,combs[2,c]]/n, na.rm = TRUE)
    }
    mean.ind.cov[lower.tri(mean.ind.cov)] <- mean.ind.cov[upper.tri(mean.ind.cov)]
    expected.J[sel, sel] <- expected.J[sel,sel] + (N-1)/N*mean.ind.cov
  }
  # print(expected.J)
  

  # cov.I <- array(NA, dim = dim(expected.I))
  cov.I <- (observed.I - expected.I)^2 / sqrt(diag(expected.I) %*% t(diag(expected.I)))
  # for (i in 1:nrow(expected.I)){
  #   for (j in 1:ncol(expected.I)){
  #     cov.I[i, j] <- (observed.I[i, j] - expected.I[i, j])^2 / sqrt(expected.I[i, i] * expected.I[j, j])
  #   }
  # }

  # cov.J <- array(NA, dim = dim(expected.J))
  cov.J <- (observed.J - expected.J)^2 / sqrt(diag(expected.J) %*% t(diag(expected.J)))
  # for (i in 1:nrow(expected.J)){
  #   for (j in 1:ncol(expected.J)){
  #     cov.J[i, j] <- (observed.J[i, j] - expected.J[i, j])^2 / sqrt(expected.J[i, i] * expected.J[j, j])
  #   }
  # }

  T_B1 <- sum(cov.I, na.rm = TRUE)
  T_B2 <- sum(cov.J, na.rm = TRUE)

  return(c("T_A" = T_A, "T_A1" = T_A1, "T_A2" = T_A2, "T_A3" = T_A3, "T_B1" = T_B1, "T_B2" = T_B2))
}


#' @export

apa_print.mptfit <- function(x){
  y <- x
  y$p <- papaja::printp(x$p)
  y$observed <- papaja::printnum(x$observed, big.mark = "{,}")
  y$expected <- papaja::printnum(x$expected, big.mark = "{,}")
  class(y) <- "data.frame"


  ## Add 'equals' where necessary
  eq <- (1:nrow(y))[!grepl(y$p, pattern = "<|>|=")]
  for (i in eq) {
    y$p[i] <- paste0("= ", y$p[i])
  }


  short <- x$statistic# ("T_A", "T_A1", "T_A2", "T_B1", "T_B2")
  shorter <- gsub(short, pattern = "T_", replacement = "")
  long <- sapply(X = shorter, FUN = function(x){paste0("T_{", x, "}")})


  for(i in 1:nrow(y)){
    y$string[i] <- paste0(
      "$$"
      , long[i]
      , "^{observed} = "
      , y$observed[i]
      , ", "
      , long[i]
      , "^{expected} = "
      , y$expected[i]
      , ", p "
      , y$p[i]
      , ifelse(i!=nrow(y), ",", ".")
      , "$$"
    )
  }

  string <- paste(y$string, collapse = "\\  ")

  return(string)

}


#' @export

apa_print.information_criterion <- function(x) {
  printnum(x, digits = 2, big.mark = ",")
}


