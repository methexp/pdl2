#' Functions for t tests
#'
#'
#' @export
apa.t.test<-function(data, id, dv, between = NULL, within = NULL, fun.aggregate = mean, method = "bonferroni", n.comparisons=1, ...){
  
  for (i in c(id, between, within)) {
    data[[i]] <- droplevels(as.factor(data[[i]]))
  }
  
  # strip whitespace from column names
  if(!is.null(between)) between <- gsub(pattern = " ", replacement = "_", between)
  if(!is.null(within))  within <- gsub(pattern = " ", replacement = "_", within)
  id <- gsub(pattern = " ", replacement = "_", id)
  dv <- gsub(pattern = " ", replacement = "_", dv)
  colnames(data) <- gsub(pattern = " ", replacement = "_", colnames(data))

  value <- list()

  if(!is.null(between)&(is.null(within))){
    if(nlevels(data[[between]])>2){
      stop ("Your factor contains more than two levels.")
    }

    agg <- fast.aggregate(data = data, dv = dv, factors = c(id, between), fun = fun.aggregate)
    t.out <- t.test(formula=as.formula(paste(c(dv, "~", between), collapse="")), data = agg, ...)
     
#     # make p adjustment available
#     if(n.comparisons!=1){
#       t.out$p.value<-p.adjust(t.out$p.value, method=method, n=n.comparisons)
#     }
    t.out$statistic <- abs(t.out$statistic)

    lev.names<-levels(agg$between)
    n<-table(agg[[between]])
    n<-n[n>0]
    
    value$stat <- apa.t(t.out,n)
    
    M <- tapply(agg[[dv]], list(agg[[between]]), FUN = mean)
    SD <- tapply(agg[[dv]], list(agg[[between]]), FUN = sd)
    des <- rep("NA", 2)
    names(des) <- names(M)
    for (i in 1:2){
      des[i] <- paste0("*M* = ", printnum(M[i], gt1 = TRUE), ", *SD* = ", printnum(SD[i], gt1=TRUE))
    }
    value$des <- des
  }
  if(is.null(between)&(!is.null(within))){
    agg <- fast.aggregate(data = data, dv = dv, factors = c(id, within), fun = fun.aggregate)
    if(all(agg[[id]][agg[[within]] == levels(agg[[within]])[1]]==agg[[id]][agg[[within]] == levels(agg[[within]])[2]])) {
      t.out <- t.test(formula = as.formula(paste0(dv, "~", within)), data = agg, paired = TRUE)
      value$stat <- apa.t(t.out, nrow(agg)/2)
    } else {
      warning("There seem to be missing cases.")
    }
    
    M <- tapply(agg[[dv]], list(agg[[within]]), FUN = mean)
    SD <- tapply(agg[[dv]], list(agg[[within]]), FUN = sd)
    des <- rep("NA", 2)
    names(des) <- names(M)
    for (i in 1:2){
      des[i] <- paste0("*M* = ", printnum(M[i], gt1 = TRUE), ", *SD* = ", printnum(SD[i], gt1=TRUE))
    }
    value$des <- des
  }
  return(value)
}

#' @export
apa.t<-function(output,n=NA){
  
  t <-format(round(output$statistic,digits=2),nsmall=2)
  df<-format(round(output$parameter,digits=2))
  p <-output$p.value
  d <-NA
  if(output$method %in% c("One Sample t-test", "Paired t-test")){
    d<-round(output$statistic/sqrt(n),digits=2)
  }
  else{
    d<-round(output$statistic * sqrt((n[1]+n[2])/ (n[1]*n[2])), digits=2)
  }
  
  d<-format(d,nsmall=2)
  
  if(p<.001){
    p<-".001"
    value<-paste(c("$t(",df,") = ", t,"$, $p < ",p,"$, $d = ", d, "$"),collapse="")
  }
  else{
    p<-format(round(p,digits=3),nsmall=3)
    p<-unlist(strsplit(p,split="\\."))[2]
    value<-paste(c("$t(", df, ") = ",t,"$, $p = .", p,"$, $d = ", d, "$"),collapse="")
  }
  
  return(value)
}