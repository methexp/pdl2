#' @export
#' 
apa.glm <- function(...){
  
  args <- list(...)
  
  # generate a description of the conducted ANOVA
  
  factors <- c(args$between, args$within)
  
  
    name <- paste0(
      nlevels(as.factor(args$data[[factors[1]]]))
      , " (*"
      , factors[1]
      , ifelse(is.factor(args$data[[factors[1]]]), paste0("*: ", paste(levels(args$data[[factors[1]]]), collapse = " vs. ")), "*")
      , ")"
      )

  
  if(length(factors)>1) {
    for (i in 2:length(factors)){
      name <- paste(c(name, paste0(nlevels(as.factor(args$data[[factors[i]]])), " (*", factors[i], ifelse(is.factor(args$data[[factors[i]]]), paste0("*: ", paste(levels(args$data[[factors[i]]]), collapse = " vs. ")), "*"), ")")), collapse = " $\\times$ ")
    }
  }
  
  # strip whitespace from factor names
  if(!is.null(args$between)) args$between <- gsub(pattern = " ", replacement = "_", args$between)
  if(!is.null(args$within)) args$within <- gsub(pattern = " ", replacement = "_", args$within)
  args$id <- gsub(pattern = " ", replacement = "_", args$id)
  args$dv <- gsub(pattern = " ", replacement = "_", args$dv)
  colnames(args$data) <- gsub(pattern = " ", replacement = "_", colnames(args$data))
  
  print_args <- list()
  print_args$MSE <- args$MSE
  
  args <- papaja:::defaults(args, set = list(MSE = NULL), set.if.null = list(return = "Anova", fun_aggregate = mean))
  
  aggregated <- papaja:::fast_aggregate(data = args$data, factors = c(args$id, args$between, args$within), dv = args$dv, fun = args$fun_aggregate)
  
  aov_out <- do.call("aov_ez", args)
  

  
  print_args <- papaja:::defaults(print_args, set.if.null= list(x = aov_out))

  tmp <- do.call(papaja::apa_print, print_args)
  print_out <- tmp$full
  print_out$table <- tmp$table
  print_out$name <- name
  
  return(print_out)

}

