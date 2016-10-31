#' Aggregate data using fast dplyr functions
#' 
#' Easily accesible, much faster implementation of aggregation operations.
#' 
#' @examples
#' apa.aggregate(data=data, factors=c("id","bf1","wf1"), dv="korrekt", fun=mean, na.rm=TRUE)
#' @export

apa.aggregate <- function(...) {
  
  args <- list(...)
  
  args <- papaja:::defaults(
    args
    , set = list(
      na.rm = NULL
    )
    , set.if.null = list(
      fun = args$fun.aggregate
    )
  )
  
  args$fun.aggregate <- NULL
  # print(args)
  
  args$factors <- gsub(args$factors, pattern = " ", replacement = "_")
  colnames(args$data) <- gsub(colnames(args$data), pattern = " ", replacement = "_")
  
  out <- do.call("fast.aggregate", args)
  colnames(out) <- gsub(colnames(out), pattern = "_", replacement = " ")
  return(out)
}

# for compatibility reasons
#' @export

.aggregate <- apa.aggregate

# non-exported
fast.aggregate <- papaja:::fast_aggregate
# rm(list=ls())
