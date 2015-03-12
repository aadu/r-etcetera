#' Switch for vectors like ifelse
#'
#' @param x Vector of values on which to apply the switch
#' @param ... switch expressions
#' @return vector of new values
#' @export
vswitch = function(x, ...){
  results = rep(NA_integer_, length(x))
  i = which(!is.na(x))
  results[i] = as.numeric(sapply(x[i], switch, ...))
  results
}
