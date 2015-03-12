#' Timed try function
#'
#' @param expr Expression to be wrapped in try function
#' @param cpu (default=Inf)
#' @param elapsed (default=Inf)
#' @return NULL or results of \code{expr}
#' @export
#'
timed_try = function(expr, cpu=Inf, elapsed=Inf) {
  results = try({
    setTimeLimit(cpu, elapsed)
    expr}, silent=TRUE)
  if(inherits(y, "try-error")) NULL else results
}
