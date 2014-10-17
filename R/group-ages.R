#' Group into our standard age groups
#'
#' @param x vector of ages
#' @return grouped ages
#' @export
#'
group_ages <- function(x){
  stopifnot(is.numeric(x))
  out <- x
  out[x < 35] <- "18-34"
  out[x >= 35 & x < 55] <- "35-54"
  out[x >= 55] <- "55+"
  out
}
