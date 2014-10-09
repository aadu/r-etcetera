#' Pretty Num with a comma as big.mark
#'
#' @param x Any numeric value
#' @return Prettified number
#' @export
pN <- function(x){prettyNum(x, big.mark = ",")}
