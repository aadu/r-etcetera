#' Pretty Num with a comma as big.mark
#'
#' @export
formatters <- function(x)(return(x))

#' @describeIn formatters
#' @export
pN <- function(x){prettyNum(x, big.mark = ",")}

#' @describeIn formatters
#' @export
totitle <- function(x){gsub("\\b([a-z])([a-z]+)", "\\U\\1\\L\\2" ,x, perl=TRUE)}

