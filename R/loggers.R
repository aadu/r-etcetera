#' Like cat except with a newline character at the end and default sep of ""
#'
#' @param ... Character expressions to be printed to screen
#' @return NULL
#' @export
println = function(...){
  cat(..., "\n", sep="")
}
