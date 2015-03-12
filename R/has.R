#' Regex to see if vector contains value
#'
#' @param x vector to be evaluated for value
#' @param regex expression
#' @return Boolean, whether or not the vector contains the value
has <- function(x, regex){
  if(length(grep(regex, x, ignore.case = TRUE)) > 0) return(TRUE) else  return(FALSE)
}


#' @describeIn has'
#' @export
safe_paths = function(...){
  paths = paste(..., "/", sep = '/', collapse = '/')
  gsub("/+", "/", paths)
}


#' @describeIn has'
#' @export
fix_slashes = function(path){
  x = gsub("\\\\", "/", readLines(path))
  writeLines(x, path)
}
