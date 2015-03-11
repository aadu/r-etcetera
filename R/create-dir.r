#' Create a new subdirectory
#'
#' @param mainDir Path to the directory to which you'd like to add a subdirectory
#' @param subDir Name of the subdirectory
#' @return NULL
#' @export
create_dir = function(mainDir, subDir=NULL){
  if(is.null(subDir))
    subDir = ""
  file = gsub("/+", "/", paste(mainDir, subDir, "/", sep = "/", collapse = "/"))
  if (!is.na(file.info(file)[1,'isdir'])) {
    println(file, " already exists.")
  } else {
    println(file, " does not exist.")
    dir.create(file.path(file))
  }
  stopifnot(file.info(file)[1,'isdir'])
}
