#' Source all files in a director
#'
#'@param path Directory path (default = \code{"R/"})
#'@return NULL
#'@export
source_dir = function(path = "R/"){
  if(!file.exists(gsub("/", "", path)))
    stop("Directory does not exist.\n")
  files = list.files(path, full.names = T)
  files = files[grep("[.][rR]$", files)]
  for(f in files){
    source(f, echo = FALSE)
  }
}
