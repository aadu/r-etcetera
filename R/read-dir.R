#' Read all (csv/tab/txt) files in a directory
#'
#' @param path Directory path
#' @param combine Boolean, whether to return one data.frame or a list of
#' data.frames
#' @return Either a single aggregated data frame (if \code{combined = T}
#' [default]) or a list of data frames (if \code{combined = F}).
#' @import dplyr
#' @export
#' @include read-any.R
#'
read_dir = function(x, combine=TRUE){
  is.dir = function(x){file.info(x)[['isdir']]}
  if(!is.dir(x)){
    cat(x, "is not a directory.\n")
    return(read_any(x))
  }
  files = list.files(x, "(?i).*[.](csv)|(txt)|(dat)$", full.names=TRUE)
  if(length(grep("[.]csv$", files)))
    sep = ","
  else
    sep = "\t"
  for(i in seq_along(files))
    cat("File", i, ":", basename(files[i]), "\n")
  out <- lapply(files, read.table, sep=sep, quote="\"",
                fill=T, comment.char="", header=T, as.is=T)
  if(combine)
    out <- rbind_all(out)
  else
    names(out) <- basename(files)
  out
}
