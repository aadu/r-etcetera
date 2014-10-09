#' Read any common file depending upon file extension
#'
#' @param x Path to file
#' @param to_keep Character vector of variables to keep (default = \code{NULL})
#' @return data frame
#' @export
read_any <- function(x, to_keep = NULL){
  if(contains(x, "[.]txt$"))
    return(read.delim(file=x, header = TRUE, as.is=TRUE))
  else if(contains(x, "[.]csv$"))
    return(read.csv(file=x, header = TRUE, as.is=TRUE))
  else if(contains(x, "[.]sav$")){
    if(exists("rxImport")){
      if(!is.null(drop)){
        return(rxImport(x, varsToKeep = to_keep))
      } else {
        return(rxImport(x))
      }
    }
    else{
      library(foreign)
      return(read.spss(x, to.data.frame = TRUE, trim_values = TRUE))
    }
  } else if(contains(x, "[.]rds$"))
    return(readRDS(x))
  else
    cat("Unable to find appropriate read function.\n")
}
