#' Read any common file depending upon file extension
#'
#' @param x Path to file
#' @param to_keep Character vector of variables to keep (default = \code{NULL})
#' @return data frame
#' @include has.R
#' @export
read_any <- function(x, to_keep = NULL){
  if(has(x, "[.]txt$"))
    return(read.delim(file=x, header = TRUE, as.is=TRUE))
  else if(has(x, "[.]csv$"))
    return(read.csv(file=x, header = TRUE, as.is=TRUE))
  else if(has(x, "[.]sav$")){
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
  } else if(has(x, "[.]rds$"))
    return(readRDS(x))
  else
    cat("Unable to find appropriate read function.\n")
}
