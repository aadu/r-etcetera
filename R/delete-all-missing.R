#' Delete all missing
#'
#' Delete any columns with all missing values
#'
#' This function passes the data table by reference, so be warned that
#' \code{data} will always be changed. If you run with delete = FALSE,
#' it will list out the columns to be deleted, but will not delete them.
#' This is advised while testing out regular expressions.
#'
#' @import data.table
#' @import parallel
#' @param data data.table containing columns to be deleted
#' @param delete boolean -- whether or not to actually delete the columns
#' @return data.table reference (it does not pass by value remember)
#' @export
#' @examples
#' #vf = delete_all_missing(vf, "electionreturns")
#' #vf = delete_all_missing(vf, "Idon'tknow", delete = FALSE)
delete_all_missing = function(data, delete=TRUE){
  stopifnot(is.data.table(data))
  if(is.null(getOption('mc.cores')))
    options(mc.cores = detectCores())

  missing <- mclapply(data, function(x)(if(all(is.na(x))) T else F))
  if(!any(unlist(missing))){
    warning("No all-missing columns identified.\nreturning unchanged.")
    return(data)
  }
   all_missing= names(missing)[unlist(missing)]
  cat("The following", length(all_missing),
      "columns will be deleted:\n")
  for(each in all_missing){
    cat(each, "\n")
  }
  if(delete)
    data[, (x) := NULL, with=FALSE]
  return(data)
}


#' @describeIn delete_all_missing
#' @export
any_missing = function(x){
  missing_ix = which(unlist(mclapply(x,function(x)(any(is.na(x))))))
  if(!length(missing_ix))
    cat("None missing.\n")
  else
    for(ix in missing_ix){
      cat(names(x)[ix], "has missing values.\n")
    }
}

