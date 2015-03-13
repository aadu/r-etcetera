#' Delete zero var
#'
#' Delete all columns in a data table that have one unique value
#'
#' This function passes the data table by reference, so be warned that
#' \code{data} will always be changed. If you run with delete = FALSE, it will
#' list out the columns to be deleted, but will not delete them. This is
#' advised while testing out regular expressions.
#'
#' @import data.table
#' @import parallel
#' @import dplyr
#' @param data data.table containing columns to be deleted
#' @param delete boolean -- whether or not to actually delete the columns
#' @return data.table reference (it does not pass by value remember)
#' @export
#' @examples
#' #vf = delete_zero_var(vf)
#' #vf = delete_zero_var(vf, delete=FALSE)
delete_zero_var = function(data, delete=TRUE){
  stopifnot(is.data.table(data))
  nm = names(data)
  len_unique = mclapply(data, unique) %>% lapply(length)
  zero_var = names(which(unlist(len_unique) == 1))
  if(!length(zero_var)){
    warning("No columns were identified with only one unique value.",
            "\nreturning unchanged.")
    return(data)
  }
  unique_vals = unlist(mclapply(data[, zero_var, with=F], unique))
  cat("The following", length(zero_var),
      "columns will be deleted:\n")
  for(i in seq_along(zero_var)){
    cat(zero_var[i], ": '", unique_vals[i], "'\n", sep="")
  }
  if(delete)
    data[, (zero_var) := NULL, with=FALSE]
  return(data)
}
