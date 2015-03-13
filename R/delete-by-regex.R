#' Delete by regex
#'
#' Delete all columns in a data table that match a particular regular expression
#'
#' This function passes the data table by reference, so be warned that
#' \code{data} will always be changed. If you run with delete = FALSE, it will
#' list out the columns to be deleted, but will not delete them. This is
#' advised while testing out regular expressions.
#'
#' @param data data.table containing columns to be deleted
#' @param regex string containing regular expression to match for column names
#' @param delete boolean -- whether or not to actually delete the columns
#' @return data.table reference (it does not pass by value remember)
#' @export
#' @examples
#' #vf = delete_by_regex(vf, "electionreturns")
#' #vf = delete_by_regex(vf, "Idon'tknow", delete=FALSE)
delete_by_regex <- function(data, regex, delete=TRUE){
  stopifnot(is.data.table(data))
  del_list <- grep(regex, names(data), value=TRUE, ignore.case=T)
  if(!length(del_list)){
    warning("regex did not match with any column names.\nreturning unchanged.")
    return(data)
  }
  cat("The following", length(del_list), "columns will be deleted:\n")
  for(each in del_list){
    cat(each, "\n")
  }
  if(delete)
    data[, (del_list) := NULL, with=FALSE]
  return(data)
}
