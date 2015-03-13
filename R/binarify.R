#' Binarify
#'
#' Function to convert character columns with 2 values to 0L and 1L.
#'
#' Function to convert all character columns of a data frame with two unique values
#' where one is equal to 'y', 'yes', or 'support' to 1L and all blanks to 0L
#'
#' @import data.table
#' @import dplyr
#' @param data data.frame containing the original voter file.
#' @param verbose boolean whether to print extra stuff.
#' @param copy_data boolean whether to force a copy of the original data
#' @return data.table -- either by reference (default) or value (if \code{copy_data = TRUE})
#' @export
#' @examples
#' #df = readRDS("/media/aaron/DATA/VF/ut.rds")
#' #binarify(df, verbose=T) # Note that by default, it edits the data by reference, no copy needed
#' #df = binarify(df, T)
#' #dt = binarify(df, T, copy_data = T) # this will force it to make a copy of the original data
binarify = function(data, verbose=FALSE, copy_data=FALSE) {
  tt <- system.time({
    if(verbose && !copy_data)
      cat("Important note.\nThis function references data by ",
          "reference by default.\nUnless copy_dt=TRUE, the ",
          "original data will be modified.\n", sep="")
    if(copy_data)
      df = copy(data)
    else
      df = data
    df = as.data.table(df)
    nm = names(df)
    nm = nm[sapply(df, class) == "character"]
    if(verbose && length(nm))
      cat(length(nm), "character columns detected.\n")
    # Return number of unique values for each column
    # (fast because its a base function)
    unique_vals <- mclapply(df[,nm, with=FALSE], unique)
    # Length
    unique_len <- unlist(mclapply(unique_vals, length))
    # select names with a length of 2
    unique_len <- unique_len[unique_len==2]
    nm <- names(unique_len)
    if(length(nm) && verbose)
      cat(length(nm), "columns identified with two unique values.\n")
    # So at this stage, we know that x is character
    # with two unique values, now we need to convert to 0 and 1
    # if one is named 'yes', 'agree', or 'support' then we choose that value
    i <- mclapply(df[,nm, with=FALSE], unique) %>%
      mclapply(function(x) (any(grepl(
        "(^ye?s?)|(^agree)|(^support)", x,
        ignore.case=T)))) %>%
      unlist %>% which %>% unname
    yeses <- nm[i]
    other <- nm[-i]
    if(length(yeses) && verbose)
      cat(length(yeses), "columns selected to be binarified ",
          "because one of their values is 'y(es)', 'agree', or 'support.'\n")
    # If nothing is found, we might as well return now
    if(!length(yeses)){
      warning("No suitable columns were identified. Returning unmodified.\n")
      return(data)
    }
    if(length(other) && verbose)
      message("A number of columns were not binarified",
              "because they did not have a 'y(es)',",
              "'support', or 'agree' statement:\n",
              paste(other, collapse=",\n"))
    if(verbose){
      cat("Attempting to convert character binary columns to integers:\n")
    }
    # the yesses should equal 1 and whatever other value should equal zero.
    # about 242 seconds
    for(nm in yeses){
      if(verbose)
        cat(nm)
      # needed to make data.table play nice
      new_val = rep(0L, nrow(df))
      if(verbose)
        cat(".")
      x <- df[,(nm), with=FALSE] %>% unlist %>% unname
      if(verbose)
        cat("..")
      new_val[grep("^ye?s?", x, ignore.case=T)] = 1L
      if(verbose)
        cat("...")
      df[,(nm) := new_val, with=F]
      if(verbose)
        cat("done\n")
    }
  })
  if(verbose)
    cat("\nBinarify completed in a total of",
        round(tt[3]/60, 2), "minutes.\n")
  return(df)
}
