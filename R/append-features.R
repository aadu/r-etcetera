#' Append features
#'
#' @param config
#' @export
#' @import data.table
#' @include println.R
append_features = function(config){
  find_char = function(x){
    names(x)[unlist(mclapply(x, is.character))]
  }
  cat("Attempting to load new data...")
  new_data = as.data.table(read.all(config$new_data))
  println("Got it.")
  n <- names(new_data)
  mn <- names(config$models)
  sdiff <- setdiff(n, c(mn, config$id))
  if(length(sdiff)){
    stop("The following variables were not in both config ",
         "file and new model data:\n  ", paste(sdiff, collapse= ", "))
  }
  cat("Attempting to load modeling table...")
  features = as.data.table(read.all(config$features))
  println("Got it.")
  if(any(unlist(mclapply(features, anyNA))))
    stop("Please ensure modeling table has been",
         "processed before starting modeling.")
  # Append modeling vars to Q2
  cat("Attempting to append modeling features to new data...")
  setkeyv(features, config$id)
  setkeyv(new_data, config$id)
  ## APPEND MODELING VARS TO Q2 ##
  universe <- features[new_data, nomatch=0, mult='first']
  ch <- find_char(universe)
  ch <- ch[!ch %in% config$id]
  cat("The following variables will be deleted: ")
  for(cha in ch){
    cat(cha, " ")
  }
  cat("\n")
  universe[,(ch):= NULL, with=F]
  universe <- as.data.frame(universe)
  println("Success!")
  if (nrow(universe) > length(unique(universe[[config$id]])))
    println("Duplicated IDs were removed!")
  universe <- universe[!duplicated(universe[[config$id]]),]
  if(!is.null(config[['remove']])){
    for(r in config[['remove']]){
      cat("Removed", r, "\n")
      universe[[r]] <- NULL
    }
  }
  saveRDS(universe, paste0(config$dump_dir, "universe.rds"))
  println("Successfully saved as",
          paste0(config$dump_dir, "universe.rds"), "\n")
  return(universe)
}
