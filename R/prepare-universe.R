#' Prepare universe for modeling
#'
#' This function takes care of preparing a universe for a particular model with
#' the model's name equal to \code{class_name}, which incidentally is the
#' class name -- go figure. Key actions that take place within this function are
#' to filter unwanted variables, split the universe into training and test sets,
#' and save them in a standardized directory (i.e., \code{save_dir}).
#'
#' @import caret
#' @param universe data frame containing variable to be predicted
#' \code{class_name} along with an id variable and all modeling variables.
#' @param class_name character string of variable to be predicted -- should
#' have two levels \code{c(0, 1)}.
#' @param save_dir character path of directory into which the training universe
#' and testing universes will be saved.
#' @param list_of_models list of names of models to be removed when
#' \code{prepare_universe} is being run on several models.
#' These variables are removed from \code{universe} prior to modeling.
#' @param id character vector of the id column. This defaults to
#' \code{"id"}, but could be changed if necessary.
#' @export
#' @keywords modeling universe
#' @seealso \link{train_universe}
#' @examples
#' # prepare_universe(df, "unsure", "/Dropbox/output_dir/")
#' @include println.R
#' @include has.R
#' @include preprocess.R
#'
prepare_universe = function(
  universe, class_name, save_dir,list_of_models=NULL){
  # remove duplicates
  universe = unique(universe)
  universe$id <- NULL
  println(class_name, " has ", sum(!is.na(universe[[class_name]])),
          " valid values", " and ", sum(is.na(universe[[class_name]])),
          " missing values.")
  universe = universe[!is.na(universe[[class_name]]), ]
  Class = universe[[class_name]]
  Class <- factor(Class, levels=c("1", "0"), labels=c("yes", "no"))
  # Remove class variables
  if(!is.null(list_of_models)){
    for(m in list_of_models){
      if(!is.null(universe[[m]]))
        universe[[m]] <- NULL
    }
  } else {
    universe[[class_name]] <- NULL
  }
  # Pre-process universe
  println("Pre-processing universe.")
  try(universe <- pre_process(universe, zscore=FALSE, impute=TRUE))
  # Add back onto predictor variables
  universe <- cbind(data.frame(Class = Class), universe)
  # Data splitting --------------------------------------------------
  # Split the universe into training dataset and test dataset
  println("Splitting universe into training and testing portions.\n")
  train_index = createDataPartition(universe$Class, p = 0.8)[[1]]
  train_data = universe[train_index, ]
  test_data = universe[-train_index, ]
  # Save the universes
  println("Saving training data to ", save_dir, class_name, "-train.rds")
  saveRDS(train_data, paste0(save_dir, class_name, "-train.rds"))
  println("Saving testing data to ", save_dir, class_name, "-test.rds")
  saveRDS(test_data, paste0(save_dir, class_name, "-test.rds"))
}


#' @describeIn prepare_universe
#' @export
#' @param config
#'
prepare_universes = function(config){
  println("Preparing the seperate modeling universes.")
  stopifnot(file.exists(paste0(config$dump_dir, "universe.rds")))
  universe = readRDS(paste0(config$dump_dir, "universe.rds"))
  for(m in names(config$models)){
    if(config$models[[m]] == FALSE){
      cat("Skipping", m, "(set to FALSE)\n")
    } else if(has(names(config$models), paste0(m, "_del"))){
      cat("Preparing universe for", m, "\n")
      i = universe[,paste0(m, "_del")]
      i <- which(!i)
      prepare_universe(
        universe[i,], m, config$testsets_dir, gsub(
          "[.]", "_", names(config$models)))
    } else if(has(m, "[.]del")) {
      cat("Skipping", m, "\n")
    } else {
      cat("Preparing universe for", m, "\n")
      prepare_universe(
        universe, m, config$testsets_dir, gsub(
          "[.]", "_", names(config$models)))
    }
  }
}
