#' Run models
#'
#' @import parallel
#' @import caret
#' @import yaml
#' @import stringr
#' @import doMC
#' @param config_path Location of yaml config file
#' @export
#' @include println.R
#' @include predict-outcomes.R
#' @include result-predictions.R
#' @include score-models.R
#' @include compare-models.R
#' @include winner.R
#' @include mc-near-zero-variance.R
#' @include append-features.R
#'
run_models = function(config_path){
  cores = detectCores()
  if(cores > 3)
    registerDoMC((cores - 2))
    options(mc.cores=(cores - 2))
  if(!file.exists(config_path))
    stop("Valid config file not found.\n")
  fix_slashes(config_path)
  config = yaml.load_file(config_path)
  parts = config$parts_to_run
  stopifnot(!is.null(config$new_data))
  stopifnot(!is.null(config$out_dir))
  stopifnot(!is.null(config$features))
  stopifnot(!is.null(config$dump_dir))
  create.dir(config$out_dir, "")
  create.dir(config$dump_dir, "")
  create.dir(mainDir = config$out_dir, subDir = "metrics")
  create.dir(mainDir = config$out_dir, subDir = "testsets")
  create.dir(mainDir = config$out_dir, subDir = "models")
  create.dir(mainDir = config$out_dir, subDir = "winners")
  create.dir(mainDir = config$out_dir, subDir = "logs")
  config = append_paths_to_config(config)
  ## PART 1: PREPARING THE UNIVERSES ##
  if(parts$load)
    append_features(config)
  ## PART 1: PREPARING THE UNIVERSES ##
  if(parts$prepare)
    prepare_universes(config)
  ## PART 2: TRAINING THE UNIVERSES ##
  if(parts$train)
    train_universes(config)
  ## METRICS NEW
  if(parts$metrics)
    compute_metrics(config)
  ## PAUSE
  if(parts$load || parts$prepare || parts$train){
    if(parts$winner || parts$score){
      readline("Please select winning models and press any key to continue.\n")
      fix_slashes(config_path)
      config = yaml.load_file(config_path)
      config = append_paths_to_config(config)
      parts = config$parts
    }
  }
  ## PICKING WINNERS
  if(parts$winner)
    models = winners(config)
  ## APPLY SCORES ##
  if(parts$score)
    score_models(config)
  # Save a copy of the config file.
  saveRDS(config, paste0(config$logs_dir, "config.rds"))
}


#' @describeIn run_models
#' @param mainDir
#' @param subDir (Default = NULL)
#' @export
create.dir = function(mainDir, subDir=NULL){
  if(is.null(subDir))
    subDir = ""
  file = gsub("/+", "/", paste(mainDir, subDir, "/", sep = "/", collapse = "/"))
  if (!is.na(file.info(file)[1, 'isdir'])){
    println(file, " already exists.")
  } else {
    println(file, " does not exist.")
    dir.create(file.path(file))
  }
  stopifnot(file.info(file)[1, 'isdir'])
}


#' @describeIn run_models
#' @export
fix_slashes = function(path){
  x = readLines(path)
  x = gsub("\\\\", "/", x)
  writeLines(x, path)
}


#' @describeIn run_models
#' @export
safe_paths = function(...){
  paths = paste(..., "/", sep = '/', collapse = '/')
  gsub("/+", "/", paths)
}

#' @describeIn run_models
#' @import data.table
#' @export
merge_by_key = function(large, small,
                        key_name='id', mult='first'){
  large = as.data.table(read.all(large))
  small = as.data.table(read.all(small))
  setkeyv(large, key_name)
  setkeyv(small, key_name)
  large[small, nomatch=0, mult=mult]
}


#' @describeIn run_models
#' @export
append_paths_to_config = function(config){
  config$metrics_dir = safe_paths(config$out_dir, "metrics")
  config$models_dir = safe_paths(config$out_dir, "models")
  config$winners_dir = safe_paths(config$out_dir, "winners")
  config$logs_dir = safe_paths(config$out_dir, "logs")
  config$testsets_dir = safe_paths(config$out_dir, "testsets")
  config
}
