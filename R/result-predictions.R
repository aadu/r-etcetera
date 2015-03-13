#' Result predictions
#'
#' @import parallel
#' @import yaml
#' @param m
#' @param config
#' @export
#'
result_predictions = function(m, config){
  files = list.files(
    config$models_dir, paste0('(?i)', m, '-.*[.]rds'), full.names=TRUE)
  models <- lapply(files, function(x)(try(readRDS(x))))
  nms = sapply(files, basename)
  names(nms) <- NULL
  nms = gsub(paste0(m,"-"), "", nms)
  nms = gsub("[.]rds", "", nms)
  nms = gsub("(?i)[^a-z]+", "", nms)
  (names(models) <- nms)
  println("Models loaded from models directory.")
  println("Computing results.")
  # Load up the training and test universes
  train_data = readRDS(paste0(config$testsets_dir, m, "-train.rds"))
  test_data = readRDS(paste0(config$testsets_dir, m, "-test.rds"))
  println("Train and test data loaded.")
  # Predict
  train_preds <- mclapply(
    models, function(x)(try(round(predict(x, newdata=train_data,
                                          type="prob")$yes, 5))))
  train_preds <- as.data.frame(do.call(cbind, train_preds))
  println("Training models all predicted.")
  # Observations
  train_obs <- data.frame(obs=relevel(train_data$Class, ref='no'))
  test_obs <- data.frame(obs=relevel(test_data$Class, ref='no'))
  # Get all of the calibration models
  cals <- lapply(
    train_preds, function(x, obs)(glm(obs ~ x, family='binomial')),
    obs=train_obs$obs)
  println("Just calculated sigmoid fixes.")
  # Fix with new models
  train_preds <- mclapply(
    cals, function(x)(try(round(predict(x, type="response"), 5))))
  tra <- mcMap(
    function(a, b)(try(unname(round(predict(
      a, newdata=data.frame(x=b), type="response"), 5)))), cals, train_preds)
  println("Training models all completed.")
  # Test predictions
  test_preds <- mclapply(models, function(x)(
    try(round(predict(x,newdata=test_data, type="prob")$yes, 5))))
  test_preds <- as.data.frame(do.call(cbind, test_preds))
  println("Test models predicted.")
  tes <- mcMap(function(a, b)(try(unname(round(predict(
    a, newdata=data.frame(x = b), type="response"), 5)))), cals, test_preds)
  tra <- cbind(data.frame(obs=train_obs), as.data.frame(tra))
  tes <- cbind(data.frame(obs=test_obs), as.data.frame(tes))
  results = list()
  results$train = tra
  results$test = tes
  println("Saving results out to",
          paste0(config$metrics_dir, m, "-results.rds"))
  saveRDS(results, paste0(config$metrics_dir, m, "-results.rds"))
  results
}


#' @describeIn result_predictions
#' @import yaml
#' @import devtools
#' @export
metricify = function(m, config){
  results = result_predictions(m, config)
  saveRDS(m, "./devel/model_name.rds")
  saveRDS(config, "./devel/config.rds")
  rmarkdown::render(
    "devel/compare-models.Rmd", output_format="html_document",
    output_file = paste0(m, "-metrics.html"), output_dir=config$metrics_dir,
    clean=TRUE)
}


#' @describeIn result_predictions
#' @param config
#' @export
compute_metrics = function(config){
  println("Metrics.\n")
  for(model_name in names(config$models)){
    if(config$models[[model_name]] != FALSE)
      try(metricify(model_name, config))
  }
}
