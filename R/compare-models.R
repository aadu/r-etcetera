#' Compare models
#'
#' @import xtable
#' @import sjPlot
#' @export
#' @include predict-outcomes.R
#' @include display-cm.R
#'
compare_models = function(models, train_data, test_data, log_dir, label){
  cat("Computing results.\n")
  # Get predictions for each model
  x.time <- system.time(results <- predict_outcomes(
    tune_list=models, train_data=train_data, test_data=test_data))
  results$times = x.time
  cat("Putting scores into bins.\n")
  buckets <- display_buckets(results$test)
  print(buckets)
  # Compare accuracy metrics
  cat("Making a confusion matrix.\n")
  cMatrix <- display_cm(results$test)
  print(cMatrix)
  # Presentation
  out = table_out(cMatrix, buckets, paste(label, "-Model Results", sep=""))
  cat(out, file=paste(log_dir,label, "-model-comparison.html", sep=""))
  return(results)
}
