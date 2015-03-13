#' Predict outcomes
#'
#' Aggregated results from a list of tuning outputs from various models
#'
#' @param tune_list List of model training outputs e.g.,
#' \code{tune_list = list(lda = lda_tune, lr = lr_tune, bn = bn_tune)}
#' @param train_data Training data set as data.frame
#' @param test_data Test data set as data.frame.
#' @param outcome Name of the dependent variable 'outcome' by default
#' @param outcome_level level of interest in the DV
#' @return List consisting of two data.frames labeled "train_results" and
#' "test_results"
#' @export
#'
predict_outcomes = function(tune_list, train_data, test_data, outcome="Class",
                            outcome_level="yes"){
  train_results = data.frame(obs=train_data[ ,outcome])
  test_results = data.frame(obs=test_data[ ,outcome])
  cat("train_data has a class of", class(train_data), "with",
      nrow(train_data), "rows.\n")
  cat("test_data has a class of", class(test_data), "with",
      nrow(test_data), "rows.\n")
  for(i in seq_along(tune_list)){
    nm = names(tune_list)[i]
    train_results[nm] <- predict(tune_list[[i]], newdata=train_data)
    nm_prob <- paste(nm, "prob", sep="_")
    train_results[nm_prob] <- predict(tune_list[[i]], newdata=train_data,
                                      type="prob")[ ,outcome_level]
    # Sigmoid correction
    cal <- glm(relevel(train_results$obs, ref='no') ~ train_results[[nm_prob]],
               family=binomial)
    train_results[paste(nm, "prob", sep="_")] <- predict(
      cal, newdata=train_results, type="response")
    test_results[nm] <- predict(tune_list[[i]], newdata=test_data)
    test_results[nm_prob] <- predict(
      tune_list[[i]], newdata=test_data, type="prob")[ ,outcome_level]
    test_results[nm_prob] <- predict(cal, newdata=test_results, type="response")
  }
  cat("train_results has a class of", class(train_results),
      "with", nrow(train_results), "rows.\n")
  cat("test_results has a class of", class(test_results),
      "with", nrow(test_results), "rows.\n")
  return(list(train=train_results, test=test_results))
}
