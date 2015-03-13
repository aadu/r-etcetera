#' Train a universe using a variety of models
#'
#' This function is where the actual modeling takes place.
#' Models are specified in a \code{config} file that acts as
#' a sort of controller for the modeling process.
#'
#' @import doMC
#' @import parallel
#' @import caret
#' @import sjPlot
#' @param model character name of model previously
#'     prepped with \code{prepare_universe()}
#' @param config list containing configuration instructs
#'     (e.g., which models to run, where to save models, etc.).
#' @keywords model modeling universe
#' @export
#' @seealso \link{prepare_universe}
#' @examples
#' #train_universe("unsure", config)
#' #train_universe("turnout", config)
train_universe = function(m, config){
  registerDoMC(cores = detectCores())
  if(!is.null(config$test_metric)){
    test_metric = config$test_metric
  } else {
    test_metric = "ROC"
  }
  # time-out-parameter
  if(!is.null(config$timeout)){
    timeout = config$timeout * 60
  } else {
    # Default of 60 minute timeout per model
    timeout = 60 * 60
  }
  # which methods to run
  run = config$methods
  # load up the training and test universes for the model
  train_data = readRDS(paste0(config$testsets_dir,
                              m, "-train.rds"))
  test_data = readRDS(paste0(config$testsets_dir,
                             m, "-test.rds"))
  # Define the test statistics
  fiveStats = function(...) c(twoClassSummary(...),
                              defaultSummary(...))
  # Examine all models with 10-fold cross-validation
  ctrl = trainControl(method="cv",
                      number=10,
                      summaryFunction=fiveStats,
                      classProbs=TRUE,
                      savePredictions=TRUE)
  # train models if specified in config
  train_model(run$rpart, "rpart", "rpart",
              data.frame(.cp=seq(0, .1, .02)))
  train_model(run$bayesglm, "bayesglm")
  glmnGrid = expand.grid(.alpha=c(0,.1,.2,.4,.6,.8,1),
                         .lambda=seq(.01, .2, length=40))
  train_model(run$glmnet, "glmnet", "glmnet", glmnGrid)
  train_model(run$lda, "lda", "MASS")
  train_model(run$treebag, "treebag", c("ipred", "plyr"))
  train_model(run$pam, "pam", c("pamr", "rda"),
              data.frame(.threshold=0:25))
  train_model(run$kernelpls, "kernelpls", "pls",
              data.frame(.ncomp=c(3,4,5,6,7,9)))
  train_model(run$ctree2, "ctree2", "party",
              expand.grid(.maxdepth=2:10))
  train_model(run$sparseLDA, "sparseLDA", "sparseLDA")
  train_model(run$Boruta, "Boruta", c("randomForest",
                                      "Boruta", "rFerns"), expand.grid(
                                        .mtry=sqrt(ncol(train_data))))
  train_model(run$rf, "rf", "randomForest",
              expand.grid(.mtry=sqrt(ncol(train_data))))
  train_model(run$mda, "mda", "mda",
              tuneGrid=expand.grid(.subclasses=1:8))
  gbm_grid = expand.grid(.interaction.depth=seq(1, 7, by=3),
                         .n.trees=seq(100, 1000, by=300),
                         .shrinkage=c(0.01, 0.1))
  train_model(run$gbm, "gbm", c("gbm", "klaR"),
              tuneGrid=gbm_grid)
}


#' @describeIn train_universe
#' @export
train_universes = function(config){
  cat("Preparing to train univereses.\n")
  for(m in names(config$models)){
    cat("\n-------------------------\n",
        "Training universe for", m,
        "\n-----------------------\n\n")
    if(length(grep("[.]del", m)) > 0)
      cat("skip", m)
    else
      try(train_universe(m, config))
  }
}


#' @describeIn train_universe
#' @export
train_model = function(condition, method, libs=NULL, tuneGrid=NULL){
  if(!condition)
    return()
  if(!is.null(libs)){
    try(do.call(library, as.list(libs)))
  }
  println("Preparing to run ", method)
  try(x <- train(Class ~ . ,
                 data=train_data,
                 trControl=ctrl,
                 metric=test_metric,
                 method=method,
                 tuneGrid=tuneGrid)
  )
  try(println("Saving ", m, " at ",
              paste0(config$models_dir, m, "-",
                     method, ".rds")))
  if(exists('x')){
    try(saveRDS(x, paste0(config$models_dir,
                          m, "-", method, ".rds")))
  }
}
