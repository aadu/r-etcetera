#' Preprocess data for modeling
#'
#' @import parallel
#' @import caret
#' @param universe Dataframe
#' @param impute Default = TRUE
#' @param freqRatioLevel Default = 100
#' @param corCutOff Default = 0.99
#' @param dummy Default = FALSE
#' @param zscore Default = FALSE
#' @export
#' @include println.R
#' @include mc-near-zero-variance.R
#'
pre_process = function(
  universe, impute=TRUE, freqRatioLevel=100, corCutOff=.99, dummy=FALSE,
  zscore=FALSE){

  if(!is.data.frame(universe))
    stop("Universe must be a data.frame")
  universe <- as.data.frame(universe)
  cat("Beginning pre-processing with", ncol(universe), "original columns.\n")
  # Remove certain columns
  charCols <- names(universe)
  remCols <- charCols[grep("(name)|(phone)|(address)", charCols)]
  remCols <- c(remCols, charCols[grep("(birth_date)|(family_id)|(match)", charCols)])
  remCols <- c(remCols, names(universe)[grep("(_id)|(_district)", names(universe))])
  remCols <- c(remCols, "sequence", "voters_fips")
  remCols <- unique(remCols)
  cat(length(remCols), "variables removed through regex selection:",
      remCols, "\n")
  try(universe[,remCols] <- list(NULL)) # remove remCols from universe
  if(sum(sapply(universe, class) == "character") > 0) {
    charCols <- names(universe)[which(sapply(universe, class) == "character")]
  } else
    charCols <- character()
  if(length(charCols > 0)){
    for(i in charCols){
      universe[ ,i] = type.convert(universe[ ,i])
      if(is.factor(universe[ ,i])){
        universe[ ,i] <- .factorize(universe[ ,i])
        if(length(levels(universe[ ,i])) > 50){
          cat(i, "has", length(levels(universe[,i])),
              "factor levels and was removed.\n")
          remCols <- c(remCols, i)
          universe[ ,i] <- NULL
        }
        else if(length(levels(universe[ ,i])) == 1){
          universe[ ,i] <- ifelse(universe[ ,i] == levels(universe[ ,i]), 1, 0)
          cat(i, "had 1 factor level and was dummied out.\n")
        } else
          cat(i, "has", length(levels(universe[ ,i])),
              "factor levels as was left as a factor.\n")
      }
    }
  }

  # Filter out variables that have near zero variance
  cat("Attempting to remove variables that have near zero variance.\n")
  nZV <- mcNearZeroVar(universe, freqCut=100, saveMetrics=T)
  to_remove = vector()
  if(any(nZV$zeroVar))
    to_remove = c(to_remove, which(nZV$zeroVar))
  if(any(nZV$nzv))
    to_remove = c(to_remove, which(nZV$nzv))
  to_remove = unique(to_remove)
  if(length(to_remove) > 0){
    cat(ncol(universe), "initial predictors.\n")
    universe <- universe[, -to_remove]
    cat(length(which(nZV$zeroVar)),
        "predictors removed because of zero variance.\n")
    cat(length(which(nZV$freqRatio > 100)),
        "predictors removed because of freq ratio greater than 100.\n")
    cat(ncol(universe), "remaining predictors after removals.\n")
  }
  # Seperate out factors
  cat("Seperating out factors.\n")
  factors <- which(as.vector(sapply(universe, class)) == "factor")
  if(length(factors)){
    cat(length(factors), "factors identified:\n")
    for(i in seq_len(length(factors))){
      cat("    ", names(factors)[i], "\n")
    }
    universe_factors <- universe[ ,factors]
    universe <- universe[ ,-factors]
  }
  if(impute){
    cat("Replacing missing variables with median values.\n")
    library(caret)
    try(universe <- mcImpute(universe))
    cat("Universe has", ncol(universe), "columns.\n")
  }
  # Remove highly correlated variables
  cat("Attempting to remove features that are highly correlated.\n")
  try(highCor <- findCorrelation(cor(universe), cutoff = corCutOff))
  if(length(highCor) > 0){
    universe <- universe[, -highCor]
    cat("Removed", length(highCor), "features because of correlation above",
        corCutOff, "\n")
  }
  # Remove linear combinations
  cat("Attempting to remove features that are linear combinations\n")
  comboInfo <- findLinearCombos(universe)
  if(!is.null(comboInfo$remove)) {
    universe <- universe[ , -comboInfo$remove]
    cat("Removed", length(comboInfo$remove),
        "features because they were perfect linear combinations.\n")
  }
  if(exists('universe_factors'))
    universe = cbind(universe, universe_factors)
  cat("Returning data set with", ncol(universe), "total columns.\n")
  return(universe)
}


#' @describeIn pre_process
#' @description A Function to help with creating factors in R
#' @param x Data
#' @param freqCut Minimum cases per level of the factor to still include
#' @param percentCut Minimum percentage cases within a level to still include
#' @param otherFactor Relabel excluded factor levels as "other"
.factorize = function(x, freqCut=NULL, percentCut=NULL, otherFactor=TRUE) {
  # Test if more than one unique value
  if (length(unique(x)) <= 1) {
    stop("Factors require at least 2 unique values.")
  }
  # Convert whitespace strings to NA
  x = tolower(gsub("^ +$", NA, x))
  # Convert to factor
  all_lvls = names(summary(factor(x)))
  # Remove factors where n < freqCut
  if (!is.null(freqCut)) {
    lvls = all_lvls[summary(factor(x)) >= freqCut]
    lvls = lvls[grep("NA's", lvls, invert=TRUE)]
  }
  else if (!is.null(percentCut)) {
    prct = summary(factor(x))/length(x) * 100
    lvls = names(prct[prct >= percentCut])
    lvls = lvls[grep("NA's", lvls, invert=TRUE)]
  }
  if (!is.null(freqCut) | !is.null(percentCut)) {
    # Convert unused factor levels to "other"
    if (otherFactor) {
      for(lvl in all_lvls[which(!all_lvls %in% lvls)]){
        x[x==lvl] = "other"
      }
      if(!"other" %in% lvls)
        lvls = c(lvls, "other")
    }
    x = factor(x, levels = lvls)
  }
  return(factor(as.numeric(factor(x))))
}
