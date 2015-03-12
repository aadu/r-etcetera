#' Use multiple cores to test for variables with near-zero-variance.
#'
#' @import parallel
#' @param x Data frame
#' @param freqCut Frequency threshold for being considered low variance.
#' @param uniqueCut Threshold for minimum number of unique values
#' @param saveMetrics Whether to return metrics or just a list of variables
#' @return Variables with zero or near-zero variance or corresponding metrics.
#' @export
#'
mcNearZeroVar <- function (x, freqCut = 95/5, uniqueCut = 10, saveMetrics = FALSE) {
  freqRatio <-  unlist(mclapply(x, function(data) {
    t <- table(data[!is.na(data)])
    if (length(t) <= 1) {
      return(0)
    }
    w <- which.max(t)
    return(max(t, na.rm = TRUE)/max(t[-w], na.rm = TRUE))
  }))
  lunique <- unlist(mclapply(x, function(data) length(unique(data[!is.na(data)]))))
  percentUnique <- 100 * lunique/unlist(mclapply(x,length))
  zeroVar <- (lunique == 1) | unlist(mclapply(x, function(data) all(is.na(data))))
  if (saveMetrics) {
    out <- data.frame(freqRatio = freqRatio,
                      percentUnique = percentUnique,
                      zeroVar = zeroVar,
                      nzv = (freqRatio > freqCut &
                             percentUnique <= uniqueCut) | zeroVar)
  }
  else {
    out <- which((freqRatio > freqCut & percentUnique <= uniqueCut) | zeroVar)
    names(out) <- NULL
  }
  out
}


#' @describeIn mcNearZeroVar
#' @export
mcImpute <- function(x){
  .impute <- function(x){
    if(all(is.na(x)))
      return(0)
    else if(!is.numeric(x))
      return(x)
    x[is.na(x)] <- median(x, na.rm=T)
    x
  }
  x[] <- mclapply(x, .impute)
  if(any(unlist(mclapply(x, function(x)(any(is.na(x)))))))
    warning("There are still some missing values.")
  x
}

