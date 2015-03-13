#' Installs a list of packages
#'
#' @param packages A list of packages, or by default my list.
#' @return \code{NULL}
#' @export
#'
install_packages = function(packages=NULL){
  if(is.null(packages)){
    packages = c("sparseLDA", "pamr", "plyr", "C50",
                 "bst", "brnn", "randomForest", "party",
                 "arm", "Cubist", "RWeka", "nnet", "penalized",
                 "mda", "proxy", "rFerns", "elasticnet", "rrcov",
                 "plsRglm", "pls", "stepPlr", "penalizedLDA",
                 "MASS", "kernlab", "ada", "frbs", "caTools")
  }
  for(each in packages){
    if(!require(each))
      install.packages(each)
  }
  NULL
}
