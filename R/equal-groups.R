#' Create three equal, randomized groups
#'
#' @param x Data frame from which to pull the groups
#' @param k Number of groups
#' @param n Optional, number of individual in each group, if left blank, the group will be split evenly into \code{k} groups
#' @param compare Cohort label upon which to check if groups have been split in equivalent portions (default = 'cohortcurrent')
#' @param path Optional directory path for which to store the group files
#' @return list of \code{k} data frames for each group
#' @export
equal_groups = function(x, k, n = NULL, compare = 'cohortcurrent', path = NULL){
  if(is.null(n))
    n = floor(nrow(x) / k)
  x[[compare]] <- factor(x[[compare]])
  k = seq_len(k)
  pass = FALSE
  counter = 1
  i = list()
  res = character()
  while(!pass){
    targets = seq_len(nrow(x))
    for(j in k){
      i[[j]] <- sample(targets, n)
      targets <- targets[(!targets %in% i[[j]])]
    }
    # make a group x var matrix
    groups <- lapply(k, function(j)(x[[compare]][i[[j]]]))
    groups <- Map(cbind, groups, sapply(k, function(x)(paste0("group", x))))
    groups <- as.data.frame(do.call(rbind, groups))
    groups[] <- lapply(groups, factor)
    res <- chisq.test(table(groups))
    print(res)
    if(res$p.value > .5)
      pass = TRUE
    cat("try", counter, "p = ", res$p.value,  "\n")
    counter = counter + 1
  }
  data = lapply(k, function(j)(x[i[[j]],]))
  # Re-randomize the order of each group
  data = lapply(data, function(x)(x[sample(1:nrow(x)),]))
  if(!is.null(path))
    lapply(k, function(x)(write.csv(data[[x]], file=paste0(path, "version-", LETTERS[x], ".csv"), row.names=FALSE, na="")))
  list(res = res, data = data)
}
