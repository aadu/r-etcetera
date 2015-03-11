#' Balanced segment read
#'
#' @param q Character vector (e.g., "Q1")
#' @param x segment data
#' @param w weights
#' @return balenced read
#' @export
#' @import data.table
#'
balance = function(q, x, w){
  x$cohort <- x[[grep('(?i)cohort', names(x), value=T)]]
  x <- x[x[[q]] != "" & x[[q]] != "Refused to Answer",]
  tab <- as.data.frame.matrix(table(x[['cohort']], x[[q]]))
  tab <- tab / apply(tab, 1, sum)
  tab$cohort <- row.names(tab); row.names(tab) <- NULL
  tab <- data.table(tab, key="cohort")
  w <- as.data.table(w)
  setkey(w, cohort)
  tab <- tab[w[,.(cohort, weight)], nomatch=0]
  w <- tab[,.(cohort, weight)]
  tab[,c('cohort', 'weight'):= NULL]
  out <- apply(w$weight * tab, 2, sum)
  out / sum(out) * 100
}
