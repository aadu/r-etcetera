#' Top line with leaners
#'
#' @param x segment data
#' @param w weights
#' @param q1 character vector of length 1 (e.g., q1 = "Q2")
#' @param q2 character vector of length 1 (e.g., q2 = "Q2")
#' @return overall read with leaners
#' @export
#' @include balance.R
#'
with_leaners = function(x, w, q1="Q2", q2="Q3"){
  x1 <- balance(q1, x, w)
  x2 <- balance(q2, x, w)
  u <- x1[grep("(?i)unsure|undecided", names(x1))]
  u2 <- x2[grep("(?i)unsure|undecided", names(x2))]
  out <- x1 + (x2*u/100)
  out[[grep("(?i)unsure|undecided", names(out))]] <- u2 * (u/100)
  out
}
