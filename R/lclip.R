#' Export data frame to linux clipboard
#'
#' @param x data frame to export
#' @param sep Seperator to use (default = \code{"\t"})
#' @param row.names Boolean -- whether to save row names (default = \code{FALSE})
#' @param col.names Boolean -- whether to save column names (default = \code{TRUE})
#' @return NULL
#' @exports
lclip <- function(x, sep="\t", row.names=FALSE, col.names=TRUE){
  con <- pipe("xclip -selection clipboard -i", open="w")
  write.table(x, con, sep=sep, row.names=row.names, col.names=col.names)
  close(con)
}
