#' Read google spreadsheet
#'
#' @param url of exported spreadsheet
#' @param sheet number
#' @return google spreadsheet in the form of a data frame
#' @import XML
#' @import httr
#' @exports
#'
readGoogle <- function(url, sheet = 1){
  #   library(XML)
  #   library(httr)
  r <- GET(url)
  html <- content(r)
  sheets <- readHTMLTable(html, header=FALSE, stringsAsFactors=FALSE)
  df <- sheets[[sheet]]
  dfClean <- function(df){
    nms <- t(df[1,])
    names(df) <- nms
    df <- df[-1,-1]
    row.names(df) <- seq(1,nrow(df))
    df
  }
  dfClean(df)
}
