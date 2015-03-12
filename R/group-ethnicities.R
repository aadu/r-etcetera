#' Group into standard ethnicities
#'
#' @param x vector of ethnicities
#' @return grouped ethnicities
#' @export
#' @import stringr
#'
group_ethnicities = function(x){
  out <- str_trim(as.character(x))
  out[grep("(?i)unknown", x)] <- "Other"
  out[grep("(?i)asian", x)] <- "Other"
  out[grep("(?i)hispanic", x)] <- "Hispanic"
  out[grep("(?i)European", x)] <- "White"
  out[grep("(?i)african", x)] <- "Black"
  out
}
