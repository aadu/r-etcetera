#' Read all (csv/tab/txt) files in a directory
#'
#' @param path Directory path
#' @param combine Boolean, whether to return one data.frame or a list of data.frames
#' @return Either a single aggregated data frame (if \code{combined = T} [default]) or a list of data frames (if \code{combined = F}).
#' @import dplyr
#' @export
read_dir = function(path, combine = T){
#   library(dplyr)
  files <- list.files(path, "(?i).*[.](csv)|(txt)|(dat)$", full.names = T)
  if(length(grep("[.]csv$", files)))
    sep = ","
  else
    sep = "\t"
  for(i in seq_along(files))
    cat("File", i, ":", basename(files[i]), "\n")
  out <- lapply(files, read.table, sep=sep, quote="\"", fill=T, comment.char="", header=T, as.is=T)
  if(combine)
    out <- rbind_all(out)
  else
    names(out) <- paste0("file", seq_along(files))
  out
}

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

#' Pretty Num with a comma as big.mark
#'
#' @param x Any numeric value
#' @return Prettified number
#' @exports
pN <- function(x){prettyNum(x, big.mark = ",")}


#' Regex to see if vector contains value
#'
#' @param x vector to be evaluated for value
#' @param regex expression
#' @return Boolean, whether or not the vector contains the value
has <- function(x, regex){
  if(length(grep(regex, x, ignore.case = TRUE)) > 0) return(TRUE) else  return(FALSE)
}

#' Read any common file depending upon file extension
#'
#' @param x Path to file
#' @param to_keep Character vector of variables to keep (default = \code{NULL})
#' @return data frame
#' @export
read_any <- function(x, to_keep = NULL){
  if(contains(x, "[.]txt$"))
    return(read.delim(file=x, header = TRUE, as.is=TRUE))
  else if(contains(x, "[.]csv$"))
    return(read.csv(file=x, header = TRUE, as.is=TRUE))
  else if(contains(x, "[.]sav$")){
    if(exists("rxImport")){
      if(!is.null(drop)){
        return(rxImport(x, varsToKeep = to_keep))
      } else {
        return(rxImport(x))
      }
    }
    else{
      library(foreign)
      return(read.spss(x, to.data.frame = TRUE, trim_values = TRUE))
    }
  } else if(contains(x, "[.]rds$"))
    return(readRDS(x))
  else
    cat("Unable to find appropriate read function.\n")
}

#' Source all files in a director
#'
#'@param path Directory path (default = \code{"R/"})
source_dir = function(path = "R/"){
  if(!file.exists(gsub("/", "", path)))
    stop("Directory does not exist.\n")
  files = list.files(path, full.names = T)
  files = files[grep("[.][rR]$", files)]
  for(f in files){
    source(f, echo = FALSE)
  }
}

#' Like cat except with a newline character at the end and default sep of ""
#'
#' @param ... Character expressions to be printed to screen
#' @return NULL
#' @export
println = function(...){
  cat(..., "\n", sep="")
}

## http://stackoverflow.com/questions/7891073/time-out-an-r-command-via-something-like-try
try_with_time_limit <- function(expr, cpu = Inf, elapsed = Inf)
{
  y <- try({setTimeLimit(cpu, elapsed); expr}, silent = TRUE)
  if(inherits(y, "try-error")) NULL else y
}

create_dir = function(mainDir, subDir=NULL){
  if(is.null(subDir))
    subDir = ""
  file = gsub("/+", "/", paste(mainDir, subDir, "/", sep = "/", collapse = "/"))
  if (!is.na(file.info(file)[1,'isdir'])) {
    println(file, " already exists.")
  } else {
    println(file, " does not exist.")
    dir.create(file.path(file))
  }
  stopifnot(file.info(file)[1,'isdir'])
}

safe_paths = function(...){
  paths = paste(..., "/", sep = '/', collapse = '/')
  gsub("/+", "/", paths)
}

fix_slashes = function(path){
  x = readLines(path)
  x = gsub("\\\\", "/", x)
  writeLines(x, path)
}

