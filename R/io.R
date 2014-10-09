
## http://stackoverflow.com/questions/7891073/time-out-an-r-command-via-something-like-try
try_with_time_limit <- function(expr, cpu = Inf, elapsed = Inf)
{
  y <- try({setTimeLimit(cpu, elapsed); expr}, silent = TRUE)
  if(inherits(y, "try-error")) NULL else y
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
