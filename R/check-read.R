#' Check segment reads prior to sending them
#'
#' @import stringr
#' @import devtools
#' @param obs_path Directory path to folder where call lists are located
#' @param targ_path Path to calc.csv file
#' @param obs_path_last Optional directory path to folder where previous weeks' call lists are located
#' @return data frame of aggregated call list
#' @export
check_read = function(obs_path, targ_path, obs_path_last = NULL){
#   library(stringr)
#   library(devtools)
#   source_url('https://gist.githubusercontent.com/aadu/fd0c34eb9ac473259dbf/raw/a75c63dfc724d83fbb5b5e15a3b3484bf938a372/io-utils.r')
  files <- list.files(obs_path)
  if(files > 1){
    obs_files <- read_dir(obs_path, F)
    obs <- do.call(rbind, obs_files)
  } else {
    obs <- read_dir(obs_path)
  }
  targ <- read.csv(as.is=T, targ_path)
  obs$cohort <- obs[[grep("(?i)cohort", names(obs), value=T)[1]]] # Save cohort as cohort
  tab <- as.data.frame.matrix(t(t(table(obs$cohort))))
  tab$cohort <- row.names(tab)
  row.names(tab) <- NULL
  tab <- tab[,c(2,1)]
  names(tab)[2] <- "actual"
  tab$goal <- targ$sample_size
  tab$diff <- tab$actual - tab$goal
  if(sum(tab$diff != 0) > 4)
    warning("Warning ", sum(tab$diff != 0), " cohorts differ from targets.\n")
  print(tab)
  i <- object.size(obs)
  cat("Size of this week's file:", prettyNum(i, big.mark=","), "\n")
  if(!is.null(obs_path_last)){
    ilast <- read_dir(obs_path_last)
    isize <- object.size(ilast)
    cat("Size of last week's file:", prettyNum(isize, big.mark=","), "\n")
    cat("Rows in last week's file:", prettyNum(nrow(ilast), big.mark=","), "\n")
  }
  cat("Total number of rows:", prettyNum(nrow(obs), big.mark=","), "\n")
  cat("Largest response rate: ", paste0(round(max(targ$rr) * 100, 2), "%"), "\n")
  cat("Smallest response rate: ", paste0(round(min(targ$rr) * 100, 2), "%"), "\n")
  cat("Average response rate: ", paste0(round(mean(targ$rr) * 100, 2), "%"), "\n")
  cat("Total response rate: ", paste0(round(sum(targ$rr) * 100, 2), "%"), "\n")
  cat("Read target size: ", paste0(round(sum(targ$target))), "\n")
  if(!anyDuplicated(obs$LALVOTERID))
    cat("No duplicated LALVOTERID\n")
  else
    stop("Duplicated LALVOTERID\n")
  if(!anyNA(obs$VoterTelephones_Phone10))
    cat("No missing phone numbers.\n")
  else
    warning("Missing phone numbers!\n")
  if(all(str_length(obs$VoterTelephones_Phone10) == 10))
    cat("All phone numbers are 10 digits.\n")
  else
    warning("Not all phone numbers are 10 digits long.\n")
  cat("First 20 cohorts\n")
  print(t(t(table(head(obs$cohort, 20)))))
  if(length(grep("Voters_FirstName", names(obs))))
    cat("Voters_FirstName in file.\n")
  else
    warning("Voters_FirstName not in file.\n")
  if(length(grep("Voters_LastName", names(obs))))
    cat("Voters_LastName in file.\n")
  else
    warning("Voters_LastName not in file.\n")
  cat("State:\n")
  print(table(gsub("(?i)LAL([a-z]+).*", "\\1", obs$LALVOTERID)))
  return(obs)
}

# obs_path = 'E:/Dropbox (Optimus)/0ptimus - Ricketts General/Analysis - Segment Monitoring/Segment Reads/10-09 Read/Sent to Vendor'
# targ_path <- 'E:/Dropbox (Optimus)/0ptimus - Ricketts General/Analysis - Segment Monitoring/Segment Reads/10-09 Read/Checks/NE-calc.csv'
# obs_path_last <- "E:/Dropbox (Optimus)/0ptimus - Ricketts General/Analysis - Segment Monitoring/Segment Reads/10-02 Read/Sent to vendor"
# obs <- check_read(obs_path, targ_path, obs_path_last)
# table(str_length(obs$VoterTelephones_Phone10))
