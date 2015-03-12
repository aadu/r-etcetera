#' Check segment reads data prior to sending to vendor
#'
#' @import stringr
#' @import devtools
#' @param obs_path Directory path to folder where call lists are located
#' @param targ_path Path to calc.csv file
#' @param obs_path_last Optional directory path to folder where previous weeks' call lists are located
#' @return data frame of aggregated call list
#' @include read-any.R
#' @include read-dir.R
#' @export
check_read <- function(obs_path, targ_path, obs_path_last = NULL){
  is.dir <- function(x){file.info(x)[['isdir']]}
  find_field <- function(data, regex){
    field_name <- grep(regex, names(data), value=T)[1]
    stopifnot(len(field_name))
    data[[field_name]]
  }
  .read_seg <- function(obs_path){
    if(!is.dir(obs_path)){
      obs <- read_any(obs_path)
    } else {
      files <- list.files(obs_path)
      if(length(files) > 1){
        obs_files <- read_dir(obs_path, FALSE)
        obs <- do.call(rbind, obs_files)
      } else {
        obs <- read_dir(obs_path)
      }
    }
    obs
  }
  obs <- .read_seg(obs_path)
  # Read in target info
  targ <- read.csv(as.is=T, targ_path)
  # Rename fields
  targ$cohort <- find_field(targ, '(?i)cohort')
  targ$sample <- find_field(targ, '(?i)sample')
  targ$target <- find_field(targ, '(?i)target')
  targ$weight <- find_field(targ, '(?i)target')
  targ$rr <- find_field(targ, '(?i)rr|response_rate')
  obs$cohort <- find_field(obs, '(?i)cohort')
  tab <- as.data.frame.matrix(t(t(table(obs$cohort))))
  tab$cohort <- row.names(tab)
  row.names(tab) <- NULL
  tab <- tab[ , c(2, 1)]
  names(tab)[2] <- "actual"
  wrong_cohorts <- tab$cohort[!tab$cohort %in% targ[['cohort']]]
  if(length(wrong_cohorts)){
    stop("Cohorts were observed that were not included in target file:\n",
         paste(wrong_cohorts, collapse=", "))
  }
  wrong_cohorts <- targ$cohort[!targ$cohort %in% tab[['cohort']]]
  if(length(wrong_cohorts)){
    warning("Cohorts were included in target file that were not observed file:\n",
            paste(wrong_cohorts, collapse=", "))
  }
  targ <- targ[targ$cohort %in% tab$cohort,]
  tab$goal <- targ$sample_size
  tab$diff <- tab$actual - tab$goal
  if(sum(tab$diff != 0) > 4)
    warning("Warning ", sum(tab$diff != 0),
            " cohorts differ from targets.\n")
  print(tab)
  cat("\n")
  if(!is.null(obs_path_last)){
    cat("Last week's file(s):\n")
    ilast <- .read_seg(obs_path_last)
    cat("  Size:", objectSize(ilast), "\n")
    cat("  Rows:", pN(nrow(ilast)), "\n")
  }
  cat("This week's file(s):\n")
  cat("  Size:", objectSize(obs), "\n")
  cat("  Rows:", pN(nrow(obs)), "\n\n")
  cat("Response rates:\n")
  cat("  Largest: ", paste0(round(max(targ$rr) * 100, 2), "%"), "\n")
  cat("  Smallest: ", paste0(round(min(targ$rr) * 100, 2), "%"), "\n")
  cat("  Average: ", paste0(round(mean(targ$rr) * 100, 2), "%"), "\n")
  cat("  Total: ", paste0(round(sum(targ$rr) * 100, 2), "%"), "\n\n")
  cat("Read Q complete target size: ", paste0(round(sum(targ$target))), "\n")
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
  if(length(grep("first_name", names(obs))))
    cat("first_name in file.\n")
  else
    warning("first_name not in file.\n")
  if(length(grep("last_name", names(obs))))
    cat("Voters_LastName in file.\n")
  else
    warning("Voters_LastName not in file.\n")
  cat("State:\n")
  print(table(gsub("(?i)LAL([a-z]+).*", "\\1", obs$LALVOTERID)))
  return(obs)
}

#' @describeIn check_read Object size nicely formatted
#' @export
objectSize <- function(x){ paste0(round(object.size(x)/1e6, 3), "MB")}

#' @describeIn check_read File size in "MB"
#' @export
fileSize <- function(x){
  paste0(round(file.info(x)[['size']] / 1e6, 3), "MB")
}
