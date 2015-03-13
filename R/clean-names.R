#' Clean names
#'
#' Ensures that column names are valid R names
#'
#' Replaces hyphens with underscores and adds a 'v' before columns
#' that start with numbers. Removes all non-alphanumeric or underscore
#'
#' @import dplyr
#' @import stringr
#' @param string names to be cleaned
#' @return string cleaned names
#' @export
#' @examples
#' # setnames(df, names(df), clean_names(names(df)))
clean_names <- function(x){
  # First replace hyphens with underscores
  gsub("-", "_", x) %>%
    # It's also bad if there are two hyphens in a row
    gsub("__+", "_", .) %>%
    # Then add a v before all variables that begin with a number
    gsub("^(\\d)", "v\\1", .) %>%
    # Then remove anything not alphanumeric or underscore
    gsub("[^A-Za-z0-9_]", "", .)
}


#' @describeIn clean_names
#' @import dplyr
#' @import stringr
#' @export
shorten_names = function(x){
  newnames <- x %>% gsub("CommercialData", "", .) %>%
    gsub("ElectionReturns", "", .) %>%
    gsub("Interest_in", "int", .) %>%
    gsub("Turnout", "", .) %>%
    gsub("In_Household", "inhh", .) %>%
    gsub("_District", "_dst", .) %>%
    gsub("InHome", "inhh", .) %>%
    gsub("Residence_Addresses_", "reg_", .) %>%
    gsub("Mailing_Addresses_", "mail_", .) %>%
    gsub("Republicans", "gop", .) %>%
    gsub("Democrats", "dem", .) %>%
    gsub("Independent", "I", .) %>%
    gsub("RegisteredVoters", "", .) %>%
    gsub("County_", "cnty_", .) %>%
    gsub("VoterTelephones_", "", .) %>%
    gsub("Description", "desc", .) %>%
    gsub("Local_or_Municipal", "local", .)  %>%
    gsub("Redistricting", "red", .)  %>%
    gsub("Legislative", "red", .)  %>%
    gsub("FECDonors_", "red", .)  %>%
    gsub("Presidential_Primary", "pp", .)
  i = which(str_length(newnames) > 60)
  # Shorten too long names
  newnames[i] = paste0(str_sub(newnames[i], end=20L),
                       str_sub(newnames[i], start=-20L))
  clean_names(newnames)
}
