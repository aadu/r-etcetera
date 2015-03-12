#' Scrub income
#'
#' Cleans up CommercialData_EstimatedIncome variable
#'
#' @import stringr
#' @param string of original variable
#' @return double scaled and centered (internally)
#' @export
#'
scrub_income = function(x) {
 # CommercialData_EstimatedIncome
  x = str_trim(x)
  income = rep(NA_real_, length(x))
  income[x == ""]               = 0
  income[x == "$1000-14999"]    = 1
  income[x == "$15000-24999"]   = 2
  income[x == "$25000-34999"]   = 3
  income[x == "$35000-49999"]   = 4
  income[x == "$50000-74999"]   = 5
  income[x == "$75000-99999"]   = 6
  income[x == "$100000-124999"] = 7
  income[x == "$125000-149999"] = 8
  income[x == "$150000-174999"] = 9
  income[x == "$175000-199999"] = 10
  income[x == "$200000-249999"] = 11
  income[x == "$250000+"]       = 12
  income = income - 6  # Center
  income = income / 6  # Scale
  income[is.na(income)] = median(income, na.rm=T)
  return(income)
}
