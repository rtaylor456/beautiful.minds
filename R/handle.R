#' Convert date variable.
#'
#' This function converts a RSA-911 date variable written in order YYYYMMDD as
#'    one number to the appropriate date.
#'
#' @param x A date variable, written as a numeric YYYYMMDD.
#' @returns The converted date variable.
#' @export
handle_date <- function(x) {
  date <- as.Date(as.character(x), format = "%Y%m%d")
  return(date)
}

handle_year <- function(x) {
  year <- as.numeric(gsub("\\D*(\\d{4})\\D*", "\\1", x))
  return(year)
}

handle_nines <- function(data, all_demographic_cols) {
  data[, (all_demographic_cols) := lapply(.SD, function(x) {
    replace(x, x == 9, 0)
  }), .SDcols = all_demographic_cols]
  return(data)
}


handle_sex <- function(x, convert_sex = FALSE) {
  if (convert_sex) {
    sex <- ifelse(x == 2, 0, x)
  } else {
    sex <- ifelse(x == 0, 2, x)
  }

  # Replace NA values in the sex vector with 9
  sex[is.na(sex)] <- 9

  return(sex)
}
