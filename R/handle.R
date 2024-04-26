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
