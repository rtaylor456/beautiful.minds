handle_date <- function(x){
  date <- as.Date(as.character(x), format = "%Y%m%d")
  return(date)
}

handle_year <- function(x){
  year <- as.numeric(gsub("\\D*(\\d{4})\\D*", "\\1", x))
  return(year)
}
