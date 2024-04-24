#' Detox the data! Clean and reorganize an RSA-911 dataset.
#'
#' This function is a one-stop for cleaning and restructuring an RSA-911 dataset
#'.   in order to prepare it for analysis and visualization. Processed include
#'.   tailored NA imputation, variable type conversion, and reorganization of
#'.   variables.
#'
#' @param data An RSA-911 dataset.
#' @param NA_check TRUE or FALSE, defaults to TRUE. If TRUE, Runs analyze_nas
#'    function on dataset after cleaning to output information on remaining NA
#'    values.
#' @param unidentified_to_O TRUE or FALSE, defaults to FALSE. If TRUE, converts
#'.   values of 9 to 0 within variables with possible values of 0, 1, 9 (where 9
#'.   represents "did not identify").
#' @param remove_strictly_NA TRUE or FALSE, defaults to FALSE. If TRUE,
#'.   identifies and removes variables with exclusively NA values after the data
#'.   have been cleaned.
#'
#' @returns The cleaned dataset (additionally, a table of NA information if
#'     NA_check param is set to TRUE. See analyze_nas documentation for info.)
#' @export
detox <- function(data, NA_check = TRUE, unidentified_to_0 = FALSE,
                      remove_strictly_NA = FALSE) {
  # Ensure data is a data.table
  setDT(data)

  # Lowercase column names
  setnames(data, tolower(names(data)))

  # Helper to replace NA with specified value using data.table syntax
  replace_na <- function(cols, value) {
    for (col in cols) {
      # Detect the mode of the column and adjust the value accordingly
      column_type <- mode(data[[col]])
      if (column_type == "numeric" || column_type == "integer") {
        typed_value <- as.numeric(value)
      } else if (column_type == "logical") {
        typed_value <- as.logical(value)
      } else {
        # For characters and factors, typically handling NAs would not involve
        #    numerical replacements
        typed_value <- as.character(value)
      }

      # Apply fifelse with corrected types
      data[ , (col) := fifelse(is.na(get(col)), typed_value, get(col))]
    }
  }

  ## AGENCY variables
  agencystaff_cols <- grep("agencystaff", names(data), value = TRUE)
  agencypurchase_cols <- grep("agencypurchase", names(data), value = TRUE)
  replace_na(c(agencystaff_cols, agencypurchase_cols), 0)

  ## PURCHASE and PROVIDER variables
  compserviceprovider_cols <- grep("compserviceprovider(?!type)", names(data),
                                   value = TRUE, perl = TRUE)
  purchaseprovider_cols <- grep("purchaseprovider", names(data),
                                value = TRUE)
  replace_na(c(compserviceprovider_cols, purchaseprovider_cols), 0)

  ## STRICTLY NUMERIC variables:
  #   EXPEND, WAGE, HOURSWORKED, AGE_
  numeric_cols <- grep("expend|wage|hoursworked|age_", names(data),
                       value = TRUE)
  data[ , (numeric_cols) := lapply(.SD, as.numeric), .SDcols = numeric_cols]

  ## DATE variables
  # dates entered in format YYYYMMDD, should be blank if not relevant
  date_cols <- grep("date|eligibilityext|compdisenrollmsg", names(data),
                    value = TRUE)
  data[, (date_cols) := lapply(.SD, handle_date), .SDcols = date_cols]

  ## PROGRAMYEAR variable
  programyear_cols <- grep("programyear", names(data), value = TRUE)
  data[, (programyear_cols) := lapply(.SD, handle_year),
       .SDcols = programyear_cols]

  ## DEMOGRAPHIC variables
  # Handle demographics using more complex conditions if needed
  demographic_cols <- c("sex", "amerindian", "asian", "black", "hawaiian",
                        "white", "hispanic", "veteran", "disability", "adult",
                        "adulted", "dislocatedworker", "jobcorps",
                        "wpempservice", "youth", "longtermunemp", "exhausttanf",
                        "fostercareyouth", "homelessorrunaway",
                        "exoffenderstatus", "lowincomestatus",
                        "englishlearner", "basicskillsdeficient",
                        "culturalbarriers", "singleparent", "dishomemaker")

  add_demographic_cols <- c("insecondaryed", "completedsomepostseced",
                            "enrolledinsecequiv")
  all_demographic_cols <- c(demographic_cols, add_demographic_cols)

  replace_value <- ifelse(unidentified_to_0, 0, 9)
  replace_na(all_demographic_cols, replace_value)

  if (NA_check == TRUE){
    analyze_nas(data)
  }

  # Check and return NA stats if desired
  if (remove_strictly_NA == TRUE) {
    na_summary <- sapply(data, function(x) sum(is.na(x)))
    # Get column names where NA proportion = 1
    cols_to_remove <- names(na_summary)[na_summary == nrow(data)]

    data <- data[, !(names(data) %in% cols_to_remove), with = FALSE]
    cat("Columns where all values are NAs:\n")
    print(cols_to_remove)
    cat("These columns been removed from the data.\n")
  }

  cat("\nData has been detoxed\n")
  return(data)
}
