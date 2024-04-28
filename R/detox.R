#' Detox the data! Clean and reorganize an RSA-911 dataset.
#'
#' This function is a one-stop for cleaning and restructuring an RSA-911 dataset
#'   in order to prepare it for analysis and visualization. Processed include
#'   tailored NA imputation, variable type conversion, and reorganization of
#'   variables.
#'
#' @param data An RSA-911 dataset.
#' @param na_check TRUE or FALSE, defaults to TRUE. If TRUE, runs analyze_nas
#'    function on dataset, after cleaning, to output information on remaining NA
#'    values.
#' @param na_file TRUE or FALSE, defaults to FALSE. If TRUE, this will write a
#'     csv file with the table of NA counts and proportions. If set to FALSE, it
#'     will print the table but not generate a csv file.
#' @param full_table_print TRUE or FALSE, defaults to FALSE. If FALSE, the NAs
#'     table will only print the head of the resulting table. If TRUE, the NAs
#'     table will print the entire table of NAs.
#' @param unidentified_to_0 TRUE or FALSE, defaults to FALSE. If TRUE, converts
#'     values of 9 to 0 within variables with possible values of 0, 1, 9 (where 9
#'     represents "did not identify").
#' @param convert_sex TRUE or FALSE, defaults to FALSE. If TRUE, converts variable
#'     'Sex' values of 2 to values of 0, providing a more traditional binary
#'     variable format.
#' @param remove_strictly_na TRUE or FALSE, defaults to FALSE. If TRUE,
#'     identifies and removes variables with exclusively NA values after the data
#'     have been cleaned.
#'
#' @returns The cleaned dataset (additionally, a table of NA information if
#'     na_check param is set to TRUE. See analyze_nas documentation for info.)
#' @export
#' @import data.table
#' @import tidyverse
detox <- function(data, na_check = TRUE, na_file = FALSE,
                  full_table_print = FALSE, unidentified_to_0 = FALSE,
                  convert_sex = FALSE,
                  remove_strictly_na = FALSE) {
  # Ensure data is a data.table
  setDT(data)

  # Lowercase column names
  setnames(data, tolower(names(data)))

  # Helper function to replace NA with specified value using data.table syntax
  replace_na <- function(cols, value) {
    for (col in cols) {
      column_type <- mode(data[[col]])
      if (column_type == "numeric" || column_type == "integer") {
        typed_value <- as.numeric(value)
      } else if (column_type == "logical") {
        typed_value <- as.logical(value)
      } else {
        typed_value <- as.character(value)
      }
      data[, (col) := fifelse(is.na(get(col)), typed_value, get(col))]
    }
  }



  # Replace NAs in specified columns
  agencystaff_cols <- grep("agencystaff", names(data), value = TRUE)
  agencypurchase_cols <- grep("agencypurchase", names(data), value = TRUE)
  replace_na(c(agencystaff_cols, agencypurchase_cols), 0)

  compserviceprovider_cols <- grep("compserviceprovider(?!type)", names(data),
                                   value = TRUE, perl = TRUE)
  purchaseprovider_cols <- grep("purchaseprovider", names(data), value = TRUE)
  replace_na(c(compserviceprovider_cols, purchaseprovider_cols), 0)

  numeric_cols <- grep("expend|wage|hoursworked|age_", names(data),
                       value = TRUE)
  data[, (numeric_cols) := lapply(.SD, as.numeric), .SDcols = numeric_cols]

  date_cols <- grep("date|eligibilityext|compdisenrollmsg", names(data),
                    value = TRUE)
  data[, (date_cols) := lapply(.SD, handle_date), .SDcols = date_cols]

  programyear_cols <- grep("programyear", names(data), value = TRUE)
  data[, (programyear_cols) := lapply(.SD, handle_year),
       .SDcols = programyear_cols]

  # if (convert_sex) {
  #   data[, "sex" := fifelse(sex == 2, 0, sex)]
  # } else {
  #   data[, "sex" := fifelse(sex == 0, 2, sex)]
  # }
  #
  # sex_col <- "sex"
  # replace_na(sex_col, 9)

  sex_cols <- grep("sex", names(data), value = TRUE)
  data[, (sex_cols) := lapply(.SD, function(x) handle_sex(x, convert_sex)),
       .SDcols = sex_cols]

  demographic_cols <- c("amerindian", "asian", "black", "hawaiian",
                        "white", "hispanic", "veteran", "disability", "adult",
                        "adulted", "dislocatedworker", "jobcorps",
                        "wpempservice", "youth", "longtermunemp", "exhausttanf",
                        "fostercareyouth", "homelessorrunaway",
                        "exoffenderstatus", "lowincomestatus", "englishlearner",
                        "basicskillsdeficient",
                        "culturalbarriers", "singleparent", "dishomemaker")
  add_demographic_cols <- c("insecondaryed", "completedsomepostseced",
                            "enrolledinsecequiv")
  all_demographic_cols <- c(demographic_cols, add_demographic_cols)


  # replace_value <- ifelse(unidentified_to_0, 0, 9)
  # replace_na(all_demographic_cols, replace_value)

  # if (unidentified_to_0 && any(get(col) == 9)) {
  #   data[get(col) == 9, (col) := 0]
  # }

  # # Check if unidentified_to_0 is TRUE and any values in demographic columns are 9
  # if (unidentified_to_0 && any(colSums(data[,
  #                                           ..all_demographic_cols] == 9) > 0)) {
  #   # Replace values of 9 with 0 in the demographic columns
  #   data[, (all_demographic_cols) := lapply(.SD, function(x) {
  #     replace(x, x == 9, 0)}), .SDcols = all_demographic_cols]
  # }

  # Check if unidentified_to_0 is TRUE and any values in demographic columns are 9
  if (unidentified_to_0) {
    data <- handle_nines(data, all_demographic_cols)
  }


  # # NA Check and analysis
  # if (na_check) {
  #   analyze_nas(data, na_file = na_file, full_table_print = full_table_print)
  # }

  # Remove strictly NA columns if specified
  if (remove_strictly_na) {
    na_summary <- sapply(data, function(x) sum(is.na(x)))
    cols_to_remove <- names(na_summary)[na_summary == nrow(data)]

    # Debugging statements
    print(na_summary)
    print(cols_to_remove)

    if (length(cols_to_remove) > 0) {
      data[, (cols_to_remove) := NULL]  # Correct use of `:=` to remove columns
      cat("Columns where all values are NAs have been removed from the data.\n")
    }
  }

  # NA Check and analysis
  if (na_check) {
    analyze_nas(data, na_file = na_file, full_table_print = full_table_print)
  }

  cat("Data has been detoxed\n")
  # return(invisible())
  return(data)
}
