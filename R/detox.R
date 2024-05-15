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
#'     values of 9 to 0 within variables with possible values of 0, 1, 9 (where
#'     9 represents "did not identify").
#' @param convert_sex TRUE or FALSE, defaults to FALSE. If TRUE, converts
#'     variable Sex' values of 2 to values of 0, providing a more traditional
#'     binary variable format.
#' @param remove_strictly_na TRUE or FALSE, defaults to FALSE. If TRUE,
#'     identifies and removes variables with exclusively NA values after the
#'     data have been cleaned.
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

  if (!na_check && (na_file || full_table_print)) {
    stop(paste("na_check must be set equal to TRUE in order to use",
               "na_file and full_table_print"))
  }

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


  # check this push
  # Replace NAs in specified columns
  # AGENCY columns
  agencystaff_cols <- grep("agencystaff", names(data), value = TRUE)
  agencypurchase_cols <- grep("agencypurchase", names(data), value = TRUE)
  replace_na(c(agencystaff_cols, agencypurchase_cols), 0)

  # PROVIDER (without "type") columns
  compserviceprovider_cols <- grep("compserviceprovider(?!type)", names(data),
                                   value = TRUE, perl = TRUE)
  purchaseprovider_cols <- grep("purchaseprovider", names(data),
                                value = TRUE)
  replace_na(c(compserviceprovider_cols, purchaseprovider_cols), 0)

  # PROGRAMYEAR columns
  programyear_cols <- grep("programyear", names(data), value = TRUE)
  data[, (programyear_cols) := lapply(.SD, handle_year),
       .SDcols = programyear_cols]

  # DATE columns
  date_cols <- grep("date|eligibilityext|compdisenrollmsg", names(data),
                    value = TRUE)

  # SEX columns
  sex_cols <- grep("sex", names(data), value = TRUE)
  # oosexitdate will be mishandled without the following line of code:
  sex_cols <- setdiff(sex_cols, date_cols)
  data[, (sex_cols) := lapply(.SD, function(x) handle_sex(x, convert_sex)),
       .SDcols = sex_cols]

  # DEMOGRAPHIC columns
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

  data[, (all_demographic_cols) := lapply(.SD, function(x) {
    handle_nines(x, unidentified_to_0)
  }),
  .SDcols = all_demographic_cols]


  # PURCHASE TYPE columns -- already handled in purchase provider columns code
  # purchase_type_cols <- grep("purchase.*type", names(data),
  #                                  value = TRUE, perl = TRUE)
  # replace_na(purchase_type_cols, 0)

  # COMPSERVICE TYPE columns and
  # comp_type_cols <- grep("comp.*type", names(data), value = TRUE, perl = TRUE)
  # # replace blanks " " and "NULL" values to "0"s --leave as characters because
  # #   we have values with numbers separated by ";"
  # data[, (comp_type_cols) := lapply(.SD, handle_blanks),
  #      .SDcols = comp_type_cols]
  # # if we don't care about the specific values of providers, we will just
  # #   convert the variable values to 1 if there is a value other than "0"
  # data[, (comp_type_cols) := lapply(.SD, function(x) ifelse(x != "0", "1", x)),
  #      .SDcols = comp_type_cols]

  # remaining TYPE columns
  # Extract column names containing "type" but not "purchase" or "comp"
  other_type_cols <- grep("type", names(data), value = TRUE, perl = TRUE)
  other_type_cols <- other_type_cols[!grepl("purchase|comp", other_type_cols)]
  replace_na(other_type_cols, 0)


  # SPECIAL CHARACTER columns
  comp_type_cols <- grep("comp.*type", names(data), value = TRUE, perl = TRUE)
  special_cols <- c(comp_type_cols, "appmonthlypubsup", "exitmonthlypubsup",
                    "appmedinscov", "exitmedinscov", "youthbuild")
  # replace blanks " " and "NULL" values to "0"s --leave as characters because
  #   we have values with numbers separated by ";"
  data[, (special_cols) := lapply(.SD, handle_blanks),
       .SDcols = special_cols]
  # if we don't care about the specific values, we will just
  #   convert the variable values to 1 if there is a value other than "0"
  data[, (special_cols) := lapply(.SD, function(x) ifelse(x != "0", "1", x)),
       .SDcols = special_cols]

  # CONVERT TO CORRECT VARIABLE TYPES:
  # convert everything to factors, except the numeric and date variables
  # variables with "date" -- already extracted: date_cols
  # variables with "expend"
  # variables with "age_"
  # variables with "wage"
  # variables with "hours"
  # specific variables:
  #  ProgramYear

  # Numeric columns
  numeric_cols <- grep("expend|wage|hours|age_|year", names(data),
                       value = TRUE)
  data[, (numeric_cols) := lapply(.SD, as.numeric), .SDcols = numeric_cols]

  # DATE columns conversion
  # date_cols <- grep("date|eligibilityext|compdisenrollmsg", names(data),
  #                   value = TRUE)
  data[, (date_cols) := lapply(.SD, handle_date), .SDcols = date_cols]

  # Convert all other columns to factors
  factor_cols <- setdiff(names(data), c(numeric_cols, date_cols))
  data[, (factor_cols) := lapply(.SD, as.factor), .SDcols = factor_cols]


  # Remove strictly NA (or strictly 0) columns if specified
  if (remove_strictly_na) {
    # na_summary <- sapply(data, function(x) sum(is.na(x)))
    # cols_to_remove <- names(na_summary)[na_summary == nrow(data)]

    # na_variables <- names(data)[sapply(data, function(x) all(is.na(x)))]
    # zero_variables <- names(data)[sapply(data, function(x) all(x == 0))]

    cols_to_remove <- names(data)[sapply(data, function(x) {
      all(is.numeric(x) & x == 0) || all(is.na(x))
      })]

    if (length(cols_to_remove) > 0) {
      print(cols_to_remove)
      data[, (cols_to_remove) := NULL]  # Correct use of `:=` to remove columns
      cat("Columns where all values are NAs have been removed from the data.\n")
    }
  }

  # NA Check and analysis
  if (na_check) {
    analyze_nas(data, na_file = na_file, full_table_print = full_table_print)
  }

  cat("Data has been detoxed\n")
  return(data)
}
