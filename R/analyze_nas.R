#' Analyze and produce information on NAs in dataset.
#'
#' This function examines the inputted dataset for NA values and outputs a table
#'     of counts and proportions of NAs for each appropriate variable in a data
#'     frame within the R console and a generated csv file, written to the
#'     current working directory.
#'
#' @param df The data frame.
#' @param na_file TRUE or FALSE, defaults to FALSE. When set to TRUE, this will
#'     write a csv file with the table of NA counts and proportions. If set to
#'     FALSE, it will print the table but not generate a csv file.
#' @param output_filename The filename for the written csv file, containing the
#'     table of NA information. Default is "NA_Proportions.csv". Note: leaving
#'     this default will cause the file to overwrite when run more than once in
#'     the same directory.
#' @param full_table_print TRUE or FALSE, defaults to FALSE. If FALSE, the NAs
#'     table will only print the head of the resulting table. If TRUE, the NAs
#'     table will print the entire table of NAs.
#'
#' @returns A data frame with columns Column_Index, Column_Name, NA_Count,
#'     NA_Proportion. The rows consist of the columns in the inputted data frame
#'     which contain NAs. If there are no NAs in the inputted data frame, an
#'     empty data frame is returned. Additionally, a csv is written to the
#'     current working directory with the same information.
#' @export
#' @import data.table
#' @importFrom utils write.csv
#' @importFrom utils head
analyze_nas <- function(df, na_file = FALSE,
                        output_filename = "NA_Proportions.csv",
                        full_table_print = FALSE) {
  # figure out how many digits to round to, based on number of observations
  #   otherwise, it may appear that there are 100% NAs in a variable when there
  #   are actually some values, simply based on rounding
  num_digits <- nchar(nrow(df))

  na_info <- lapply(names(df), function(col_name) {
    column <- df[[col_name]]
    na_count <- sum(is.na(column))
    if (na_count > 0) { # Only proceed if there are NAs
      total_count <- length(column)
      na_proportion <- na_count / total_count
      column_index <- which(names(df) == col_name)
      return(list(Column_Index = column_index, Column_Name = col_name,
                  NA_Count = na_count, NA_Proportion = round(na_proportion,
                                                             num_digits)))
    }
  })

  na_info <- na_info[!sapply(na_info, is.null)]

  if (length(na_info) > 0) {
    na_df <- do.call(rbind, lapply(na_info, function(x) {
      data.frame(t(unlist(x)), stringsAsFactors = FALSE)
    }))
    rownames(na_df) <- NULL

    # Convert NA_Proportion to numeric and sort by descending NA proportion
    na_df$NA_Proportion <- as.numeric(na_df$NA_Proportion)
    na_df <- na_df[order(-na_df$NA_Proportion), ]

    # Format the NA_Proportion to show as a fixed decimal place
    na_df$NA_Proportion <- sprintf(paste("%.", num_digits, "f", sep = ""),
                                   na_df$NA_Proportion)

    # Writing the data frame to a CSV file if na_file is TRUE
    if (na_file) {
      write.csv(na_df, file = output_filename, row.names = FALSE)
      cat("NA summary has been saved to a .csv file in the working directory.
          \n\n")
    }

    # Conditional printing based on full_table_print
    if (full_table_print == TRUE) {
      print(na_df)  # Print the full NA table
    } else {
      print(head(na_df))  # Print only the head of the NA table
    }
    cat("\n")
    return(invisible())
  } else {
    cat("There are no variables with NA values.\n")
    return(data.frame()) # Return an empty data frame if there are no NAs
  }
}
