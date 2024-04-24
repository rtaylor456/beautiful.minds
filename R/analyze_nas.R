#' Analyze and produce information on NAs in dataset.
#'
#' This function examines the inputted dataset for NA values and outputs a table
#'     of counts and proportions of NAs for each appropriate variable in a data
#'     frame within the R console and a generated csv file, written to the
#'     current working directory.
#'
#' @param df The data frame.
#' @param output_filename The filename for the written csv file, containing the
#'     table of NA information. Default is "NA_Proportions.csv". Note: leaving
#'     this default will cause the file to overwrite when run more than once in
#'     the same directory.
#'
#' @returns A data frame with columns Column_Index, Column_Name, NA_Count,
#'     NA_Proportion. The rows consist of the columns in the inputted data frame
#'     which contain NAs. If there are no NAs in the inputted data frame, an
#'     empty data frame is returned. Additionally, a csv is written to the
#'     current working directory with the same information.
#' @export
analyze_nas <- function(df, output_filename = "NA_Proportions.csv") {
  na_info <- lapply(names(df), function(col_name) {
    column <- df[[col_name]]
    na_count <- sum(is.na(column))
    if (na_count > 0) { # Only proceed if there are NAs
      total_count <- length(column)
      na_proportion <- (na_count / total_count)
      column_index <- which(names(df) == col_name)
      # Return a list with column index, name, counts, and proportions
      return(list(Column_Index=column_index, Column_Name=col_name,
                  NA_Count=na_count, NA_Proportion=round(na_proportion, 2)))
    }
  })

  na_info <- na_info[!sapply(na_info, is.null)]

  if (length(na_info) > 0) {
    na_df <- do.call(rbind, lapply(na_info, function(x)
      data.frame(t(unlist(x)), stringsAsFactors = FALSE)))
    rownames(na_df) <- NULL
    # Sort by descending NA proportion
    na_df$NA_Proportion <- as.numeric(na_df$NA_Proportion)
    # Sort by descending NA proportion
    na_df <- na_df[order(-na_df$NA_Proportion), ]
    # Writing the data frame to a CSV file
    write.csv(na_df, file = output_filename, row.names = FALSE)
    cat("NA summary has been saved to a .csv file in working directory. \n\n")
    print(na_df)
    cat("\n")
    return(na_df)
  } else {
    cat("There are no variables with NA values.\n")
    return(data.frame()) # Return an empty data frame if there are no NAs
  }
}
