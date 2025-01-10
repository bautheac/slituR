# parse_table_caption_for_latex_output #########################################
## main ########################################################################
#' @export
invert_columns_on_condition <- function(df, col1, col2, condition = NULL){

  # If no condition is provided, default to col2 < col1
  # (i.e., swap wherever col2 is less than col1)
  if (is.null(condition)) {
    condition <- df[[col2]] < df[[col1]]
  }

  # Identify rows where the two columns are inverted (based on condition)
  swap_rows <- condition

  # Temporarily store col1 values
  temp <- df[[col1]][swap_rows]

  # Assign col2's values into col1 for those rows
  df[[col1]][swap_rows] <- df[[col2]][swap_rows]

  # Assign the old col1 values into col2
  df[[col2]][swap_rows] <- temp

  return(df)
}
