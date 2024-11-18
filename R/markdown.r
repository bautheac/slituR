# parse_table_caption_for_latex_output #########################################
## helpers #####################################################################
escape_percentage_sign_before_numbers_in_latex <- function(x) {
  gsub("(\\d)\\%", "\\1\\\\%", x, ignore.case = TRUE, perl = TRUE)
}
escape_ampersand_sign_in_latex <- function(x) {
  gsub("&", "\\\\&", x, ignore.case = TRUE, perl = TRUE)
}
unescape_star_sign_in_latex <- function(x) {
  gsub("\\\\\\*", "*", x, ignore.case = TRUE, perl = TRUE)
}
parse_references_in_latex <- function(x) {
  gsub("\\\\@ref\\((.+?)\\)", "\\\\ref{\\1}", x, ignore.case = TRUE, perl = TRUE)
}
## main ########################################################################
#' @export
parse_table_caption_for_latex_output <- function(x){
  x <- escape_percentage_sign_before_numbers_in_latex(x)
  x <- escape_ampersand_sign_in_latex(x)
  x <- unescape_star_sign_in_latex(x)
  x <- parse_references_in_latex(x)

  return(x)
}
# main #########################################################################
#' @export
caption_latex <- function(x){ return(parse_table_caption_for_latex_output(x)) }


# parse_table_caption_for_html_output ##########################################
## helpers #####################################################################
escape_percentage_sign_before_numbers_in_html <- function(x) {
  gsub("(\\d)\\\\%", "\\1%", x, ignore.case = TRUE, perl = TRUE)
}
escape_ampersand_sign_in_html <- function(x) {
  gsub("\\\\&", "&", x, ignore.case = TRUE, perl = TRUE)
}
unescape_star_sign_in_html <- function(x) {
  gsub("\\*", "\\\\\\*", x, ignore.case = TRUE, perl = TRUE)
}
parse_references_in_html <- function(x) {
  gsub("\\\\ref\\{(.+?)\\}", "\\\\@ref(\\1)", x, ignore.case = TRUE, perl = TRUE)
}
## main ########################################################################
#' @export
parse_table_caption_for_html_output <- function(x){
  x <- escape_percentage_sign_before_numbers_in_html(x)
  x <- escape_ampersand_sign_in_html(x)
  x <- unescape_star_sign_in_html(x)
  x <- parse_references_in_html(x)

  return(x)
}
## main ########################################################################
#' @export
caption_html <- function(x){ parse_table_caption_for_html_output(x) }


# get_results_from_results_table_by_analysis ###################################
## helpers #####################################################################
check_if_no_table_found <- function(results, analysis){
  if (!length(results)) {
    message <- paste0("No table found for analysis: ", analysis)
    message(message); return(NULL)
  } else { return(results) }
}
check_if_multiple_tables_found <- function(results, analysis){
  if (length(results) > 1L) {
    message <- paste0("Multiple tables found for analysis: ", analysis)
    message(message); return(NULL)
  } else { return(results) }
}
## main ########################################################################
#' @export
get_results_from_results_table_by_analysis <- function(tables, analysis){
  results <- tables[tables$analysis  == analysis, ]$results

  results <- check_if_no_table_found(results, analysis)
  results <- check_if_multiple_tables_found(results, analysis)

  return(results[[1]])
}
## main ########################################################################
#' @export
get_results <- function(tables, analysis){
  get_results_from_results_table_by_analysis(tables, analysis)
}

# style_table ##################################################################
## helpers #####################################################################
style_latex_table <- function(table){
  kableExtra::kable_paper(
    table, font_size = 9L, latex_options = c("repeat_header"),
    repeat_header_text = "\\textit{(continued)}", repeat_header_method = "replace"
  )
}
style_html_table <- function(table){
  kableExtra::kable_minimal(table, full_width = TRUE, html_font = "cambria")
}
## main ########################################################################
#' @export
style_table <- function(table, type){
  fun <- paste("style", type, "table", sep = "_")
  do.call(fun, list(table))
}

# paste_forward_slash ##########################################################
#' @export
paste_forward_slash <- function(...) paste(..., sep = "/")

# make_series_of_repeated_chars ################################################
#' @export
make_series_of_repeated_chars <- function(char_list)
  purrr::imap(char_list, ~ strrep(.y, .x)) %>% purrr::reduce(paste0)


# collapse_rows ################################################################
## helpers #####################################################################
make_integer_sequence <- function(start, end) start:end
extract_column_values <- function(df, column_index) df[[column_index]]
blank_sequentially_repeated_values <- function(values){
  if(NROW(values) %in% c(0L, 1L)) return(values)

  reference <- values[1L]
  for (i in make_integer_sequence(2L, NROW(values))) {
    value <- values[i]
    if(value == reference) { values[i] <- "" } else { reference <- value }
  }
  return(values)
}
collapse_rows_in_all_columns <- function(df){
  df <- collapse_rows_in_named_columns(df, colnames(df))
  return(df)
}
extract_last_value <- function(vector) vector[NROW(vector)]
collapse_rows_in_last_column_named <- function(df, column_names){
  collapse_column_name <- extract_last_value(column_names)
  dplyr::group_by(df, !!!rlang::syms(column_names)) %>%
    dplyr::mutate(
      !!collapse_column_name :=
        blank_sequentially_repeated_values(!!rlang::sym(collapse_column_name))
    ) %>%
    dplyr::ungroup()
}
collapse_rows_in_named_columns <- function(df, column_names){

  for(i in NROW(column_names):1L){
    df <- collapse_rows_in_last_column_named(df, column_names[1L:i])
  }
  return(df)
}
## main ########################################################################
#' @export
collapse_rows <- function(df, column_names = NULL) {
  if (!is.null(column_names)) {
    df <- collapse_rows_in_named_columns(df, column_names)
  } else {
    df <- collapse_rows_in_all_columns(df)
  }
  return(df)
}
