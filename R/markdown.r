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
#' @export
parse_table_caption_for_latex_output <- function(x){
  x <- escape_percentage_sign_before_numbers_in_latex(x)
  x <- escape_ampersand_sign_in_latex(x)
  x <- unescape_star_sign_in_latex(x)
  x <- parse_references_in_latex(x)

  return(x)
}
#' @export
caption_latex <- function(x){ return(parse_table_caption_for_latex_output(x)) }



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
#' @export
parse_table_caption_for_html_output <- function(x){
  x <- escape_percentage_sign_before_numbers_in_html(x)
  x <- escape_ampersand_sign_in_html(x)
  x <- unescape_star_sign_in_html(x)
  x <- parse_references_in_html(x)

  return(x)
}
#' @export
caption_html <- function(x){ parse_table_caption_for_html_output(x) }


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
#' @export
get_results_from_results_table_by_analysis <- function(tables, analysis){
  results <- tables[tables$analysis  == analysis, ]$results

  results <- check_if_no_table_found(results, analysis)
  results <- check_if_multiple_tables_found(results, analysis)

  return(results[[1]])
}
#' @export
get_results <- function(tables, analysis){
  get_results_from_results_table_by_analysis(tables, analysis)
}

style_latex_table <- function(table){
  kableExtra::kable_paper(
    table, font_size = 9L, latex_options = c("repeat_header"),
    repeat_header_text = "\\textit{(continued)}", repeat_header_method = "replace"
  )
}
style_html_table <- function(table){
  kableExtra::kable_minimal(table, full_width = TRUE, html_font = "cambria")
}

#' @export
style_table <- function(table, type){

  fun <- paste("style", type, "table", sep = "_")

  do.call(fun, list(table))
}


#' @export
paste_forward_slash <- function(...) paste(..., sep = "/")


#' @export
make_series_of_repeated_chars <- function(char_list)
  purrr::imap(char_list, ~ strrep(.y, .x)) %>% purrr::reduce(paste0)
