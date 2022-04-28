caption_latex <- function(x){
  x <- gsub("(\\d)\\%", "\\1\\\\%", x, ignore.case = TRUE, perl = TRUE)
  x <- gsub("&", "\\\\&", x, ignore.case = TRUE, perl = TRUE)
  x <- gsub("\\\\\\*", "*", x, ignore.case = TRUE, perl = TRUE)
  x <- gsub("\\\\@ref\\((.+?)\\)", "\\\\ref{\\1}", x, ignore.case = TRUE, perl = TRUE)

  x
}

caption_html <- function(x){
  x <- gsub("(\\d)\\\\%", "\\1%", x, ignore.case = TRUE, perl = TRUE)
  x <- gsub("\\\\&", "&", x, ignore.case = TRUE, perl = TRUE)
  x <- gsub("\\*", "\\\\\\*", x, ignore.case = TRUE, perl = TRUE)
  x <- gsub("\\\\ref\\{(.+?)\\}", "\\\\@ref(\\1)", x, ignore.case = TRUE, perl = TRUE)

  x
}

get_results <- function(tables, analysis){
  results <- tables[tables$analysis  == analysis, ]$results

  if (!length(results)) {
    message <- paste0("No table found for analysis: ", analysis)
    message(message); return(NULL)
  }

  if (length(results) > 1L) {
    message <- paste0("Multiple tables found for analysis: ", analysis)
    message(message); return(NULL)
  }

  return(results[[1]])
}

style_table <- function(type, table){

  if (type == 'html') {
    table <- kableExtra::kable_minimal(table, full_width = TRUE, html_font = "cambria")
    return(table)
  }

  if (type == 'latex') {
    table <- kableExtra::kable_paper(
      table, font_size = 9L, latex_options = c("repeat_header"),
      repeat_header_text = "\\textit{(continued)}", repeat_header_method = "replace"
    )
    return(table)
  }

  return(NULL)
}
