#' @export
paste_forward_slash <- function(...) paste(..., sep = "/")


#' @export
make_series_of_repeated_chars <- function(char_list){
  series_list <- purrr::imap(char_list, ~ strrep(.y, .x))
  series_string <- purrr::reduce(series_list, paste0)
  return(series_string)
}

#' @export
percentize <- function(value) paste0(round(value, digits = 4L) * 100, "%")
