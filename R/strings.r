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

#' @export
get_path_last_element <- function(path = "") {

  if (path == "") return(NULL)

  if (!grepl("/", path)) return(path)

  path_elements <- unlist(strsplit(path, "/"))
  last_element <- tail(path_elements, n = 1L)
  return(last_element)
}

#' @export
make_shiny_main_directory_path <- function(local = "", server = "") {

  path_directory <- ifelse(
    Sys.getenv("SHINY_ENV") == "local", here::here(local), here::here(server)
  )

  return(path_directory)
}
