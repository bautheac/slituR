#' @export
significance <- function(x) {
  dplyr::case_when(x >= 0.1 ~ "", x > 0.05 ~ "*", x > 0.01 ~ "**", TRUE ~ "***")
}
