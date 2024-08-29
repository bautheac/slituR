#' @export
floor_to_nearest_multiple <- function(number, multiple)
  number - number %% multiple


#' @export
floor_year_to_nearest_decade <- function(year)
  floor_to_nearest_multiple(year, 10L)


#' @export
remove_decade_from_year <- function(year) year %% 10L

