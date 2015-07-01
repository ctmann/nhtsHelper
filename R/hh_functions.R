#' Get the market segment
#'
#' @param vehicles vector with the number of vehicles in the household.
#' @param people vector with the number of people to compare against the number
#'   of vehicles.
#'
#' @return a vector with the market segment the household belongs to.
get_segment <- function(vehicles, people, type){

  ifelse(vehicles == 0, "zerocar", paste(people, type, sep = " "))
}
