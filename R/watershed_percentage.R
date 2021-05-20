#' @title watershed percentage
#'
#' @description Normalise data to watershed area in each spatial unit.
#'
#' @param x a `data.frame` or `tibble`.
#' @param percentage a `data.frame` or `tibble` containing the percentage of each spatial unit in a watershed.
#' @param multiplier multiply by a number (e.g. to calculate hectars instead of km^2^).
#' 
#' @details Administrative boundaries often does not match with watershed boundaries. The function `watershed_percentage`
#' normalise data to watershed area in each spatial unit. The x argument need a column called "sp_unit" and another
#' column with percentages of each spatial unit in a watershed (0-1). The second can have any name.
#'
#' @export
#' @importFrom dplyr %>% as_tibble inner_join pull

watershed_percentage <- function(x, percentage, multiplier = NULL){

  x <- as_tibble(x)
  percentage <- as_tibble(percentage)

  x <- inner_join(x, percentage[, "sp_unit"], by = "sp_unit")
  percentage <- inner_join(percentage, x[, "sp_unit"], by = "sp_unit") %>%
    pull(2)


  i <- unlist(lapply(x, is.numeric))
  if(!is.null(multiplier)){
    x[i] <- x[i] * multiplier
  }

  x[i] <- sweep(x[i], 1, percentage, "*")

  as_tibble(x)
}
