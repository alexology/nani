#' @title as_nani
#'
#' @description Returns a tibble that fit with the `nani` package format.
#'
#' @param x data to transform into the `nani` format.
#' @param input the type of input.
#' @param multiplier scaling factor (e.g. to calculate hectars instead of km^2^).
#'
#' @details Inputs can be calculated in several ways dependending on the available datasets.
#' The `nani` package implements widely used calculations methods, but to cover the entire set of possibile calculation methods is unrealistic.
#' The function `as_nani` allows to transfrom user data into a format suitable to the `nani` package. For instance,
#' data on animal excretion can be calculated with a different method, but transformed into a format suitable to the `nani` package.
#' Several options are provided, use the function `as_nani_options` for details.
#'
#' @seealso as_nani_options
#'
#' @export
#' @importFrom dplyr as_tibble mutate across
#' @importFrom tidyr replace_na


as_nani <- function(x, input, multiplier = NULL){
  x <- as_tibble(x)

  x <- suppressMessages(x %>%
                          mutate(across(where(is.numeric), replace_na, replace = 0)))

  if(!is.null(multiplier)){
    x <- suppressMessages(x %>%
                            mutate(across(where(is.numeric), ~ . * multiplier)))
  }

  attr_nani <- nani_options %>%
    pull(input)

  if(! input %in% attr_nani){
    stop(paste(input, "is not a valid input"))
  }

  attr(x, "type") <- input

  x
}
