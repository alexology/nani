#' @title adult_animal_unit
#'
#' @description Calculate adult animal units.
#'
#' @param animal_numbers number of each animal species.
#' @param coef_tbl coefficients.
#' @param multiplier multiply by a number (e.g. to calculate hectars instead of km^2^).
#'
#' @details TBD
#'
#' @export
#' @importFrom dplyr filter as_tibble %>% pull mutate select
#' @importFrom tidyr pivot_wider pivot_longer replace_na

adult_animal_unit <- function(animal_numbers, coef_tbl, multiplier = NULL){

  animal <- as_tibble(animal_numbers)
  coef_tbl <- as_tibble(coef_tbl)

  # avoid rcmd checks
  variable <- coefficient <- sp_unit <- numbers <- . <- aau <- type <- level <- NULL

  # prepare the coefficient table for the join
  coef_tbl <- coef_tbl %>%
    filter(type == "animal") %>%
    filter(variable == "aau") %>%
    select(level, coefficient)

  # calculate aau
  res <- animal %>%
    pivot_longer(-sp_unit, names_to = "level", values_to = "numbers") %>%
    inner_join(coef_tbl, by = "level") %>%
    mutate(aau = numbers * coefficient) %>%
    { if (!is.null(multiplier)) mutate(., aau = aau * multiplier) else . } %>%
    select(sp_unit, level, aau) %>%
    pivot_wider(names_from = level, values_from = aau) %>%
    mutate(across(where(is.numeric), replace_na, replace = 0))

  attr(res, "type") <- c("adult_animal_unit")
  res
}
