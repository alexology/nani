#' @title animal production
#'
#' @description Animal production for human consumption.
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

animal_production <- function(animal_numbers, coef_tbl, multiplier = NULL){

  # transform input to tibble
  animal <- as_tibble(animal_numbers)
  coef_tbl <- as_tibble(coef_tbl)

  # avoid rcmd checks
  type <- variable <- level <- coefficient <- element_consumption <-
    element_excretion <- human_consumption <- butchered_fraction <-
    sp_unit <- numbers <- . <- res_ap <- NULL

  # prepare the coefficient table for the join
  coef_tbl <- coef_tbl %>%
    filter(type == "animal") %>%
    filter(variable %in% c("element_consumption", "element_excretion", "human_consumption", "butchered_fraction")) %>%
    select(level, variable, coefficient) %>%
    pivot_wider(level, names_from = variable, values_from = coefficient)

  # calculate animal production
  res <- animal %>%
    pivot_longer(-sp_unit, names_to = "level", values_to = "numbers") %>%
    inner_join(coef_tbl, by = "level") %>%
    mutate(res_ap = numbers * ((element_consumption - element_excretion) * (1 - human_consumption) * butchered_fraction)) %>%
    { if (!is.null(multiplier)) mutate(., res_ap = res_ap * multiplier) else . } %>%
    select(sp_unit, level, res_ap) %>%
    pivot_wider(names_from = level, values_from = res_ap) %>%
    mutate(across(where(is.numeric), replace_na, replace = 0))

  attr(res, "type") <- c("animal_production")
  res
}
