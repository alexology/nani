#' @title animal consumption
#'
#' @description Quantity of an element ingested by animals.
#'
#' @param animal_numbers number of each animal species.
#' @param coef_tbl coefficients.
#' @param multiplier multiply by a number (e.g. to calculate hectars instead of km^2^).
#'
#' @details Animal consumption is calculated according tothe following equation:
#'
#' \deqn{afc=animal\cdot consumption_rate}
#'
#' where afc is the animal feed consumption, animal is the the number of each animal species, consumption_rate is the nutrient consumption.
#' For example, nitrogen has a consumption_rate usually expressed as kg ind^-1^ year^-1^.
#'
#' @export
#' @importFrom dplyr filter as_tibble %>% pull mutate select
#' @importFrom tidyr pivot_wider pivot_longer replace_na

animal_feed_consumption <- function(animal_numbers, coef_tbl, multiplier = NULL){

  animal <- as_tibble(animal_numbers)
  coef_tbl <- as_tibble(coef_tbl)

  # avoid rcmd checks
  type <- variable <- level <- coefficient <- element_consumption <-
    sp_unit <- numbers <- . <- res_afc <- NULL

  # prepare the coefficient table for the join
  coef_tbl <- coef_tbl %>%
    filter(type == "animal") %>%
    filter(variable == "element_consumption") %>%
    select(level, coefficient)

  # calculate animal feed consumption
  res <- animal %>%
    pivot_longer(-sp_unit, names_to = "level", values_to = "numbers") %>%
    inner_join(coef_tbl, by = "level") %>%
    mutate(res_afc = numbers * coefficient) %>%
    { if (!is.null(multiplier)) mutate(., res_afc = res_afc * multiplier) else . } %>%
    select(sp_unit, level, res_afc) %>%
    pivot_wider(names_from = level, values_from = res_afc) %>%
    mutate(across(where(is.numeric), replace_na, replace = 0))

  attr(res, "type") <- c("animal_feed_consumption")
  res
}
