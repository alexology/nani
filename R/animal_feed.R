#' @title animal feed
#'
#' @description Element content in animal feed.
#'
#' @param crop crop biomass as calculated with `crop_production`.
#' @param coef_tbl coefficients.
#' @param multiplier multiply by a number (e.g. to calculate hectars instead of km^2^).
#'
#' @details TBD
#'
#' @export
#' @importFrom dplyr as_tibble filter select %>% mutate across
#' @importFrom tidyr pivot_wider pivot_longer replace_na

animal_feed <- function(crop, coef_tbl, multiplier = NULL){

  crop <- as_tibble(crop)
  coef_tbl <- as_tibble(coef_tbl)

  if(! identical(attr(crop, "type"), ("crop_production"))){
    stop("crop production is needed")
  }
  
  # avoid rcmd checks
  type <- variable <- level <- coefficient <- human_food_fraction <-
    fraction_loss_during_animal_processing <- sp_unit <- numbers <-
    . <- res_animal_feed <- NULL



  # prepare the coefficient table for the join
  coef_tbl <- coef_tbl %>%
    filter(type == "crop") %>%
    filter(variable %in% c("human_food_fraction", "fraction_loss_during_animal_processing")) %>%
    select(level, variable, coefficient) %>%
    pivot_wider(level, names_from = variable, values_from = coefficient)

  # calculate animal feed
  res <- crop %>%
    pivot_longer(-sp_unit, names_to = "level", values_to = "numbers") %>%
    inner_join(coef_tbl, by = "level") %>%
    mutate(res_animal_feed = numbers * (1 - human_food_fraction) * (1 - fraction_loss_during_animal_processing) ) %>%
    { if (!is.null(multiplier)) mutate(., res_animal_feed = res_animal_feed * multiplier) else . } %>%
    select(sp_unit, level, res_animal_feed) %>%
    pivot_wider(names_from = level, values_from = res_animal_feed) %>%
    mutate(across(where(is.numeric), replace_na, replace = 0))

  attr(res, "type") <- c("animal_feed")
  res
}
