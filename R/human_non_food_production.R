#' @title human non food production
#'
#' @description Quantity of an element produced by humans for factors other than food.
#'
#' @param human number of inhabitants.
#' @param coef_tbl coefficients.
#' @param multiplier multiply by a number (e.g. to calculate hectars instead of km^2^).
#'
#' @details TBD
#'
#' @export
#' @importFrom dplyr filter as_tibble %>% pull mutate select across
#' @importFrom tidyr pivot_longer replace_na

human_non_food_production <- function(human, coef_tbl, multiplier = NULL){
  
  human <- as_tibble(human)
  coef_tbl <- as_tibble(coef_tbl)
  
  # avoid rcmd checks
  type <- level <- coefficient <- sp_unit <- numbers <- . <- hum_non_food_prod <- NULL
  
  # prepare the coefficient table for the join
  coef_tbl <- coef_tbl %>%
    filter(type == "human") %>%
    filter(level %in% c("human_non_food_production")) %>%
    select(level, coefficient)
  
  
  # calculate food consumption
  res <- human %>%
    pivot_longer(-sp_unit, names_to = "level", values_to = "numbers") %>%
    mutate(hum_non_food_prod = numbers * pull(.data = coef_tbl, var = coefficient)) %>%
    { if (!is.null(multiplier)) mutate(., hum_non_food_prod = hum_non_food_prod * multiplier) else . } %>%
    select(sp_unit, hum_non_food_prod) %>%
    mutate(across(where(is.numeric), replace_na, replace = 0))
  
  attr(res, "type") <- c("human_non_food_production")
  res
}
