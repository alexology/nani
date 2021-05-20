#' @title fertilizers
#'
#' @description Fertilizers application.
#'
#' @param fertilizer_quantity fertilizers application.
#' @param coef_tbl coefficients.
#' @param multiplier multiply by a number (e.g. to calculate hectars instead of km^2^).
#'
#' @details TBD
#'
#' @export
#' @importFrom dplyr filter as_tibble %>% pull mutate select across
#' @importFrom tidyr pivot_wider pivot_longer replace_na

fertilizers <- function(fertilizer_quantity, coef_tbl, multiplier = NULL){

  fertilizer_quantity <- as_tibble(fertilizer_quantity)
  coef_tbl <- as_tibble(coef_tbl)

  # avoid rcmd checks
  type <- variable <- level <- coefficient <-
    sp_unit <- numbers <- . <- res_fert <- NULL

  # prepare the coefficient table for the join
  coef_tbl <- coef_tbl %>%
    filter(type == "fert") %>%
    select(level, coefficient)

  # calculate fertilizers load
  res <- fertilizer_quantity %>%
    pivot_longer(-sp_unit, names_to = "level", values_to = "numbers") %>%
    inner_join(coef_tbl, by = "level") %>%
    mutate(res_fert = numbers * coefficient) %>%
    { if (!is.null(multiplier)) mutate(., res_fert = res_fert * multiplier) else . } %>%
    select(sp_unit, level, res_fert) %>%
    pivot_wider(names_from = level, values_from = res_fert) %>%
    mutate(across(where(is.numeric), replace_na, replace = 0))

  attr(res, "type") <- "fertilizers"
  res
}
