#' @title calculate nitrogen from crops
#'
#' @description Calculate nitrogen from cultivation.
#'
#' @param crop crop for each species.
#' @param coef_tbl coefficients.
#' @param multiplier multiply by a number (e.g. to calculate hectars instead of km^2^).
#'
#' @details TBD
#'
#' @export
#' @importFrom dplyr filter as_tibble %>% pull mutate select across
#' @importFrom tidyr pivot_wider pivot_longer replace_na

crop_production <- function(crop, coef_tbl, multiplier = NULL){

  crop <- as_tibble(crop)
  coef_tbl <- as_tibble(coef_tbl)

  # avoid rcmd checks
  type <- variable <- level <- coefficient <- dry_weight_percentage <-
    element_content_percentage <- sp_unit <- numbers <- . <- res_cp <-  NULL

  # prepare the coefficient table for the join
  coef_tbl <- coef_tbl %>%
    filter(type == "crop") %>%
    filter(variable %in% c("dry_weight_percentage", "element_content_percentage")) %>%
    select(level, variable, coefficient) %>%
    pivot_wider(level, names_from = variable, values_from = coefficient)


  # calculate crop production
  res <- crop %>%
    pivot_longer(-sp_unit, names_to = "level", values_to = "numbers") %>%
    inner_join(coef_tbl, by = "level") %>%
    mutate(res_cp = numbers * dry_weight_percentage * element_content_percentage) %>%
    { if (!is.null(multiplier)) mutate(., res_cp = res_cp * multiplier) else . } %>%
    select(sp_unit, level, res_cp) %>%
    pivot_wider(names_from = level, values_from = res_cp) %>%
    mutate(across(where(is.numeric), replace_na, replace = 0))

  attr(res, "type") <- c("crop_production")
  res
}
