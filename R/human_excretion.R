#' @title human excretion
#'
#' @description Element content in human excretion.
#'
#' @param human number of inhabitants.
#' @param coef_tbl coefficients.
#' @param multiplier multiply by a number (e.g. to calculate hectars instead of km^2^).
#'
#' @details TBD
#'
#' @export
#' @importFrom dplyr filter as_tibble %>% pull mutate select
#' @importFrom tidyr pivot_longer replace_na

human_excretion <- function(human, coef_tbl, multiplier = 0.001){

  human <- as_tibble(human)
  coef_tbl <- as_tibble(coef_tbl)

  # avoid rcmd checks
  type <- level <- coefficient <- sp_unit <- numbers <- . <- hum_excr <- NULL

  # prepare the coefficient table for the join
  coef_tbl <- coef_tbl %>%
    filter(type == "human") %>%
    filter(level %in% c("population_equivalent")) %>%
    select(level, coefficient)


  # calculate human excretion
  res <- human %>%
    pivot_longer(-sp_unit, names_to = "level", values_to = "numbers") %>%
    mutate(hum_excr = numbers * pull(.data = coef_tbl, var = coefficient) * 365) %>%
    { if (!is.null(multiplier)) mutate(., hum_excr = hum_excr * multiplier) else . } %>%
    select(sp_unit, hum_excr) %>%
    mutate(across(where(is.numeric), replace_na, replace = 0))

  attr(res, "type") <- c("human_excretion")
  res
}
