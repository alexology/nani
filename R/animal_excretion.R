#' @title animal excretion
#'
#' @description Element content in animal excretion.
#'
#' @param animal_numbers number of each animal species.
#' @param coef_tbl coefficients.
#' @param multiplier multiply by a number (e.g. to calculate hectars instead of km^2^).
#' @param type_excretion gross or net.
#'
#' @details TBD
#'
#' @export
#' @importFrom dplyr as_tibble filter select %>% mutate across
#' @importFrom tidyr pivot_wider pivot_longer replace_na

animal_excretion <- function(animal_numbers, coef_tbl, multiplier = NULL, type_excretion = "net"){

  animal <- as_tibble(animal_numbers)
  coef_tbl <- as_tibble(coef_tbl)

  if(! type_excretion %in% c("net", "gross")){
    stop("type_excretion must be one of net or gross")
  }

  # avoid rcmd checks
  variable <- level <- coefficient <- net_excretion <- element_excretion <-
    type <- sp_unit <- . <- numbers <- res_exc <- NULL

  # prepare the coefficient table for the join
  coef_tbl <- coef_tbl %>%
    filter(type == "animal") %>%
    filter(variable %in% c("net_excretion", "element_excretion")) %>%
    select(level, variable, coefficient) %>%
    pivot_wider(level, names_from = variable, values_from = coefficient)


  # calculate excretion
  res <- animal %>%
    pivot_longer(-sp_unit, names_to = "level", values_to = "numbers") %>%
    inner_join(coef_tbl, by = "level") %>%
    { if (type_excretion == "net") mutate(., res_exc = numbers * net_excretion) else mutate(., res_exc = numbers * element_excretion) } %>%
    { if (!is.null(multiplier)) mutate(., res_exc = res_exc * multiplier) else . } %>%
    select(sp_unit, level, res_exc) %>%
    pivot_wider(names_from = level, values_from = res_exc) %>%
    mutate(across(where(is.numeric), replace_na, replace = 0))


  if(identical(type_excretion, "net")){
    attr(res, "type") <- c("animal_excretion_net")
  }

  if(identical(type_excretion, "gross")){
    attr(res, "type") <- c("animal_excretion_gross")
  }

  res
}
