#' @title nitrogen fixation
#'
#' @description Symbiotic and non-symbiotic nitrogen fixation.
#'
#' @param cultivated_area crop area for each species.
#' @param crop crop biomass for each species.
#' @param coef_tbl coefficients.
#' @param fix_tbl data.frame with a list of nitrogen fixing species as specified below.
#' @param multiplier scaling factor (e.g. to calculate hectars instead of km^2^). It can be a vector to multiplier
#' different database.
#'
#' @details TBD
#'
#' @export
#' @importFrom dplyr rename filter left_join ungroup summarise group_by inner_join

nitrogen_fixation <- function(cultivated_area, crop, coef_tbl, fix_tbl, multiplier = NULL){

  cultivated_area <- as_tibble(cultivated_area)
  crop <- as_tibble(crop)
  coef_tbl<- as_tibble(coef_tbl)
  fix_tbl<- as_tibble(fix_tbl)

  # avoid rcmd checks
  type <- variable <- level <- coefficient <- code <- sp_unit <- . <- group <-
    value <- group_sum <- NULL

  # assure that the two data.frame have the same row and column names and order
  coef_tbl <- coef_tbl %>%
    filter(type == "nfix") %>%
    filter(variable == "nfix_rate_total") %>%
    select(level, coefficient) %>%
    rename(group = level)

  cul_sp_unit <- sub_df <- cultivated_area[, "sp_unit"]

  if(any("cultivated_area" %in% pull(fix_tbl, type))){
    temp_culti <- fix_tbl %>%
      filter(type == "cultivated_area") %>%
      pull(code)
    sub_df <- suppressMessages(cultivated_area %>%
      select(sp_unit, temp_culti) %>%
      left_join(sub_df, ., by = "sp_unit"))
    if(!is.null(multiplier) & length(multiplier) > 1){
      i <- unlist(lapply(sub_df, is.numeric))
      sub_df[i] <- sub_df[i] * multiplier[1]
    }
  }

  if(any("crop" %in% pull(fix_tbl, type))){
    temp_biom <- fix_tbl %>%
      filter(type == "crop") %>%
      pull(code)
    sub_df <- suppressMessages(crop %>%
                                 select(sp_unit, temp_biom) %>%
                                 left_join(sub_df, ., by = "sp_unit"))
    if(!is.null(multiplier) & length(multiplier) > 1){
      i <- unlist(lapply(sub_df, is.numeric))
      sub_df[i] <- sub_df[i] * multiplier[2]
    }
  }



  res <- suppressMessages(sub_df %>%
    pivot_longer(-sp_unit, names_to = "code") %>%
    left_join(fix_tbl, by = "code") %>%
    select(-code) %>%
    group_by(sp_unit, group) %>%
    summarise(group_sum = sum(value, na.rm = TRUE)) %>%
    ungroup() %>%
    inner_join(coef_tbl, by = "group") %>%
    group_by(sp_unit, group) %>%
    summarise(group_sum = group_sum * coefficient) %>%
    ungroup() %>%
    pivot_wider(sp_unit, names_from = group, values_from = group_sum) %>%
    left_join(cul_sp_unit, .))


  # transform results if needed
  if(!is.null(multiplier)){
    if(length(multiplier) > 1){
      i <- unlist(lapply(res, is.numeric))
      res[i] <- res[i] * multiplier[3]
    }
    if(length(multiplier) == 1){
      i <- unlist(lapply(res, is.numeric))
      res[i] <- res[i] * multiplier
    }
  }

  attr(res, "type") <- c("nitrogen_fixation")
  res
}
