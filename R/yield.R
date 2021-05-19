#' @title calculate yield
#'
#' @description calculate yield from total crop and cultivated area
#'
#' @param cultivated cultivated area for each species.
#' @param crop crop for each species.
#' @param multiplier multiply by a number (e.g. to calculate hectars instead of km^2^).
#'
#' @details Yield is calculated as the product between cultivated area and crop
#'
#' @export
#'
#' @importFrom tibble as_tibble
#' @importFrom dplyr mutate_if %>% inner_join


yield <- function(cultivated, crop, multiplier = NULL){

  cultivated <- as_tibble(cultivated)
  crop <- as_tibble(crop)

  # assure that the two data.frame have the same row and column names and order
  cultivated <- inner_join(cultivated, crop[, "sp_unit"], by  = "sp_unit", sort = FALSE)
  crop <- inner_join(crop, cultivated[, "sp_unit"], by  = "sp_unit")
  crop <- crop[match(pull(cultivated, "sp_unit"), pull(crop, "sp_unit")), match(colnames(cultivated), colnames(crop))]

  # calculate yields
  res <- (crop[, !colnames(crop) %in% "sp_unit"] / cultivated[, !colnames(cultivated) %in% "sp_unit"]) %>%
    mutate_if(is.numeric, function(x) ifelse(is.infinite(x), 0, x))
  res[is.na(res)] <- 0


  # transform results if needed
  if(!is.null(multiplier)){
    res <- res * multiplier
  }

  res <- as_tibble(cbind(cultivated[, "sp_unit"], res))
  attr(res, "type") <- "yield"
  res
}
