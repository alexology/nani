#' @title net anthropogenic input
#'
#' @description Calculates the net anthropogenic input of the desired variable.
#'
#' @param element can be one of `nitrogen` and `phosphorus`.
#' @param ... objects needed for calculated anthropogenic input (see details).
#'
#' @details TBD
#'
#' @export
#' @importFrom dplyr rowwise summarise inner_join as_tibble mutate c_across


net_anthropogenic_input <- function(..., element = "nitrogen"){
  arguments <- list(...)

  # avoid rcmd checks
  sp_unit <- . <- net_import_feed <- net_import_food <- depositions <- net_import_food_feed <- NULL

  if(identical(element, "nitrogen")){
    input_needed <- c("nitrogen_fixation", "fertilizers", "depositions", "crop_production", "human_food",
                      "human_food_consumption", "human_excretion", "animal_feed", "animal_feed_consumption",
                      "animal_production", "animal_excretion_gross")
  }
  
  if(identical(element, "phosphorus")){
    input_needed <- c("fertilizers", "depositions", "crop_production", "human_food",
                      "human_food_consumption", "human_excretion", "animal_feed", "animal_feed_consumption",
                      "animal_production", "animal_excretion_gross")
  }
  

  input_user <- unlist(lapply(arguments, function(x) attr(x, "type")))

  if(length(input_needed) < length(input_user)){
    stop("more objects than needed have been provided")
  }

  if(any(!input_needed %in% input_user)){
    input_diff <- setdiff(input_needed, input_user)
    stop(paste("missing input:" , paste(input_diff, collapse = ", ")))
  }

  res <- arguments[[1]] %>%
    select(sp_unit)

  for(i in 1:length(arguments)){
    res <- suppressMessages(arguments[[i]]  %>%
                              rowwise(sp_unit) %>%
                              summarise(col = sum(c_across(where(is.numeric)))) %>%
                              inner_join(res, ., by = "sp_unit"))

  }

  colnames(res) <- c("sp_unit", input_user)
  res <- res[, match(c("sp_unit", input_needed), colnames(res))]
  attr(res, "type") <- NULL
  res %>%
    mutate(net_import_feed = animal_feed_consumption - animal_feed) %>%
    mutate(net_import_food = human_food_consumption - animal_production - human_food) %>%
    mutate(net_import_food_feed = net_import_feed + net_import_food) %>%
    mutate(net_input = nitrogen_fixation + fertilizers + depositions + net_import_food_feed) %>%
    as_tibble()

}
