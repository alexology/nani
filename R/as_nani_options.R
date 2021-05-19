#' @title as_nani options
#'
#' @description Options to import datasets into nani format.
#'
#' @param element can be `all`, `nitrogen` and `phosphorus`.
#'
#' @details Data can be obtained with algorithms not implemented in the `nani` an imported into the `nani`
#' package format with `as_nani`. The function `as_nani_options` provides codes to import data for the target element.
#'
#' @seealso as_nani
#'
#' @export
#' @importFrom dplyr filter
#' @examples
#'
#' # list all of the codes for nitrogen
#' as_nani_options(element = "nitrogen")



as_nani_options <- function(element = "all"){

  if(identical(element, all)){
    elem_list <- pull(nani_options, "element")
    element <- unique(unlist(strsplit(elem_list, ", ")))
  }

  nani_options %>%
    filter(element %in% element)

}
