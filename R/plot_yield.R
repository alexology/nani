#' @title plot yield
#'
#' @description plot yield
#'
#' @param yield a yield object.
#' @param relative scale all values to unit.
#' @param add_bound add boundaries to level 1 properties.
#'
#' @details Yield is calculated as the product between cultivated area and biomass
#'
#' @export
#' @importFrom plotly plot_ly layout
#' @importFrom tidyr pivot_longer %>% separate
#' @importFrom dplyr select across mutate
#' @importFrom RColorBrewer brewer.pal
#' @importFrom grDevices colorRampPalette

plot_yield <- function(yield, relative = TRUE, add_bound = TRUE){

  if(! attr(yield, "type") %in% "yield"){
    stop("an object from calc_yield is needed")
  }

  if(relative){
    row_sum <- apply(select(yield, -sp_unit), 1, sum)

    yield <- yield %>%
      mutate(across(where(is.numeric), ~ ./row_sum))

  }

  # avoid rcmd checks
  sp_unit <- name <- value <- NULL

  to_plot <- yield %>%
    pivot_longer(cols = !sp_unit) %>%
    separate(name, sep = "-", into = c("lev_1", "lev_2")) %>%
    mutate(value = round(value * 100, 2))


  x <- list(
    title = ""
  )

  y <- list(
    title = "yield"
  )

  if(relative){
    y <- list(
      title = "% yield"
    )
  }


  n_lev2 <- length(unique(to_plot$lev_2))
  lev2_colors <- colorRampPalette(brewer.pal(8, "Set2"))(n_lev2)

  if(add_bound){
    fig <- plot_ly(to_plot, x = ~sp_unit, y = ~value, color = ~lev_2, type = 'bar', text = to_plot$lev_1, colors = lev2_colors,
                   marker = list(line = list(color = 'rgb(211,211,211)',
                                             width = 0.5))) %>%
      layout(yaxis = y, xaxis = x, barmode = 'stack')
  } else {
    fig <- plot_ly(to_plot, x = ~sp_unit, y = ~value, color = ~lev_2, type = 'bar', text = to_plot$lev_1, colors = lev2_colors) %>%
      layout(yaxis = y, xaxis = x, barmode = 'stack')
  }

  fig
}
