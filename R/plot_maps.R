#' @title plot maps
#'
#' @description plot maps
#'
#' @param x a data.frame.
#' @param shape a shapefile.
#' @param sp_unit spatial unit to plot.
#' @param variable variable to plot
#' @param alpha set transparency.
#' @param background set an OpenStreetMaps backround. Default to `NULL`.
#' Options are many, useful are "open-street-map" and "stamen-terrain".
#' See plotly reference \href{https://plotly.com/r/reference/layout/mapbox/}{here} in the style section.
#'
#' @details Yield is calculated as the product between cultivated area and biomass
#'
#' @export
#' @importFrom plotly plot_ly plotly_build
#' @importFrom sf st_bbox st_transform
#' @importFrom stats as.formula

plot_maps <- function(x, shape , sp_unit = NULL, variable = NULL, alpha = 1, background = NULL){

  if(is.null(sp_unit)){
    stop("please set sp_unit.")
  }

  if(is.null(variable)){
    stop("please set variable.")
  }

  colnames(x)[colnames(x) %in% "sp_unit"] <- sp_unit

  shape_merged <- merge(shape, x, by = sp_unit)
  shape_merged <- st_transform(shape_merged, crs = "epsg:4326" )

  suppressPlotlyMessage <- function(p) {
    suppressMessages(plotly_build(p))
  }

  sp_lit <- as.formula(paste("~", sp_unit))
  col_our <- as.formula(paste("~", "`", variable, "`", sep = ""))



  fig <- suppressPlotlyMessage(plot_ly(shape_merged,
          split = sp_lit,
          color = col_our,
          alpha = alpha,
          showlegend = FALSE,
          type = "scatter",
          line = list(color = 'rgb(211, 211, 211)', width = 0.5)))

  if(is.character(background)){
    bb_range <- st_bbox(shape_merged)
    bb_x <- mean(bb_range[c(1,3)])
    bb_y <- mean(bb_range[c(2,4)])

    fig <- suppressPlotlyMessage(plot_ly(shape_merged,
                                         split = sp_lit,
                                         color = col_our,
                                         alpha = alpha,
                                         showlegend = FALSE,
                                         type = "scattermapbox",
                                         line = list(color = 'rgb(211, 211, 211)', width = 0.5))) %>%
      layout(mapbox = list(
        style = background,
        zoom =2.5,
        center = list(lon = bb_x, lat = bb_y)))
  }

  fig

}
