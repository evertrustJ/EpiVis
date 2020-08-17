#' Plot a heatmap.
#'
#' High-level function for plotting a heatmap given a EpiVis dataframe.
#'
#' @details It should be noted that this `epi_heatmap` function is faily high-leveled;
#'
#'
#' @importFrom graphics title
#' @importFrom graphics points
#' @importFrom graphics legend
#' @importFrom maps map
#'
#' @param epi a proper EpiVis data frame.
#' @param method a string denoting the method of interpolation. Only support bayesian kriging at the moment.
#' @param pts a numeric denoting the number of points to interpolate. Generally points interpolated would be less that the floor of `pts`.
#' @export
#' @return draw a heatmap on default r plot

epi_heatmap <- function(epi=NULL,
                        method='krig',
                        pts=-1){
  if (epi_check(epi)==FALSE) return (cat("Please enter a proper EpiVis data frame."))

  if (!method %in% c('krig')) return('This method is not yet supported.')
  ## Interpolate the grid.
  grid_pred <- epi_interpolate(epi=epi, method=method, pts=pts)
  
  color_repeat_times <- nrow(grid_pred)/4
  col=c(rep('green3', color_repeat_times),
        rep('greenyellow', color_repeat_times),
        rep('yellow2', color_repeat_times),
        rep('red', color_repeat_times+color_repeat_times%%4))

  map('world', epi$map[1])
  points(grid_pred[,1], grid_pred[,2], col=col, pch=15, cex=1.5)
  rate_pred = signif(grid_pred[,3], 2)
  title(main=expression('Rate of Disease per 10000 people'),  xlab="Longitude", ylab="Latitude")
  legend('bottomright', col=c('green3','greenyellow','yellow2','red'), pch=19,
         legend=c(paste("[",rate_pred [1],",",rate_pred [color_repeat_times],"]"),
                  paste("[",rate_pred [color_repeat_times+1],",",rate_pred [color_repeat_times*2],"]"),
                  paste("[",rate_pred [color_repeat_times*2+1],",",rate_pred [color_repeat_times*3],"]"),
                  paste("[",rate_pred [color_repeat_times*3+1],",",rate_pred [length(rate_pred)],"]")),
         cex=0.75)
}

