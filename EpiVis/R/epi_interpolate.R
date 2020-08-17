#' A wrapper for interpolation methods.
#'
#' This wrappter is used so that further methods are more easily added.
#'
#' @details It should be noted that this function is a relatively high-level function;
#'    users would not be able to make changes to the function's default parameters.
#'    To allow such change, users should call those functions directly. Also, this method could be significantly slow if too many points are to be calculated,
#'   as calculating Covariance matrix is O(n^2)
#'
#' @param epi a proper EpiVis data frame.
#' @param pts an integer denoting the number of points to on the heatmap. Note that points = -1 if and only if kernal density is used.
#' @param method a string denoting the function of interpolation.
#' @return a function call to the method of choice.
#' @export
#'
epi_interpolate<- function(epi, pts, method='krig'){
  if(method=='krig'){
    grid <- epi_grid(epi, pts)
    myfit <- epi_bayesian_krig(epi, grid, mod=method, iter=400)
    r <- extract(myfit, pars="rate_pred") ## all the rate parameters
    rate_pred <- NULL
    for (i in 1:ncol(r$rate_pred)){
      rate_pred[i] = mean(r$rate_pred[401:800,i])
    }
    grid_pred <- cbind(grid$x, grid$y, rate_pred)
    grid_pred <- grid_pred[order(rate_pred),] ## return the grid in the ascending order that
                                              ##   rate predicted.
    grid_pred
  }
  else return(paste0("Method: ", method, " is not supported."))
}
