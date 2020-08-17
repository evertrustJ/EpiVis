#' Interpolate a grid using bayesian kriging (MCMC).
#' 
#' Using coordinate grid and proper EpiVis dataframe to interpolate the grid with MCMC. 
#'       
#' @details Note that this method could be significantly slow if too many points are to be calculated,
#'   as calculating Covariance matrix is O(n^2). Also, the rate is multiplied by 10000 to avoid machine precision
#'   errors.is a relatively low-level function allowing users to customize their model, chains,
#'   and iterations. The output model and samples are saved locally for future use.
#'   
#' @importFrom rstan sampling
#' @importFrom rstan stan_model
#' @importFrom rstan extract
#' 
#' @param epi a proper EpiVis data frame.
#' @param grid an `n x 2` matrix of coordinates that one wish to predict at.
#' @param mod a string denoting the name of the stan file, but with exactly four `data` entries: nobs, npred, rate_obs, coord.
#' @param ... additional parameters passed to rstan::sampling.
#' 
#' @return fitted stan model.
#' @export
epi_bayesian_krig <- function(epi, grid, mod, ...){
  stan_mod <- stan_model(file=paste0(mod,'.stan'))
  data <- list(nobs=nrow(epi), npred=nrow(grid), 
               ## rate is multiplied by 10000 to avoid machine precision errors.
               rate_obs=epi$rate*10000, coord=rbind(cbind(epi$x, epi$y),
                                                    cbind(grid$x, grid$y)))
  stan_fit <- sampling(stan_mod, data=data, ...)
  save(stan_fit, stan_mod, file="spatial_stan.Rda")
  stan_fit 
}