
#' Generic euclidean distance function; not to be exported.
#' @param coord Matrix of `n x 2` covariate locations (each row is an observation).
#' @return an `n x n` matrix of euclidean distances.
euclid_dist <- function (coord){
  n = nrow(coord)
  dist <- matrix(nrow=5, ncol=5)
  for(ii in 1:n) {
    for(jj in 1:(ii-1)) {
      dist[ii, jj] = sqrt((coord[ii,1]-coord[jj,1])^2+
                            (coord[ii,2]-coord[jj,2])^2)
      dist[jj, ii] = dist[ii, jj]
    }
    dist[ii, ii] = 0
  }
  dist
}

#' Calculates the spherical covariance matrix.
#' 
#' @param dist a `n x n` distance matrix.
#' @param range an numeric denoting the range of variogram
#' @param sill an numeric denoting the sill of variogram
#' @param nugget an numeric denoting the nugget of variogram
#' @return an `n x n` matrix of spherical covariances.
sph_cov <- function(coord, range, sill, nugget){
  n <- nrow(coord)
  dist <- euclid_dist(coord)
  C <- matrix(nrow=n, ncol=n)
  for(ii in 1:n) {
    for(jj in 1:ii) {
      if (dist[ii, jj] > range){
        C[ii,jj] = 0
      }
      else {
        C[ii,jj] = (sill-nugget)*(1-1.5*dist[ii,jj]/range+0.5*(dist[ii,jj]/3)^3)
      }
      
      C[jj, ii] = C[ii, jj] ## Covariance matrix is symmetric
    }
  }
  C
}

#' Logposterior for the spherical (isotropic) Gaussian process.
#'
#' @author Martin Lysy
#' @param beta0 Mean parameter (scalar).
#' @param range an numeric denoting the range of variogram
#' @param sill an numeric denoting the sill of variogram
#' @param nugget an numeric denoting the nugget of variogram
#' @param rate Vector of `n` observations.
#' @param rate_pred Nuisance Parameter
#' @param coord Matrix of `n x 2` covariate locations (each row is an observation).
#' @return The logposterior (scalar) up to a normalizing constant.
gsph_logpost <- function(mu, range, sill, nugget, rate_obs, coord, rate_pred) {
  dist <- euclid_dist(coord)
  r <- rate_pred
  lpi <- -0.99*log(range) +0.01*range- 1.01*log(sill) -0.01*sill - log(sill) # prior contribution
  # loglikelihood contribution
  ll <- dmNorm(rate_obs, mu = rep(mu, length(rate_obs)), log = TRUE,
               Sigma = sph_cov(coord, range, sill, nugget))
  ll + lpi
}

#' General Gaussian process predictions.
#' @author Martin Lysy
#' @param xO Vector of observations.
#' @param muO Vector of (unconditional) mean of observations.
#' @param VOO Covariance matrix of observations.
#' @param muP Mean of predictions.
#' @param VPP Covariance matrix of predictions.
#' @param VOP Covariance matrix between observations and predictions.
#' @param n Number of random samples to draw.
#' @return

#' - If `n` is not missing, returns `n` samples from the conditional predictive distribution `p(xP | xO)`.
#' - If `n` is missing, returns a list with elements `mu` and `V` containing the mean and variance of the predictive distribution.
#' @details Gaussian Process Regression (GPR) operates under the assumption that `(xO, xP)` are jointly distributed as multivariate normal, with
#' ```
#' E[xO] = muO,      E[xP] = muP,
#' var(xO) = vOO,    var(xP) = VPP,    cov(xO, xP) = VOP.
#' ```
#' Hence, the predictive distribution `p(xP | xO)` is also multivariate normal, and it is this distribution which is computed by `gp_pred()`.
max.tol = 5
gp_pred <- function(n, xO, muO, VOO, muP, VPP, VOP) {
  IP <- crossprod(solve(VOO, VOP), cbind(xO - muO, VOP))
  mu <- muP + IP[,1]
  V <- VPP - IP[,-1]
  if(missing(n)) {
    out <- list(mu = mu, V = V)
  } else {
    out <- rmNorm(n, mu = mu, Sigma = V)
  }
  out
}
