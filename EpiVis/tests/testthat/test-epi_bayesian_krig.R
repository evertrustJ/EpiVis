#--- automated tests for qr_fit ------------------------------------------------
#' Author: Martin Lysy
require(rstan)
require(mniw)

#context("testing_functions")
source('testing_functions.R', echo=TRUE)

set.seed(42422) ## Sometimes a lot of NA's are generated; this interferes with our testing mechanism
# check that Stan and R logposteriors are identical up to a constant

# simulate some data
n_obs <- 5
coord <- matrix(rnorm(n_obs*2), n_obs, 2)
range = rexp(1)
sill = rexp(1)
nugget = runif(1,min=0,max=sill)
mu = runif(1)

rate_all <- rmNorm(1, mu = rep(mu, n_obs),
               Sigma = sph_cov(coord, range, sill, nugget))

# compile stan model
spat_mod <- stan_model("krig.stan")

# instantiate the model corresponding to p(mu, sigma, lambda | y, X) in Stan
spat_dat <- list(nobs = nrow(coord), npred = 0,coord = coord, rate_obs = rate_all)
spat_fit <- sampling(spat_mod, data = spat_dat, iter = 500,
                    verbose = TRUE, chains = 1)

# simulate some parameter values
n_sim <- 15
Pars <- replicate(n = n_sim, expr = {
  list(range = rexp(1),
       sill = rexp(1),
       nugget = runif(1,min=0,max=sill),
       mu = runif(1),
       rate_obs = rate_all,
       rate_pred = numeric(0))
}, simplify = FALSE)

# log-posterior calculation in R
lp_r <- sapply(1:n_sim, function(ii) {
  do.call(gsph_logpost, c(list(coord=coord), Pars[[ii]]))
})


# log-posterior calculation in Stan
lp_stan <- sapply(1:n_sim, function(ii) {
  upars<-unconstrain_pars(object = spat_fit, pars = Pars[[ii]])
  log_prob(object = spat_fit, upars=upars, adjust_transform = FALSE)
})

## in this case these are equivalent, but will generate NaN for our test result
for (i in 1:length(lp_r)){
  if (is.na(lp_r[i])) lp_r[i] = 0
}
for (i in 1:length(lp_stan)){
  if (lp_stan[i]==-Inf||lp_stan[i]==Inf) lp_stan[i] <- 0
}

all(lp_r - lp_stan<max.tol) # check that they are within an error tolerance.

