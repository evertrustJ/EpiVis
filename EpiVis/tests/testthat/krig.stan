functions {
  
  /// Calculate the distance for two locations (in latitude and longitude).
  //
  /// @param loc1 A coordinate of longitude and latitue.
  /// @param loc2 A coordinate of longitude and latitue.
  /// @return A real number denoting distance in 250 km.
  real harv_dist(row_vector loc1, row_vector loc2){
    real R = 6371.01/250; // the radius of earch in 250 km
    
    // coordinates:
    real lat1 = loc1[1]; 
    real lon1 = loc1[2];
    real lat2 = loc2[1];
    real lon2 = loc2[2];
    
    // distance:
    real dlat = fabs(lat1-lat2);
    real dlon = fabs(lon1-lon2);
    
    // harvesian distance formula
    real a = sin(dlat/2)^2 + cos(lat1) * cos(lat2) * sin(dlon/2)^2;
    real c = 2 * atan2(sqrt(a), sqrt(1-a));
    return R * c;
  }
  
  /// Generalization of harv_dist function to multiple locations (greater than 2).
  //
  /// @param coord A nx2 matrix of coordinates in longitude and latitude
  /// @return A symmetric matrix of distances in 250 km.
  matrix gen_harv(matrix coord){
    int n = rows(coord);
    matrix[n,n] dist;
    for(ii in 1:n) {
      for(jj in 1:(ii-1)) {
        dist[ii, jj] = harv_dist(coord[ii], coord[jj]);
        dist[jj, ii] = dist[ii, jj]; // symmetric distance function
      }
    dist[ii, ii] = 0;  
    }
    return dist;
  }
  
  /// Generalization of distance function to multiple locations (greater than 2).
  //
  /// @param coord A nx2 matrix of coordinates in longitude and latitude
  /// @return A symmetric matrix of euclidean distances of longitude and latitude.
  matrix euclidean_distance(matrix coord){
    int n = rows(coord);
    matrix[n,n] dist;
    for(ii in 1:n) {
      for(jj in 1:(ii-1)) {
        dist[ii, jj] = distance(coord[ii], coord[jj]);
        dist[jj, ii] = dist[ii, jj]; // symmetric euclidean distance
      }
    dist[ii, ii] = 0;
    }
    return dist;
  }
  
  /// Two-dimensional spherical (isotropic) covariance matrix.
  //
  /// @param coord An `n x 2` matrix of covariate locations (each row is an observation.)
  /// @param lambda Vector of two positive lengthscales.
  
  matrix sph_cov(matrix dist, real sill,
                 real nugget, real range){
    int n = rows(dist);
    matrix[n, n] C;
    for(ii in 1:n) {
      for(jj in 1:(ii-1)) {
        if (dist[ii, jj] > range) C[ii,jj] = 0;
        else C[ii,jj] = (sill-nugget)*(1-1.5*dist[ii,jj]/range+0.5*(dist[ii,jj]/3)^3);
        C[jj, ii] = C[ii, jj]; // covariance matrix is symmetric
      }
      C[ii, ii] = sill;
    }
    return C;
    }
}


data {
  int nobs; // number of measurements on the map
  int npred; // locations to predict/interpolate
  vector[nobs] rate_obs;
  matrix[nobs+npred, 2] coord;
}

parameters {
  real<lower = 0> nugget;
  real<lower = 0> sill;
  real<lower = 0> range;
  real mu;
  //real betaX;
  //real betaY;
  vector<lower = 0>[npred] rate_pred;
}

model {
  int n = nobs+npred;
  matrix[n,n] C; // covariance matrix for both obs and pred
  matrix[n,n] dist; // distance matrix for both obs and pred
  vector[n] rate_all = append_row(rate_obs, rate_pred);
  real ll;
  real lpi;
  
  // fairly weak priors
  range ~ gamma(0.01, 0.01);
  sill ~ inv_gamma(0.01, 0.01);
  mu ~ uniform(-1000, 1000);
  //betaX ~ uniform(-1000,1000);
  //betaY ~ uniform(-1000,1000);
  nugget ~ uniform(0, sill);

  
  dist = euclidean_distance(coord); // we found harvesine distance does not work
  
  // we wish to use a spherical covariance matrix
  C = sph_cov(dist, sill, nugget, range);
  
  lpi = -0.99*log(range) +0.01*range- 1.01*log(sill) -0.01*sill - log(sill);
  ll = multi_normal_lpdf(rate_all|rep_vector(mu, n), C);
  // the rate follows a GP with mean mu and covariance C
  target += lpi+ll;
}
