library(rSPDE)

setup = function(a_res,c){
  l_res = a_res*c # Total Latent length of one side
  l_pts = cbind(rep(1:l_res,l_res),rep(1:l_res,each=l_res)) # Latent Grid
  a_pts = cbind(rep(1:a_res,a_res),rep(1:a_res,each=a_res)) # Aerial Grid
  
  l_D = as.matrix(dist(l_pts))/(l_res-1) # Find the distances. Defaults to Euclidean

    #l_D = as.matrix(dist(l_pts))
  # K is basically an indicator matrix showing which indices of the latent grid correspond to which index of the aerial grid
  K = matrix(0, a_res^2,l_res^2)
  for(i in 1:a_res^2){
    K[i,] = l_pts[,1] >= a_pts[i,1]*c-(c-1) & l_pts[,1] <= a_pts[i,1]*c & 
      l_pts[,2] >= a_pts[i,2]*c-(c-1) & l_pts[,2] <= a_pts[i,2]*c
  }
  K = K/c^2

  return(list(K=K,l_D=l_D,a_pts=a_pts,l_pts=l_pts))
}


# Matern Covariance Function: https://en.wikipedia.org/wiki/Mat%C3%A9rn_covariance_function
mk_cov = function(s2,rho,nu,setup){
  d <- setup$l_D+diag(1e-9,nrow(setup$l_D)) # Distance between two points, pull from setup, include slight variance on diags for stability
  #s2 * 2^(1-nu) / gamma(nu) * (sqrt(2*nu) * d / rho)^nu * besselK(sqrt(2*nu) * d/rho, nu)
  kappa <- sqrt(2*nu)/rho
  # Function reference: https://search.r-project.org/CRAN/refmans/rSPDE/html/matern.covariance.html
  matern.covariance(d, kappa, nu, sqrt(s2))
}

get_likelihood_manual <- function(s2, rho, nu, y_block, setup) {
  mat <- mk_cov(s2, rho, nu, setup)
  sig <- setup$K %*% mat %*% t(setup$K)
  dmvnorm(x = c(y_block), sigma=sig, log=TRUE)
}

get_likelihood_1_param_optim <- function(log_params, y_block, setup, rho, nu) {
  s2 <- exp(log_params[[1]])
  mat <- mk_cov(s2, rho, nu, setup)
  sig <- setup$K %*% mat %*% t(setup$K)
  -dmvnorm(c(y_block), sigma=sig, log = TRUE)
}

get_likelihood_2_param_optim <- function(log_params, y_block, setup, nu) {
  s2 <- exp(log_params[[1]])
  rho <- exp(log_params[[2]])
  mat <- mk_cov(s2, rho, nu, setup)
  sig <- setup$K %*% mat %*% t(setup$K)
  -dmvnorm(c(y_block), sigma=sig, log=TRUE)
}

get_likelihood_3_param_optim <- function(log_params, y_block, setup) {
  s2 <- exp(log_params[[1]])
  rho <- exp(log_params[[2]])
  nu <- exp(log_params[[3]])
  mat <- mk_cov(s2, rho, nu, setup)
  sig <- setup$K %*% mat %*% t(setup$K)
  -dmvnorm(c(y_block), sigma=sig, log=TRUE)
}

# Param list: Sigma2, Rho, Nu
mvnorm_lik <- function(log_params, y_block, setup, param_list = c(0, 0, 0)) {
  # param_list[[x]] != 0 means it is a static value and should not be included in root finding
  # See if we should include Sigma2
  index <- 1

  # Handle s2
  if (param_list[[1]] != 0) {
    s2 <- param_list[[1]]
  } else {
    s2 <- exp(log_params[index])
    index <- index + 1
  }

  # Handle rho
  if (param_list[[2]] != 0) {
    rho <- param_list[[2]]
  } else {
    rho <- exp(log_params[index])
    index <- index + 1
  }

  # Handle nu
  if (param_list[[3]] != 0) {
    nu <- param_list[[3]]
  } else {
    nu <- exp(log_params[index])
    index <- index + 1
  }
  
  mat <- mk_cov(s2, rho, nu, setup)
  sig <- setup$K %*% mat %*% t(setup$K)
  -dmvnorm(c(y_block), sigma=sig, log=TRUE)
}