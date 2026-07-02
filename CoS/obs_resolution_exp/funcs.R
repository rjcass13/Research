#library(rSPDE)
library(mvtnorm)
library(ggplot2)

setup <- function(a_res,c){
  l_res <- a_res*c # Total Latent length of one side
  l_pts <- cbind(rep(1:l_res,l_res),rep(1:l_res,each=l_res)) # Latent Grid
  a_pts <- cbind(rep(1:a_res,a_res),rep(1:a_res,each=a_res)) # Aerial Grid
  
  l_D <- as.matrix(dist(l_pts))/(l_res-1) # Find the distances. Defaults to Euclidean

    #l_D = as.matrix(dist(l_pts))
  # K is basically an indicator matrix showing which indices of the latent grid correspond to which index of the aerial grid
  K = matrix(0, a_res^2,l_res^2)
  for(i in 1:a_res^2){
    K[i,] <- l_pts[,1] >= a_pts[i,1]*c-(c-1) & l_pts[,1] <= a_pts[i,1]*c & 
      l_pts[,2] >= a_pts[i,2]*c-(c-1) & l_pts[,2] <= a_pts[i,2]*c
  }
  K <- K/c^2

  return(list(K=K,l_D=l_D,a_pts=a_pts,l_pts=l_pts))
}


setup_rect <- function(A_W, A_H, L_W, L_H){
  full_L_W <- A_W * L_W
  full_L_H <- A_H * L_H
  l_pts = cbind(rep(1:full_L_W,full_L_H),rep(1:full_L_H,each=full_L_W)) # Latent Grid
  a_pts = cbind(rep(1:A_W,A_H),rep(1:A_H,each=A_W)) # Aerial Grid

  # 2. Normalize coordinates independently to a 0-1 scale
  l_pts_norm = l_pts
  l_pts_norm[,1] = (l_pts[,1] - 1) / (full_L_W - 1) # Normalize X to [0,1]
  l_pts_norm[,2] = (l_pts[,2] - 1) / (full_L_H - 1) # Normalize Y to [0,1]
  
  l_D = as.matrix(dist(l_pts_norm)) # Find the distances. Defaults to Euclidean

    #l_D = as.matrix(dist(l_pts))
  # K is basically an indicator matrix showing which indices of the latent grid correspond to which index of the aerial grid
  K = matrix(0, (A_W*A_H), (full_L_W * full_L_H))
  for(i in 1:(A_W*A_H)){
    # X bounds using width scaling (c_w)
    x_min = a_pts[i,1] * L_W - (L_W - 1)
    x_max = a_pts[i,1] * L_W
    
    # Y bounds using height scaling (c_h)
    y_min = a_pts[i,2] * L_H - (L_H - 1)
    y_max = a_pts[i,2] * L_H
    
    # Logical check for inclusions
    K[i,] = (l_pts[,1] >= x_min & l_pts[,1] <= x_max & 
            l_pts[,2] >= y_min & l_pts[,2] <= y_max)
  }
  K = K/(L_W*L_H)

  return(list(K=K,l_D=l_D,a_pts=a_pts,l_pts=l_pts))
}

# Matern Covariance Function: https://en.wikipedia.org/wiki/Mat%C3%A9rn_covariance_function
mk_cov = function(s2,rho,nu,setup){
  d <- setup$l_D+diag(1e-9,nrow(setup$l_D)) # Distance between two points, pull from setup, include slight variance on diags for stability
  s2 * 2^(1-nu) / gamma(nu) * (sqrt(2*nu) * d / rho)^nu * besselK(sqrt(2*nu) * d/rho, nu)
  #kappa <- sqrt(2*nu)/rho
  # Function reference: https://search.r-project.org/CRAN/refmans/rSPDE/html/matern.covariance.html
  #matern.covariance(d, kappa, nu, sqrt(s2))
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
  
  # Fast Cholesky speedup with built-in safety
  R <- tryCatch(chol(sig), error = function(e) NULL)
  
  if (is.null(R)) {
    # GRADUATED PENALTY: Add a penalty proportional to how far out the params are
    # This prevents flat gradients and forces L-BFGS-B back into valid space
    return(10000 + sum(log_params^2)) 
  }
  
  # Fast manual calculation
  n <- length(y_block)
  scaled_y <- backsolve(R, c(y_block), transpose = TRUE)
  quad_form <- sum(scaled_y^2)
  log_det <- 2 * sum(log(diag(R)))
  
  return(0.5 * (n * log(2 * pi) + log_det + quad_form))
}

make_histogram <- function(df, value, true_val, bins = 30) {
  ggplot(df, aes(x = vals)) +
    geom_histogram(aes(y = after_stat(count / sum(count))), fill = "steelblue", color = "white", bins = bins) +
    geom_vline(xintercept = true_val, col = 'red') + 
    theme_minimal() +
    labs(title = value, x = 'Parameter Estimate', y = 'Frequency')
}

run_sim <- function(tot_lat_W, tot_lat_H, Sig, log_params, test, param_list) {
  sim_time_start <- Sys.time()
  yobs <- rmvnorm(1, mean = rep(0, tot_lat_W * tot_lat_H), sigma = Sig)
  y_block_sim <- test$K %*% t(yobs)
  
  val <- optim(log_params, mvnorm_lik, method = "L-BFGS-B", lower = c(-7, -7, -7), upper = c(3, 3, 3), y_block = y_block_sim, setup = test, param_list = c(0,0,0))
  sim_time_end <- Sys.time()
  sim_duration <- as.numeric(sim_time_end - sim_time_start, units = 'secs')
  
  # Duration, TotalPoints, A_W, A_H, L_W, L_H, true_s2, true_rho, true_nu, est_s2, est_rho, est_nu
  c(sim_duration, A_W*A_H*L_W*L_H, A_W, A_H, L_W, L_H, 
    true_s2, true_rho, true_nu, exp(val$par[1]), exp(val$par[2]), exp(val$par[3]))
}
