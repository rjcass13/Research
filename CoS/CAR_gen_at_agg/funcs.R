#library(rSPDE)
library(mvtnorm)
library(ggplot2)
library(parallel)

calc_latent_distance <- function(A_W, A_H, L_W, L_H) {
  full_L_W <- A_W * L_W
  full_L_H <- A_H * L_H
  l_pts = cbind(rep(1:full_L_W,full_L_H),rep(1:full_L_H,each=full_L_W)) # Latent Grid

  # 2. Normalize coordinates independently to a 0-1 scale
  l_pts_norm = l_pts
  l_pts_norm[,1] = (l_pts[,1] - 1) / (full_L_W - 1) # Normalize X to [0,1]
  l_pts_norm[,2] = (l_pts[,2] - 1) / (full_L_H - 1) # Normalize Y to [0,1]
  
  l_D = as.matrix(dist(l_pts_norm)) # Find the distances. Defaults to Euclidean
  l_D
}

gen_K <- function(A_W, A_H, L_W, L_H){
  full_L_W <- A_W * L_W
  full_L_H <- A_H * L_H
  l_pts = cbind(rep(1:full_L_W,full_L_H),rep(1:full_L_H,each=full_L_W)) # Latent Grid
  a_pts = cbind(rep(1:A_W,A_H),rep(1:A_H,each=A_W)) # Aerial Grid

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

  return(K)
}

# Matern Covariance Function: https://en.wikipedia.org/wiki/Mat%C3%A9rn_covariance_function
mk_cov <- function(s2,rho,nu,l_D){
  cov <- s2 * 2^(1-nu) / gamma(nu) * (sqrt(2*nu) * l_D / rho)^nu * besselK(sqrt(2*nu) * l_D/rho, nu)
  diag(cov) <- s2
  cov
  #kappa <- sqrt(2*nu)/rho
  # Function reference: https://search.r-project.org/CRAN/refmans/rSPDE/html/matern.covariance.html
  #matern.covariance(d, kappa, nu, sqrt(s2))
}

# Param list: Sigma2, Rho, Nu
mvnorm_lik <- function(log_params, y_block, l_D, K, param_list = c(0, 0, 0)) {
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
  
  mat <- mk_cov(s2, rho, nu, l_D)
  sig <- tcrossprod(K %*% mat, K)
  
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

test_true_vals <- function(true_vals, n_sim, progress_file, log_params, aerial_dims, latent_dims, K_vec, l_D, cl) {
  # True Data
  sim_true_s2 <- true_vals[[1]]
  sim_true_rho <- true_vals[[2]]
  sim_true_nu <- true_vals[[3]]
  sig_true <- mk_cov(sim_true_s2, sim_true_rho, sim_true_nu, l_D)
  chol_true <- chol(sig_true)

  cat(sprintf("[True Values: %g, %g, %g] Started.\n", sim_true_s2, sim_true_rho, sim_true_nu), file = progress_file, append = TRUE)
  
  clusterExport(cl, c("sig_true", "chol_true"),
                envir = environment())

  results_list <- parLapply(cl, 1:n_sim, run_one_sim, 
    aerial_dims = aerial_dims, latent_dims = latent_dims, 
    K_vec = K_vec, l_D = l_D, log_params = log_params,
    chol_true = chol_true, sim_true_s2 = sim_true_s2,
    sim_true_rho = sim_true_rho, sim_true_nu = sim_true_nu,
    progress_file = progress_file)
  # Combine: each list element is a 9-row matrix; rbind all 50 → 450 rows
  res <- as.data.frame(do.call(rbind, results_list))

  cat(sprintf("[True Values: %g, %g, %g] Complete.\n", sim_true_s2, sim_true_rho, sim_true_nu), file = progress_file, append = TRUE)
  res
}

# Each sim: generate y_true, then run all 9 dimension setups sequentially.
# Returns a matrix with 9 rows (one per aerial_dim).
run_one_sim <- function(j, aerial_dims, latent_dims, K_vec, l_D, log_params, chol_true, sim_true_s2, sim_true_rho, sim_true_nu, progress_file) {
  
  cat(sprintf("[Sim %d] Started.\n", j), file = progress_file, append = TRUE)
  
  #y_true <- rmvnorm(1, mean = rep(0, 48 * 48), sigma = sig_true)
  y_true <- as.vector(crossprod(chol_true, rnorm(nrow(chol_true))))
  #t_y_true <- t(y_true)

  sim_rows <- matrix(NA, nrow = length(aerial_dims), ncol = 12)
  for (i in seq_along(aerial_dims)) {
    A <- aerial_dims[i]
    L <- latent_dims[i]
    max_dim <- A*L
    K <- K_vec[[i]]

    #y_obs <- K %*% t_y_true
    y_obs <- K %*% y_true

    start_time <- Sys.time()
    val <- optim(
      log_params, mvnorm_lik,
      method = "L-BFGS-B", lower = c(-7, -7, -7), upper = c(3, 3, 3),
      y_block = y_obs, K = K, l_D = l_D, param_list = c(0, 0, 0)
    )
    sim_duration <- as.numeric(Sys.time() - start_time, units = "secs")

    # Duration, TotalPoints, A_W, A_H, L_W, L_H, true_s2, true_rho, true_nu, est_s2, est_rho, est_nu
    sim_rows[i, ] <- c(
      sim_duration, max_dim^2, A, A, L, L,
      sim_true_s2, sim_true_rho, sim_true_nu,
      exp(val$par[1]), exp(val$par[2]), exp(val$par[3])
    )
    cat(sprintf("[Sim %d] Dim %d/%d (A=%d) done in %.1f secs.\n", j, i, length(aerial_dims), A, sim_duration),
        file = progress_file, append = TRUE)
  }
  cat(sprintf("[Sim %d] Complete.\n", j), file = progress_file, append = TRUE)
  sim_rows
}

make_histogram <- function(df, value, true_val, bins = 30) {
  ggplot(df, aes(x = vals)) +
    geom_histogram(aes(y = after_stat(count / sum(count))), fill = "steelblue", color = "white", bins = bins) +
    geom_vline(xintercept = true_val, col = 'red') + 
    theme_minimal() +
    labs(title = value, x = 'Parameter Estimate', y = 'Frequency')
}

##########################
# Type of Sig Test Funcs #
##########################

# Adjacency matrix
mkgrid = function(size){
  grid = t(matrix(1:size^2,size,size))
  ind = numeric()
  for(i in 1:size){
    for(j in 1:size){
      if(j < size) ind = rbind(ind,c(grid[i,j],grid[i,j+1]))
      if(i < size) ind = rbind(ind,c(grid[i,j],grid[i+1,j]))
    }
  }
  A = matrix(0,size^2,size^2)
  for(i in 1:dim(ind)[1]){
    A[ind[i,,drop=F]] = 1
  }
  A=A+t(A)
  return(list(A=A,ind=ind))
}

# W is the adjancency matrix
# n is num samples (typically 1)
# sig is sd
# try it with different values of sig^2
ricar = function(n,W,sig){
  iSig = (diag(apply(W,2,sum)) - W)
  eiS = eigen(iSig)
  iS_aux = eiS$vectors[,order(eiS$values)]
  D_aux = sort(eiS$values)
  Zs = matrix(0,dim(W)[1],n)
  for(i in 1:n) Zs[-1,i]=rnorm(dim(W)[1]-1,0,sqrt(1/D_aux[-1]))
  return(sig*(iS_aux%*%Zs))
}

test_sig <- function(sig_name, sig_true, n_sim, progress_file, log_params, aerial_dims, latent_dims, K_vec, l_D, cl) {
  chol_true <- chol(sig_true)

  cat(sprintf("[Covariance: %s] Started.\n", sig_name), file = progress_file, append = TRUE)
  
  clusterExport(cl, c("sig_true", "chol_true"),
                envir = environment())

  results_list <- parLapply(cl, 1:n_sim, run_one_sim, 
    aerial_dims = aerial_dims, latent_dims = latent_dims, 
    K_vec = K_vec, l_D = l_D, log_params = log_params,
    chol_true = chol_true, sim_true_s2 = sig_name,
    sim_true_rho = sig_name, sim_true_nu = sig_name,
    progress_file = progress_file)
  # Combine: each list element is a 9-row matrix; rbind all 50 → 450 rows
  res <- as.data.frame(do.call(rbind, results_list))

  cat(sprintf("[Covariance: %s] Complete.\n", sig_name), file = progress_file, append = TRUE)
  res
}

run_one_sim_agg <- function(j, sig_name, aerial_dims, latent_dims, K_vec, l_D, log_params, sim_true_s2, sim_true_rho, sim_true_nu, progress_file) {

  cat(sprintf("[Sim %d] Started.\n", j), file = progress_file, append = TRUE)
  
  sim_rows <- matrix(NA, nrow = length(aerial_dims), ncol = 12)
  for (i in seq_along(aerial_dims)) {
    A <- aerial_dims[i]
    L <- latent_dims[i]
    max_dim <- A*L
    K <- K_vec[[i]]

    # Generate adjacency structure for 48x48: matrix of 1/0 for whatever is next to it
    W <- mkgrid(A)$A

    if (sig_name == 'iCAR') {
      y_true <- as.vector(ricar(1, W, 1))
    } else if (sig_name == 'car75') {
      sig <- 1
      # Precision matrix Q = (D - 0.75*W) / sig^2; proper CAR (PD)
      Q <- (diag(apply(W, 2, sum)) - .75 * W) / sig^2
      # backsolve(chol(Q), z) ~ N(0, Q^{-1})
      R <- chol(Q)
      y_true <- backsolve(R, rnorm(nrow(R)))
    } else if (sig_name == 'car95') {
      sig <- 1
      # Precision matrix Q = (D - 0.95*W) / sig^2; proper CAR (PD)
      Q <- (diag(apply(W, 2, sum)) - .95 * W) / sig^2
      # backsolve(chol(Q), z) ~ N(0, Q^{-1})
      R <- chol(Q)
      y_true <- backsolve(R, rnorm(nrow(R)))
    }
    
    #y_obs <- K %*% t_y_true
    y_obs <- y_true

    start_time <- Sys.time()
    val <- optim(
      log_params, mvnorm_lik,
      method = "L-BFGS-B", lower = c(-7, -7, -7), upper = c(3, 3, 3),
      y_block = y_obs, K = K, l_D = l_D, param_list = c(0, 0, 0)
    )
    sim_duration <- as.numeric(Sys.time() - start_time, units = "secs")

    # Duration, TotalPoints, A_W, A_H, L_W, L_H, true_s2, true_rho, true_nu, est_s2, est_rho, est_nu
    sim_rows[i, ] <- c(
      sim_duration, max_dim^2, A, A, L, L,
      sim_true_s2, sim_true_rho, sim_true_nu,
      exp(val$par[1]), exp(val$par[2]), exp(val$par[3])
    )
    cat(sprintf("[Sim %d] Dim %d/%d (A=%d) done in %.1f secs.\n", j, i, length(aerial_dims), A, sim_duration),
        file = progress_file, append = TRUE)
  }
  cat(sprintf("[Sim %d] Complete.\n", j), file = progress_file, append = TRUE)
  sim_rows
}

test_sig_gen_agg <- function(sig_name, n_sim, progress_file, log_params, aerial_dims, latent_dims, K_vec, l_D, cl) {

  cat(sprintf("[Covariance: %s] Started.\n", sig_name), file = progress_file, append = TRUE)

  results_list <- parLapply(cl, 1:n_sim, run_one_sim_agg, 
    sig_name = sig_name, aerial_dims = aerial_dims, latent_dims = latent_dims, 
    K_vec = K_vec, l_D = l_D, log_params = log_params,
    sim_true_s2 = sig_name, sim_true_rho = sig_name, sim_true_nu = sig_name,
    progress_file = progress_file)
  # Combine: each list element is a 9-row matrix; rbind all 50 → 450 rows
  res <- as.data.frame(do.call(rbind, results_list))

  cat(sprintf("[Covariance: %s] Complete.\n", sig_name), file = progress_file, append = TRUE)
  res
}