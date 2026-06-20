# Want to see how long it takes to process different grid sizes/densities
# Only applies to square grids
source('funcs.R')
library(mvtnorm)
library(ggplot2)
library(patchwork)
library(parallel)

set.seed(1337)

# Initialize dimensions
true_s2 <- 1.5
true_rho <- 2
true_nu <- 3/2
# Initial parameters for root finding: s2, rho, nu
log_params <- log(c(1, 2, 1))

# Initialize grids
aerial_width <- seq(2, 6, by = 1)
aerial_height <- seq(2, 6, by = 1)
lat_width <- seq(1, 6, by = 1)
lat_height <- seq(1, 6, by = 1)
dims <- expand.grid(aerial_width, aerial_height, lat_width, lat_height)
overall_start_time <- Sys.time()

n_sim <- 10
iters <- 1:n_sim
# Duration, TotalPoints, A_W, A_H, L_W, L_H, true_s2, true_rho, true_nu, est_s2, est_rho, est_nu
res <- matrix(NA, nrow = nrow(dims)*n_sim, ncol = 12)

n_cores <- parallel::detectCores()


for (i in 1:nrow(dims)) {
  A_W <- dims[i,1]
  A_H <- dims[i,2]
  L_W <- dims[i,3]
  L_H <- dims[i,4]
  start_time <- Sys.time()
  tot_lat_W <- A_W * L_W
  tot_lat_H <- A_H * L_H
  # Generate sample data
  test <- setup_rect(A_W, A_H, L_W, L_H)
  Sig = mk_cov(true_s2, true_rho, true_nu, test)
  yobs <- rmvnorm(1, mean = rep(0, tot_lat_W * tot_lat_H), sigma = Sig)
  y_block = test$K%*%t(yobs)[, 1]
  
  res_list <- mclapply(iters, run_sim, tot_lat_W = tot_lat_W, tot_lat_H = tot_lat_H, 
    Sig = Sig, log_params = log_params, test = test,
    mc.cores = 1)
  # Convert the list to a matrix
  res_dim <- do.call(rbind, res_list)
  start_ind <- ((i-1)*n_sim)+1
  end_ind <- i*n_sim
  res[start_ind:end_ind, ] <- res_dim

  curr_elapsed_time <- round(as.numeric(Sys.time() - overall_start_time, units = 'secs'))
  cat(sprintf("\rCurrent Step | Block: %d/%d | ElapsedTime: %.0f secs.  ", i, nrow(dims), curr_elapsed_time))
  flush.console() # Forces the text to update immediately
  end_time <- Sys.time()
}

overall_end_time <- Sys.time()

total_elapsed_time <- overall_end_time - overall_start_time
print(total_elapsed_time)

print(Sys.time())

# Prep for, and write, all results
res <- as.data.frame(res)
colnames(res) <- c('Duration', 'TotalPoints', 'A_W', 'A_H', 'L_W', 'L_H', 
  'true_s2', 'true_rho', 'true_nu', 'est_s2', 'est_rho', 'est_nu')
write.csv(res, 'sim_results_6_6_6_6_10_sim_rect_parallel.csv', row.names = FALSE)




