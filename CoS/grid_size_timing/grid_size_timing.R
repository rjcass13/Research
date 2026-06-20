# Want to see how long it takes to process different grid sizes/densities
# Only applies to square grids
source('funcs.R')
library(mvtnorm)
library(ggplot2)
library(patchwork)

set.seed(1337)

# Initialize dimensions
true_s2 <- 1.5
true_rho <- 2
true_nu <- 3/2
# Initial parameters for root finding: s2, rho, nu
log_params <- log(c(1, 2, 1))


# Initialize grids
dims <- read.csv('possible_dims_576.csv')

# aerial_width <-  c(2, 3, 4, 6, 8, 12, 24)
# aerial_height <- c(2, 3, 4, 6, 8, 12, 24)
# lat_width <-    c(12, 8, 6, 4, 3, 2, 1)
# lat_height <-   c(12, 8, 6, 4, 3, 2, 1)
# dims <- cbind(aerial_width, aerial_height, lat_width, lat_height)

#aerial_width <- seq(2, 6, by = 1)
#aerial_height <- seq(2, 6, by = 1)
#lat_width <- seq(1, 6, by = 1)
#lat_height <- seq(1, 6, by = 1)
#dims <- expand.grid(aerial_width, aerial_height, lat_width, lat_height)
overall_start_time <- Sys.time()

n_sim <- 10
# Duration, TotalPoints, A_W, A_H, L_W, L_H, true_s2, true_rho, true_nu, est_s2, est_rho, est_nu
res <- matrix(NA, nrow = nrow(dims)*n_sim, ncol = 12)

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
  #yobs <- rmvnorm(1, mean = rep(0, tot_lat_W * tot_lat_H), sigma = Sig)
  #y_block = test$K%*%t(yobs)[, 1]
  
  for (j in 1:n_sim) {
    sim_time_start <- Sys.time()
    curr_elapsed_time <- round(as.numeric(Sys.time() - overall_start_time, units = 'secs'))
    cat(sprintf("\rCurrent Step | Block: %d/%d | Sim: %d/%d | ElapsedTime: %.0f secs.  ", i, nrow(dims), j, n_sim, curr_elapsed_time))
    flush.console() # Forces the text to update immediately
    yobs <- rmvnorm(1, mean = rep(0, tot_lat_W * tot_lat_H), sigma = Sig)
    y_block_sim <- test$K %*% t(yobs)
    
    val <- optim(log_params, mvnorm_lik, method = "L-BFGS-B", lower = c(-7, -7, -7), upper = c(3, 3, 3), y_block = y_block_sim, setup = test, param_list = c(0,0,0))
    sim_time_end <- Sys.time()
    sim_duration <- as.numeric(sim_time_end - sim_time_start, units = 'secs')
    
    # Duration, TotalPoints, A_W, A_H, L_W, L_H, true_s2, true_rho, true_nu, est_s2, est_rho, est_nu
    res[j + n_sim*(i-1), ] <- c(sim_duration, A_W*A_H*L_W*L_H, A_W, A_H, L_W, L_H, 
      true_s2, true_rho, true_nu, exp(val$par[1]), exp(val$par[2]), exp(val$par[3]))
  }

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
write.csv(res, 'sim_results_576_possible_dims_10_sims.csv', row.names = FALSE)




