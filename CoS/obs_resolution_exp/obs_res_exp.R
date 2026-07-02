#### Question ####
# Does the ability to estimate parameters change depending on the resolution of the aerial grid?
# ie. Given the same true underlying data, if we aggregate the Observed to different levels, 
  # how does that affect prediction

#### Notes ####
# Only care about square grids
# Will use a grid dimensionality that provides lots of factors (48x48)

source('funcs.R')
library(mvtnorm)
library(ggplot2)
library(patchwork)

set.seed(1337)

# Initialize dimensions
true_s2 <- 1.5
true_rho <- 2
true_nu <- 3/2
# True Data
setup_true <- setup(48, 1)
sig_true <- mk_cov(true_s2, true_rho, true_nu, setup_true)


# Initial parameters for root finding: s2, rho, nu
log_params <- log(c(1, 3, 1))

# Initialize dimensions to test
aerial_dims <-  c(2, 3, 4, 6, 8, 12, 16, 24, 48)

overall_start_time <- Sys.time()

n_sim <- 50
# Duration, TotalPoints, A_W, A_H, L_W, L_H, true_s2, true_rho, true_nu, est_s2, est_rho, est_nu
res <- matrix(NA, nrow = length(aerial_dims)*n_sim, ncol = 12)

for (j in 1:n_sim) {
  # Generate true data for this sim
  y_true <- rmvnorm(1, mean = rep(0, 48 * 48), sigma = sig_true)

  for (i in 1:length(aerial_dims)) {
    A <- aerial_dims[i]
    L <- 48/A
    start_time <- Sys.time()
    tot_W <- 48
    tot_H <- 48
    # Determine Aggregate Date
    dim_setup <- setup(A, L)
    y_obs <- dim_setup$K %*% t(y_true)
  
    sim_time_start <- Sys.time()
    curr_elapsed_time <- round(as.numeric(Sys.time() - overall_start_time, units = 'secs'))
    cat(sprintf("\rCurrent Step | Block: %d/%d | Sim: %d/%d | ElapsedTime: %.0f secs.  ", i, length(aerial_dims), j, n_sim, curr_elapsed_time))
    flush.console() # Forces the text to update immediately
    
    val <- optim(log_params, mvnorm_lik, method = "L-BFGS-B", lower = c(-7, -7, -7), upper = c(3, 3, 3), y_block = y_obs, setup = dim_setup, param_list = c(0,0,0))
    sim_time_end <- Sys.time()
    sim_duration <- as.numeric(sim_time_end - sim_time_start, units = 'secs')
    
    # Duration, TotalPoints, A_W, A_H, L_W, L_H, true_s2, true_rho, true_nu, est_s2, est_rho, est_nu
    res[j + n_sim*(i-1), ] <- c(sim_duration, 48*48, A, A, L, L, 
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
write.csv(res, 'square_48_sims_50.csv', row.names = FALSE)




