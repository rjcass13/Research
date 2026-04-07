source('funcs.R')
library(mvtnorm)
library(ggplot2)

# Initialize dimensions
s2 <- 1.5
rho <- 2
nu <- 3/2

# Initial parameters: s2, rho, nu
log_params <- log(c(1, 2, 1))

aerial_blocks <- seq(1, 10, by = 1)
lat_per_dim <- 3

overall_start_time <- Sys.time()

n_sim <- 10
times <- numeric(length(aerial_blocks))
pars <- matrix(NA, nrow = length(aerial_blocks), ncol = 3)
for (i in 1:length(aerial_blocks)) {
  start_time <- Sys.time()
  tot_lat_len <- aerial_blocks[[i]] * lat_per_dim
  # Generate sample data
  test <- setup(aerial_blocks[[i]], lat_per_dim)
  Sig = mk_cov(s2, rho, nu, test)
  yobs <- rmvnorm(1, mean = rep(0, tot_lat_len^2), sigma = Sig)
  y_block = test$K%*%t(yobs)[, 1]
  
  par_est <- matrix(NA, nrow = n_sim, ncol = 3)
  for (j in 1:n_sim) {
    curr_elapsed_time <- round(as.numeric(Sys.time() - overall_start_time, units = 'secs'))
    cat(sprintf("\rCurrent Step | Block: %d/%d | Sim: %d/%d | ElapsedTime: %.0f secs", i, length(aerial_blocks), j, n_sim, curr_elapsed_time))
    flush.console() # Forces the text to update immediately
    yobs <- rmvnorm(1, mean = rep(0, tot_lat_len^2), sigma = Sig)
    y_block_sim <- test$K %*% t(yobs)
    
    val <- optim(log_params, mvnorm_lik, y_block = y_block_sim, setup = test, param_list = c(0,0,0))
    par_est[j,] <- exp(val$par)
  }

  end_time <- Sys.time()
  times[i] <- as.numeric(end_time - start_time, units = 'secs')
  pars[i, ] <- apply(par_est, 2, mean)
}

overall_end_time <- Sys.time()

total_points <- (aerial_blocks*lat_per_dim)^2
plot(total_points, times, ylab = 'Times')
plot(total_points, pars[, 1], ylab = 'MLE s2', ylim = c(0, 5))
plot(total_points, pars[, 2], ylab = 'MLE rho', ylim = c(0, 5))
plot(total_points, pars[, 3], ylab = 'MLE nu', ylim = c(0, 5))

total_elapsed_time <- overall_end_time - overall_start_time
print(total_elapsed_time)

