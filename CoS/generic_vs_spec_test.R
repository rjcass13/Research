source('funcs.R')
library(mvtnorm)
library(ggplot2)

# Initialize dimensions
s2 <- 1.5
rho <- 2
nu <- 3/2
aerial_blocks <- 3
lat_per_dim <- 3
tot_lat_len <- aerial_blocks * lat_per_dim

# Generate sample data
test <- setup(aerial_blocks, lat_per_dim)
Sig = mk_cov(s2, rho, nu, test)
yobs <- rmvnorm(1, mean = rep(0, tot_lat_len^2), sigma = Sig)
y_block = test$K%*%t(yobs)[, 1]


# Initial parameters: s2, rho, nu
log_params <- log(c(1, 2, 1))

n_sim <- 500
res <- matrix(NA, nrow = n_sim, ncol = 3)
pb <- txtProgressBar(min = 1, max = n_sim, style = 3)
start_time <- Sys.time()
for (i in 1:n_sim) {
  yobs <- rmvnorm(1, mean = rep(0, tot_lat_len^2), sigma = Sig)
  y_block_sim <- test$K %*% t(yobs)
  
  val <- optim(log_params, get_likelihood_3_param_optim, y_block = y_block_sim, setup = test)
  res[i,] <- exp(val$par)
  setTxtProgressBar(pb, i)
}
end_time <- Sys.time()
close(pb)

spef_duration <- end_time - start_time


res <- matrix(NA, nrow = n_sim, ncol = 3)
pb <- txtProgressBar(min = 1, max = n_sim, style = 3)
start_time <- Sys.time()
for (i in 1:n_sim) {
  yobs <- rmvnorm(1, mean = rep(0, tot_lat_len^2), sigma = Sig)
  y_block_sim <- test$K %*% t(yobs)
  
  val <- optim(log_params, mvnorm_lik, y_block = y_block_sim, setup = test, param_list = c(0,0,0))
  res[i,] <- exp(val$par)
  setTxtProgressBar(pb, i)
}
end_time <- Sys.time()
close(pb)

gen_duration <- end_time - start_time

print(spef_duration)
print(gen_duration)