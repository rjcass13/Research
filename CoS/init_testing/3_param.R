# This code tests for Sigma2, Rho, and Nu

source('funcs.R')
library(mvtnorm)
library(ggplot2)

# Initialize dimensions
s2 <- 1.5
rho <- 2
nu <- 3/2
aerial_blocks <- 5
lat_per_dim <- 3
tot_lat_len <- aerial_blocks * lat_per_dim

# Generate sample data
test <- setup(aerial_blocks, lat_per_dim)
Sig = mk_cov(s2, rho, nu, test)
yobs <- rmvnorm(1, mean = rep(0, tot_lat_len^2), sigma = Sig)
y_block = test$K%*%t(yobs)[, 1]

# Initial parameters: s2, rho, nu
params <- log(c(1, 1.5, 1))

n_sim <- 100
res <- matrix(NA, nrow = n_sim, ncol = 3)
pb <- txtProgressBar(min = 1, max = n_sim, style = 3)
for (i in 1:n_sim) {
  yobs <- rmvnorm(1, mean = rep(0, tot_lat_len^2), sigma = Sig)
  y_block_sim <- test$K %*% t(yobs)
  
  val <- optim(params, get_likelihood_3_param_optim, y_block = y_block_sim, setup = test)
  res[i,] <- exp(val$par)
  setTxtProgressBar(pb, i)
}
close(pb)

par(mfrow = c(2,2))
hist(res[,1], main="Distribution of MLE s2", xlab="Estimated s2")
abline(v = s2, col="red", lwd=2) # True value

hist(res[,2], main="Distribution of MLE rho", xlab="Estimated rho")
abline(v = rho, col="red", lwd=2) # True value

hist(res[,3], main="Distribution of MLE nu", xlab="Estimated nu")
abline(v = nu, col="red", lwd=2) # True value
par(mfrow = c(1,1))

print(mean(res[,1]))
print(mean(res[,2]))
print(mean(res[,3]))
