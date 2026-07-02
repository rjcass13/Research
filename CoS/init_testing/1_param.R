# Ths code tests for Sigma2

source('funcs.R')
library(mvtnorm)

# Initialize dimensions
s2 <- 1.5
rho <- 2
nu <- 5/2
aerial_blocks <- 3
lat_per_dim <- 3
tot_lat_len <- aerial_blocks * lat_per_dim
test <- setup(aerial_blocks, lat_per_dim)
# Generate sample data
Sig = mk_cov(s2, rho, nu, test)
# cSig = chol(Sig)
# yobs = c(t(cSig)%*%matrix(rnorm(tot_lat_len^2),ncol=1))
# y_block = test$K%*%yobs/(tot_lat_len/2)
yobs <- rmvnorm(1, mean = rep(0, tot_lat_len^2), sigma = Sig)
mean(yobs)
#y_block = (test$K%*%t(yobs)/(tot_lat_len/2))[, 1]
y_block = test$K%*%t(yobs)[, 1]


# See if it will return the same parameters I start with
# Root find
res <- optimize(get_likelihood_1_param_optim, interval = c(-10, 1), y_block = y_block, setup = test, rho = rho, nu = nu)
# The likelihood roots over the log params, so convert to regular scale
exp(res$minimum)


s2_vec <- seq(.1, 3, length = 100)
lik <- mapply(get_likelihood_manual, s2 = s2_vec, MoreArgs = list(rho = rho, nu = nu, y_block = y_block, setup = test))
s2_vec[which.max(lik)]
plot(s2_vec, lik)

# Code from Gemini
# Test over 50 iterations to see the distribution of the MLE
mle_results <- replicate(300, {
  yobs <- rmvnorm(1, mean = rep(0, tot_lat_len^2), sigma = Sig)
  y_block_sim <- test$K %*% t(yobs)
  

  # Finding the peak
  lik_sim <- mapply(get_likelihood_manual, s2 = s2_vec, MoreArgs = list(rho = rho, nu = nu, y_block = y_block_sim, setup = test))
  s2_vec[which.max(lik_sim)]
})

hist(mle_results, main="Distribution of MLE s2", xlab="Estimated s2")
abline(v = 1.5, col="red", lwd=2) # True value
print(mean(mle_results))
