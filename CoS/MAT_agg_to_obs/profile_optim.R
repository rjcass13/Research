# Profile a single optim call to identify the dominant cost in mvnorm_lik.
# Change A to compare different aerial dimension setups:
#   A = 48 → 1 latent pt per cell  (largest K %*% mat multiply)
#   A =  8 → 6 latent pts per cell (mid-range)
#   A =  2 → 24 latent pts per cell (smallest aerial, but still full l_D)

source("funcs.R")
library(mvtnorm)
library(profvis)

max_dim    <- 48
log_params <- log(c(1, 3, 1))
l_D        <- calc_latent_distance(max_dim, max_dim, 1, 1)

A <- 2        # <-- change this to 8 or 2 to compare
L <- max_dim / A
K <- gen_K(A, A, L, L)

set.seed(5927)
y_obs <- rnorm(A^2)

profvis({
  optim(
    log_params, mvnorm_lik,
    method  = "L-BFGS-B",
    lower   = c(-7, -7, -7),
    upper   = c(3,   3,  3),
    y_block = y_obs,
    K       = K,
    l_D     = l_D,
    param_list = c(0, 0, 0),
    control = list(maxit = 10)
  )
})

  optim(
    log_params, mvnorm_lik,
    method  = "L-BFGS-B",
    lower   = c(-7, -7, -7),
    upper   = c(3,   3,  3),
    y_block = y_obs,
    K       = K,
    l_D     = l_D,
    param_list = c(0, 0, 0),
    control = list(maxit = 10)
  )

  print(Sys.time())




source("CoS/true_val_exp/funcs.R")

max_dim    <- 48
log_params <- log(c(1, 3, 1))
l_D        <- calc_latent_distance(max_dim, max_dim, 1, 1)

A <- 2
L <- max_dim / A
K <- gen_K(A, A, L, L)

s2 <- 1; rho <- 3; nu <- 1

cat("Matrix size (l_D):", nrow(l_D), "x", ncol(l_D), "\n")
cat("K size:           ", nrow(K), "x", ncol(K), "\n\n")

cat("mk_cov (besselK on 2304x2304):\n")
print(system.time(mat <- mk_cov(s2, rho, nu, l_D)))

cat("\nK %*% mat:\n")
print(system.time(Kmat <- K %*% mat))

cat("\ntcrossprod:\n")
print(system.time(sig <- tcrossprod(Kmat, K)))

cat("\nchol:\n")
print(system.time(R <- chol(sig)))