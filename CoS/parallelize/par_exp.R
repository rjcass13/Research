#### Question ####
# How can I parallelize this to make it operate more quickly?

source('funcs.R')
library(mvtnorm)
library(ggplot2)
library(patchwork)
library(parallel)
library(pbapply)

progress_file <- tempfile()

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
aerial_dims <- c(2, 3, 4, 6, 8, 12, 16, 24, 48)

overall_start_time <- Sys.time()

n_sim <- 50
n_cores <- detectCores() - 1

# Each sim: generate y_true, then run all 9 dimension setups sequentially.
# Returns a matrix with 9 rows (one per aerial_dim).
run_one_sim <- function(j) {
  cat(sprintf("Began sim %d.\n", j), file = progress_file, append = TRUE)
  y_true <- rmvnorm(1, mean = rep(0, 48 * 48), sigma = sig_true)

  sim_rows <- matrix(NA, nrow = length(aerial_dims), ncol = 12)
  for (i in seq_along(aerial_dims)) {
    
    A <- aerial_dims[i]
    L <- 48 / A

    dim_setup <- setup(A, L)
    y_obs <- dim_setup$K %*% t(y_true)

    start_time <- Sys.time()
    val <- optim(
      log_params, mvnorm_lik,
      method = "L-BFGS-B", lower = c(-7, -7, -7), upper = c(3, 3, 3),
      y_block = y_obs, setup = dim_setup, param_list = c(0, 0, 0)
    )
    sim_duration <- as.numeric(Sys.time() - start_time, units = "secs")

    # Duration, TotalPoints, A_W, A_H, L_W, L_H, true_s2, true_rho, true_nu, est_s2, est_rho, est_nu
    sim_rows[i, ] <- c(
      sim_duration, 48 * 48, A, A, L, L,
      true_s2, true_rho, true_nu,
      exp(val$par[1]), exp(val$par[2]), exp(val$par[3])
    )

    cat(sprintf("Ended sim %d.\n", j), file = progress_file, append = TRUE)
  }
  sim_rows
}

# Parallelize over simulations; sig_true is shared read-only via fork (copy-on-write)
# Socket cluster avoids macOS fork+BLAS crash (mclapply returns NULL on macOS with Accelerate)
cl <- makeCluster(n_cores)
clusterSetRNGStream(cl, 1337)
clusterExport(cl, c("sig_true", "aerial_dims", "log_params", "true_s2", "true_rho", "true_nu"))
clusterEvalQ(cl, { source("funcs.R"); library(mvtnorm) })

results_list <- pblapply(1:n_sim, run_one_sim, cl = cl)
stopCluster(cl)

overall_end_time <- Sys.time()
total_elapsed_time <- overall_end_time - overall_start_time
print(total_elapsed_time)
print(Sys.time())

# Combine: each list element is a 9-row matrix; rbind all 50 → 450 rows
res <- as.data.frame(do.call(rbind, results_list))
colnames(res) <- c(
  "Duration", "TotalPoints", "A_W", "A_H", "L_W", "L_H",
  "true_s2", "true_rho", "true_nu", "est_s2", "est_rho", "est_nu"
)
write.csv(res, "square_48_sims_50.csv", row.names = FALSE)