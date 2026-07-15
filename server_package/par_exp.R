#### Question ####
# How can I parallelize this to make it operate more quickly?

source('funcs.R')
library(mvtnorm)
library(ggplot2)
library(patchwork)
library(parallel)

progress_file <- "progress_log.txt"
cat(sprintf("Progress log: %s\n", progress_file))

# Initialize dimensions
true_s2 <- c(1.5, 3, 5)
true_rho <- c(1, 2, 4)
true_nu <- c(3/2, 5/2, 4)
true_vals <- expand.grid(true_s2, true_rho, true_nu)

# Each sim: generate y_true, then run all 9 dimension setups sequentially.
# Returns a matrix with 9 rows (one per aerial_dim).
run_one_sim <- function(j, sig_true, sim_true_s2, sim_true_rho, sim_true_nu) {
  cat(sprintf("[Sim %d] Started.\n", j), file = progress_file, append = TRUE)
  
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
      sim_true_s2, sim_true_rho, sim_true_nu,
      exp(val$par[1]), exp(val$par[2]), exp(val$par[3])
    )
    cat(sprintf("[Sim %d] Dim %d/%d (A=%d) done in %.1f secs.\n", j, i, length(aerial_dims), A, sim_duration),
        file = progress_file, append = TRUE)
  }
  cat(sprintf("[Sim %d] Complete.\n", j), file = progress_file, append = TRUE)
  sim_rows
}

one_true_val_test <- function(true_vals) {
  # True Data
  sim_true_s2 <- true_vals[[1]]
  sim_true_rho <- true_vals[[2]]
  sim_true_nu <- true_vals[[3]]
  setup_true <- setup(48, 1)
  sig_true <- mk_cov(sim_true_s2, sim_true_rho, sim_true_nu, setup_true)

  # Initial parameters for root finding: s2, rho, nu
  log_params <- log(c(1, 3, 1))

  # Initialize dimensions to test
  aerial_dims <- c(2, 3, 4, 6, 8, 12, 16, 24, 48)

  overall_start_time <- Sys.time()

  n_sim <- 50
  n_cores <- detectCores() - 1

  # Parallelize over simulations; sig_true is shared read-only via fork (copy-on-write)
  # Socket cluster avoids macOS fork+BLAS crash (mclapply returns NULL on macOS with Accelerate)
  cl <- makeCluster(n_cores)
  clusterSetRNGStream(cl, 1337)
  clusterExport(cl, c("sig_true", "aerial_dims", "log_params", "sim_true_s2", "sim_true_rho", "sim_true_nu", "progress_file"), envir = environment())
  clusterEvalQ(cl, { source("funcs.R"); library(mvtnorm) })

  results_list <- parLapply(cl, 1:n_sim, run_one_sim, 
    sig_true = sig_true, sim_true_s2 = sim_true_s2,
    sim_true_rho = sim_true_rho, sim_true_nu = sim_true_nu)
  stopCluster(cl)
  results_list

  cat(sprintf("[True Values: %g, %g, %g] Complete.\n", sim_true_s2, sim_true_rho, sim_true_nu), file = progress_file, append = TRUE)
}

final_list <- apply(true_vals, 1, one_true_val_test)

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