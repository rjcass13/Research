source('funcs.R')
library(mvtnorm)
library(ggplot2)
library(patchwork)
library(parallel)

progress_file <- "progress_log.txt"
# Clear the progress log at the start of each run
cat("", file = progress_file)  # or writeLines(character(0), progress_file)
cat(sprintf("Progress log: %s\n", progress_file))


# Gen data, then use image.plot to make sure it looks like spatial data
# image.plot(matrix(y_obs, 48, 48), zlim = c(-3, 3))

# Number of sims per Covariance Matrix
n_sim <- 50

# Initial parameters for root finding: s2, rho, nu
log_params <- log(c(1, 3, 1))

# Initialize dimensions to test
max_dim <- 48
aerial_dims <- (2:max_dim)[max_dim %% (2:max_dim) == 0]
latent_dims <- max_dim/aerial_dims
l_D <- calc_latent_distance(max_dim, max_dim, 1, 1)
K_vec <- mapply(function(A, L) gen_K(A, A, L, L), aerial_dims, latent_dims, SIMPLIFY = FALSE)

overall_start_time <- Sys.time()

# Socket cluster avoids macOS fork+BLAS crash (mclapply returns NULL on macOS with Accelerate)
n_cores <- detectCores() - 1
cl <- makeCluster(n_cores)
clusterSetRNGStream(cl, 1337)
clusterEvalQ(cl, { source("funcs.R"); library(mvtnorm) })

# Run the simulation
iCAR_test <- test_sig_gen_agg(sig_name = 'iCAR',
  progress_file = progress_file, n_sim = n_sim, log_params = log_params,
  aerial_dims = aerial_dims, latent_dims = latent_dims,
  K_vec = K_vec, l_D = l_D, cl = cl)
iCAR_test <- t(as.data.frame(do.call(rbind, iCAR_test)))
car95_test <- test_sig_gen_agg(sig_name = 'car95',
  progress_file = progress_file, n_sim = n_sim, log_params = log_params,
  aerial_dims = aerial_dims, latent_dims = latent_dims,
  K_vec = K_vec, l_D = l_D, cl = cl)
car95_test <- t(as.data.frame(do.call(rbind, car95_test)))
car75_test <- test_sig_gen_agg(sig_name = 'car75',
  progress_file = progress_file, n_sim = n_sim, log_params = log_params,
  aerial_dims = aerial_dims, latent_dims = latent_dims,
  K_vec = K_vec, l_D = l_D, cl = cl)
car75_test <- t(as.data.frame(do.call(rbind, car75_test)))
final_list <- rbind(iCAR_test, car95_test, car75_test)

# Close Cluster
stopCluster(cl)

# Save the data
colnames(final_list) <- c(
  "Duration", "TotalPoints", "A_W", "A_H", "L_W", "L_H",
  "true_s2", "true_rho", "true_nu", "est_s2", "est_rho", "est_nu"
)
write.csv(final_list, "square_48_sims_1_new.csv", row.names = FALSE)

# Get and record final runtimes
overall_end_time <- Sys.time()
total_elapsed_time <- overall_end_time - overall_start_time
cat(sprintf("Total Elapsed Time: %s\n", total_elapsed_time), file = progress_file, append = TRUE)
cat(sprintf("End Time: %s\n", format(Sys.time(), "%H:%M:%S")), file = progress_file, append = TRUE)
