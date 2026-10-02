source("~/GitRepos/Research/CoS/funcs.R")
library(mvtnorm)
library(ggplot2)
library(patchwork)
library(parallel)

progress_file <- "progress_log.txt"
# Clear the progress log at the start of each run
cat("", file = progress_file)  # or writeLines(character(0), progress_file)
cat(sprintf("Progress log: %s\n", progress_file))

# True Parameters
# Examine rerunning the simulation with sets of parameter values that characterize
# unique/identifiable patterns (ie. make sure I can always generate the dataset)
true_s2 <- c(1, 4, 9)
true_rho <- c(1/10, 1/3, 1)
true_nu <- c(1/2, 3/2, 5/2)
# true_s2 <- 1
# true_rho <- 1/10
# true_nu <- 1/2
true_vals <- expand.grid(true_s2, true_rho, true_nu)
# Number of sims per true parameter set
n_sim <- 50

# Initial parameters for root finding: s2, rho, nu
log_params <- log(c(1, 3, 1))

# Initialize dimensions to test
max_dim <- 24
aerial_dims <- (2:max_dim)[max_dim %% (2:max_dim) == 0]
latent_dims <- max_dim/aerial_dims
l_D <- calc_latent_distance(max_dim, max_dim, 1, 1)
K_vec <- mapply(function(A, L) gen_K(A, A, L, L), aerial_dims, latent_dims, SIMPLIFY = FALSE)

overall_start_time <- Sys.time()

# Socket cluster avoids macOS fork+BLAS crash (mclapply returns NULL on macOS with Accelerate)
n_cores <- detectCores() - 1
cl <- makeCluster(min(n_sim, n_cores))
cl_pids <- clusterEvalQ(cl, Sys.getpid())
clusterSetRNGStream(cl, 1337)
clusterEvalQ(cl, { source("~/GitRepos/Research/CoS/funcs.R"); library(mvtnorm); library(spaMM) })


# Run the simulation with checkpointing
checkpoint_file <- "checkpoint.rds"
if (file.exists(checkpoint_file)) {
  saved <- readRDS(checkpoint_file)
  final_list <- saved$results
  start_row <- saved$next_row
  cat(sprintf("Resuming from parameter set %d\n", start_row))
} else {
  final_list <- vector("list", nrow(true_vals))
  start_row <- 1
}

for (row_i in start_row:nrow(true_vals)) {
  cat(sprintf("Starting parameter set %d/%d\n", row_i, nrow(true_vals)))
  final_list[[row_i]] <- test_true_vals_geo(
    true_vals[row_i, ],
    progress_file = progress_file, n_sim = n_sim, log_params = log_params,
    aerial_dims = aerial_dims, latent_dims = latent_dims,
    K_vec = K_vec, l_D = l_D, cl = cl
  )
  # Save checkpoint and encourage GC
  saveRDS(list(results = final_list, next_row = row_i + 1), checkpoint_file)
  gc()
}
final_list <- as.data.frame(do.call(rbind, final_list))


# Close Cluster
on.exit({
  tryCatch(stopCluster(cl), error = function(e) tools::pskill(cl_pids))
}, add = TRUE)

# Save the data
colnames(final_list) <- c(
  "ManDuration", "GeoDuration", "TotalPoints", 
  "A_W", "A_H", "L_W", "L_H",
  "true_s2", "true_rho", "true_nu", 
  "est_s2_man", "est_rho_man", "est_nu_man",
  "est_s2_geo", "est_rho_geo", "est_nu_geo",
  "total_var"
)
write.csv(final_list, "square_48_sims_1_test.csv", row.names = FALSE)

# Get and record final runtimes
overall_end_time <- Sys.time()
total_elapsed_time <- overall_end_time - overall_start_time
cat(sprintf("Total Elapsed Time: %s\n", total_elapsed_time), file = progress_file, append = TRUE)
cat(sprintf("End Time: %s\n", format(Sys.time(), "%H:%M:%S")), file = progress_file, append = TRUE)


# troubleshoot nu = 4, rho = 4, see where it's breaking/why I wasn't getting error messages when running those sims

