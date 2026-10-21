source("~/GitRepos/Research/CoS/funcs.R")
library(ggplot2)
library(patchwork)
library(pbapply)

###########################
######## Functions ########
###########################

mvnorm_lik_simple <- function(test_vals) {
  test_s2 <- test_vals[1]
  test_rho <- test_vals[2]
  test_nu <- test_vals[3]
  mat <- mk_cov(test_s2, test_rho, test_nu, l_D)
  sig <- tcrossprod(K %*% mat, K)
  
  # Fast Cholesky speedup with built-in safety
  R <- tryCatch(chol(sig), error = function(e) NULL)
  
  if (is.null(R)) return(NA)
  
  # Fast manual calculation
  n <- length(y_obs)
  scaled_y <- backsolve(R, c(y_obs), transpose = TRUE)
  quad_form <- sum(scaled_y^2)
  log_det <- 2 * sum(log(diag(R)))
  
  return(0.5 * (n * log(2 * pi) + log_det + quad_form))
}

set.seed(1337)

# True Parameters
# true_s2 <- c(1, 4, 9)
# true_rho <- c(1/10, 1/3, 1)
# true_nu <- c(1/2, 3/2, 5/2)
true_s2 <- 1
true_rho <- 1/3
true_nu <- 1/2
true_vals <- expand.grid(true_s2, true_rho, true_nu)

# Initialize dimensions to test
max_dim <- 48
aerial_dims <- (2:max_dim)[max_dim %% (2:max_dim) == 0]
latent_dims <- max_dim/aerial_dims
l_D <- calc_latent_distance(max_dim, max_dim, 1, 1)
K_vec <- mapply(function(A, L) gen_K(A, A, L, L), aerial_dims, latent_dims, SIMPLIFY = FALSE)

sig_true <- mk_cov(true_s2, true_rho, true_nu, l_D)
chol_true <- chol(sig_true)
y_true <- as.vector(crossprod(chol_true, rnorm(nrow(chol_true))))

i <- 6
A <- aerial_dims[i]
K <- K_vec[[i]]

y_obs <- K %*% y_true

test_s2 <- true_s2
test_rho <- seq(0.01, 1.2, length = 30)
test_nu <- seq(0.2, 3, length = 30)
test_vals <- expand.grid(test_s2, test_rho, test_nu)
colnames(test_vals) <- c('s2', 'rho', 'nu')

test_vals$lik <- pbapply(test_vals, 1, mvnorm_lik_simple)

q <- .5

p <- ggplot(test_vals, aes(x = rho, y = nu, fill = lik)) + 
  geom_raster(na.rm = TRUE) +
  scale_fill_viridis_c(
    direction = -1,  # lower NLL = better, flip so "good" = bright
    limits = c(min(test_vals$lik, na.rm = TRUE), quantile(test_vals$lik, q, na.rm = TRUE)),
    oob = scales::squish,
    na.value = "grey80"
  ) +
  geom_vline(xintercept = true_rho, color = "red", linetype = "dashed") +
  geom_hline(yintercept = true_nu, color = "red", linetype = "dashed") +
  coord_fixed() +                            
  theme_minimal() +
  labs(title = "Negative Log-Likelihood Surface", fill = "NLL")

write.csv(test_vals, "i6.csv", row.names = FALSE)

file_name <- paste0('lik_plot_q_', as.character(q), '_dim_', as.character(A), ".png")

ggsave(file_name, plot = p, width = 4, height = 4, units = "in", dpi = 300)
