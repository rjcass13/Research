library(dplyr)
source('funcs.R')
dt <- read.csv('square_48_sims_50_new.csv')

dt_agg <- dt %>%
  group_by(true_s2, true_rho, true_nu, A_W, A_H, L_W, L_H) %>%
  summarize(mean_duration = mean(Duration),
    mean_s2 = mean(est_s2),
    var_est_s2 = var(est_s2),
    mean_rho = mean(est_rho),
    var_est_rho = var(est_rho),
    mean_nu = mean(est_nu), 
    var_est_nu = var(est_nu),
    mean_true_s2 = mean(true_s2),
    mean_true_rho = mean(true_rho),
    mean_true_nu = mean(true_nu))

dt_agg <- dt_agg[dt_agg$A_W > 4, ]

plot(dt_agg$A_W, dt_agg$mean_s2, col = dt_agg$true_s2)
abline(h = dt_agg$true_s2, col = dt_agg$true_s2)


plot(dt_agg$A_W, dt_agg$mean_rho, col = dt_agg$true_rho)
abline(h = dt_agg$true_rho, col = dt_agg$true_rho)

plot(dt_agg$A_W, dt_agg$mean_nu, col = dt_agg$true_rho)
abline(h = dt_agg$true_nu, col = dt_agg$true_nu)

nu_exp <- dt_agg[dt_agg$true_nu == 1.5, ]
plot(nu_exp$A_W, nu_exp$mean_nu, col = nu_exp$true_rho)
abline(h = nu_exp$true_nu, col = nu_exp$true_nu)

# Color Coding different dimension true values

s2 <- 1
rho <- .33
nu <- 2.5
l_D <- calc_latent_distance(48, 48, 1, 1)
#diag(l_D) <- 1e-9
cov <- mk_cov(s2, rho, nu, l_D)
diag(cov) <- s2
y_true <- rmvnorm(1, mean = rep(0, 48 * 48), sigma = cov)

image.plot(matrix(y_true, 48, 48), zlim = c(-3, 3))


 

