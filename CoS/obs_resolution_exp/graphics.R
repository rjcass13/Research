library(dplyr)
dt <- read.csv('square_48_sims_100.csv')

dt_agg <- dt %>%
  group_by(A_W, A_H, L_W, L_H) %>%
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

plot(dt_agg$A_W, dt_agg$mean_s2)
abline(h = dt_agg$mean_true_s2, col = 'blue')

plot(dt_agg$A_W, dt_agg$var_est_s2)


plot(dt_agg$A_W, dt_agg$mean_rho)
abline(h = dt_agg$mean_true_rho, col = 'blue')

plot(dt_agg$A_W, dt_agg$var_est_rho)

plot(dt_agg$A_W, dt_agg$mean_nu)
abline(h = dt_agg$mean_true_nu, col = 'blue')

plot(dt_agg$A_W, dt_agg$var_est_nu)
