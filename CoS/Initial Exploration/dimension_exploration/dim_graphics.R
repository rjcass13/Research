library(ggplot2)
library(dplyr)

dt <- read.csv('sim_results_100_possible_dims_25_sims.csv')

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
dt_agg$total_points <- dt_agg$A_W*dt_agg$L_W*dt_agg$A_H*dt_agg$L_H
dt_agg$d <- (dt_agg$A_W*dt_agg$L_W*dt_agg$A_H*dt_agg$L_H) / 100
dt_agg$theta <- atan((dt_agg$A_H*dt_agg$L_H)/(dt_agg$A_W*dt_agg$L_W))
dt_agg$total_aerial <- dt_agg$A_W*dt_agg$A_H
dt_agg$total_latent <- dt_agg$L_W*dt_agg$L_H


dt_agg <- dt_agg[dt_agg$total_points >= 20, ]

# Duration
ggplot(dt_agg, aes(x = theta, y = d, color = mean_duration)) +
  geom_point(size = 3) +
  coord_radial(theta = "x", start = 0, end = pi/2, expand = FALSE) + # Map x-axis to the circle angle
  scale_color_viridis_c() + # Custom color palette
  theme_minimal()


# S2 Estimates
ggplot(dt_agg, aes(x = theta, y = d, color = mean_s2-mean_true_s2)) +
  geom_point(size = 3) +
  coord_radial(theta = "x", start = 0, end = pi/2, expand = FALSE) + # Map x-axis to the circle angle
  scale_color_viridis_c() + # Custom color palette
  theme_minimal()

# S2 Variance
ggplot(dt_agg, aes(x = theta, y = d, color = var_est_s2)) +
  geom_point(size = 3) +
  coord_radial(theta = "x", start = 0, end = pi/2, expand = FALSE) + # Map x-axis to the circle angle
  scale_color_viridis_c() + # Custom color palette
  theme_minimal()


# Rho Estimates
ggplot(dt_agg, aes(x = theta, y = d, color = mean_rho-mean_true_rho)) +
  geom_point(size = 3) +
  coord_radial(theta = "x", start = 0, end = pi/2, expand = FALSE) + # Map x-axis to the circle angle
  scale_color_viridis_c() + # Custom color palette
  theme_minimal()

ggplot(dt_agg, aes(x = theta, y = d, color = var_est_rho)) +
  geom_point(size = 3) +
  coord_radial(theta = "x", start = 0, end = pi/2, expand = FALSE) + # Map x-axis to the circle angle
  scale_color_viridis_c() + # Custom color palette
  theme_minimal()

# Nu Estimates
ggplot(dt_agg, aes(x = theta, y = d, color = mean_nu-mean_true_nu)) +
  geom_point(size = 3) +
  coord_radial(theta = "x", start = 0, end = pi/2, expand = FALSE) + # Map x-axis to the circle angle
  scale_color_viridis_c() + # Custom color palette
  theme_minimal()

ggplot(dt_agg, aes(x = theta, y = d, color = var_est_nu)) +
  geom_point(size = 3) +
  coord_radial(theta = "x", start = 0, end = pi/2, expand = FALSE) + # Map x-axis to the circle angle
  scale_color_viridis_c() + # Custom color palette
  theme_minimal()

ggplot(dt_agg, aes(x = total_aerial, y = total_latent, color = var_est_nu)) +
  geom_point(size = 3) +
  scale_color_viridis_c() + # Custom color palette
  theme_minimal()

ggplot(dt_agg, aes(x = total_latent, y = total_aerial, color = var_est_rho)) +
  geom_point(size = 3) +
  scale_color_viridis_c() + # Custom color palette
  theme_minimal()

max(dt_agg$theta)
