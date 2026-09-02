library(dplyr)
library(ggplot2)
library(patchwork)
library(cowplot)
library(stringr) # Required for text wrapping
library(grid)

# Load and prep Data
dt <- read.csv('square_48_sims_50_new.csv')

dt_agg <- dt %>%
  group_by(true_s2, true_rho, true_nu, A_W, A_H, L_W, L_H) %>%
  summarize(mean_duration = mean(Duration),
    mean_s2 = mean(est_s2),
    var_est_s2 = var(est_s2),
    mean_rho = mean(est_rho),
    var_est_rho = var(est_rho),
    mean_nu = mean(est_nu), 
    var_est_nu = var(est_nu))

# Mean estimates
if (TRUE) {
  p1 <- ggplot(dt_agg, aes(x = A_W, y = mean_s2, color = true_s2)) +
    geom_point() + 
    labs(
      title = "S2",
      x = "Observed Width",
      y = "Mean Estimated S2",
      color = "Covariance"
    )

  p2 <- ggplot(dt_agg, aes(x = A_W, y = mean_rho, color = true_rho)) +
    geom_point() + 
    labs(
      title = "Rho",
      x = "Observed Width",
      y = "Mean Estimated Rho",
    ) +
    theme(legend.position = "none")

  p3 <- ggplot(dt_agg, aes(x = A_W, y = mean_nu, color = true_nu)) +
    geom_point() + 
    labs(
      title = "Nu",
      x = "Observed Width",
      y = "Mean Estimated Nu",
    ) +
    theme(legend.position = "none")

  my_legend <- get_legend(p1)
  p1 <- p1 + theme(legend.position = "none")

  long_text <- "All data points represent the mean of 50 simulations of the Covariance:Observed dimension combination."
  wrapped_text <- str_wrap(long_text, width = 50) 

  my_label <- ggplot() +
    annotate("text", x = 0.5, y = 0.5, label = wrapped_text, size = 3.5, fontface = "italic") +
    theme_void() # Removes all borders, background grid lines, and axis labels

  bottom_right_cell <- plot_grid(
    my_legend, 
    my_label, 
    ncol = 1, 
    rel_heights = c(0.5, 0.5) 
  )
  plot_grid(
    p1, p2,
    p3, bottom_right_cell,
    ncol = 2, 
    nrow = 2
  )
}

# Variance of estimates
if (TRUE) {
  p1 <- ggplot(dt_agg, aes(x = A_W, y = var_est_s2, color = true_s2)) +
    geom_point() + 
    labs(
      title = "S2",
      x = "Observed Width",
      y = "Var Estimated S2",
      color = "Covariance"
    )

  p2 <- ggplot(dt_agg, aes(x = A_W, y = var_est_rho, color = true_rho)) +
    geom_point() + 
    labs(
      title = "Rho",
      x = "Observed Width",
      y = "Var Estimated Rho",
    ) +
    theme(legend.position = "none")

  p3 <- ggplot(dt_agg, aes(x = A_W, y = var_est_nu, color = true_nu)) +
    geom_point() + 
    labs(
      title = "Nu",
      x = "Observed Width",
      y = "Var Estimated Nu",
    ) +
    theme(legend.position = "none")

  my_legend <- get_legend(p1)
  p1 <- p1 + theme(legend.position = "none")

  long_text <- "All data points represent the variance of 50 simulations of the Covariance:Observed dimension combination."
  wrapped_text <- str_wrap(long_text, width = 50) 

  my_label <- ggplot() +
    annotate("text", x = 0.5, y = 0.5, label = wrapped_text, size = 3.5, fontface = "italic") +
    theme_void() # Removes all borders, background grid lines, and axis labels

  bottom_right_cell <- plot_grid(
    my_legend, 
    my_label, 
    ncol = 1, 
    rel_heights = c(0.5, 0.5) 
  )
  plot_grid(
    p1, p2,
    p3, bottom_right_cell,
    ncol = 2, 
    nrow = 2
  )
}








p1 <- ggplot(dt_agg, aes(x = A_W, y = mean_s2, color = true_s2)) +
  geom_point() + 
  labs(
    x = NULL, 
    y = NULL,
    color = "Covariance"
  )

p2 <- ggplot(dt_agg, aes(x = A_W, y = mean_rho, color = true_rho)) +
  geom_point() + 
  labs(x = NULL, y = NULL) + 
  theme(legend.position = "none")

p3 <- ggplot(dt_agg, aes(x = A_W, y = mean_nu, color = true_nu)) +
  geom_point() + 
  labs(x = NULL, y = NULL) + 
  theme(legend.position = "none")

p4 <- ggplot(dt_agg, aes(x = A_W, y = var_est_s2, color = true_s2)) +
  geom_point() + 
  labs(x = NULL, y = NULL) + 
  theme(legend.position = "none")

p5 <- ggplot(dt_agg, aes(x = A_W, y = var_est_rho, color = true_rho)) +
  geom_point() + 
  labs(x = NULL, y = NULL) + 
  theme(legend.position = "none")

p6 <- ggplot(dt_agg, aes(x = A_W, y = var_est_nu, color = true_nu)) +
  geom_point() + 
  labs(x = NULL, y = NULL) + 
  theme(legend.position = "none")

my_legend <- get_legend(p1)
p1 <- p1 + theme(legend.position = "none")

long_text <- "All data points represent the mean of 50 simulations of the Covariance:Observed dimension combination."
wrapped_text <- str_wrap(long_text, width = 50) 

my_label <- ggplot() +
  annotate("text", x = 0.5, y = 0.5, label = wrapped_text, size = 3.5, fontface = "italic") +
  theme_void() # Removes all borders, background grid lines, and axis labels

# 1. Create text elements for labels
col1_label <- wrap_elements(textGrob("Mean of Estimates", gp = gpar(fontsize = 14, fontface = "bold")))
col2_label <- wrap_elements(textGrob("Var. of Estimates", gp = gpar(fontsize = 14, fontface = "bold")))

row1_label <- wrap_elements(textGrob("S2", rot = 90, gp = gpar(fontsize = 14, fontface = "bold")))
row2_label <- wrap_elements(textGrob("Rho", rot = 90, gp = gpar(fontsize = 14, fontface = "bold")))
row3_label <- wrap_elements(textGrob("Nu", rot = 90, gp = gpar(fontsize = 14, fontface = "bold")))

# Empty spacer for the top-left corner where row and column labels meet
empty_corner <- wrap_elements(nullGrob()) 

final_layout <- (
  empty_corner + col1_label + col2_label +
  row1_label   + p1         + p4         +
  row2_label   + p2         + p5         +
  row3_label   + p3         + p6         +
  empty_corner + my_legend  + my_label
) + 
  plot_layout(ncol = 3, widths = c(0.1, 1, 1), heights = c(0.2, 1, 1, 1, .5)) +
  plot_annotation(
    title = "CAR: Generated at Latent",
    theme = theme(
      plot.title = element_text(
        hjust = 0.5,             # Centers the title
        # family = "serif",        # Changes font family (e.g., "sans", "serif", "mono")
        face = "bold",           # Makes font bold
        size = 18                # Adjusts font size
      )
    )
  )

ggsave(
  filename = "car_gen_at_lat.png",
  plot = final_layout,
  width = 8,          # Width in inches (or cm / mm)
  height = 10,         # Height in inches
  dpi = 300            # Standard print/publication quality
)
