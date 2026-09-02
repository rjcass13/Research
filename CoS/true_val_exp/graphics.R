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
    true_s2 = mean(true_s2),
    mean_rho = mean(est_rho),
    var_est_rho = var(est_rho),
    true_rho = mean(true_rho),
    mean_nu = mean(est_nu), 
    var_est_nu = var(est_nu),
    true_nu = mean(true_nu))


p1 <- ggplot(dt_agg, aes(x = A_W, y = mean_s2, color = factor(true_s2))) +
  geom_point() + 
  geom_hline(
    aes(yintercept = true_s2, color = factor(true_s2)), 
    alpha = 0.6
  ) +
  scale_color_discrete(labels = function(x) round(as.numeric(x), 2)) + 
  labs(x = NULL, y = NULL, color = "true_s2")

p2 <- ggplot(dt_agg, aes(x = A_W, y = mean_rho, color = factor(true_rho))) +
  geom_point() + 
  geom_hline(
    aes(yintercept = true_rho, color = factor(true_rho)), 
    alpha = 0.6
  ) +
  scale_color_discrete(labels = function(x) round(as.numeric(x), 2)) + 
  labs(x = NULL, y = NULL, color = "true_rho")

p3 <- ggplot(dt_agg, aes(x = A_W, y = mean_nu, color = factor(true_nu))) +
  geom_point() + 
  geom_hline(
    aes(yintercept = true_nu, color = factor(true_nu)), 
    alpha = 0.6
  ) +
  scale_color_discrete(labels = function(x) round(as.numeric(x), 2)) + 
  labs(x = NULL, y = NULL, color = "true_nu")

p4 <- ggplot(dt_agg, aes(x = A_W, y = var_est_s2, color = factor(true_s2))) +
  geom_point() + 
  labs(x = NULL, y = NULL) + 
  theme(legend.position = "none")

p5 <- ggplot(dt_agg, aes(x = A_W, y = var_est_rho, color = factor(true_rho))) +
  geom_point() + 
  labs(x = NULL, y = NULL) + 
  theme(legend.position = "none")

p6 <- ggplot(dt_agg, aes(x = A_W, y = var_est_nu, color = factor(true_nu))) +
  geom_point() + 
  labs(x = NULL, y = NULL) + 
  theme(legend.position = "none")

s2_legend <- get_legend(p1)
rho_legend <- get_legend(p2)
nu_legend <- get_legend(p3)
my_legend <- wrap_elements(
  plot_grid(s2_legend, rho_legend, nu_legend, ncol = 3, align = "h")
)
p1 <- p1 + theme(legend.position = "none")
p2 <- p2 + theme(legend.position = "none")
p3 <- p3 + theme(legend.position = "none")

long_text <- "All data points represent the mean of 50 simulations of the Covariance:Observed dimension combination."
wrapped_text <- str_wrap(long_text, width = 50) 

my_label <- ggplot() +
  annotate("text", x = 0.5, y = 0.5, label = wrapped_text, size = 3.5, fontface = "italic") +
  theme_void() # Removes all borders, background grid lines, and axis labels

# 1. Create text elements for labels
col1_label <- wrap_elements(textGrob("Mean of Estimates", gp = gpar(fontsize = 14, fontface = "bold")))
col2_label <- wrap_elements(textGrob("Variance of Estimates", gp = gpar(fontsize = 14, fontface = "bold")))

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
    title = "True Value Exploration: Generated at Latent",
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
  filename = "true_val_exp_gen_at_lat.png",
  plot = final_layout,
  width = 8,          # Width in inches (or cm / mm)
  height = 10,         # Height in inches
  dpi = 300            # Standard print/publication quality
)
