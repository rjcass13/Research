source('funcs.R') 
library(patchwork)

res <- read.csv('sim_results_6_6_50_sim_bounded.csv')
res$est_s2 <- exp(res$est_s2)


A_W <- sort(unique(res$A_W))
true_s2 <- max(res$true_s2)
plot_list <- lapply(A_W, function(aw) {
  make_histogram(data.frame(vals = log(res$est_s2[res$A_W == aw])), aw, true_val = true_s2)
})
# Grids all plots in the list automatically
wrap_plots(plot_list, ncol = 3) + 
  plot_annotation(title = "Estimated S2 by Aerial Width")


L_W <- sort(unique(res$L_W))
true_s2 <- max(res$true_s2)
plot_list <- lapply(L_W, function(lw) {
  make_histogram(data.frame(vals = log(res$est_s2[res$L_W == lw])), lw, true_val = true_s2)
})
# Grids all plots in the list automatically
wrap_plots(plot_list, ncol = 3) + 
  plot_annotation(title = "Estimated S2 by Latent Width")


TP <- sort(unique(res$TotalPoints))
TP <- TP[seq(1, length(TP), 2)]
true_s2 <- max(res$true_s2)
plot_list <- lapply(TP, function(tp) {
  make_histogram(data.frame(vals = log(res$est_s2[res$TotalPoints == tp])), tp, true_val = true_s2)
})
# Grids all plots in the list automatically
wrap_plots(plot_list, ncol = 3) + 
  plot_annotation(title = "Estimated S2 by Total Points")




TP <- sort(unique(res$TotalPoints))
TP <- TP[seq(1, length(TP), 2)]
true_s2 <- max(res$true_s2)
plot_list <- lapply(TP, function(tp) {
  make_histogram(data.frame(vals = res$est_rho[res$TotalPoints == tp]), tp, true_val = true_s2)
})
# Grids all plots in the list automatically
wrap_plots(plot_list, ncol = 3) + 
  plot_annotation(title = "Estimated Rho by Total Points")


TP <- sort(unique(res$TotalPoints))
TP <- TP[seq(1, length(TP), 2)]
true_nu <- max(res$true_nu)
plot_list <- lapply(TP, function(tp) {
  make_histogram(data.frame(vals = res$est_nu[res$TotalPoints == tp]), tp, true_val = true_nu)
})
# Grids all plots in the list automatically
wrap_plots(plot_list, ncol = 3) + 
  plot_annotation(title = "Estimated Nu by Total Points")