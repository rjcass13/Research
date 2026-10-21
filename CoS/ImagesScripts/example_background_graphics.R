# Load required libraries
library(ggplot2)
library(ggforce)   # For the zoom feature
library(patchwork) # To display the plots together if desired

# Dimensions of the Aerial grid
A_W <- 3 # N blocks wide
A_H <- 3 # N blocks tall
# Dimensions of the Sub-grid
L_W <- 4 # N points wide
L_H <- 4 # N points tall

total_width <- A_W*L_W
total_height <- A_H*L_H

pts <- as.data.frame(cbind(rep(1:total_width,total_height)-.5,rep(1:total_height, each=total_width)-.5))
colnames(pts) <- c('x', 'y')

# Define the boundaries of the bottom-left square (Square_X = 1, Square_Y = 1)
rect_annot <- data.frame(xmin = 0, xmax = L_W, ymin = 0, ymax = L_H)

# =========================================================================
# PLOT 1: Aerial Grid
# =========================================================================
p1 <- ggplot() +
  # Vertical and Horizontal Lines
  geom_vline(xintercept = (0:A_W)*L_W, lwd = 1.5) +
  geom_hline(yintercept = (0:A_H)*L_H, lwd = 1.5) +
  # Axis number labels
  scale_x_continuous(breaks = (0:(A_W))*L_W, limits = c(0, A_W*L_W)) +
  scale_y_continuous(breaks = (0:(A_H))*L_H, limits = c(0, A_H*L_H)) +
  labs(title = "Areal Grid", x = NULL, y = NULL) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(), 
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(size = 14, face = "bold"), 
    axis.text.y = element_text(size = 14, face = "bold")) +
  annotate(geom = "text", label = "A[i]", x = L_W/2, y = A_H/2+.5, parse = TRUE, size = 10)


# =========================================================================
# PLOT 2: Subpoints included + Red box around bottom-left cell
# =========================================================================
p2 <- ggplot(pts, aes(x = x, y = y)) +
  # Vertical and Horizontal Lines
  geom_vline(xintercept = (0:A_W)*L_W, lwd = 1) +
  geom_hline(yintercept = (0:A_H)*L_H, lwd = 1) +
  # Latent subpoints
  geom_point(color = "blue", size = 2) +
  # Add the red box around the bottom-left square
  geom_rect(data = rect_annot, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            inherit.aes = FALSE, color = "red", fill = NA, linewidth = 1.2) +
  # Axis number labels
  scale_x_continuous(breaks = (0:(A_W))*L_W, limits = c(0, A_W*L_W)) +
  scale_y_continuous(breaks = (0:(A_H))*L_H, limits = c(0, A_H*L_H)) +
  labs(title = "Areal Grid with Latent Points", x = NULL, y = NULL) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(), 
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(size = 14, face = "bold"), 
    axis.text.y = element_text(size = 14, face = "bold"))


# =========================================================================
# PLOT 3: Zoomed-in version of that one square (using ggforce)
# =========================================================================
single_unit <- pts[(pts$x <= L_W & pts$y <= L_H), ]
p3 <- ggplot(single_unit[-1,], aes(x = x, y = y)) +
  geom_vline(xintercept = c(0, L_W), lwd = 1.5, col = "red") +
  geom_hline(yintercept = c(0, L_H), lwd = 1.5, col = "red") +
  scale_x_continuous(breaks = c(0, L_W), limits = c(0, L_W)) +
  scale_y_continuous(breaks = c(0, L_H), limits = c(0, L_H)) +
  geom_point(color = "blue", size = 3) +
  labs(title = "Single Areal Unit", x = NULL, y = NULL) +
  theme_minimal() + 
  theme(
    panel.grid = element_blank(), 
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(size = 14, face = "bold"), 
    axis.text.y = element_text(size = 14, face = "bold")) + 
  annotate(geom = "text", label = "S[paste(i, ',', j)]", x = .5, y = .5, parse = TRUE, size = 10)


# Display the plots individually or together
# Individual: p1, p2, p3
# Combined using patchwork:
combined_plot_stacked <- p1 / p2 / p3
combined_plot_square <- (p1 | p2) / (p3 | plot_spacer())

ggsave(
  filename = "areal_grid.png", 
  plot = p1, 
  width = 6, height = 6, units = "in", dpi = 300
)

ggsave(
  filename = "latent_grid.png", 
  plot = p2, 
  width = 6, height = 6, units = "in", dpi = 300
)

ggsave(
  filename = "single_block_sub_grid.png", 
  plot = p3, 
  width = 6, height = 6, units = "in", dpi = 300
)

ggsave(
  filename = "areal_latent_plots_square.png", 
  plot = combined_plot_square, 
  width = 10, height = 10, units = "in", dpi = 300
)

ggsave(
  filename = "areal_latent_plots_stacked.png", 
  plot = combined_plot_stacked, 
  width = 5, height = 15, units = "in", dpi = 300
)

# =========================================================================
# PLOT 4: Latent plot vs. Areal Plot
# =========================================================================

set.seed(1337)
pts = cbind(rep(1:50,50),rep(1:50,each=50))
D = as.matrix(dist(pts))/49
Sig = exp(-D)+diag(1e-6,50^2)
cSig = chol(Sig)
yobs = c(t(cSig)%*%matrix(rnorm(50^2),ncol=1))
K = matrix(0,100,2500)
blocks = cbind(rep(1:10,10),rep(1:10,each=10))
for(i in 1:100){
  K[i,] = pts[,1] >= blocks[i,1]*5-4 & pts[,1] <= blocks[i,1]*5 & pts[,2] >= blocks[i,2]*5-4 & pts[,2] <= blocks[i,2]*5
}

y_block = K%*%yobs/25

z_limits <- range(yobs)

# Reshape matrix 1 (yobs) into a long-format data frame
mat_yobs <- matrix(yobs, 50, 50)
df_yobs <- expand.grid(X = 1:50, Y = 1:50)
df_yobs$Z <- as.vector(mat_yobs)

# Reshape matrix 2 (y_block) into a long-format data frame
mat_yblock <- matrix(y_block, 10, 10)

# Expand the 10x10 coordinates so they stretch over the 1:50 space
# rep(1:10, each=5) creates: 1,1,1,1,1, 2,2,2,2,2, ... 10,10,10,10,10
df_yblock <- expand.grid(
  X = rep(1:10, each = 5), 
  Y = rep(1:10, each = 5)
)
# Extract values from the 10x10 matrix using the un-expanded 1:10 grids
df_yblock$Z <- apply(df_yblock, 1, function(row) mat_yblock[row["X"], row["Y"]])

# Replace the 1:10 block indices in the data frame with actual 1:50 spatial coordinates
df_yblock$X <- 1:50
df_yblock$Y <- rep(1:50, each = 50)

# Create Plot 1
p1 <- ggplot(df_yobs, aes(x = X, y = Y, fill = Z)) +
  geom_raster() +
  scale_fill_viridis_c(limits = z_limits) + # Syncs color limits
  coord_fixed() +                            # Keeps cells square
  theme_minimal() +
  labs(title = "Latent Process")

# Create Plot 2
p2 <- ggplot(df_yblock, aes(x = X, y = Y, fill = Z)) +
  geom_raster() +
  scale_fill_viridis_c(limits = z_limits) + # Syncs color limits
  coord_fixed() + 
  theme_minimal() +
  labs(title = "Areal Observations")

# Put them side-by-side using patchwork syntax
example_aggregation <- p1 + p2

ggsave(
  filename = "example_aggregation.png", 
  plot = example_aggregation, 
  width = 8, height = 4, units = "in", dpi = 300
)
