library(fields)
library(ggplot2)
library(dplyr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(splines)
library(splines2)
library(viridis)
library(xtable)

data_2024 <- read.csv('stats_2024.csv')

# World Maps
if (FALSE) {
  # Get relevant data
  week_1_2024 <- data_2024[which(data_2024$Week == '1'), ]
  # Convert to -180 to 180 longitude frame
  week_1_2024 <- week_1_2024 %>%
    mutate(lon_adj = ifelse(Lon > 180, Lon - 360, Lon))
  world_map <- map_data("world")
  
  # Temperature
  ggplot() +
    # Add ocean/land background (optional)
    geom_polygon(data = world_map, aes(x = long, y = lat, group = group),
                fill = "lightblue", color = "gray80") +
    # Add temperature tiles/raster
    geom_tile(data = week_1_2024, aes(x = lon_adj, y = Lat, fill = Temp)) +
    # Add world map outlines on top
    geom_polygon(data = world_map, aes(x = long, y = lat, group = group),
                fill = NA, color = "gray40", linewidth = 0.2) +
    # Color scale for temperature
    scale_fill_viridis_c(name = "Temperature (°C)") +
    # Set aspect ratio
    coord_fixed(1) +
    # Clean theme
    theme_void() +
    labs(title = "Global Temperature Map")

  # Precipitation
  ggplot() +
    # Add ocean/land background (optional)
    geom_polygon(data = world_map, aes(x = long, y = lat, group = group),
                fill = "lightblue", color = "gray80") +
    # Add temperature tiles/raster
    geom_tile(data = week_1_2024, aes(x = lon_adj, y = Lat, fill = Precip)) +
    # Add world map outlines on top
    geom_polygon(data = world_map, aes(x = long, y = lat, group = group),
                fill = NA, color = "gray40", linewidth = 0.2) +
    # Color scale for temperature
    scale_fill_viridis_c(name = "Precipitation (m)") +
    # Set aspect ratio
    coord_fixed(1) +
    # Clean theme
    theme_void() +
    labs(title = "Global Precipitation Map")

  # Elevation
  ggplot() +
    # Add ocean/land background (optional)
    geom_polygon(data = world_map, aes(x = long, y = lat, group = group),
                fill = "lightblue", color = "gray80") +
    # Add Elevation tiles/raster
    geom_tile(data = week_1_2024, aes(x = lon_adj, y = Lat, fill = Elev)) +
    # Add world map outlines on top
    geom_polygon(data = world_map, aes(x = long, y = lat, group = group),
                fill = NA, color = "gray40", linewidth = 0.2) +
    # Color scale for temperature
    scale_fill_viridis_c(name = "Elevation (m)") +
    # Set aspect ratio
    coord_fixed(1) +
    # Clean theme
    theme_void() +
    labs(title = "Global Elevation Map")
}

# Average data across the year
yearly_means <- data_2024 %>%
  group_by(Lat, Lon) %>%
  summarise(across(c(Temp, Elev), mean, na.rm = TRUE))

# Average data across the Longitudes
lat_means <- data_2024 %>%
  group_by(Week, Lat) %>%
  summarise(across(c(Temp), mean, na.rm = TRUE))

# Yearly-Average Temp. vs. Elev.
plot(yearly_means$Elev, yearly_means$Temp, xlab = 'Elevation (m)', ylab = 'Temperature (C)', main = '2024 Year-Average Temperature vs. Elevation')

# Yearly-Average Temp. vs. Lat.
plot(yearly_means$Lat, yearly_means$Temp, xlab = 'Latitude', ylab = 'Temperature (C)', main = '2024 Year-Average Temperature vs. Latitude')

# Latitude x Week
ggplot(lat_means, aes(x = Week, y = Lat, color = Temp)) +
  geom_point() +
  labs(y = 'Latitude', title = '2024 Temperature by Week & Latitude') +
  scale_colour_viridis_c(name = "Temperature (°C)")

# Elevation x Latitude
ggplot(yearly_means, aes(x = Elev, y = Temp, color = Lat)) +
  geom_point() +
  labs(x = 'Elevation (m)', y = 'Temperature (C)', title = '2024 Temperature x Elevation x Latitude') +
  annotate("label", x = 3900, y = 15, label = "1") +
  annotate("label", x = 5350, y = 2, label = "2") +
  annotate("label", x = 3500, y = -27, label = "3") +
  annotate("label", x = 4300, y = -51, label = "4") +
  scale_colour_viridis_c(name = "Latitude")
ggplot(yearly_means, aes(x = Elev, y = Temp, color = abs(Lat))) +
  geom_point() +
  labs(x = 'Elevation (m)', y = 'Temperature (C)', title = '2024 Temperature x Elevation x abs(Latitude)') +
  scale_colour_viridis_c(name = "abs(Latitude)")



##### MODEL #####
mod <- lm(Temp ~ Lat + I(Lat^2) + ns(Elev, df = 3) +  Elev:Lat + bSpline(Week, df = 6, Boundary.knots = c(0, 52), periodic = TRUE)*Lat*Elev, data = data_2024)

mod <- lm(Temp ~ (ns(Lat, df = 5) + ns(Elev, df = 3) + bSpline(Week, df = 6, Boundary.knots = c(0, 52), periodic = TRUE))^3, data = data_2024)
summary(mod)

# Interaction of Week, Latitude
if (TRUE) {
  n_lat <- 30
  lat <- seq(-90, 90, length = n_lat)
  week <- 0:52
  elev <- 0
  colors <- viridis(n_lat, option = 'viridis')
  grid <- as.data.frame(expand.grid(week, lat, elev))
  colnames(grid) <- c('Week', 'Lat', 'Elev')
  grid$Preds <- predict(mod, newdata = grid)
  ggplot(grid, aes(x = Week, y = Preds, group = Lat, color = Lat)) +
    geom_line() + 
    scale_color_viridis_c() + 
    labs(y = 'Predicted Temperature (C)', 
        title = '2024 Model: Interaction of Week, Latitude (Elev = 0)')
}

# Table showing boundary conditions on week
if (FALSE) {
  Location <- c('Provo', 'Cincinnati', 'Quito', 'Sydney', 'Falklands')
  Lat <- c(40.5, 39.5, -0.5, -33.5, -51.5)
  Lon <- c(248.5, 360 - 84.5, 360 - 78.5, 151.5, 360 - 59.5)
  n_loc <- length(Location)
  Week <- 0:52
  locs <- cbind(Location, Lat, Lon)
  result <- as.data.frame(locs[rep(1:nrow(locs), times = length(Week)), ])
  result$Week <- rep(Week, each = nrow(locs))
  result$Preds <- predict(mod, newdata = result)
  ggplot(result, aes(x = Week, y = Preds, group = Location, color = Location)) +
    geom_line() + 
    scale_color_viridis_c() + 
    labs(y = 'Predicted Temperature (C)', 
        title = '2024 Model: Interaction of Week, Latitude (Elev = 0)')
}

# Interacion of Latitude, with interaction on Week
if (TRUE) {
  week <- c(4:30)
  n_week <- length(week)
  lat <- seq(-89.5, 89.5, by = 1)
  elev <- 1000
  colors <- viridis(n_week, option = 'viridis')
  for (i in 1:n_week) {
    week_val <- week[i]
    grid <- as.data.frame(expand.grid(lat, week_val, elev))
    colnames(grid) <- c('Lat', 'Week', 'Elev')
    preds <- predict(mod, newdata = grid)
    if (i == 1) {
      plot(grid$Lat, preds, type = 'l', col = colors[i], 
        xlab = 'Latitude', ylab = 'Temperature', main = 'Interaction Effect of Latitude, Week (2024)')
    } else {
      lines(grid$Lat, preds, col = colors[i])
    }
  }
  legend("bottom", title = 'Week', legend = week[c(1, 27)], col = colors[c(1, 27)], lty = 1)
}




# Residuals GIF

library(gganimate)
library(gifski)
world_map <- map_data("world")
plot_list <- vector(mode = "list", length = 52)
for (i in 1:52) {
  week_ind <- which(data_2024$Week == i)
  week_dat <- data_2024[week_ind, ]
  # Convert to -180 to 180 longitude frame
  week_dat <- week_dat %>% mutate(lon_adj = ifelse(Lon > 180, Lon - 360, Lon))
  stuff_to_plot <- cbind(week_dat$Lat, week_dat$lon_adj, mod$residuals[week_ind])
  colnames(stuff_to_plot) <- c('Lat', 'Lon', 'Residuals')
  
  # Temperature
  p <- ggplot() +
    # Add ocean/land background (optional)
    geom_polygon(data = world_map, aes(x = long, y = lat, group = group),
                fill = "lightblue", color = "gray80") +
    # Add temperature tiles/raster
    geom_tile(data = stuff_to_plot, aes(x = Lon, y = Lat, fill = Residuals)) +
    # Add world map outlines on top
    geom_polygon(data = world_map, aes(x = long, y = lat, group = group),
                fill = NA, color = "gray40", linewidth = 0.2) +
    # Color scale for temperature
    scale_fill_viridis_c(name = "Residuals") +
    # Set aspect ratio
    coord_fixed(1) +
    # Clean theme
    theme_void() +
    labs(title = paste0("Global Residuals Map: Week ", i))

  plot_list[[i]] <- p
}


plot(plot_list[[10]])

library(magick) # For the animation
library(gtools) # For the ordering of the files
setwd("c:\\Users\\RJ\\Documents\\GitRepos\\Research\\images")
for (i in seq_along(plot_list)) {
  ggsave(filename = paste0("plot_frame_", i, ".png"), plot = plot_list[[i]])
}
img_list <- image_read(mixedsort(list.files(pattern = "plot_frame_")))
animation <- image_animate(img_list, fps = 4)
image_write(animation, "animation.gif")
setwd("c:\\Users\\RJ\\Documents\\GitRepos\\Research")





library(tidyr)
data_wide <- data.frame(
  x = 1:10,
  line1 = runif(10, 0, 1),
  line2 = runif(10, 0, 2),
  line3 = runif(10, 0, 3),
  line4 = runif(10, 0, 4)
)

# 2. Reshape the data to "long" format
data_long <- data_wide %>%
  gather(key = "line_id", value = "y_value", -x)

# Convert line_id to a numeric value to enable a color gradient
data_long <- data_long %>%
  mutate(line_value = as.numeric(gsub("line", "", line_id)))

# 3. Plot using a single ggplot call with color mapped to the numeric variable
ggplot(data_long, aes(x = x, y = y_value, group = line_value)) +
  geom_line() +
  scale_color_gradient(low = "blue", high = "red", name = "Line Value") + # Apply the color gradient
  labs(title = "Lines with Color Gradient")
