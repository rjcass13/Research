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
library(magick) # For the animation
library(gtools) # For the ordering of the files

data_2024 <- read.csv('data/stats_2024.csv')

# Load world map, force it to be on same -180:180 scale, make Russian peninsula its own group
world_map <- map_data("world") %>%
  mutate(group = ifelse(long > 180, 1628, group)) %>%
  mutate(long = ifelse(long > 180, long - 360, long))

# World Maps
if (TRUE) {
  # Get relevant data
  week_1_2024 <- data_2024[which(data_2024$Week == '1'), ]

  # Temperature
  p <- ggplot() +
    # Add temperature tiles/raster
    geom_tile(data = week_1_2024, aes(x = Lon, y = Lat, fill = Temp)) +
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

  print(p)

  # Precipitation
  p <- ggplot() +
    # Add temperature tiles/raster
    geom_tile(data = week_1_2024, aes(x = Lon, y = Lat, fill = Precip)) +
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

  print(p)

  # Elevation
  p <- ggplot() +
    # Add Elevation tiles/raster
    geom_tile(data = week_1_2024, aes(x = Lon, y = Lat, fill = Elev)) +
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

  print(p)

  # NDVI
  p <- ggplot() +
    # Add Elevation tiles/raster
    geom_tile(data = week_1_2024, aes(x = Lon, y = Lat, fill = NDVI)) +
    # Add world map outlines on top
    geom_polygon(data = world_map, aes(x = long, y = lat, group = group),
                fill = NA, color = "gray40", linewidth = 0.2) +
    # Color scale for temperature
    scale_fill_viridis_c(name = "NDVI") +
    # Set aspect ratio
    coord_fixed(1) +
    # Clean theme
    theme_void() +
    labs(title = "Global NDVI Map")

  print(p)
}

# EDA, individual and interaction effects plots
if (FALSE) {
  # Average data across the year
  yearly_means <- data_2024 %>%
    group_by(Lat, Lon) %>%
    summarise(across(c(Temp, Elev, NDVI), \(x) mean(x, na.rm = TRUE)))

  # Average data across the Longitudes
  lat_means <- data_2024 %>%
    group_by(Week, Lat) %>%
    summarise(across(c(Temp, NDVI), \(x) mean(x, na.rm = TRUE)))

  # Yearly-Average Temp. vs. NDVI
  plot(yearly_means$NDVI, yearly_means$Temp, xlab = 'NDVI', ylab = 'Temperature (C)', main = '2024 Year-Average Temperature vs. NDVI')

  # Yearly-Average Temp. vs. Elev.
  plot(yearly_means$Elev, yearly_means$Temp, xlab = 'Elevation (m)', ylab = 'Temperature (C)', main = '2024 Year-Average Temperature vs. Elevation')

  # Yearly-Average Temp. vs. Lat.
  plot(yearly_means$Lat, yearly_means$Temp, xlab = 'Latitude', ylab = 'Temperature (C)', main = '2024 Year-Average Temperature vs. Latitude')
  
  
  # Latitude x Week
  ggplot(yearly_means, aes(x = Lat, y = Temp, color = Elev)) +
    geom_point() +
    labs(x = 'Latitude', y = 'Temperature', title = '2024 Temperature by Latitude and Elevation') +
    scale_colour_viridis_c(name = "Elevation (m)")

  # NDVI x Latitude
  ggplot(data_2024, aes(x = NDVI, y = Temp, color = Lat)) +
    geom_point() +
    labs(y = 'Temperature', title = '2024 Temperature by NDVI & Latitude') +
    scale_colour_viridis_c(name = "Latitude")

  # NDVI x Week
  ggplot(data_2024, aes(x = NDVI, y = Temp, color = Week)) +
    geom_point() +
    labs(y = 'Temperature', title = '2024 Temperature by NDVI & Week') +
    scale_colour_viridis_c(name = "Week")


  ggplot(lat_means, aes(x = Week, y = Lat, col = NDVI)) +
    geom_point() +
    labs(y = 'Latitude', title = '2024 NDVI by Week & Latitude') +
    scale_colour_viridis_c(name = "NDVI")

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
}


##### MODEL #####
mod <- lm(Temp ~ (bs(Lat, df = 4) + ns(Elev, df = 3) + bSpline(Week, df = 6, Boundary.knots = c(0, 52), periodic = TRUE))^3, data = data_2024)
mod_ndvi <- lm(Temp ~ (bs(Lat, df = 4) + ns(Elev, df = 3) + bSpline(Week, df = 6, Boundary.knots = c(0, 52), periodic = TRUE) + NDVI)^3, data = data_2024)
# Save the model
saveRDS(mod, '2024_model.rds')
#mod <- lm(Temp ~ bs(Lat, df = 4)*ns(Elev, df = 3) + bSpline(Week, df = 6, Boundary.knots = c(0, 52), periodic = TRUE)*bs(Lat, df = 4):ns(Elev, df = 3), data = data_2024)
#mod <- lm(Temp ~ bs(Lat, df = 4)*ns(Elev, df = 3):bSpline(Week, df = 6, Boundary.knots = c(0, 52), periodic = TRUE), data = data_2024)


# Week, color Latitude
if (FALSE) {
  n_lat <- 30
  lat <- seq(-90, 90, length = n_lat)
  week <- 0:52
  elev <- 1000
  colors <- viridis(n_lat, option = 'viridis')
  grid <- as.data.frame(expand.grid(week, lat, elev))
  colnames(grid) <- c('Week', 'Lat', 'Elev')
  grid$Preds <- predict(mod, newdata = grid)
  ggplot(grid, aes(x = Week, y = Preds, group = Lat, color = Lat)) +
    geom_line(linewidth = 1.5) + 
    scale_color_viridis_c() + 
    labs(y = 'Predicted Temperature (C)', 
        title = '2024 Model: Effect of Week per Latitude (Elev = 1000)')
}

# Latitude, color Elevation
if (FALSE) {
  n_lat <- 30
  n_elev <- 30
  lat <- seq(-90, 90, length = n_lat)
  week <- 27
  elev <- seq(0, 5000, length = n_elev)
  colors <- viridis(n_lat, option = 'viridis')
  grid <- as.data.frame(expand.grid(week, lat, elev))
  colnames(grid) <- c('Week', 'Lat', 'Elev')
  grid$Preds <- predict(mod, newdata = grid)
  
  ggplot(grid, aes(x = Lat, y = Preds, group = Elev, color = Elev)) +
    geom_line(linewidth = 1.3) + 
    scale_color_viridis_c() + 
    labs(y = 'Predicted Temperature (C)', 
        title = '2024 Model: Effect of Latitude per Elevation (Week = 27)')
}


plot_list <- vector(mode = "list", length = 52)
for (i in 1:52) {
  n_lat <- 100
  n_elev <- 100
  lat <- seq(-90, 90, length = n_lat)
  week <- i
  elev <- seq(0, 5500, length = n_elev)
  colors <- viridis(n_lat, option = 'viridis')
  grid <- as.data.frame(expand.grid(week, lat, elev))
  colnames(grid) <- c('Week', 'Lat', 'Elev')
  grid$Preds <- predict(mod, newdata = grid)
  
  # p <- ggplot() +
  #   # Add temperature tiles/raster
  #   geom_tile(data = grid, aes(x = Lat, y = Elev, fill = Preds)) +
  #   # Color scale for temperature
  #   scale_fill_viridis_c(name = "Temperature (°C)", limits = c(-50, 30), oob = scales::oob_squish ) +
  #   labs(x = 'Latitude', y = 'Elevation', title = paste0("2024 Model Temperature by Latitude x Elevation - Week ", i))

  p <- ggplot(data = grid, aes(x = Lat, y = Elev, z = Preds)) +
    # Add temperature tiles/raster
    geom_contour(aes(color = after_stat(level)), bins=20) +
    # Color scale for temperature
    scale_color_viridis_c(name = "Temperature (°C)", limits = c(-50, 30), oob = scales::oob_squish ) +
    labs(x = 'Latitude', y = 'Elevation', title = paste0("2024 Model Temperature by Latitude x Elevation - Week ", i))

  plot_list[[i]] <- p
}

for (i in seq_along(plot_list)) {
  ggsave(filename = paste0("plot_frame_", i, ".png"), plot = plot_list[[i]])
}
img_list <- image_read(mixedsort(list.files(pattern = "plot_frame_")))
animation <- image_animate(img_list, fps = 4)
image_write(animation, "animation.gif")



# Table showing boundary conditions on week
if (FALSE) {
  Location <- c('Provo', 'Cincinnati', 'Quito', 'Sydney', 'Falklands')
  Lat <- c(40.5, 39.5, -0.5, -33.5, -51.5)
  Lon <- c(360 - 248.5, 360 - (360 - 84.5), 360 - (360 - 78.5), 151.5, 360 - (360 - 59.5))
  n_loc <- length(Location)
  Week <- 0:52
  Elev <- 1000
  locs <- cbind(Location, Lat, Lon)
  result <- as.data.frame(locs[rep(1:nrow(locs), times = length(Week)), ])
  result$Lat <- as.numeric(result$Lat)
  result$Location <- as.factor(result$Location)
  result$Week <- rep(Week, each = nrow(locs))
  result$Elev <- rep(Elev, each = nrow(result))
  result$Preds <- predict(mod, newdata = result[, c(2, 4, 5)])
  ggplot(result, aes(x = Week, y = Preds, group = Location, color = Location)) +
    geom_line() + 
    scale_color_viridis_d() + 
    labs(y = 'Predicted Temperature (C)', 
        title = '2024 Model: Interaction of Week, Latitude (Elev = 0)')
}

# Latitude, color Week (First Half of Year)
if (FALSE) {
  week <- c(1:26)
  lat <- seq(-89.5, 89.5, by = 1)
  elev <- 1000
  grid <- as.data.frame(expand.grid(lat, week, elev))
  colnames(grid) <- c('Lat', 'Week', 'Elev')
  grid$Preds <- predict(mod, newdata = grid)

  ggplot(grid, aes(x = Lat, y = Preds, group = Week, color = Week)) +
    geom_line() + 
    scale_color_viridis_c() + 
    labs(y = 'Predicted Temperature (C)', x = 'Latitude',
        title = '2024 Model: Effect of Latitude per Week (Elev = 1000)')
}

# Latitude, color Week (Second Half of Year)
if (FALSE) {
  week <- c(31:52, 1:3)
  lat <- seq(-89.5, 89.5, by = 1)
  elev <- 1000
  grid <- as.data.frame(expand.grid(lat, week, elev))
  colnames(grid) <- c('Lat', 'Week', 'Elev')
  grid$Preds <- predict(mod, newdata = grid)

  ggplot(grid, aes(x = Lat, y = Preds, group = Week, color = Week)) +
    geom_line() + 
    scale_color_viridis_c() + 
    labs(y = 'Predicted Temperature (C)', x = 'Latitude',
        title = '2024 Model: Effect of Latitude per Week (Elev = 1000)')
}


# Residuals GIF
if (TRUE) {
  plot_list <- vector(mode = "list", length = 52)
  for (i in 1:52) {
    week_ind <- which(data_2024$Week == i)
    week_dat <- data_2024[week_ind, ]
    stuff_to_plot <- cbind(week_dat$Lat, week_dat$Lon, mod$residuals[week_ind])
    colnames(stuff_to_plot) <- c('Lat', 'Lon', 'Residuals')
    
    # Temperature
    p <- ggplot() +
      # Add temperature tiles/raster
      geom_tile(data = stuff_to_plot, aes(x = Lon, y = Lat, fill = Residuals)) +
      # Add world map outlines on top
      geom_polygon(data = world_map, aes(x = long, y = lat, group = group),
                  fill = NA, color = "gray40", linewidth = 0.2) +
      # Color scale for temperature
      scale_fill_viridis_c(name = "Residuals", limits = c(-15, 15), oob = scales::oob_squish) +
      # Set aspect ratio
      coord_fixed(1) +
      # Clean theme
      theme_void() +
      labs(title = paste0("Global Residuals Map: Week ", i))

    plot_list[[i]] <- p
  }

  for (i in seq_along(plot_list)) {
    ggsave(filename = paste0("plot_frame_", i, ".png"), plot = plot_list[[i]])
  }
  img_list <- image_read(mixedsort(list.files(pattern = "plot_frame_")))
  animation <- image_animate(img_list, fps = 4)
  image_write(animation, "animation.gif")
}


# Resid Limit Count - Unstandardized
if (TRUE) {
  resids <- data_2024[, c("Lat", "Lon")]
  # Residuals Count Above Limit
  resid_limit <- 7.5
  resids$Res <- mod$residuals > resid_limit

  map_resids_ind_avg <- resids %>%
      group_by(Lat, Lon) %>%
      summarise(across(Res, \(x) sum(x, na.rm = TRUE)))

  ggplot() +
    # Add temperature tiles/raster
    geom_tile(data = map_resids_ind_avg, aes(x = Lon, y = Lat, fill = Res)) +
    # Add world map outlines on top
    geom_polygon(data = world_map, aes(x = long, y = lat, group = group),
                fill = NA, color = "gray40", linewidth = 0.2) +
    # Color scale for temperature
    scale_fill_viridis_c(name = "Res") +
    # Set aspect ratio
    coord_fixed(1) +
    # Clean theme
    theme_void() +
    labs(title = paste0("Global Residuals Map - Residual Limit: ", resid_limit))
}

# Resid Limit Count - Standardized
if (TRUE) {
  resids <- data_2024[, c("Lat", "Lon")]

  # Standardize Residuals
  rmse <- sqrt(mean(mod$residuals^2))
  resid_stand_lim <- 2.5
  stand_resids <-  mod$residuals/rmse
  resids$Res <- stand_resids > resid_stand_lim
  resids$Res <- resids$Res/52

  map_resids_ind_avg <- resids %>%
      group_by(Lat, Lon) %>%
      summarise(across(Res, \(x) sum(x, na.rm = TRUE)))

  ggplot() +
    # Add temperature tiles/raster
    geom_tile(data = map_resids_ind_avg, aes(x = Lon, y = Lat, fill = Res)) +
    # Add world map outlines on top
    geom_polygon(data = world_map, aes(x = long, y = lat, group = group),
                fill = NA, color = "gray60", linewidth = 0.2) +
    # Color scale for temperature
    scale_fill_viridis_c(name = "Proportion") +
    # Set aspect ratio
    coord_fixed(1) +
    # Clean theme
    theme_void() +
    labs(title = paste0("Standardized Residuals Map - Stand. Res. Lim:", resid_stand_lim))
}

# Residuals Floor GIF
if (TRUE) {
  world_map <- map_data("world")
  plot_list <- vector(mode = "list", length = 52)
  resid_limit <- 12.5
  for (i in 1:52) {
    week_ind <- which(data_2024$Week == i)
    week_dat <- data_2024[week_ind, ]
    stuff_to_plot <- as.data.frame(cbind(week_dat$Lat, week_dat$Lon, mod$residuals[week_ind]))
    colnames(stuff_to_plot) <- c('Lat', 'Lon', 'Residuals')

    stuff_to_plot$ind <- ifelse(abs(stuff_to_plot$Residuals) > resid_limit, 1, 0)
    
    # Temperature
    p <- ggplot() +
      # Add temperature tiles/raster
      geom_tile(data = stuff_to_plot, aes(x = Lon, y = Lat, fill = ind)) +
      # Add world map outlines on top
      geom_polygon(data = world_map, aes(x = long, y = lat, group = group),
                  fill = NA, color = "gray40", linewidth = 0.2) +
      # Color scale for temperature
      scale_fill_viridis_c(name = "ind") +
      # Set aspect ratio
      coord_fixed(1) +
      # Clean theme
      theme_void() +
      labs(title = paste0("Global Residuals Map: Week ", i, " Residual Limit: ", resid_limit))

    plot_list[[i]] <- p
  }

  for (i in seq_along(plot_list)) {
    ggsave(filename = paste0("plot_frame_", i, ".png"), plot = plot_list[[i]])
  }
  img_list <- image_read(mixedsort(list.files(pattern = "plot_frame_")))
  animation <- image_animate(img_list, fps = 4)
  image_write(animation, "animation.gif")
}


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


lat_means$Temp[lat_means$Lat == -89.5]
