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
