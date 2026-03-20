library(fields)
library(ggplot2)
library(dplyr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(splines)
library(splines2)
library(viridis)

# World Map with Temperature Overlay
if (TRUE) {
  # Get relevant data
  data_2024 <- read.csv('data/stats_2024.csv')
  week_1_2024 <- data_2024[which(data_2024$Week == '1'), ]

  world_map <- map_data("world")
  # 3. Create the plot with multiple layers
  ggplot() +
    # Add temperature tiles/raster
    geom_tile(data = week_1_2024, aes(x = Lon, y = Lat, fill = Temp)) +
    # Add world map outlines on top
    geom_polygon(data = world_map, aes(x = long, y = lat, group = group),
                fill = NA, color = "gray40", linewidth = 0.2) +
    # Color scale for temperature
    scale_fill_gradient2(low = "blue", mid = "yellow", high = "red",
                        midpoint = 0, name = "Temperature (°C)") +
    # Set aspect ratio
    coord_fixed(1) +
    # Clean theme
    theme_void() +
    labs(title = "Global Temperature Map")
}

# World Map with Precipitation Overlay
if (TRUE) {
  # Get relevant data
  data_2024 <- read.csv('stats_2024.csv')
  week_1_2024 <- data_2024[which(data_2024$Week == '1'), ]
  # Convert to -180 to 180 longitude frame
  week_1_2024 <- week_1_2024 %>%
    mutate(lon_adj = ifelse(Lon > 180, Lon - 360, Lon))

  world_map <- map_data("world")
  # 3. Create the plot with multiple layers
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
    scale_fill_gradient2(low = "blue", mid = "yellow", high = "red",
                        midpoint = .001, name = "Precipitation (m)") +
    # Set aspect ratio
    coord_fixed(1) +
    # Clean theme
    theme_void() +
    labs(title = "Global Precipitation Map")
}

# World Map with Elevation Overlay
if (TRUE) {
  # Get relevant data
  data_2024 <- read.csv('stats_elev.csv')
  # Convert to -180 to 180 longitude frame
  data_2024 <- data_2024 %>%
    mutate(lon_adj = ifelse(Lon > 180, Lon - 360, Lon))

  world_map <- map_data("world")
  # 3. Create the plot with multiple layers
  ggplot() +
    # Add ocean/land background (optional)
    geom_polygon(data = world_map, aes(x = long, y = lat, group = group),
                fill = "lightblue", color = "gray80") +
    # Add Elevation tiles/raster
    geom_tile(data = data_2024, aes(x = lon_adj, y = Lat, fill = Elev)) +
    # Add world map outlines on top
    geom_polygon(data = world_map, aes(x = long, y = lat, group = group),
                fill = NA, color = "gray40", linewidth = 0.2) +
    # Color scale for temperature
    scale_fill_gradient2(low = "blue", mid = "yellow", high = "red",
                        midpoint = 2500, name = "Elevation (m)") +
    # Set aspect ratio
    coord_fixed(1) +
    # Clean theme
    theme_void() +
    labs(title = "Global Geopotential Map")
}

# World Map with NDVI Overlay
if (TRUE) {
  # Get relevant data
  data_2024 <- read.csv('stats_ndvi.csv')
  week_1_2024 <- data_2024[which(data_2024$Week == '1'), ]
  # Convert to -180 to 180 longitude frame
  week_1_2024 <- week_1_2024 %>%
    mutate(lon_adj = ifelse(Lon > 180, Lon - 360, Lon))


  world_map <- map_data("world")
  # 3. Create the plot with multiple layers
  ggplot() +
    # Add ocean/land background (optional)
    # geom_polygon(data = world_map, aes(x = long, y = lat, group = group),
    #             fill = "lightblue", color = "gray80") +
    # Add Elevation tiles/raster
    geom_tile(data = week_1_2024, aes(x = lon_adj, y = Lat, fill = NDVI)) +
    # Add world map outlines on top
    geom_polygon(data = world_map, aes(x = long, y = lat, group = group),
                fill = NA, color = "gray40", linewidth = 0.2) +
    # Color scale for temperature
    scale_fill_gradient2(low = "blue", mid = "yellow", high = "red",
                        midpoint = 2500, name = "NDVI") +
    # Set aspect ratio
    coord_fixed(1) +
    # Clean theme
    theme_void() +
    labs(title = "Global Geopotential Map")
}


# Location Plots
if (FALSE) {
  dt <- read.csv('agg_1x1_2015_2024_temp_precip.csv')

  provo_lat <- 40.5
  provo_lon <- 248.5

  cin_lat <- 39.5
  cin_lon <- 360 - 84.5

  quito_lat <- -.5
  quito_lon <- 360 - 78.5

  sydney_lat <- -33.5
  sydney_lon <- 151.5

  provo <- dt[which(dt$Lat == provo_lat & dt$Lon == provo_lon), ]
  cincinnati <- dt[which(dt$Lat == cin_lat & dt$Lon == cin_lon), ]
  quito <- dt[which(dt$Lat == quito_lat & dt$Lon == quito_lon), ]
  sydney <- dt[which(dt$Lat == sydney_lat & dt$Lon == sydney_lon), ]

  provo$week <- sub(".*-", "", provo$Year_Week)
  cincinnati$week <- sub(".*-", "", cincinnati$Year_Week)
  quito$week <- sub(".*-", "", quito$Year_Week)
  sydney$week <- sub(".*-", "", sydney$Year_Week)

  plot(provo$week, provo$Temp)
  plot(cincinnati$week, cincinnati$Temp)
  plot(quito$week, quito$Temp)
  plot(sydney$week, sydney$Temp)
}

data_2024 <- read.csv('stats_2024.csv')

# plot(dt$Lat, dt$Temp) # Appears negative quadratic (peaks at 0 Lat)
# plot(dt$Geop, dt$Temp) # Appears negative, slightly curved s shape, but with 2 distinct regions

ggplot(data_2024, aes(x = Lat, y = Elev, color = Temp)) +
  geom_point() +
  scale_color_gradient2(low = "blue", mid = "yellow", high = "red",
                        midpoint = 0, name = "Temperature (°C)")

plot(data_2024$Lat, data_2024$Temp) # Appears negative quadratic (peaks at 0 Lat)



###### Elevation ######
# # Appears negative, slightly curved s shape, but with 2 distinct regions
# Aggregate data for the year so there is 1 value per location for the year
yearly_means <- data_2024 %>%
  group_by(Lat, Lon) %>%
  summarise(across(c(Temp, Elev), mean, na.rm = TRUE))
plot(yearly_means$Elev, yearly_means$Temp, xlab = 'Elevation (m)', ylab = 'Temperature (C)', main = 'Year-Average Temperature vs. Elevation: 2024')

# Create a function to generate a continuous color palette (e.g., red to blue)
rbPal <- colorRampPalette(c('red','blue'))
yearly_means$Col <- rbPal(10)[as.numeric(cut(abs(yearly_means$Lat), breaks = 10))]
plot(yearly_means$Elev, yearly_means$Temp, col = yearly_means$Col)

# Following appears to be Antarctica
lower_arm <- yearly_means[which(yearly_means$Elev >= 3000 & yearly_means$Temp < -30), ]
# Following appears to be the Himalayas primarily, with some of the Chilean/Bolivian Sierras
upper_arm <- yearly_means[which(yearly_means$Elev >= 3000 & yearly_means$Temp > -10), ]
# Following appears to be Greenland
small_arm <- yearly_means[which(yearly_means$Elev >= 2800 & yearly_means$Temp > -25 & yearly_means$Temp < -18), ]
# Try adding a color code with Latitude


###### Latitude ######
plot(yearly_means$Lat, yearly_means$Temp)


###### Latitude * Week ######
# Aggregate by Longitude so we can see a weekly average at each Latitude 
lat_means <- data_2024 %>%
  group_by(Week, Lat) %>%
  summarise(across(c(Temp), mean, na.rm = TRUE))
ggplot(lat_means, aes(x = Week, y = Lat, color = Temp)) +
  geom_point() +
  scale_color_gradient2(low = "blue", mid = "yellow", high = "red",
                        midpoint = -20, name = "Temperature (°C)")
# Northern hemisphere is High-Low-High
# Southern is Low-High-Low
# Middle region is relatively flat



###### NDVI ######
plot(data_2024$NDVI, data_2024$Temp)


mod <- lm(Temp ~ Lat + I(Lat^2) + ns(Elev, df = 3) + Week + Elev:Lat + Week:Lat, data = data_2024)
AIC(mod) # 23258004

mod <- lm(Temp ~ Lat + I(Lat^2) + ns(Elev, df = 3) + ns(Week, df = 5) + Elev:Lat +ns(Week, df = 5):Lat, data = data_2024)
AIC(mod) # 21140430

mod <- lm(Temp ~ Lat + I(Lat^2) + ns(Elev, df = 3) +  Elev:Lat + bSpline(Week, df = 10, Boundary.knots = c(0, 52), periodic = TRUE)*Lat*Elev, data = data_2024)
AIC(mod) # 20860650

mod <- lm(Temp ~ Lat + (I(Lat^2) + ns(Elev, df = 3) +  bSpline(Week, df = 10, Boundary.knots = c(0, 52), periodic = TRUE))^2 + bSpline(Week, df = 10, Boundary.knots = c(0, 52), periodic = TRUE)*Lat*Elev, data = data_2024)
red_mod <- step(mod, direction = "both")
AIC(red_mod) # 20418279



dm <- model.matrix(mod)
plot(dm[, 4], dm[, 6])

# Get relevant data
week_1_ind <- which(data_2024$Week == '1')
week_1_2024 <- data_2024[week_1_ind, ]
# Convert to -180 to 180 longitude frame
week_1_2024 <- week_1_2024 %>% mutate(lon_adj = ifelse(Lon > 180, Lon - 360, Lon))
stuff_to_plot <- cbind(week_1_2024$Lat, week_1_2024$lon_adj, mod$residuals[week_1_ind])
colnames(stuff_to_plot) <- c('Lat', 'Lon', 'Residuals')
world_map <- map_data("world")

  
# Temperature
ggplot() +
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
  labs(title = "Global Residuals Map")

