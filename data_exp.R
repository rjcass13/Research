library(fields)
library(ggplot2)
library(dplyr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

dt <- read.csv('agg_1x1_2015_2024_temp_precip.csv')

# Location Plots
if (FALSE) {
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


# World Map with Overlay
if (TRUE) {
  # Get relevant data
  data_2024 <- read.csv('stats-2024.csv')
  week_1_2024 <- data_2024[which(data_2024$Year_Week == '2024-1'), ]
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
    geom_tile(data = week_1_2024, aes(x = lon_adj, y = Lat, fill = Temp)) +
    # Add world map outlines on top
    geom_polygon(data = world_map, aes(x = long, y = lat, group = group),
                fill = NA, color = "gray40", linewidth = 0.2) +
    # Color scale for temperature
    scale_fill_gradient2(low = "blue", mid = "yellow", high = "red",
                        midpoint = 10, name = "Temperature (°F)") +
    # Set aspect ratio
    coord_fixed(1) +
    # Clean theme
    theme_void() +
    labs(title = "Global Temperature Map")
}




############ ToDo ############
# Get elevation data aggregated

# Research question: 
# Model temperature as a function of Lat/Elev/Time
# Look at different splits of the dependents, etc.
# after accounting for latitude and time, is the effect of elevation linear or not
# What is the explanation of why temperatures are colder at higher elevations?
# Think about what might be the physical interpretations of WHY things interact the way they do
### Do a little write-up about all the actual physical features that appear to be important
### Do a little write-up about all the statistical features that appear to be important
