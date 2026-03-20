library(ncdf4)

#########################################
#### Aggregate the Geopotential data ####
#########################################
data_red <- matrix(NA, 0, 3) 

# Get Geopotential data
file <- paste0('daily_stats_2024_geop.nc')
geop_data <- nc_open(file)
geop <- ncvar_get(geop_data, "z") # Geopotential
nc_close(geop_data)

# Define the starting latitude parameters
lat_val <- 89.5
for (lat_ind in 1:180) {
  # Define the range of indices to average over for Latitude
  lat_ind_range <- (lat_ind * 4 - 3):ifelse(lat_ind == 180, 721, (lat_ind * 4))

  lon_val <- .5
  data_lat <- matrix(NA, 360, 3) 
  for (lon_ind in 1:360) {
    # Define the range of indices to average over for Longitude
    lon_ind_range <- (lon_ind * 4 - 3):(lon_ind * 4)
    
    # Get the average temp and precip for this combo of week/lat/lon
    geop_avg <- mean(geop[lon_ind_range, lat_ind_range, 1])

    # Store the average values with their corresponding lat/lon/week values
    data_lat[lon_ind,] <- c(lat_val, lon_val, geop_avg)

    lon_val <- lon_val +1
  }

  # Take the data for that latitude and add it to the week's data and clear the memory
  data_red <- rbind(data_red, data_lat)
  rm(data_lat)
  
  lat_val <- lat_val - 1
}

# Convert Geopotential to Height (meters). Use this as reference: https://unidata.github.io/MetPy/latest/api/generated/metpy.calc.geopotential_to_height.html
geop <- data_red[, 3]
rE <- 6371000 # Average earth radius
g <- 9.80665 # Assumed ravitational constant
z <- (geop * rE)/(g * rE - geop) # Approxiamate height
data_red[ ,3] <- z

data_red <- as.data.frame(data_red)
colnames(data_red) <- c('Lat', 'Lon', 'Elev')

data_red$Lon <- ifelse(data_red$Lon > 180, data_red$Lon - 360, data_red$Lon)

file_name <- paste0('stats_elev.csv')
write.csv(data_red, file = file_name, row.names = FALSE)
