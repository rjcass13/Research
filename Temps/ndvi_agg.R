library(ncdf4)

#################################
#### Aggregate the NDVI data ####
#################################
# First half of year
# Get NDVI data
file <- paste0('ndvi_2022_jan_jun.nc4')
ndvi_data <- nc_open(file)
ndvi <- ncvar_get(ndvi_data, "ndvi") # NDVI

# NDVI Dataset: On the .0833 (1/12) Lat/Lon scale 
# 2 data points per month (total of 12 per dataset)

nc_close(ndvi_data)


data_red <- matrix(NA, 0, 4) 
# Set the starting week parameters
for (week_val in 1:26) {
  # Define the range of indices to average over for week
  week_ind_range <- ceiling(week_val*12/26)
  
  # Define the starting latitude parameters
  lat_val <- 89.5
  #year_week <- paste0(year, '-', week_val)
  # Predefine a matrix to hold all the data for one week
  data_week <- matrix(NA, 0, 4) 
  for (lat_ind in 1:180) {
    # Define the range of indices to average over for Latitude
    lat_ind_range <- (lat_ind * 12 - 11):(lat_ind * 12)

    lon_val <- -179.5
    data_lat <- matrix(NA, 360, 4) 
    for (lon_ind in 1:360) {
      # Need to address how the rows get filled to match the same structure as the other tables (0.5:179.5, -179.5:-0.5)
      if (lon_val <= 0) {
        lon_row_ind <- 360 - abs(ceiling(lon_val))
      } else {
        lon_row_ind <- ceiling(lon_val)
      }

      # Define the range of indices to average over for Longitude
      lon_ind_range <- (lon_ind * 12 - 11):(lon_ind * 12)
      
      # Get the average temp and precip for this combo of week/lat/lon
      ndvi_avg <- mean(ndvi[lon_ind_range, lat_ind_range, week_ind_range], na.rm = TRUE)

      if(is.na(ndvi_avg)){ndvi_avg <- -5000}

      # Store the average values with their corresponding lat/lon/week values
      data_lat[lon_row_ind,] <- c(lat_val, lon_val, week_val, ndvi_avg)

      lon_val <- lon_val +1
    }

    # Take the data for that latitude and add it to the week's data and clear the memory
    data_week <- rbind(data_week, data_lat)
    rm(data_lat)
    
    lat_val <- lat_val - 1
  }

  # Add the week's data to the overall dataset and clear the memory
  data_red <- rbind(data_red, data_week)
  rm(data_week)


  if (week_val %% 10 == 0) { cat("Done: Week", week_val, '\n') }
}

############################

# Second half of year
# Get NDVI data
file <- paste0('ndvi_2022_jul_dec.nc4')
ndvi_data <- nc_open(file)
ndvi <- ncvar_get(ndvi_data, "ndvi") # NDVI

# NDVI Dataset: On the .0833 (1/12) Lat/Lon scale 
# 2 data points per month (total of 12 per dataset)
nc_close(ndvi_data)

# Set the starting week parameters
for (week_val in 1:26) {
  # Define the range of indices to average over for week
  week_ind_range <- ceiling(week_val*12/26)
  week_val <- week_val + 26
  
  # Define the starting latitude parameters
  lat_val <- 89.5
  #year_week <- paste0(year, '-', week_val)
  # Predefine a matrix to hold all the data for one week
  data_week <- matrix(NA, 0, 4) 
  for (lat_ind in 1:180) {
    # Define the range of indices to average over for Latitude
    lat_ind_range <- (lat_ind * 12 - 11):(lat_ind * 12)

    lon_val <- -179.5
    data_lat <- matrix(NA, 360, 4) 
    for (lon_ind in 1:360) {
      # Need to address how the rows get filled to match the same structure as the other tables (0.5:179.5, -179.5:-0.5)
      if (lon_val <= 0) {
        lon_row_ind <- 360 - abs(ceiling(lon_val))
      } else {
        lon_row_ind <- ceiling(lon_val)
      }

      # Define the range of indices to average over for Longitude
      lon_ind_range <- (lon_ind * 12 - 11):(lon_ind * 12)
      
      # Get the average temp and precip for this combo of week/lat/lon
      ndvi_avg <- mean(ndvi[lon_ind_range, lat_ind_range, week_ind_range], na.rm = TRUE)

      if(is.na(ndvi_avg)){ndvi_avg <- -5000}

      # Store the average values with their corresponding lat/lon/week values
      data_lat[lon_row_ind,] <- c(lat_val, lon_val, week_val, ndvi_avg)

      lon_val <- lon_val +1
    }


    
    # Take the data for that latitude and add it to the week's data and clear the memory
    data_week <- rbind(data_week, data_lat)
    rm(data_lat)
    
    lat_val <- lat_val - 1
  }

  # Add the week's data to the overall dataset and clear the memory
  data_red <- rbind(data_red, data_week)
  rm(data_week)


  if (week_val %% 10 == 0) { cat("Done: Week", week_val, '\n') }
}


data_red <- as.data.frame(data_red)
colnames(data_red) <- c('Lat', 'Lon', 'Week', 'NDVI')

file_name <- paste0('stats_ndvi.csv')
write.csv(data_red, file = file_name, row.names = FALSE)
