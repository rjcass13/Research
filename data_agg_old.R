library(ncdf4)

# Vars
# Temperature # Degrees Kelvin
# Longitude # degrees east
# Latitude # degrees north
# Time # days since start of year


# Get temperature data
temp_data <- nc_open('daily_stats_2024_temp.nc')
# Get the number of days for the year
n_days <- temp_data$dim$valid_time$len
temp <- ncvar_get(temp_data, "t2m") # Degrees Kelvin
temp <- temp - 273.15 # Convert to Celsius
nc_close(temp_data)


# Get Precipitation data
precip_data <- nc_open('daily_stats_2024_precip.nc')
precip <- ncvar_get(precip_data, "tp") # Total Precipitation
nc_close(precip_data)

temp[1, 1, 1]




for (year in years) {

  # Set the starting week parameters
  week_val <- 1
  for (week_ind in 1:52) {
    # Define the range of indices to average over for week
    week_ind_range <- (week_ind * 7 - 6):ifelse(week_ind == 52, n_days, (week_ind * 7))
    
    # Define the starting latitude parameters
    lat_val <- -89.5
    year_week <- paste0(year, '-', week_val)
    # Predefine a matrix to hold all the data for one week
    data_week <- matrix(NA, 0, ncols) 
    for (lat_ind in 1:180) {
      # Define the range of indices to average over for Latitude
      lat_ind_range <- (lat_ind * 4 - 3):ifelse(lat_ind == 180, 721, (lat_ind * 4))

      lon_val <- .5
      data_lat <- matrix(NA, 360, ncols) 
      for (lon_ind in 1:360) {
        # Define the range of indices to average over for Longitude
        lon_ind_range <- (lon_ind * 4 - 3):(lon_ind * 4)
        
        # Get the average temp and precip for this combo of week/lat/lon
        temp_avg <- mean(temp[lon_ind_range, lat_ind_range, week_ind_range])
        precip_avg <- mean(precip[lon_ind_range, lat_ind_range, week_ind_range])

        # Store the average values with their corresponding lat/lon/week values
        data_lat[lon_ind,] <- c(lat_val, lon_val, year_week, temp_avg, precip_avg)

        lon_val <- lon_val +1
      }

      # Take the data for that latitude and add it to the week's data and clear the memory
      data_week <- rbind(data_week, data_lat)
      rm(data_lat)
      
      lat_val <- lat_val + 1
    }

    # Add the week's data to the overall dataset and clear the memory
    data_red <- rbind(data_red, data_week)
    rm(data_week)


    if (week_val %% 10 == 0) { cat("Done: Week", week_val, '\n') }
    week_val <- week_val + 1
  }
}




