# Built following this example: https://rokuk.org/projects/climateviz/
# Animation: https://www.r-bloggers.com/2021/05/animated-graph-gif-with-gganimate-ggplot/

library(ncdf4)
library(ggplot2)
library(gganimate)
library(gifski)
library(reshape2)

# # Open file
# data <- nc_open("daily_stats_2024_temp.nc")
# print(data)

# # Get vars
# Temperature # Degrees Kelvin
# Longitude # degrees east
# Latitude # degrees north
# Time # days since start of year

# # Close file
# nc_close(data)


# # Provo: 40.25 N, 248.25 E (111.75 W)
# start_date <- as.Date("2024-01-01")

# provo_temp <- temp[248.25*4 + 1, 40.25*4 + 1, ]
# plot(time + start_date, provo_temp)



years <- c('2024', '2023', '2022', '2021', '2020', '2019', '2018', '2017', '2016', '2015')

# Predefine the destination matrix columns
ncols <- 5
data_red <- matrix(NA, 0, ncols) 

for (year in years) {
  cat('Processing', year, '\n')
  
  # Get temperature data
  file <- paste0('daily_stats_', year, '_temp.nc')
  temp_data <- nc_open(file)
  # Get the number of days for the year
  n_days <- temp_data$dim$valid_time$len
  temp <- ncvar_get(temp_data, "t2m") # Degrees Kelvin
  nc_close(temp_data)

  cat('Temp data read', '\n')

  # Get Precipitation data
  file <- paste0('daily_stats_', year, '_precip.nc')
  precip_data <- nc_open(file)
  precip <- ncvar_get(precip_data, "tp") # Total Precipitation
  nc_close(precip_data)

  cat('Precip data read', '\n')

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

  rm(temp)
  rm(precip)
  rm(temp_data)
  rm(precip_data)
  cat(year, 'complete', '\n')
}

colnames(data_red) <- c('Lat', 'Lon', 'Year_Week', 'Temp', 'Precip')
write.csv(data_red, file = "agg_1x1_2015_2024_temp_precip.csv", row.names = FALSE)
Sys.time()
object_size_bytes <- object.size(data_red)
print(format(object_size_bytes, units = "auto"))



