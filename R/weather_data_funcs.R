

# For each year in "year", get all intraday forecasts for one grid point
get_weather_1_location_all_years <- function(index_lon, index_lat, year) {
  list_of_df <- list()
  for (i in 1:length(year)) {
    list_of_df[[i]] <- get_weather_1_location_1_year(year[i], index_lon, index_lat)
  }
  out <- do.call("rbind", list_of_df)
  return(out)
}

# For each year, get all intraday forecasts (step < 24) for one grid point
# Note: we'll get 23 forecasts from 1am to 11pm. We do NOT get a midnight forecast, we'll get by interpolation.
# (one could do things better)
get_weather_1_location_1_year <- function(year, index_lon, index_lat) {
  
  files <- list.files(paste0("Wind_data/HRES_France/", year))
  list_of_forecasts <- list()
  start_dates <- as_datetime(seq(as.Date(paste0(year, "-01-01")), as.Date(paste0(year, "-12-31")), by="day"))
  
  # Loop over files containing forecasts for each day, for the chosen year
  for (i in 1:length(files)) {
    nc <- nc_open(paste0("Wind_data/HRES_France/", year,"/",files[i]), write = FALSE)
    df <- get_weather_1_location_1_day(nc, index_lon, index_lat, start_dates[i])
    df <- df[df$step < 24,]
    list_of_forecasts[[i]] <- df
    nc_close(nc)
  }
  
  out <- do.call("rbind", list_of_forecasts)
  
  return(out)
}



# Get weather forecast from one grid point, for one day (one .nc file) at all available lead times ("step")
get_weather_1_location_1_day <- function(nc, index_lon, index_lat, start_date) {

  step = ncvar_get(nc , "step")
  
  # Wind speed 10m
  u10 <- ncvar_get(nc , "u10" , c(index_lon, index_lat, 1), c(1, 1, length(step)))
  v10 <- ncvar_get(nc , "v10" , c(index_lon, index_lat, 1), c(1, 1, length(step)))
  
  # Wind speed 100m
  u100 <- ncvar_get(nc , "u100" , c(index_lon, index_lat, 1), c(1, 1, length(step)))
  v100 <- ncvar_get(nc , "v100" , c(index_lon, index_lat, 1), c(1, 1, length(step)))
  
  out <- data.frame("datetime" = start_date + hours(step), 
                    "step" = step, 
                    "u10" = u10, "v10" = v10,
                    "u100" = u100, "v100" = v100)
  
  return(out)
  
}




