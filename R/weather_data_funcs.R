

# For each year in "years", get all next day forecasts for one grid point
get_weather_1_location_all_years <- function(index_lon, index_lat, years, min_step = 24) {
  
  # get all file names corresponding to years specified
  # and sort so they correspond to date-time order
  files <- sapply(
    years, 
    \(y) list.files("Wind_data/HRES_France", pattern = paste0("CEP0125.", y))
  ) |> 
    unlist() |> 
    sort()
  
  list_of_forecasts <- list()
  
  # loop over all dates within years
  start_dates <- seq(
    as.Date(paste0(min(years),"-01-01")), 
    as.Date(paste0(max(years),"-12-31")), 
    by="day"
  ) |> 
    as_datetime()
  
  # Files and start times might be misaligned if some are missing
  if (length(files) != length(start_dates)) stop("Files don't cover whole date range")
  
  # Loop over files containing forecasts for each day
  # Should loop over all date-times 
  for (i in 1:length(files)) {
    # open file corresponding to date-time i
    nc <- nc_open(paste0("Wind_data/HRES_France/", files[i]), write = FALSE)
    
    # get weather forecasts from date_time + min_step to 24 hours later
    df <- get_weather_1_location_1_day(nc, index_lon, index_lat, start_dates[i]) |> 
      filter(step >= min_step & step < min_step + 24)
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




