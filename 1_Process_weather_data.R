library(ncdf4)
library(tidyr)
library(readr)
library(zoo)
library(lubridate)
library(raster)
library(parallel)
library(abind)
library(dplyr)

source("R/weather_data_funcs.R")

################
# [1] Load and process data from the .nc files
################

# setwd("~/Desktop/All/Dropbox/Work/Projects/Data/EDF_Wind")

# Obtain long and lat coordinates of gridded weather forecast
file <- "Wind_data/HRES_France/CEP0125.201801010000.nc"
nc <- nc_open(file , write = FALSE)
longitude <- ncvar_get(nc , "longitude") 
latitude <- ncvar_get(nc , "latitude")  
nc_close(nc)

# Grid over which we want to extract the forecasts
geo_grid <- expand.grid(longitude, latitude)
geo_grid_idx <- expand.grid(1:length(longitude), 1:length(latitude))

nlat <- length(latitude)
nlon <- length(longitude)
ngrid <- nrow(geo_grid)

min_lat <- min(latitude)
max_lat <- max(latitude)
min_lon <- min(longitude)
max_lon <- max(longitude)

# Check that grid matches what "raster" expects
rast <- raster(matrix(geo_grid[ , 1], nlat, nlon, byrow = T), xmn=min_lon, xmx=max_lon, ymn=min_lat, ymx=max_lat)
plot(rast, col = hcl.colors(100, "terrain"))

rast <- raster(matrix(geo_grid[ , 2], nlat, nlon, byrow = T), xmn=min_lon, xmx=max_lon, ymn=min_lat, ymx=max_lat)
plot(rast, col = hcl.colors(100, "terrain"))

# NOTE: this takes while!
# Put weather data into a list of data.frames. One data.frame for each grid point, over for whole period.
# Set to FALSE if you are going to load "weather_data.RData"
ncores <- 24
cl <- makePSOCKcluster(ncores)
setDefaultCluster(cl)
clusterExport(NULL, c("longitude", "latitude", "geo_grid_idx"), envir = environment())
clusterEvalQ(NULL, {
  library(ncdf4)
  library(tidyr)
  library(readr)
  library(dplyr)
  library(zoo)
  library(lubridate)
  library(raster)
  source("R/weather_data_funcs.R")
})

nwf <- parLapply(NULL, 1:ngrid, function(ii){
  out <- get_weather_1_location_all_years(geo_grid_idx[ii, 1], geo_grid_idx[ii, 2], years = 2018:2023) 
  return(out)
})

stopCluster(cl)

# NOTE: DO NOT parallelise this, even on 2 cores, as it takes tons of memory.
# Create half-hourly weather data for each grid point by linear interpolation, for each point on the grid.
# Note that the interpolation is quite crude around midnight because we have predictions from 1am to 11pm, 
# which we interpolate to get 11:30pm, 12am and 12:30am (so the width of the interpolation interval is 2hour, rather
# than 2 as during the rest of the day). We could do better but we are being lazy.
all_w_data <- lapply(1:ngrid, function(ii){
  weather_dat <- nwf[[ii]]
  qhdt <- seq(from = weather_dat$datetime[1], to = weather_dat$datetime[nrow(weather_dat)], by = "15 min")
  qh_data <- dplyr::left_join(data.frame(datetime = qhdt), weather_dat, by = "datetime")
  qh_data <- zoo(qh_data[,-1], qh_data$datetime) |> 
    na.approx() |> 
    data.frame(datetime = qh_data$datetime)
  hh_data <- qh_data |> 
    filter(minute(datetime) %in% c(15, 45)) |> 
    mutate(datetime = datetime - minutes(15))
  return(hh_data)
})

# Check that the weather data looks ok visually
par(mfrow = c(3, 2), mar = c(1, 1, 1, 1))
nams <- c("u10", "v10", "u100", "v100")
for(ii in nams){
  tmp <- sapply(all_w_data, function(o) o[[ii]][1]) 
  rast <- raster(matrix(tmp, nlat, nlon, byrow = TRUE), xmn=min_lon, xmx=max_lon, ymn=min_lat, ymx=max_lat)
  plot(rast, col = hcl.colors(100, "terrain"))
}

# Save each grid point weather data into separate .RData files to avoid loading a single massive object
for(ii in 1:length(all_w_data)){
  tmp <- all_w_data[[ii]][ , c("datetime", "u10", "v10", "u100", "v100")]
  rownames(tmp) <- NULL
  save(tmp, file = paste0("Wind_data/Processed_weather_data/w_dat_", ii, ".RData"))
}

coord <- data.frame("index" = 1:length(all_w_data), "lon" = geo_grid[ , 1], "lat" = geo_grid[ , 2])
save(coord, file = "Wind_data/coordinates.RData")




