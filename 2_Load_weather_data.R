#############
# Example of loading only a part of the weather data grid
#############
library(raster)
library(dplyr)

setwd("~/Desktop/All/Dropbox/Work/Projects/Data/EDF_Wind")

load("Wind_data/coordinates.RData")

files_weather <- list.files("Wind_data/Processed_weather_data")

my_grid <- dplyr::filter(coord, lon < 0 & lat < 45)

# Load only the grid points we want
my_dat <- lapply(1:nrow(my_grid), function(ii){
  load(paste0("Wind_data/Processed_weather_data/", files_weather[ii]))
  return(tmp)
})

# Convert list into a single data frame containing all the data
nams <- c("u10", "v10", "u100", "v100")
for(ii in nams){
  my_dat[[1]][[ii]] <- sapply(my_dat, function(o) o[[ii]])
}
my_dat <- my_dat[[1]]
# Now each the columns u10, v10, u100, v100 are matrices containing the gridded
# forecast for 1 time step, for all selected grid points

# Check visually this is correct
min_lat <- min(my_grid$lat)
max_lat <- max(my_grid$lat)
min_lon <- min(my_grid$lon)
max_lon <- max(my_grid$lon)

nlat <- length( table(my_grid$lat) )
nlon <- length( table(my_grid$lon) )

par(mfrow = c(2, 2), mar = c(1, 1, 1, 1))
nams <- c("u10", "v10", "u100", "v100")
for(ii in nams){
  rast <- raster(matrix(my_dat[[ii]][1, ], nlat, nlon, byrow = TRUE), xmn=min_lon, xmx=max_lon, ymn=min_lat, ymx=max_lat)
  plot(rast, col = hcl.colors(100, "terrain"))
}