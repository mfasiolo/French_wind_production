#############
# Example of loading only a part of the weather data grid
#############
library(raster)
library(dplyr)

setwd("~/Desktop/All/Dropbox/Work/Projects/Data/EDF_Wind")

load("Wind_data/coordinates.RData")

nfil <- nrow(coord)

my_grid <- dplyr::filter(coord, lat > 47 & lon < 0)

# Load only the grid points we want
kk <- 1
my_dat <- lapply(my_grid$index, function(ii){
  if( (kk %% 10) == 0 ){ cat(".") }
  if( (kk %% 100) == 0 ){ cat("\n") }
  load(paste0("Wind_data/Processed_weather_data/", "w_dat_", ii, ".RData"))
  kk <<- kk + 1
  return(tmp)
})

# Convert list into a single data frame containing all the data
n <- nrow(my_dat[[1]])
big_dat <- data.frame("date" = my_dat[[1]]$date,
                      "u10" = rep(NA, n), "v10" = rep(NA, n), "u100" = rep(NA, n), "v100" = rep(NA, n))
nams <- c("u10", "v10", "u100", "v100")
for(ii in nams){
  cat(".")
  big_dat[[ii]] <- sapply(my_dat, function(o) o[[ii]])
}
rm(my_dat)
gc()
# Now each the columns u10, v10, u100, v100 are matrices containing the gridded
# forecast for 1 time step, for all selected grid points

# Check visually this is correct
min_lat <- min(my_grid$lat)
max_lat <- max(my_grid$lat)
min_lon <- min(my_grid$lon)
max_lon <- max(my_grid$lon)

nlat <- length( unique(my_grid$lat) )
nlon <- length( unique(my_grid$lon) )

par(mfrow = c(2, 2), mar = c(1, 1, 1, 1))
nams <- c("u10", "v10", "u100", "v100")
for(ii in nams){
  rast <- raster(matrix(big_dat[[ii]][1, ], nlat, nlon, byrow = TRUE), xmn=min_lon, xmx=max_lon, ymn=min_lat, ymx=max_lat)
  plot(rast, col = hcl.colors(100, "terrain"))
}

# big_dat is the data we can use for modelling