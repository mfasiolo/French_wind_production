library(readr)
library(dplyr)

source("R/combine_data_funcs.R")

# Look at names of regions
reg_names <- read_csv("production/wind_installed_capacity_2018-2023.csv",
         locale = locale(tz = "UTC")) |> 
  names()

reg_names[-1]

# Pick a region, e.g. Grand Est
region <- "Grand Est"

# Use combine reg ave to load production and region averages for this region
GE_dat <- load_region_ave(region)


# Alternatively, if other summaries of regional data are to be combined,
# can use combine_data directly (replace wind_dat with preferred data for region)
wind_dat <- read_csv(paste0("Wind_data/Regional_wind_data/Grand.Est_wind_data.csv"),
                     locale = locale(tz = "UTC"))
GE_dat <- combine_data(region, wind_dat)
