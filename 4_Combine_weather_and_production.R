source("R/combine_data_funcs.R")



read_csv("production/wind_installed_capacity_2018-2023.csv",
         locale = locale(tz = "UTC")) |> 
  names()


wind_dat <- read_csv("temp/PACA_wind_data.csv",
                     locale = locale(tz = "UTC"))
region <- "PACA"



dat <- combine_data(region, wind_dat)