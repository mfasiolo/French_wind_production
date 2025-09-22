# function to combine weather data with production data in a given region
# region should follow naming of columns in production
combine_data <- function(region, weather_data) {

  # load capacity and filter by region
  region_cap <- read_csv("production/wind_installed_capacity_2018-2023.csv",
                       locale = locale(tz = "UTC")) |>
    rename(datetime = Date) |>
    select("datetime", paste0(region)) |>
    rename(capacity = paste0(region))

  # load production, filter by region, and combine with weather and capacity
  # discarding missing values
  region_dat <- read_csv("production/wind_regional_production_2018-2023.csv",
                       locale = locale(tz = "UTC")) |>
    rename(datetime = Date) |>
    select("datetime", paste0(region)) |>
    rename(production = paste0(region)) |>
    inner_join(region_cap, by = "datetime") |>
    inner_join(weather_data, by = "datetime")

  region_dat$prod_div_cap <- region_dat$production / region_dat$capacity

  region_dat$time <- format(region_dat$datetime, format = "%H:%M")
  region_dat$toy <- yday(region_dat$datetime)
  region_dat$tod <- hour(region_dat$datetime) + minute(region_dat$datetime)/60
  region_dat$trend <- as.numeric(difftime(region_dat$datetime, region_dat$datetime[1], units = "days"))


  region_dat$windspeed10 <- sqrt(region_dat$u10^2 + region_dat$v10^2)
  region_dat$windspeed100 <- sqrt(region_dat$u100^2 + region_dat$v100^2)

  region_dat$direction10 <- atan2(region_dat$u10, region_dat$v10) * 180 / pi %% 360
  region_dat$direction100 <- atan2(region_dat$u100, region_dat$v100) * 180 / pi %% 360

  return(region_dat)
}




# Function to output combined production and regional weather averages given just  region
load_region_ave <- function(region) {
  # load regional weather data
  region_file_name <- gsub("-", ".", gsub(" ", ".", region))
  wind_dat <- read_csv(paste0("Wind_data/Regional_wind_data/", region_file_name, "_wind_data.csv"),
                       locale = locale(tz = "UTC"))

  combine_data(region, wind_dat)
}
