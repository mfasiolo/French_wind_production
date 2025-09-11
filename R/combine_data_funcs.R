# function to combine weather data with production data in a given region
combine_data <- function(region, weather_data) {
  
  # load capacity and filter by region
  region_cap <- readr::read_csv("production/wind_installed_capacity_2018-2023.csv",
                       locale = readr::locale(tz = "UTC")) |> 
    dplyr::rename(datetime = Date) |> 
    dplyr::select("datetime", paste0(region)) |> 
    dplyr::rename(capacity = paste0(region))
  
  # load production, filter by region, and combine with weather and capacity
  # discarding missing values
  region_dat <- readr::read_csv("production/wind_regional_production_2018-2023.csv",
                       locale = readr::locale(tz = "UTC")) |> 
    dplyr::rename(datetime = Date) |> 
    dplyr::select("datetime", paste0(region)) |> 
    dplyr::rename(production = paste0(region)) |> 
    dplyr::inner_join(region_cap, by = "datetime") |> 
    dplyr::inner_join(weather_data, by = "datetime")
  
  return(region_dat)
}
