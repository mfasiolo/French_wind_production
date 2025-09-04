library(sf)
library(dplyr)
library(purr)
library(tibble)
library(ggplot2)
load("Geo_data/coordinates.RData")

file_path <- "./Geo_data/fr.json"

target_id <- "FRPAC" # PACA region

france_data <- st_read(file_path) # read France GeoJSON

frpac_region <- france_data %>%
  filter(id == target_id) # subset PACA region

print(frpac_region)

bbox <- st_bbox(frpac_region)

bbox_polygon <- st_as_sfc(bbox)

ggplot() +
  geom_sf(data = frpac_region, fill = "skyblue", alpha = 0.7) +
  geom_sf(data = bbox_polygon, fill = NA, color = "red", linewidth = 1.2) +
  labs(
    title = "Map of FRPAC Region with Bounding Box",
    subtitle = "Provence Alpes Côte d'Azu, France",
    caption = "Data source: fr.json"
  ) +
  theme_minimal()


grid_sf <- st_as_sf(coord, coords = c("lon", "lat"), crs = 4326) # convert into an sf object

points_inside <- grid_sf[frpac_region, ]  # spatially intersect to find the points in geo_grid that falls in the PACA region

ggplot() +
  geom_sf(data = frpac_region, fill = "skyblue", alpha = 0.7) +
  geom_sf(data = bbox_polygon, fill = NA, color = "red", linewidth = 1.2) +
  geom_sf(data = points_inside, color = "navy", size = 0.5, shape = 19)+
  labs(
    title = "Map of FRPAC Region with Bounding Box",
    subtitle = "Provence Alpes Côte d'Azu, France",
    caption = "Data source: fr.json"
  ) +
  theme_minimal()


# function to search for wind_data files for points fall in a particular region and average u10, v10, u100, and v100 for each time stamp.
# Input: region that you are interested in, path to wind_data i.e. "./Wind_data/Processed_weather_data/"

region_ave <- function(region, path_to_wind_data) {
  geo_grid <- expand.grid(lon = longitude, lat = latitude) %>%
    mutate(index = row_number())
  
  grid_sf <- st_as_sf(geo_grid, coords = c("lon", "lat"), crs = 4326)
  points_inside <- grid_sf[region, ]
  
  indices_to_average <- points_inside$index
  filepaths <- file.path(path_to_wind_data, paste0("w_dat_", indices_to_average, ".RData"))
  list_to_average <- map(filepaths, ~ get(load(.x)))
  
  averaged_df <- map_dfr(list_to_average, ~ rowid_to_column(.x, "row_id")) %>%
    group_by(row_id) %>%
    summarise(
      across(c(u10, v10, u100, v100), ~ mean(.x, na.rm = TRUE)),
      datetime = first(datetime),
      .groups = "drop"
    ) %>%
    select(-row_id)
  
  return(averaged_df)
}

path_to_wind_data <- "./Wind_data/Processed_weather_data"

