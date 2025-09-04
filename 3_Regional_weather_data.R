library(sf)
library(dplyr)
library(purrr)
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

# coord is a data set contains index, lon and lat
regional_all_data <- function(region, path_to_wind_data, coord, exact_region = TRUE){
  grid_sf <- st_as_sf(coord, coords = c("lon", "lat"), crs = 4326)
  if(exact_region == TRUE){
    points_inside <- grid_sf[region, ]
    indices_to_average <- points_inside$index
  }else{
    bbox <- st_bbox(region)
    bbox_polygon <- st_as_sfc(bbox)
    points_inside <- grid_sf[bbox_polygon, ]
  }
  filepaths <- file.path(path_to_wind_data, paste0("w_dat_", indices_to_average, ".RData"))
  list_regional_data <- map(filepaths, ~ get(load(.x)))
  return(list_regional_data)
}

region_ave <- function(list_regional_data, use_weights = FALSE, weights = NULL){
  if (use_weights == TRUE && is.null(weights)) {
    stop("To use weighted average, you must provide a 'weights' vector.")
  }
  if (!is.null(weights) && length(list_regional_data) != length(weights)) {
    stop("The number of weights must equal the number of data frames.")
  }
  averaged_df <- map_dfr(list_regional_data, ~ rowid_to_column(.x, "row_id")) %>%
    group_by(row_id) %>%
    summarise(
      across(
        c(u10, v10, u100, v100),
        ~ if (use_weights) {
          weighted.mean(.x, w = weights, na.rm = TRUE)
        } else {
          mean(.x, na.rm = TRUE)
        }
      ),
      datetime = first(datetime),
      .groups = "drop"
    ) %>%
    select(-row_id)
  
  return(averaged_df)
}

path_to_wind_data <- "./Wind_data/Processed_weather_data"

list_regional_data <- regional_all_data(frpac_region, path_to_wind_data,coord)
PACA_wind_data <- region_ave(list_regional_data)


library(readr)

output_dir <- "Wind_data/Regional_wind_data"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

for (region_name in france_data$name) {
  # skip Corse region
  if (region_name == "Corse") {
    next 
  }
  current_region_sf <- france_data[france_data$name == region_name, ]
  
  list_regional_data <- regional_all_data(current_region_sf, path_to_wind_data, coord)
  averaged_wind_data <- region_ave(list_regional_data)
  
  # file name consistant with prod data
  if (region_name == "Provence Alpes Côte d'Azu") {
    file_name <- "PACA_wind_data.csv"
  } else if (region_name == "Île de France") {
    file_name <- "Ile.de.France_wind_data.csv"
  } else {
    formatted_name <- gsub(" ", ".", region_name)
    file_name <- paste0(formatted_name, "_wind_data.csv")
  }
  

  full_path <- file.path(output_dir, file_name)
  
  write_csv(averaged_wind_data, full_path)
  print(paste("Successfully saved:", file_name))
}

