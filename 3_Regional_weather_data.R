library(sf)
library(dplyr)
library(purrr)
library(tibble)
library(ggplot2)
library(geosphere)
library(readr)
library(lubridate)
library(spatstat)
library(raster)
library(concaveman) #for concave hull
library(spatstat)
load("Geo_data/coordinates.RData")

file_path <- "./Geo_data/fr.json"

france_data <- st_read(file_path) # read France GeoJSON

france_data <- france_data %>%
  mutate(region_code = case_when(
    name == "Île de France"            ~ 11,
    name == "Centre Val de Loire"      ~ 24,
    name == "Bourgogne Franche Comté"  ~ 27,
    name == "Normandie"                ~ 28,
    name == "Hauts de France"          ~ 32,
    name == "Grand Est"                ~ 44,
    name == "Pays de la Loire"         ~ 52,
    name == "Bretagne"                 ~ 53,
    name == "Nouvelle Aquitaine"       ~ 75,
    name == "Occitanie"                ~ 76,
    name == "Auvergne Rhône Alpes"     ~ 84,
    name == "Provence Alpes Côte d'Azu" ~ 93,
    name == "Corse"                    ~ 94
  ))


# Wind farms------
wind_farm <- read.csv("./Geo_data/wind_farms_france.csv")
unique(wind_farm$code_reg)
# 76 27 75 44 84 32 28 53 52 93 11 24 94

wind_farm[wind_farm$etat_parc=="En démantèlement",]

## Wind farm data processing ------------
wind_farm <- wind_farm %>%
  group_by(id_parc) %>%
  slice_head(n = 1) %>%
  ungroup()

wind_farm[wind_farm$nom_usuel=="Fitou","date_mise_en_service"] <- "2006-01-01" #dont know the exact date, but before 2008
wind_farm_inService <- wind_farm[wind_farm$statut_parc != "En instruction" & wind_farm$etat_parc!= "En attente de construction"& wind_farm$etat_parc!="En construction"& wind_farm$etat_parc!="",]
wind_farm_inService$date_mise_en_service <- as.POSIXct(wind_farm_inService$date_mise_en_service,tz="UTC")




  
  
# function to search for wind_data files for points fall in a particular region
# Input: region that you are interested in, path to wind_data i.e. "./Wind_data/Processed_weather_data/"
# coord is a data set contains columns: index, lon and lat
# if exact_region=FALSE, it uses points inside the smallest rectangle that includes the region



regional_all_data <- function(region, path_to_wind_data, coord,
                              method = "exact",
                              wind_farm_data = NULL, region_code = NULL, wf_method = NULL) {
  if (!require(sf)) stop("sf package is required.")
  if (!require(dplyr)) stop("dplyr package is required.")
  if (!require(purrr)) stop("purrr package is required.")
  if (!require(concaveman)) stop("concaveman package is required.")
  if (!require(spatstat)) stop("spatstat package is required for the 'kde' method.")
  if (!require(raster)) stop("raster package is required for the 'kde' method.")
  
  grid_sf <- st_as_sf(coord, coords = c("lon", "lat"), crs = 4326)
  weights <- NULL
  
  if (method == "exact") {
    points_inside <- grid_sf[region, ]
  } else if (method == "bbox") {
    bbox <- st_bbox(region)
    bbox_polygon <- st_as_sfc(bbox)
    points_inside <- grid_sf[bbox_polygon, ]
  } else if (method == "wind_farm") {
    if (is.null(wind_farm_data) || is.null(region_code) || is.null(wf_method)) {
      stop("For 'wind_farm' method, provide 'wind_farm_data', 'region_code', and a 'wf_method' ('convex', 'concave', or 'kde').")
    }
    region_farms <- wind_farm_data %>%
      filter(code_reg %in% region_code)

    region_farms_sf <- st_as_sf(region_farms,
                                coords = c("x_parc", "y_parc"),
                                crs = 4326)
    if(wf_method == "convex"){
      polygon_buffer <- st_convex_hull(st_union(region_farms_sf))
      points_inside <- grid_sf[polygon_buffer, ] 
    }else if(wf_method == "concave"){
      polygon_buffer <- concaveman(region_farms_sf)
      polygon_buffer <- st_make_valid(polygon_buffer)
      points_inside <- grid_sf[polygon_buffer, ]
    }  else if (wf_method == "kde") {
      region_farms_proj <- st_transform(region_farms_sf, crs = 2154)
      grid_proj <- st_transform(grid_sf, crs = 2154)
      
      farm_coords <- st_coordinates(region_farms_proj)
      farm_power <- region_farms$puissance_parc_mw
      
      bbox_base <- st_bbox(region_farms_proj)
      bbox_expanded <- bbox_base
      bbox_expanded[c("xmin", "ymin")] <- bbox_expanded[c("xmin", "ymin")] - 50000
      bbox_expanded[c("xmax", "ymax")] <- bbox_expanded[c("xmax", "ymax")] + 50000
      
      ppp_base <- ppp(
        x = farm_coords[, "X"],
        y = farm_coords[, "Y"],
        window = as.owin(bbox_base),
        marks = farm_power
      )
      h_opt <- bw.ppl(ppp_base)
      
      kde <- density.ppp(ppp_base,
                         sigma = h_opt,
                         weights = marks(ppp_base))
      
      kde_raster <- raster(kde)
      raster::crs(kde_raster) <- sp::CRS(SRS_string = "EPSG:2154")
      
      bbox_poly <- st_as_sfc(bbox_expanded)
      points_inside <- st_filter(grid_proj, bbox_poly)
      
      if (nrow(points_inside) > 0) {
        weights <- raster::extract(kde_raster, points_inside)
        weights[is.na(weights)] <- 0
      }
    } else {
      stop("Invalid 'wf_method'. Please choose 'convex', 'concave', or 'kde'.")
    }
  } else {
    stop("Invalid method. Please choose one of: 'exact', 'bbox', or 'wind_farm'.")
  }
  
  indices_to_average <- points_inside$index
  filepaths <- file.path(path_to_wind_data, paste0("w_dat_", indices_to_average, ".RData"))
  list_regional_data <- map(filepaths, ~ get(load(.x)))
  
  return(list(data = list_regional_data, weights = weights))
}



# Given the list of files found for the region, compute average u10, v10, u100, and v100 for each time stamp and output a regional average data
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
    dplyr::select(-row_id)
  
  return(averaged_df)
}


### Produce exact regional average ----

output_dir <- "Wind_data/Regional_wind_data"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

path_to_wind_data <- "Wind_data/Processed_weather_data"
  
# loop over 12 France regions (exclude Corse) and save a data frame for each region
# we ensure that file names are consistent with prod data column names
for (region_name in france_data$name) {
  # skip Corse region
  if (region_name == "Corse") {
    next 
  }
  current_region_sf <- france_data[france_data$name == region_name, ]
  
  list_regional_data <- regional_all_data(current_region_sf, path_to_wind_data, coord)
  averaged_wind_data <- region_ave(list_regional_data$data)
  
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



### process wind speed data with wind farm info with convex hull----

output_dir <- "Wind_data/Regional_wind_data_wind_farms_convex"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

for (region_name in france_data$name) {
  # skip Corse region
  if (region_name == "Corse") {
    next
  }
  region_code <- france_data$region_code[france_data$name == region_name]
  current_region_sf <- france_data[france_data$name == region_name, ]
  
  list_regional_data <- regional_all_data(current_region_sf, path_to_wind_data, coord,method = "wind_farm",wind_farm_data = wind_farm_inService,region_code = region_code,wf_method="convex")
  averaged_wind_data <- region_ave(list_regional_data$data)
  
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


### process wind speed data with wind farm info with concave hull----

output_dir <- "Wind_data/Regional_wind_data_wind_farms_concave"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

for (region_name in france_data$name) {
  # skip Corse region
  if (region_name == "Corse") {
    next
  }
  region_code <- france_data$region_code[france_data$name == region_name]
  current_region_sf <- france_data[france_data$name == region_name, ]
  
  list_regional_data <- regional_all_data(current_region_sf, path_to_wind_data, coord,method = "wind_farm",wind_farm_data = wind_farm_inService,region_code = region_code,wf_method="concave")
  averaged_wind_data <- region_ave(list_regional_data$data)
  
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


### process wind speed data with wind farm info with kde ----

output_dir <- "Wind_data/Regional_wind_data_wind_farms_kde"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# for this method, we combine Paris region with Centre Val de Loire
# There are warning messages for PACA region due to sparse points

codes_to_merge <- c(11, 24) 
merged_name <- "Region_11_24_Merged"
merged_code <- "11_24" 

merged_region_row <- france_data %>%
  filter(region_code %in% codes_to_merge) %>%
  summarise(geometry = st_union(geometry)) %>% # Dissolve the polygons
  mutate(
    name = merged_name,
    region_code = merged_code
  )

other_regions <- france_data %>%
  filter(!region_code %in% codes_to_merge)%>%
  mutate(region_code = as.character(region_code))

processed_france_data <- bind_rows(other_regions, merged_region_row)

for (i in 1:nrow(processed_france_data)) {
  
  # Get the data for the current iteration
  current_region_sf <- processed_france_data[i, ]
  region_name <- current_region_sf$name
  current_code <- current_region_sf$region_code
  
  # skip Corse region if it exists
  if (region_name == "Corse") {
    next
  }
  
  if (current_code == merged_code) {
    codes_for_function <- codes_to_merge 
  } else {
    codes_for_function <- as.numeric(current_code)
  }
  
  # Call the data processing function
  list_regional_data <- regional_all_data(
    current_region_sf, 
    path_to_wind_data, 
    coord,
    method = "wind_farm",
    wind_farm_data = wind_farm_inService,
    region_code = codes_for_function, # Use our logic from the if/else
    wf_method = "kde"
  )
  
  # Calculate the weighted average
  averaged_wind_data <- region_ave(
    list_regional_data$data, 
    use_weights = TRUE,
    weights = list_regional_data$weights
  )
  
  # Handle cases where no data was returned
  if (nrow(averaged_wind_data) == 0) {
    print(paste("Skipping", region_name, "- no data found."))
    next
  }
  
  # Format the file name and save the output
  formatted_name <- gsub(" ", ".", region_name)
  file_name <- paste0(formatted_name, "_wind_data.csv")
  full_path <- file.path(output_dir, file_name)
  
  write_csv(averaged_wind_data, full_path)
  print(paste("Successfully saved:", file_name))
}



