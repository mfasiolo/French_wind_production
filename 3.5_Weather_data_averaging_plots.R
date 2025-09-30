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


region_code <- 75
target_id <- france_data$id[france_data$region_code==region_code]
target_name <- france_data$name[france_data$region_code==region_code]
interest_region <- france_data %>%
  filter(id == target_id) # subset PACA region

grid_sf <- st_as_sf(coord, coords = c("lon", "lat"), crs = 4326) # convert into an sf object

# Exact region-----
points_inside <- grid_sf[interest_region, ]  # spatially intersect to find the points in geo_grid that falls in the PACA region

ggplot() +
  geom_sf(data = interest_region, fill = "skyblue", alpha = 0.7) +
  geom_sf(data = points_inside, color = "navy", size = 0.5, shape = 19)+
  labs(
    title = "Map of Interested Region with Bounding Box",
    subtitle = target_name,
    caption = "Data source: fr.json"
  ) +
  theme_minimal()



# Circular buffer------

interest_region_proj <- st_transform(interest_region, crs = 2154)
grid_sf_proj <- st_transform(grid_sf, crs = 2154)

centroid_paca <- st_centroid(st_union(interest_region_proj))
center_sf_proj <- st_transform(centroid_paca, crs = 2154)

bbox_proj <- st_transform(bbox, crs = 2154)
bbox_polygon <- st_as_sfc(bbox_proj)

boundary <- st_boundary(interest_region_proj)
boundary_points <- st_cast(boundary, "POINT")
distances <- st_distance(boundary_points, center_sf_proj)
max_dist <- max(distances)

circle_buffer <- st_buffer(center_sf_proj, dist = max_dist)
points_inside <- grid_sf_proj[circle_buffer, ]

ggplot() +
  geom_sf(data = interest_region_proj, fill = "skyblue", alpha = 0.7) +
  geom_sf(data = points_inside, color = "navy", size = 0.5, shape = 19)+
  geom_sf(data = center_sf_proj,color="red", size = 0.5, shape = 19)+
  labs(
    title = "Map of Interested Region",
    subtitle = target_name,
    caption = "Data source: fr.json"
  ) +
  theme_minimal()



# Wind farms------
wind_farm <- read.csv("./Geo_data/wind_farms_france.csv")
unique(wind_farm$code_reg)
# 76 27 75 44 84 32 28 53 52 93 11 24 94

wind_farm[wind_farm$etat_parc=="En démantèlement",]

a <- wind_farm[wind_farm$date_mise_en_service == "" & wind_farm$statut_parc != "En instruction" & wind_farm$etat_parc!= "En attente de construction"& wind_farm$etat_parc!="En construction", ]
# AQUETTES is in permitting and development phase
# Fitou is operating before 2008

check <- wind_farm %>%
  group_by(id_parc) %>%
  summarise(
    n_puissance = n_distinct(puissance_parc_mw),
    n_nombre    = n_distinct(nombre_aerogenerateur),
    n_x         = n_distinct(x_parc),
    n_y         = n_distinct(y_parc),
    .groups = "drop"
  )

all(dplyr::select(check, starts_with("n_")) == 1)

## Wind farm data processing ------------
wind_farm <- wind_farm %>%
  group_by(id_parc) %>%
  slice_head(n = 1) %>%
  ungroup()

wind_farm[wind_farm$nom_usuel=="Fitou","date_mise_en_service"] <- "2006-01-01" #dont know the exact date, but before 2008
wind_farm_inService <- wind_farm[wind_farm$statut_parc != "En instruction" & wind_farm$etat_parc!= "En attente de construction"& wind_farm$etat_parc!="En construction"& wind_farm$etat_parc!="",]
wind_farm_inService$date_mise_en_service <- as.POSIXct(wind_farm_inService$date_mise_en_service,tz="UTC")

total_capacity <- wind_farm_inService %>%
  distinct(nom_usuel, .keep_all = TRUE) %>%   # keep only one row per farm
  summarise(total_capacity = sum(puissance_parc_mw, na.rm = TRUE))


## Circular buffer -----
create_wind_farm_map <- function(target_region_codes, end_year, wind_farm_data, france_geo_data, grid_data, fixed_bbox, custom_title = NULL) {
  if (!is.null(custom_title)) {
    target_name <- custom_title
  } else {
    target_name <- paste(france_geo_data$name[france_geo_data$region_code %in% target_region_codes], collapse = " & ")
  }
  
  interest_region <- france_geo_data %>%
    filter(region_code %in% target_region_codes)
  
  region_farms <- wind_farm_data %>%
    filter(code_reg %in% target_region_codes) %>%
    filter(year(date_mise_en_service) < end_year)
  
  if (nrow(region_farms) == 0) {
    message(paste("Skipping:", target_name, "for year <", end_year, "- No wind farms in service."))
    return(NULL)
  }
  
  interest_region_proj <- st_transform(interest_region, crs = 2154)
  grid_sf_proj <- st_transform(grid_data, crs = 2154)
  region_farms_sf <- st_as_sf(region_farms,
                              coords = c("x_parc", "y_parc"),
                              crs = 4326)
  region_farms_proj <- st_transform(region_farms_sf, crs = 2154)
  
  centroid_proj <- st_centroid(st_union(region_farms_proj))
  max_radius <- max(st_distance(region_farms_proj, centroid_proj))
  circle_buffer <- st_buffer(centroid_proj, dist = max_radius)
  
  points_inside <- grid_sf_proj[circle_buffer, ]
  
  map_plot <- ggplot() +
    geom_sf(data = interest_region_proj, fill = "skyblue", alpha = 0.7) +
    geom_sf(data = points_inside, color = "navy", size = 0.5, shape = 19) +
    geom_sf(data = centroid_proj, color = "red", size = 2, shape = 4, stroke = 1.5) +
    # Set fixed coordinates for the plot axes
    coord_sf(xlim = c(fixed_bbox["xmin"], fixed_bbox["xmax"]),
             ylim = c(fixed_bbox["ymin"], fixed_bbox["ymax"]),
             expand = FALSE) +
    labs(
      title = paste("Wind Farm Distribution in", target_name),
      subtitle = paste("Farms commissioned before", end_year),
      caption = "Data source: fr.json"
    ) +
    theme_minimal()
  
  return(map_plot)
}

region_codes <- unique(france_data$region_code)

# exclude 94, merge 11 and 24
region_groups <- region_codes[region_codes != "94"]
region_groups <- setdiff(region_groups, c(11, 24))
region_groups <- c(region_groups, list(c(11, 24)))

region_groups <- lapply(region_groups, function(x) as.character(x))

year_limits <- 2018:2023

for (job_codes in region_groups) {
  
  interest_region <- france_data %>% filter(region_code %in% job_codes)
  interest_region_proj <- st_transform(interest_region, crs = 2154)
  
  all_farms_in_job <- wind_farm_inService %>% filter(code_reg %in% job_codes)
  
  plot_bbox <- if (nrow(all_farms_in_job) > 0) {
    all_farms_sf <- st_as_sf(all_farms_in_job, coords = c("x_parc", "y_parc"), crs = 4326)
    all_farms_proj <- st_transform(all_farms_sf, crs = 2154)
    
    max_centroid <- st_centroid(st_union(all_farms_proj))
    max_radius <- max(st_distance(all_farms_proj, max_centroid))
    max_circle_buffer <- st_buffer(max_centroid, dist = max_radius)
    
    combined_geometry <- st_union(st_union(interest_region_proj), max_circle_buffer)
    
    bbox <- st_bbox(combined_geometry)
    x_range <- bbox["xmax"] - bbox["xmin"]
    y_range <- bbox["ymax"] - bbox["ymin"]
    bbox["xmin"] <- bbox["xmin"] - (0.05 * x_range)
    bbox["xmax"] <- bbox["xmax"] + (0.05 * x_range)
    bbox["ymin"] <- bbox["ymin"] - (0.05 * y_range)
    bbox["ymax"] <- bbox["ymax"] + (0.05 * y_range)
    bbox
  } else {
    st_bbox(st_union(interest_region_proj))
  }
  
  plot_title <- if(length(job_codes) > 1) {
    paste("Combined:", paste(france_data$name[france_data$region_code %in% job_codes], collapse=" & "))
  } else {
    france_data$name[france_data$region_code %in% job_codes]
  }
  
  for (year_limit in year_limits) {
    plot_object <- create_wind_farm_map(
      target_region_codes = job_codes, 
      end_year = year_limit,
      wind_farm_data = wind_farm_inService,
      france_geo_data = france_data,
      grid_data = grid_sf,
      fixed_bbox = plot_bbox, 
      custom_title = plot_title
    )
    
    if (!is.null(plot_object)) {
      print(plot_object)
    }
  }
}
# The circle buffer doesn't change much over the year, because new built wind farms usually lie within the circle buffer


## Kernel density --------

### Existing package -------
kde_windfarms <- function(region_codes, year_limit, 
                          wind_farm_data, france_data, grid_coords,
                          expand_m = 50000) {
  
  interest_regions <- france_data %>%
    filter(region_code %in% region_codes)
  
  region_farms <- wind_farm_data %>%
    filter(code_reg %in% region_codes,
           lubridate::year(date_mise_en_service) < year_limit)
  
  if (nrow(region_farms) < 2) {
    message(paste("Skipping:", paste(region_codes, collapse = ","),
                  "for year <", year_limit, "- Not enough wind farms."))
    return(NULL)
  }
  
  region_farms_sf <- st_as_sf(region_farms, coords = c("x_parc", "y_parc"), crs = 4326)
  grid_sf <- st_as_sf(grid_coords, coords = c("lon", "lat"), crs = 4326)
  
  grid_proj <- st_transform(grid_sf, crs = 2154)
  region_farms_proj <- st_transform(region_farms_sf, crs = 2154)
  interest_region_proj <- st_transform(interest_regions, crs = 2154)
  
  coords <- st_coordinates(region_farms_proj)
  
  # base bbox (tight)
  bbox_base <- st_bbox(region_farms_proj)
  
  # expanded bbox
  bbox_expanded <- bbox_base
  bbox_expanded[c("xmin","ymin")] <- bbox_expanded[c("xmin","ymin")] - expand_m
  bbox_expanded[c("xmax","ymax")] <- bbox_expanded[c("xmax","ymax")] + expand_m
  
  # bandwidth from base window
  ppp_base <- ppp(
    x = coords[, "X"],
    y = coords[, "Y"],
    window = as.owin(bbox_base),
    marks = region_farms_proj$puissance_parc_mw
  )
  h_opt <- withCallingHandlers({
    # This is the line that can produce the warning.
    # Consider adding a wider srange here to prevent the warning in the first place,
    # e.g., bw.ppl(ppp_base, srange = c(1000, 100000))
    bw.ppl(ppp_base, srange = c(1940, 120000))
  },
  warning = function(w) {
    # This function runs ONLY if a warning is detected in the code above.
    # We check if the warning message contains the text we're looking for.
    if (grepl("Likelihood Cross-Validation", w$message)) {
      message(paste("--> Bandwidth Warning For:",
                    "\n  Regions:", paste(region_codes, collapse = ", "),
                    "\n  Year <", year_limit))
    }
    # This allows the original warning to still be displayed after our message.
    invokeRestart("muffleWarning")
  }
  )
  print(h_opt)
  # density on expanded bbox
  ppp_expanded <- ppp(
    x = coords[, "X"],
    y = coords[, "Y"],
    window = as.owin(bbox_expanded),
    marks = region_farms_proj$puissance_parc_mw
  )
  
  kde <- density.ppp(ppp_expanded,
                     sigma = h_opt,
                     weights = marks(ppp_expanded))
  
  kde_raster <- raster(kde)
  crs(kde_raster) <-  CRS("+init=EPSG:2154")
  kde_df <- as.data.frame(kde_raster, xy = TRUE)
  colnames(kde_df) <- c("lon", "lat", "density")
  
  p <- ggplot() +
    geom_raster(data = kde_df, aes(x = lon, y = lat, fill = density)) +
    geom_sf(data = interest_region_proj, 
            fill = "transparent", color = "black", linewidth = 0.8) +
    geom_sf(data = region_farms_proj, 
            aes(size = puissance_parc_mw), 
            shape = 21, alpha = 0.7, fill = "white") +
    scale_fill_viridis_c(option = "plasma", name = "Density") +
    labs(
      title = "Weighted Kernel Density of Wind Farm Capacity",
      subtitle = paste("Regions:", paste(region_codes, collapse = ", "),
                       "| Year <", year_limit),
      x = "Longitude", y = "Latitude", size = "Capacity (MW)"
    ) +
    theme_minimal() +
    coord_sf()
  
  return(list(
    plot = p,
    kde_df = kde_df     
  ))
}




# centroids provided by EDF
region_centers <- data.frame(
  region_code = c("11", "24", "27", "28", "32", "44", "52",
                  "53", "75", "76", "84", "93"),
  lon = c(2.327143, 1.852361, 4.760281,
          0.484465, 2.697961, 4.791011, -1.073892,
          -2.891275, 0.162408, 2.805463,
          4.010230, 5.312797),
  lat = c(48.369048, 47.771220, 47.508721,
          49.431828, 49.949846, 48.759325, 47.372904,
          48.156994, 46.243702, 43.546042,
          45.117476, 43.573382)
)

region_centers_sf <- st_as_sf(region_centers,
                              coords = c("lon", "lat"),
                              crs = 4326)
region_centers_proj <- st_transform(region_centers_sf, crs = 2154)

results <- lapply(region_groups, function(regs) {
  res_years <- lapply(year_limits, function(y) {
    kde_windfarms(region_codes   = regs,
                  year_limit     = y,
                  wind_farm_data = wind_farm_inService,
                  france_data    = france_data,
                  grid_coords    = grid_sf)
  })
  names(res_years) <- year_limits
  return(res_years)
})

names(results) <- sapply(region_groups, paste, collapse = "_")

region_density_ranges <- lapply(names(results), function(reg) {
  all_dens <- do.call(rbind, lapply(results[[reg]], function(x) x$kde_df))
  c(min = min(all_dens$density, na.rm = TRUE),
    max = max(all_dens$density, na.rm = TRUE))
})
names(region_density_ranges) <- names(results)

for (reg in names(results)) {
  lims <- region_density_ranges[[reg]]
  
  # get centroid(s) for this region group
  reg_codes <- unlist(strsplit(reg, "_"))  # region codes as string
  centroids_to_plot <- region_centers_proj %>%
    filter(region_code %in% reg_codes)
  
  for (yr in names(results[[reg]])) {
    p <- results[[reg]][[yr]]$plot +
      scale_fill_viridis_c(option = "plasma",
                           name = "Density",
                           limits = lims) +
      geom_sf(data = centroids_to_plot, 
              color = "red", size = 3, shape = 19) +
      ggtitle(paste("Region:", reg, "| Year:", yr))
    
    print(p)
  }
}

### My code -------
generate_wind_farm_density_map <- function(region_codes,
                                           year_limit,
                                           bandwidth,
                                           wind_farm_data,
                                           france_data,
                                           grid_coords,
                                           density_threshold = 0) {
  
  interest_regions <- france_data %>%
    filter(region_code %in% !!region_codes)
  
  region_farms <- wind_farm_data %>%
    filter(code_reg %in% !!region_codes, year(date_mise_en_service) < year_limit)
  
  if (nrow(region_farms) == 0) {
    stop("No wind farms found for the selected regions and year.")
  }
  
  grid_sf <- st_as_sf(grid_coords, coords = c("lon", "lat"), crs = 4326)
  region_farms_sf <- st_as_sf(region_farms, coords = c("x_parc", "y_parc"), crs = 4326)
  
  # Project to a suitable CRS for accurate distance
  grid_proj <- st_transform(grid_sf, crs = 2154)
  region_farms_proj <- st_transform(region_farms_sf, crs = 2154)
  
  dist_matrix <- st_distance(grid_proj, region_farms_proj)
  dist_matrix <- units::drop_units(dist_matrix)
  
  kernel_matrix <- exp(-(dist_matrix^2) / (2 * bandwidth^2))
  
  weights <- region_farms_sf$puissance_parc_mw
  density_values <- as.numeric(kernel_matrix %*% weights)
  
  if (sum(weights) > 0) {
    density_values_wtd <- density_values / sum(weights)
  } else {
    density_values_wtd <- 0
  }
  
  grid_coords$density <- density_values_wtd
  
  grid_coords <- grid_coords %>%
    filter(!is.na(density) & density > density_threshold)
  
  if (nrow(grid_coords) == 0) {
    warning("No grid points were found above the density threshold. The plot may be empty.")
  }
  
  map_plot <- ggplot() +
    geom_tile(data = grid_coords, aes(x = lon, y = lat, fill = density)) +
    geom_sf(data = interest_regions, fill = "transparent", color = "black", linewidth = 0.8) +
    geom_sf(data = region_farms_sf, aes(size = puissance_parc_mw), shape = 21, alpha = 0.7, fill = "white") +
    scale_fill_viridis_c(option = "plasma", name = "Density") +
    scale_size_continuous(name = "Capacity (MW)") +
    labs(
      title = paste("Wind Farm Density in", interest_regions$name),
      subtitle =  paste("Farms commissioned before", year_limit),
      x = "Longitude",
      y = "Latitude"
    ) +
    theme_minimal() +
    coord_sf(crs = 4326)
  
  output <- list(
    plot = map_plot,
    data = grid_coords
  )
  return(output)
}

Nouvelle_Aquitaine <- generate_wind_farm_density_map(region_code=region_code,year_limit = 2024,bandwidth = h_opt,wind_farm_data = wind_farm_inService,france_data = france_data,grid_coords = coord, density_threshold = 1e-3)
Nouvelle_Aquitaine$plot

France_regions <- generate_wind_farm_density_map(region_code=france_data[france_data$name!="Corse",]$region_code,year_limit = 2024,bandwidth = 30000,wind_farm_data = wind_farm_inService,france_data = france_data,grid_coords = coord)
France_regions$plot


## Convex and concave hull -----
plots <- map(france_data$region_code, function(region_code) {
  interest_region <- france_data %>% filter(region_code == !!region_code)
  target_name <- interest_region$name
  
  region_farms <- wind_farm_inService %>% filter(code_reg == region_code)
  
  if (nrow(region_farms) == 0) {
    message("No wind farms for region ", target_name)
    return(NULL)
  }
  
  region_farms_sf <- st_as_sf(region_farms,
                              coords = c("x_parc", "y_parc"),
                              crs = 4326)
  
  polygon_buffer <- st_convex_hull(st_union(region_farms_sf))
  
  polygon_buffer_concave <- concaveman(region_farms_sf)
  polygon_buffer_concave <- st_make_valid(polygon_buffer_concave)
  
  points_inside <- grid_sf[polygon_buffer, ]
  points_inside_concave <- grid_sf[polygon_buffer_concave, ]
  
  ggplot() +
    geom_sf(data = interest_region, fill = "skyblue", alpha = 0.7) +
    geom_sf(data = points_inside, color = "navy", size = 0.5, shape = 19) +
    geom_sf(data = points_inside_concave, color = "red", size = 0.5, shape = 19) +
    labs(
      title = paste("Map of Region:", target_name),
      subtitle = paste("Region code:", region_code),
      caption = "Data source: fr.json"
    ) +
    theme_minimal()
})

plots[[1]] 
plots[[2]] 
plots[[3]]
plots[[4]]
plots[[5]]
plots[[6]]
plots[[7]]
plots[[8]]
plots[[9]]
plots[[10]]  # Corse
plots[[11]]
plots[[12]]
plots[[13]]

