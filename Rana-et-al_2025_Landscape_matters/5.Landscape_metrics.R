library(raster)
library(terra)
library(sf)
library(dplyr)
library(landscapemetrics)

####### Preparing LULC raster ######
#Same raster stack for all species 
lulc =  raster("./rasters_1km/LULC_WorldCover_2020_1km.tif")

####Converting lulc raster to utm for landscapemetric input
utm_crs = "+proj=utm +zone=44 +ellps=WGS72 +units=m +no_defs" #predictions were transformed using same setting
lulc_utm = projectRaster(lulc, crs = utm_crs, method = "ngb")

writeRaster(lulc_utm, "./input_data/LULC_utm.tif")

######## Loading and reclassifying lulc#######

#1. Crude lulc (all categories) --> for calculating np, pd, ed, shdi
lulc_utm =  raster("./input_data/LULC_utm.tif")
landscape <- terra::rast(lulc_utm)

plot(landscape)

#2. Reclassified lulc (2 categories) --> for calculating lpi, pland for natural areas excluding water

# Define reclassification matrix: 1 "closed", 2 "open", 3 "modified", 4 "water", 5 "wetland"
# look-up table (old code -> new class)
lut <- data.frame(
  code  = c(10, 20, 30, 60, 40, 50, 70, 80, 90, 95, 100),
  class = c( 1,  1,  1,  1,  3,  3,  0,  0,  1,  1,   1)
)

# ensure the raster layer name matches 'code' column OR specify 'by'
names(landscape) <- "code"

landscape_reclass <- subst(landscape, from = lut$code, to = lut$class)
landscape_reclass[landscape_reclass == 0] <- NA

plot(landscape_reclass)

####Identify habitat buffers for each grid#########

# Load grid shapefile
grid <- st_read("./input_data/grid_5deg.shp")

# Reproject grid to match LULC raster
grid_utm <- st_transform(grid, crs = crs(landscape))
plot(grid_utm)

# Define grid and species lists
grid_list <- c(2,3,9,10,15,16,17,18,19,22,23,24,25,26,27,30,31,32,34,37)
species_list <- c("SB", "FC", "DH", "GJ", "IW", "JC", "LC", "LP", "RSC", "SH", "TG")

#species = "SB"
all_buffers <- list()

# Loop over selected grids
for (g in grid_list) {
  message(paste("Processing grid", g, "with all species"))
  
  # Extract grid polygon
  grid_poly <- grid_utm[grid_utm$grid_id == g, ]
  
  # Initialize list to hold habitat rasters
  habitat_stack <- list()
  
  # Loop through species and collect habitat rasters
  for (species in species_list) {
    habitat_file <- paste0("./network/", species, "_grid", g, "_model_ensemble_all_utm_bin_int.tif")
    
    if (file.exists(habitat_file)) {
      habitat_raster <- raster(habitat_file)
      habitat_crop <- crop(habitat_raster, grid_poly)
      habitat_crop[habitat_crop == 0] <- NA
      habitat_stack[[species]] <- habitat_crop
    } else {
      warning(paste("File not found for species", species, "in grid", g))
    }
  }
  
  # Combine all habitat rasters into one (union)
  reference_raster <- habitat_stack[[1]]
  
  # Resample others to match
  for (species in names(habitat_stack)[-1]) {
    habitat_stack[[species]] <- resample(habitat_stack[[species]], reference_raster, method = "ngb")
  }
  
  habitat_stack_raster <- stack(habitat_stack)
  combined_habitat <- calc(habitat_stack_raster, fun = max)
  combined_habitat[combined_habitat == 0] <- NA

  # Convert to polygons and buffer
  habitat_vect <- rasterToPolygons(combined_habitat, dissolve = TRUE, na.rm = TRUE)
  habitat_sf <- st_as_sf(habitat_vect)
  habitat_buffer <- st_buffer(habitat_sf, 10000)  # 10 km buffer
  habitat_buffer <- st_intersection(habitat_buffer, st_as_sf(grid_poly))  # clip to grid
    
  all_buffers[[as.character(g)]] <- habitat_buffer
}

# Combine all buffers into one sf object
combined_buffers <- do.call(rbind, all_buffers)
st_write(combined_buffers, "./output_data/all_grid_habitat_buffers.shp", overwrite = T)

# Assign CRS from combined_buffers to trial
st_crs(trial) <- st_crs(combined_buffers)

par(mfrow = c(1,1))
plot(trial$geometry)

######Extracting landscape metrics from habitat buffer for each grid####

habitat_grid = st_read("./output_data/all_grid_habitat_buffers.shp")
landscape_metrics_df <- data.frame()

# grid polygon
  for (g in habitat_grid$grid_id[1:length(habitat_grid$grid_id)]) {
  
    habitat_grid_poly <- habitat_grid[habitat_grid$grid_id == g, ]

    #plot(lulc_reclass_masked)
    ## 1. Metric calculation --> np, pd, ed, shdi
    # Crop crude lulc raster by habitat grid
    lulc_crop <- crop(landscape, habitat_grid_poly)
    lulc_masked <- mask(lulc_crop, habitat_grid_poly)
    lulc_masked[lulc_masked == 0] <- NA
    
    metrics1 <- calculate_lsm(lulc_masked, 
                             what = c("lsm_l_ed","lsm_l_np", "lsm_l_pd", "lsm_l_shdi"))
    
    ## 2. Metric calculation --> pland, lpi
    ## Crop Lulc reclassified by habitat grid
    lulc_reclass_crop <- crop(landscape_reclass, habitat_grid_poly)
    lulc_reclass_masked <- mask(lulc_reclass_crop, habitat_grid_poly)
    lulc_reclass_masked[lulc_reclass_masked == 0] <- NA
  
    metrics2 <- calculate_lsm(lulc_reclass_masked, 
                              what = c("lsm_c_pland","lsm_c_lpi"))
    metrics2 = metrics2 %>% filter(class == 1)
    
    metrics = rbind(metrics1, metrics2)
    metrics = metrics %>% 
      mutate(grid = g) %>%
      dplyr::select(grid_id, metric, value)
  
    landscape_metrics_df <- rbind(landscape_metrics_df, metrics)
    
  }
  
write.csv(landscape_metrics_df, "./results/all_landscape_metric_cropped.csv", row.names = FALSE)

network_metrics = read.csv("./results/all_network_properties.csv") %>%
  dplyr::filter(prune_type == "g_pruned") %>%
  dplyr::select(-X) 

landscape_metrics = read.csv("./results/all_landscape_metric_cropped.csv") %>%
  pivot_wider(names_from = metric, values_from = value) 

species_traits = read.csv("./occurrence/species_traits.csv") %>%
  dplyr::select(-X)

# Make a complete metric sheet joining species, habitat and netwwork traits
metrics <- left_join(landscape_metrics, network_metrics, by = "grid")

metrics <- left_join(metrics, species_traits, by = "species")

metrics = metrics %>%
  dplyr::select(-Dispersal_km,-Home_range,-Adult_body_length_mm,-g_assortativity_degree, -max_longevity_d, -g_clustering_coefficient)

write.csv(metrics, "./results/final_metrics_cropped.csv", row.names = FALSE)

####Metric exploration#####

library(tidyr)
library(dplyr)
library(corrplot)
library(ggplot2)

landscape_metrics_df = read.csv("./results/all_landscape_metric_cropped.csv")

wide_df <- landscape_metrics_df %>%
  pivot_wider(names_from = metric, values_from = value)

###### Checking correlations
# Compute correlation matrix
cor_matrix <- wide_df %>% dplyr::select(-grid_id) %>% cor(use = "complete.obs")

# Plot correlation
corrplot(cor_matrix, method = "color", type = "upper", tl.cex = 0.8, addCoef.col = "black")

###### Checking distributions

scaled_df <- landscape_metrics_df %>%
  group_by(metric) %>%
  mutate(scaled_value = scale(value)) %>%
  ungroup()

ggplot(scaled_df, aes(x = metric, y = scaled_value)) +
  geom_boxplot(fill = "tomato", color = "black") +
  theme_minimal() +
  labs(title = "Standardized Distribution of Landscape Metrics",
       x = "Metric", y = "Scaled Value")

landscape_metrics_df = landscape_metrics_df %>% filter(metric != "np")
ggplot(landscape_metrics_df, aes(x = metric, y = value)) +
  geom_boxplot(fill = "skyblue", color = "darkblue") +
  theme_minimal() +
  labs(title = "Distribution of Landscape Metrics", x = "Metric", y = "Value")

#########

