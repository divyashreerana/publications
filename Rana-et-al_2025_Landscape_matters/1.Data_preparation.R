library(tidyverse)
library(raster)
library(sf)
library(ggplot2)
library(dplyr)
library(sp)
library(spThin)
######Raster Data preparation########
#Same raster stack for all species --> "./input_data/pred_ras.tif"
rasterlist =  list.files("./rasters_1km", pattern= "\\.tif$", full.names=T)
rasterlist
pred_ras = stack(rasterlist)

# Extract source filenames from each layer
source_names = sapply(pred_ras@layers, function(x) x@file@name)
source_names = basename(source_names)
source_names = sub("\\_1km.tif$", "", source_names)
names(pred_ras) = source_names

pred_ras = rast(pred_ras)

# Convert to matrix (columns = layers, rows = pixel values)
ras_matrix = as.matrix(pred_ras, na.rm=TRUE)

# Compute correlation matrix and find correlated rasters
cor_matrix = cor(ras_matrix, use="pairwise.complete.obs")
highly_correlated = findCorrelation(cor_matrix, cutoff=0.7, names=TRUE)

# Drop correlated layers
filtered_ras = pred_ras[[!names(pred_ras) %in% highly_correlated]]

writeRaster(filtered_ras, filename = "./input_data/pred_ras.tif", overwrite = TRUE)

######Species Data Preparation#########
####Loading and thinning occurence dataset
data1 = read.csv("./occurrence/TG_PresenceOnly.csv") %>%
  dplyr::select("Latitude..degree.decimal.", "Longitude..degree.decimal.") %>%
  mutate(type = "PO") %>%
  mutate(species = "RSC")%>%
  mutate(Detected..Yes.1.No.0. = 1) %>%
  rename(Latitude..degree.decimal. = Latitude..degree.decimal., Longitude..degree.decimal. = Longitude..degree.decimal.)
data2 = read.csv("./occurrence/TG_PresenceAbsence.csv") %>%
  dplyr::select("Detected..Y.1..N.0.", "Latitude..degree.decimal.", "Longitude..degree.decimal.") %>%
  mutate(type = "PA") %>%
  mutate(species = "RSC") %>%
  rename(Detected..Yes.1.No.0. = Detected..Y.1..N.0.)
data = rbind(data1, data2)

data = data %>%
  filter(Detected..Yes.1.No.0. == 1) %>%
  rename(latitude = Latitude..degree.decimal., longitude = Longitude..degree.decimal., detected = Detected..Yes.1.No.0.)

# Thinning species data -> thin.par (in km) # 2.5 for SB, FC, GJ; 3.5 for DH; 1.5 for JC; 2 for LP; 1 for LC; 0.5 for RSC; 4.5 for SH; 4 for TG
species_thinned = spThin::thin(data, lat.col = "latitude", long.col = "longitude", spec.col = "species",
                                thin.par = 4, reps = 1, locs.thinned.list.return = TRUE,
                                write.files = FALSE, write.log.file = FALSE)[[1]]

species_thinned <- species_thinned %>% drop_na(Longitude, Latitude)

#Plotting thinned and raw data
ggplot() +
  geom_point(data = data, aes(x = longitude, y = latitude), color = "red", alpha = 0.5, size = 2) +
  geom_point(data = species_thinned, aes(x = Longitude, y = Latitude), color = "blue", size = 2) +  theme_minimal() 

####Creating grids across India extent (5x5 degree = 49 grids)

# Create a bounding sf polygon
bbox = st_bbox(c(xmin = 67.00, ymin = 7.00, xmax = 98.00, ymax = 38.00), crs = st_crs(4326))
bbox_sf = st_as_sfc(bbox) %>% st_sf()

# Generate grid
cell_size = 5 #units in degrees
grid_sf = st_make_grid(bbox_sf, cellsize = cell_size, square = TRUE) %>%
  st_sf() %>%
  mutate(grid_id = 1:n())
st_crs(grid_sf) = 4326

####Assigning grid id to presence and background data

# Convert presence data to sf format
presence_sf = st_as_sf(species_thinned, coords = c("Longitude", "Latitude"), crs = 4326) %>%
  mutate(detection = 1)

ggplot() +
  geom_sf(data = grid_sf, fill = NA, color = "black") + 
  geom_sf(data = presence_sf, aes(color = "red")) +  
  geom_sf_text(data = grid_sf, aes(label = grid_id), size = 3) + theme_minimal()

# Spatial join: Assign data points to grid cells
occurrence_sf = st_join(presence_sf, grid_sf, join = st_intersects)


# Converting it back to normal dataframe as left_join is problematic with sf dataframe
occurrence = occurrence_sf %>%
  mutate(lon = st_coordinates(geometry)[,1],  # Extract longitude
         lat = st_coordinates(geometry)[,2])  # Extract latitude
occurrence = occurrence %>% st_drop_geometry()

# Count points in each grid
grid_summary = occurrence %>%
  group_by(grid_id) %>%  
  summarize(count = n(), .groups = "drop")

occurrence = left_join(occurrence, grid_summary, by = "grid_id")

# Identify grids with more than 15 occurrence points, more than 10 for thin.par over 5
grids_for_bg = grid_sf %>%
  inner_join(grid_summary %>% filter(count > 15) %>% dplyr::select(grid_id), by = "grid_id")

# Merge occurrence counts with selected grids
grids_for_bg <- grids_for_bg %>%
  left_join(grid_summary, by = "grid_id") %>%
  mutate(n_pseudo = count * 10)

# Generate pseudoabsences: 10× the occurrence count per grid
set.seed(42)
bg_points <- grids_for_bg %>%
  group_by(grid_id, n_pseudo) %>%
  summarise() %>%
  st_sample(size = .$n_pseudo, by_polygon = TRUE) %>%  # per grid sampling
  st_as_sf() %>%
  mutate(detection = 0,
         grid_id = rep(grids_for_bg$grid_id, times = grids_for_bg$n_pseudo))

# Assign grid IDs to background points (spatial join)
bg_data_sf = st_join(bg_points, grids_for_bg, join = st_intersects)
bg_data_sf = bg_data_sf %>%
  dplyr::select(-grid_id.y) %>%  
  rename(grid_id = grid_id.x)  

bg_data = bg_data_sf %>%
  mutate(lon = st_coordinates(x)[,1],  # Extract longitude
         lat = st_coordinates(x)[,2])  # Extract latitude
bg_data = bg_data %>% st_drop_geometry()

# Combine occurrence data and background data
final_data = bind_rows(occurrence %>% dplyr::select(-count), bg_data)


ggplot() +
  geom_sf(data = grid_sf, fill = NA, color = "black", linewidth = 0.2) +  
  geom_sf(data = bg_data_sf, aes(color = "Background"), size = 0.5, alpha = 0.5) +
  geom_sf(data = occurrence_sf, aes(color = "Occurrence"), size = 1) +  
  scale_color_manual(values = c("Occurrence" = "red", "Background" = "blue")) +
  theme_minimal() +
  labs(title = "Occurrence vs Background Points in Selected Grids",
       color = "Point Type")

write.csv(final_data, "./occurrence/TG_thinned_presence_bg.csv") #change species name

####Extracting raster data for presence and background data 

filtered_ras <- rast("./input_data/pred_ras.tif")

# Convert final_data to an sf object
final_data_sf = st_as_sf(final_data, coords = c("lon", "lat"), crs = 4326)

# Extract raster values
raster_values = raster::extract(filtered_ras, st_coordinates(final_data_sf))

# Combine raster values with final_data
final_data = cbind(final_data, raster_values)

write.csv(final_data, "./input_data/TG_occ_covariate.csv") #change species name
saveRDS(grids_for_bg, "./input_data/TG_selected_grids.rds") #change species name

