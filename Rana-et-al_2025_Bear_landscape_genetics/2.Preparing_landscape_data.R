library(terra)
library(raster)
library(ggplot2)
library(sf)
library(units)
library(dplyr)
library(corrplot)

######Raster Data preparation########

##### For raster input data - EVI, distance to water, distance to settlement
# Define target resolutions (in meters as rasters are in UTM)
resolutions <- c("1km" = 1000, "2km" = 2000, "5km" = 5000, "10km" = 10000)

# Raster data input
rast <- rast("./input/raw_rasters/dist_settlement_10m_utm.tif")

# Loop over resolutions
for (res_name in names(resolutions)) {
  
  res_val <- resolutions[[res_name]]
  
  # Make output directory for this resolution
  outdir <- file.path("./input", paste0("rasters_", res_name), "test")
  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  
  # Create a template raster at target resolution
  target <- rast(ext(rast), res = res_val, crs = crs(rast))
  
  # Resample distance raster to match target grid
  r_resampled <- resample(rast, target, method = "bilinear")
  
  # Save as GeoTIFF
  out_tif <- file.path(outdir, paste0("dist_settlement_utm", res_name, ".tif"))
  writeRaster(r_resampled, out_tif, overwrite = TRUE)
  
  message("Saved: ", out_tif)
}

#####For vector input density data - Agriculture density

# Vector input data
vec <- st_read("./input/modified_vectors/agriculture_density_utm_1km.shp")

for (res_name in names(resolutions)) {
  res_val <- resolutions[[res_name]]
  
  # Make output directory for this resolution
  outdir <- file.path("./input", paste0("rasters_", res_name), "test")
  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  
  # Create template raster
  target <- rast(ext(vec), res = res_val, crs = st_crs(vec)$wkt)
  
  # Rasterize with aggregation (mean for density)
  r_density <- rasterize(vect(vec), target, field = "agri_dens", fun = mean)
  
  # Replace NA with 0
  r_density[is.na(r_density)] <- 0
  
  # Save outputs
  out_tif <- file.path(outdir, paste0("agriculture_density_utm_", res_name, ".tif"))
  writeRaster(r_density, out_tif, overwrite = TRUE)
  
  message("Saved: ", out_tif)
}

#####For vector input length data - Road density

# Vector input data
vec <- st_read("./input/modified_vectors/road_length_utm_1km.shp")
vec$road_km[is.na(vec$road_km)] <- 0

# convert to terra vector and rasterize at 1km to preserve original
gv <- vect(vec)

for (res_name in names(resolutions)) {
  res_val <- resolutions[[res_name]]
  
  # Make output directory for this resolution
  outdir <- file.path("./input", paste0("rasters_", res_name), "test")
  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  
  # Create template raster
  target <- rast(ext(gv), res = res_val, crs = crs(gv))
  
  # Rasterize with aggregation (mean for density)
  r_density <- rasterize(gv, target, field = "road_km", fun = sum)
  
  # Replace NA with 0
  r_density[is.na(r_density)] <- 0
  
  #compute density per km^2: sum_length_km / area_km2
  area_km2 <- (res(target)[1] * res(target)[2]) / 1e6
  r_resampled <- r_density / area_km2
  
  # Save outputs
  out_tif <- file.path(outdir, paste0("road_density_utm_", res_name, ".tif"))
  writeRaster(r_resampled, out_tif, overwrite = TRUE)
  
  message("Saved: ", out_tif)
}
####### Fixing rasters: same extent and resolution within each spatial class ######

folders <- list.dirs(path = "./input", recursive = FALSE, full.names = TRUE)
folders <- folders[grepl("rasters_", basename(folders))]

# Define the layer names you expect, in the correct order
layer_names <- c("agri_pct", "settlement_density", "dist_water", "evi", "road_density")

resampled_by_folder <- lapply(folders, function(folder) {
  # Go into the `test` subfolder
  subfolder <- file.path(folder, "test")
  
  rasters <- list.files(subfolder, pattern = "\\.tif$", full.names = TRUE)
  raster_objs <- lapply(rasters, rast)
  
  # Reference raster to match resolution/extents
  ref_raster <- raster_objs[[1]]
  
  processed_rasters <- lapply(seq_along(raster_objs), function(i) {
    r <- raster_objs[[i]]
    r_aligned <- if (!compareGeom(r, ref_raster, stopOnError = FALSE)) {
      resample(r, ref_raster, method = "bilinear")
    } else {
      r
    }
    
    if (any(is.na(values(r_aligned)))) {
      r_filled <- focal(r_aligned, w = matrix(1, 3, 3), fun = mean, na.policy = 'only',
                        na.rm = TRUE)
      names(r_filled) <- layer_names[i]
      return(r_filled)
    } else {
      names(r_aligned) <- layer_names[i]
      return(r_aligned)
    }
  })
  
  return(processed_rasters)
})

####### Writing the fixed rasters in tif and ascii format #######

invisible(lapply(seq_along(resampled_by_folder), function(i) {
  folder <- folders[i]
  subfolder <- file.path(folder, "test")   # go into test/
  rasters <- resampled_by_folder[[i]]
  
  # Get original file names (from test/)
  original_files <- list.files(subfolder, pattern = "\\.tif$", full.names = FALSE)
  base_names <- tools::file_path_sans_ext(original_files)
  
  # Save each raster
  for (j in seq_along(rasters)) {
    r <- rasters[[j]]
    base_name <- base_names[j]
    
    # Save as .tif (GeoTIFF)
    writeRaster(r, filename = file.path(subfolder, paste0(base_name, "_fixed.tif")),
                overwrite = TRUE)
    
    # Save as .asc (ASCII)
    writeRaster(r, filename = file.path(subfolder, paste0(base_name, "_fixed.asc")),
                overwrite = TRUE)
  }
}))

###### Checking the ascii files by plotting them #######

# Loop over folders
for (folder in folders) {
  subfolder <- file.path(folder, "test")  # since your rasters live in test/
  cat("Reading from:", subfolder, "\n")
  
  # Get all _fixed.asc files in the folder
  asc_files <- list.files(subfolder, pattern = "_fixed\\.asc$", full.names = TRUE)
  
  # Read as SpatRaster stack
  r_stack <- rast(asc_files)
  
  # Determine plot layout
  n <- nlyr(r_stack)
  n_col <- 3
  n_row <- ceiling(n / n_col)
  
  # Plot NA maps
  par(mfrow = c(n_row, n_col))
  for (i in 1:nlyr(r_stack)) {
    layer <- r_stack[[i]]
    na_map <- is.na(layer)
    plot(na_map,
         col = c("transparent", "red"),
         legend = FALSE,
         main = paste0("NA in: ", names(r_stack)[i]))
  }
  
  # Reset plotting layout
  par(mfrow = c(1, 1))
  
  # Plot all rasters in the stack
  plot(r_stack, nc = n_col, nr = n_row, main = names(r_stack))
}

##### Checking correlation between rasters ####

folders <- list.dirs(path = "./input", recursive = FALSE, full.names = TRUE)
folders <- folders[grepl("rasters_", basename(folders))]

for (folder in folders) {
  subfolder <- file.path(folder, "test")  # since your rasters live in test/
  cat("Reading from:", subfolder, "\n")
  
  # Get all _fixed.asc files in the folder
  asc_files <- list.files(subfolder, pattern = "_fixed\\.asc$", full.names = TRUE)
  
  # Load as stack
  ras_stack <- stack(asc_files)
  
  # Convert to dataframe (mask NA so correlations don’t break)
  ras_df <- as.data.frame(ras_stack, na.rm = TRUE)
  
  # Compute correlation matrix
  cor_mat <- cor(ras_df, use = "pairwise.complete.obs", method = "pearson")
  # Extract scale (e.g., "1km" from "rasters_1km")
  scale_label <- sub("rasters_", "", basename(folder))
  
  # Plot with corrplot and heading
  corrplot(cor_mat, method = "color", type = "upper", 
           tl.col = "black", tl.cex = 0.8, 
           addCoef.col = "black", number.cex = 0.6,
           title = paste0("Correlation matrix (", scale_label, ")"),
           mar = c(0,0,2,0))  # adds margin for title
}

##### Plotting input rasters ####

rasters <- list.files("./input/rasters_5km/", full.names = TRUE, pattern = "\\.asc$")

agri = rast(rasters[[1]])
water = rast(rasters[[2]])
evi = rast(rasters[[3]])
road = rast(rasters[[4]])

hist(water)

cols <- colorRampPalette(c(
  "antiquewhite",  # low
  "#88CCEE",  # mid
  "#117733"   # high
))

plot(evi,
     col = cols(100),
     main = "evi",
     axes = FALSE, box = FALSE)

rasters <- list.files("./resistanceGA/multisurface/SSoptim_5km/", full.names = TRUE, pattern = "\\.asc$")
plot(rast(rasters))


