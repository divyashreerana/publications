library(ResistanceGA) #remotes::install_github("wpeterman/ResistanceGA")
library(raster)
library(sf)
library(vcfR)
library(poppr)
library(parallel)
library(ggplot2)
library(vegan)
library(MASS) # for kde2d
library(adegenet)
library(units)
library(terra)


par(mfrow = c(1,1))

##### Preparing genetic and geographic data ########

##### create Spatial point format coordinate data
# xy= read.csv("./unique_ind_coord.csv", header = TRUE)
# loc_sf= st_as_sf(xy, coords = c("Lon", "Lat"), crs = 4326)  # WGS84
# SPs <- sp::SpatialPoints(sf::as_Spatial(loc_sf), 
#                          proj4string = sp::CRS("+proj=longlat +datum=WGS84"))

## in utm
xy= read.csv("./unique_ind_coord.csv", header = TRUE)
loc_sf= st_as_sf(xy, coords = c("Lon", "Lat"), crs = 4326)  # WGS84

loc_sf_utm <- st_transform(loc_sf, 32644)
SPs <- sp::SpatialPoints(sf::as_Spatial(loc_sf_utm), 
                         proj4string = CRS("+proj=utm +zone=44 +datum=WGS84 +units=m +no_defs"))

##### Prepare genetic data --> genetic dissimilarity matrix
vcf=read.vcfR("./output/vcf/allsb_sunbear_mac3_964x66_60indmiss_40snpmiss.vcf")

# Fixing sample names and ordering it according to xy with sample coordinates
sample_names = colnames(vcf@gt)[-1]
cleaned_names = sub("^([^_]+_[^_]+).*", "\\1", sample_names) # Trim everything after second underscore
colnames(vcf@gt)[-1] = cleaned_names # Update VCF sample names
target_order = xy$ind # Desired order from xy
reorder_indices = match(target_order, colnames(vcf@gt)[-1]) # Get indices for reordering
vcf@gt = cbind(vcf@gt[, 1, drop = FALSE], vcf@gt[, reorder_indices + 1]) # Reorder genotype matrix (keep first column "FORMAT", then reorder sample columns)

# Calculating genetic distance
genind_obj = vcfR2genind(vcf, return.alleles = FALSE)
gendist= diss.dist(genind_obj, percent = T, mat = TRUE)
gendist = gendist[lower.tri(gendist)]## need single column vector format for gdist.prep


##### Exploring genetic vs geographic distance patterns: IBD/ Mantel tests ######
geo_dist <- sp::spDists(SPs, longlat = TRUE)  # in kilometers if coords are WGS84
gen_dist <- diss.dist(genind_obj, percent = TRUE, mat = TRUE)

pop.list <- sub("^[^_]+_([A-Z]+)[0-9]+_.*$", "\\1", rownames(genind_obj@tab))
genind_obj@pop = as.factor(pop.list)

genind_obj$other=xy[,c(3,4)]
dat=genind2genpop(genind_obj, pop = xy$ind)

Dgen <- dist.genpop(dat,method=2)

pts <- st_as_sf(dat$other, coords = c("Lon", "Lat"), crs = 4326)
Dgeo <- st_distance(pts)  # returns units in meters
Dgeo <- as.dist(drop_units(Dgeo))  # remove units and convert to dist

ibd <- mantel.randtest(Dgen,Dgeo)
ibd

gen_vec <- as.vector(as.dist(Dgen))
geo_vec <- as.vector(as.dist(Dgeo))

# Create density surface
dens <- kde2d(geo_vec, gen_vec, n = 300)
dens_df <- data.frame(
  x = rep(dens$x, each = length(dens$y)),
  y = rep(dens$y, times = length(dens$x)),
  z = as.vector(dens$z)
)

# Build the plot
ggplot(data.frame(geo = geo_vec, gen = gen_vec), aes(x = geo, y = gen)) +
  geom_point(alpha = 0.3, color = "#88CCEE", size = 0.5) +
  geom_contour(data = dens_df, aes(x = x, y = y, z = z), color = "grey60", linewidth = 0.2) +
  geom_tile(data = dens_df, aes(x = x, y = y, fill = z), alpha = 0.7) +
  scale_fill_gradientn(colors = c("white", "blue", "gold", "orange", "red"), name = "Density") +
  geom_smooth(method = "lm", color = "black", se = FALSE, linewidth = 0.7) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Isolation by Distance (IBD) Plot",
    x = "Geographic Distance (m)",
    y = "Genetic Distance"
  )



####### Single surface optimization across scales ########

# Define spatial scales
scales = c("1km", "2km", "5km", "10km")

# Create response matrix (genetic distances) only once
response_mat = matrix(0, length(SPs), length(SPs))
response_mat[lower.tri(response_mat)] = gendist


# Prepare gdist input (shared across scales)
## Prepares pairwise cost distances for optimizing resistance surfaces
gdist_input = gdist.prep(
  n.Pops   = length(SPs),
  response = gendist,
  samples  = SPs
)

# Loop through each spatial scale
for (scale in scales) {
  
  message(paste0("\nRunning optimization for ", scale, "..."))
  
  # Define input/output directories
  raster_dir = file.path("./input", paste0("rasters_", scale,"/test"))
  name = paste0("SSoptim_utm_", scale, "/")
  output_dir = paste(getwd(), "resistanceGA", name, sep = "/")
  
  # Create the directory if it doesn’t exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # GA optimization setup
  GA.inputs = GA.prep(
    ASCII.dir   = raster_dir,
    Results.dir = output_dir,
    method      = "AIC",
    max.cont    = 1000,
    maxiter     = 1000,
    parallel    = 4  # or use: parallel::detectCores() - 1
  )
  
  # Run optimization
  SS_RESULTS = SS_optim(
    gdist.inputs = gdist_input,
    GA.inputs    = GA.inputs
  )
  
  # Save optimization results
  saveRDS(SS_RESULTS, file = file.path(output_dir, paste0("SS_RESULTS_", scale, ".rds")))
  
  # Bootstrapping
  mat_list = SS_RESULTS$cd
  k_vals   = SS_RESULTS$k
  
  AIC_boot = Resist.boot(
    mod.names    = names(mat_list),
    dist.mat     = mat_list,
    n.parameters = k_vals[, 2],
    sample.prop  = 0.75,
    iters        = 1000,
    obs          = length(SPs),
    genetic.mat  = response_mat
  )
  
  # Save bootstrapping output
  write.csv(AIC_boot, file = file.path(output_dir, paste0("bootstrap_", scale, ".csv")))
  
  message(paste0("Finished processing ", scale, "\n"))
}

####### Multiple surface optimization across combinations ########

######### Step 1: Generating all multisurface combinations
# List your resistance surfaces (ASCII files or raster names)
surfaces <- list.files("./resistanceGA/multisurface/SSoptim_5km", 
                       full.names = TRUE, pattern = "\\.asc$")

# Generate all subsets (only 2 or more surfaces)
all_combos <- unlist(
  lapply(2:length(surfaces), function(k) combn(surfaces, k, simplify = FALSE)),
  recursive = FALSE
)

# 3) Create folders and copy rasters
for (i in seq_along(all_combos)) {
  
  # Define subset folder
  subset_dir <- file.path(getwd(), "resistanceGA", paste0("/subset_", i))
  raster_dir <- file.path(subset_dir, "raster")
  
  # Make dirs
  dir.create(raster_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Copy rasters
  file.copy(all_combos[[i]], raster_dir, overwrite = TRUE)
}

# Save lookup table
lookup <- data.frame(
  subset_id   = seq_along(all_combos),
  rasters_used = sapply(all_combos, function(x) paste(basename(x), collapse = ";"))
)
write.csv(lookup, "./resistanceGA/subset_lookup.csv", row.names = FALSE)

########## Step 2: Running multisurface optimization for all combinations

# Load lookup table created in step 1
lookup <- read.csv("./resistanceGA/subset_lookup.csv")

# Define global parameter vector (triplets per raster, in same order as 'surfaces')
# Raster ordered alphabetically - agriculture density, distance to water, evi, road density
# PARM <- c(5, 10.28, 999, #wgs
#           7, 1.12, 919,
#           1, 3.27, 980,
#           7, 0.50, 706)

#Rmax from results
PARM <- c(1, 1.23, 981, #utm
          7, 1.14, 981,
          7, 5.17, 996,
          7, 1.16, 885)

# Map raster names to their PARM triplets
parm_map <- split(PARM, rep(1:(length(PARM)/3), each = 3))
names(parm_map) <- basename(list.files("./resistanceGA/multisurface/SSoptim_5km",
                                       full.names = TRUE, pattern = "\\.asc$"))

# Empty list to store results
results <- list()

for (i in 1:length(seq_len(nrow(lookup)))) {
  
  # Define input/output dirs for this subset
  raster_dir  <- file.path(getwd(), "resistanceGA", paste0("/subset_", i), "raster")
  name = paste0("subset_", i, "/")
  results_dir <- paste(getwd(), "resistanceGA", name, sep = "/")
  
  cat("\n▶ Running subset", i, "with rasters:", lookup$rasters_used[i], "\n")
  
  # --- Prepare GA input ---
  compiledGA_input <- GA.prep(
    ASCII.dir   = raster_dir,
    Results.dir = results_dir,
    method      = "AIC",
    max.cont    = 1000,
    maxiter     = 1000,    
    parallel    = 4,
    k.value     = 4
  )
  
  ## --- Extract correct PARM values for this subset ---
  rasters_in_subset <- strsplit(lookup$rasters_used[i], ";")[[1]]
  parm_subset <- unlist(parm_map[rasters_in_subset])
  
  # --- Combine surfaces ---
  Resistance <- Combine_Surfaces(
    PARM         = parm_subset,
    gdist.inputs = gdist_input,
    GA.inputs    = compiledGA_input,
    out          = NULL,
    rescale      = TRUE,
    p.contribution = TRUE
  )
  
  # --- Prepare gdist input for MS_optim ---
  gdist_input = gdist.prep(
    n.Pops   = length(SPs),
    response = gendist,
    samples  = SPs
  )
  
  # --- Run optimization ---
  results[[i]] <- MS_optim(
    gdist.inputs = gdist_input,
    GA.inputs    = compiledGA_input
  )
}

save(results, file = "./resistanceGA/multisurface_results.rda")
  
# Extract AICc.tab and k for all combinations
aic_extract <- do.call(rbind, lapply(1:length(results), function(i) {
  if (!is.null(results[[i]])) {
    data.frame(
      subset_id = i,
      AICc      = results[[i]]$AICc.tab,
      k         = results[[i]]$k
    )
  }
}))

write.csv(aic_extract, "./resistanceGA/AIC_across_combinations.csv", row.names = FALSE) 

results[[6]]$percent.contribution


### Extracting model parameters and output from summary text file #####

library(stringr)
library(purrr)
library(dplyr)

# Folders to scan 
subset_ids <- seq_len(nrow(lookup))  

# Function to extract values from a single txt file
# Helper: find first match for a regex with a capture group and return numeric (or NA)
find_num_after_label <- function(lines, pattern) {
  # pattern should include one capture group for the numeric value, e.g. "R2m\\):\\s*([-+]?[0-9]*\\.?[0-9]+(?:[eE][-+]?[0-9]+)?)"
  m <- str_match(lines, pattern)
  # m is matrix with [,2] = capture group; find first non-NA capture
  ix <- which(!is.na(m[,2]))[1]
  if (is.na(ix)) return(NA_real_)
  as.numeric(m[ix, 2])
}

# Main extractor
extract_summary <- function(subset_id) {
  file <- file.path(getwd(), "resistanceGA", paste0("subset_", subset_id), "Results", "Multisurface_Optim_Summary.txt")
  if (!file.exists(file)) 
    return(tibble(subset = subset_id, k = NA_integer_, AIC = NA_real_, AICc = NA_real_,                   R2m = NA_real_, R2c = NA_real_, logLik = NA_real_))
  lines <- readLines(file, warn = FALSE)
  
  k      <- find_num_after_label(lines, "^k\\s*=\\s*([0-9]+)")
  AIC <- find_num_after_label(lines, "^Minimum AIC:\\s*([-+]?[0-9]*\\.?[0-9]+(?:[eE][-+]?[0-9]+)?)")
  AICc   <- find_num_after_label(lines, "^AICc:\\s*([-+]?[0-9]*\\.?[0-9]+(?:[eE][-+]?[0-9]+)?)")
  R2m    <- find_num_after_label(lines, "R2m\\):\\s*([-+]?[0-9]*\\.?[0-9]+(?:[eE][-+]?[0-9]+)?)")
  R2c    <- find_num_after_label(lines, "R2c\\):\\s*([-+]?[0-9]*\\.?[0-9]+(?:[eE][-+]?[0-9]+)?)")
  logLik <- find_num_after_label(lines, "^Log Likelihood:\\s*([-+]?[0-9]*\\.?[0-9]+(?:[eE][-+]?[0-9]+)?)")
  
  tibble(
    subset = subset_id,
    k = k,
    AIC = AIC,
    AICc = AICc,
    R2m = R2m,
    R2c = R2c,
    logLik = logLik,
    )
}

# Apply across all subsets
summary_df <- map_dfr(seq_len(nrow(lookup)), extract_summary)

summary_df


#### Preparing circuitscape input: Saving resistance and protected area node file as ascii ######

# Read resistance raster
res_rast <- rast("./resistanceGA/subset_6/Results/evi_5km_fixed.road_density_5km_fixed.asc")

# Pick UTM zone based on your study area (e.g., EPSG:32644 for UTM zone 44N)
utm_crs <- "EPSG:32644"

# Reproject resistance raster
res_rast_utm <- project(res_rast, utm_crs, method="bilinear")

# Reproject node shapefile
node <- st_read("./figure/Central_terai_PA.shp")

# Create a unique ID for each PA
node$PA_ID <- 1:nrow(node)

node_utm <- st_transform(node, crs(res_rast_utm))

# Rasterize: assign unique ID to each PA, background = 0
r <- rasterize(vect(node_utm), res_rast_utm, field = "PA_ID", background = 0)

# Check values
freq(r)
plot(r)

all.equal(ext(res_rast_utm), ext(r))
all.equal(res(res_rast_utm), res(r))

writeRaster(res_rast_utm, file = "./figure/resistance_UTM_fixed_subset6.asc", overwrite = TRUE)
writeRaster(r, file = "./figure/Central_terai_PA.asc", overwrite = TRUE)

pa = raster("./figure/Central_terai_PA.asc")
plot(pa)

##### Plotting circuitscape output #########

# 1. Load Circuitscape output
cs_out <- rast("./output/circuitscape_fixed_subset6_cum_curmap.asc")
crs(cs_out) <- "+proj=utm +zone=44 +datum=WGS84 +units=m +no_defs"

# 2. Load your Protected Areas shapefile
# Replace with your actual PA shapefile path
#res_rast_utm = rast("./figure/resistance_UTM.asc")
node <- st_read("./figure/Central_terai_PA.shp")
st_crs(node) <- 4326  
node_utm <- st_transform(node, crs(cs_out))
node_vect <- vect(node_utm)   # sf -> terra SpatVector


cs_out_log <- log1p(cs_out)  # As the values are highly skewed, log transformation helps in visualization

# 3. Classic Circuitscape colour scheme
cs_cols <- colorRampPalette(c(
  "antiquewhite",  # low
  "#CC6677",  # mid
  "#117733"   # high
))

# 4. Plot
plot(cs_out,
     col = cs_cols(100),
     main = "Circuitscape Cumulative Current",
     axes = FALSE, box = FALSE)

plot(node_vect, 
     add = TRUE, 
     border = "white", 
     col = "#117733", 
     lwd = 2)


