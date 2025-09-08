The available data and code has been used in the manuscript titled "Landscape matters: Deriving a generalizable understanding of population connectivity using empirical data and graph theory" submitted to bioRxiv

The description of attached files are as follows:

Codes:
1. Data_Preparation a. Creating raster stack b. Removing highly correlated rasters c. Spatial thinning of presence records d. Generating background pseudo-absences d. Creating and joining data to grids of 5x5 degree e. Selecting grids (>15 occurence records) for each species #Steps 2 - 5 were implemented within each selected grid for each species
2. Model_tuning a. Training data extraction for each species and grid b. Model tuning for Maxent using ENMeval c. Model tuning for other algorithms using caret (*GAM doesn't have hyperparameters to tune and optimize)
3. Model_running a. Running all algorithms with tuned model parameters b. Model thresholds and evaluation statistics were extracted for each model c. Generating AUC weighted ensemble habitat suitability prediction
4. Network_generation_and_properties a. Graphab project creation b. Node identification: Habitats with minimum area of species homerange from binary habitat suitability layer c. Resistance layer generation: Negative exponential transformation of the habitat suitability values ranging from 1 to 100 d. Complete spatial graph generation e. Pruned connectivity networks based on dispersal distance e. Computing network metrics from igraph R package
5. Landscape_metrics a. Creating habitat buffers within each grid b. Computing landscape traits from landscapemetrics R package 
6. Glm_analyses a. Identifying model family for each network property b. Running Generalized Linear models for network properties with species and landscape traits b. Assessing relative importance of variables for groups of network properties c. Threshold senstivity analysis (+-25% dispersal distance)

Data*:
pred_ras.tif: Uncorrelated raster stack with 1x1 km resolution, WGS84
species_occurence: Collated presence records of species
*Will be made available upon publication of the manuscript.
