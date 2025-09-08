rm(list = ls())

library(raster)
library(sdm)
library(dplyr)
library(tidyverse)
library(sf)

# Set seed for reproducibility
set.seed(1243)

# List of species
species_list <- c("SB", "FC", "DH", "GJ", "IW", "JC", "LC", "LP", "RSC", "SH", "TG")  
error_log = list()

# Loop over each species
for(species in species_list) {
  
# Define file paths dynamically based on species
occ_file <- paste0("./input_data/", species, "_occ_covariate.csv")
grid_file <- paste0("./input_data/", species, "_selected_grids.rds")
  
  # Read the data files
occ <- read.csv(occ_file)
grid <- readRDS(grid_file)
pred_ras <- stack("./input_data/pred_ras.tif")

pred_ras_names = names(pred_ras)
t3 = Sys.time()

# Looping over grids for each species to generate model predictions
for (g in grid$grid_id[3:length(grid$grid_id)]) {
  
  # Read tuned parameters for each model type
  
  tune_brt_par = read.csv(paste0("./output_data/", species,"_grid", g, "_tunebrtpar.csv"))
  tune_mars_par = read.csv(paste0("./output_data/", species,"_grid", g, "_tunemarspar.csv"))
  tune_rf_par = read.csv(paste0("./output_data/", species,"_grid", g, "_tunerfpar.csv"))
  tune_svm_par = read.csv(paste0("./output_data/", species,"_grid", g, "_tunesvmpar.csv"))
  tune_maxent_par = read.csv(paste0("./output_data/", species,"_grid", g, "_tunemaxentpar.csv"))
  
  
  if (!is.null(tune_brt_par) && !all(is.na(tune_brt_par$dev_mean))) {
    tune_brt_par <- tune_brt_par |> slice(which.min(dev_mean))
  }
  
  if (!is.null(tune_maxent_par) && !all(is.na(tune_maxent_par$auc.val.avg))) {
    tune_maxent_par <- tune_maxent_par |> slice(which.max(auc.val.avg))
  }
  
  if (!is.null(tune_mars_par) && !all(is.na(tune_mars_par$nprune))) {
    tune_mars_par <- tune_mars_par |> slice_min(nprune, n = 1, with_ties = FALSE)
  }
  
  if (!is.null(tune_rf_par) && !all(is.na(tune_rf_par$mtry))) {
    tune_rf_par <- tune_rf_par |> slice_min(mtry, n = 1, with_ties = FALSE)
  }
  
  if (!is.null(tune_svm_par) && !all(is.na(tune_svm_par$sigma))) {
    tune_svm_par <- tune_svm_par |> slice_min(sigma, n = 1, with_ties = FALSE)
  }
  
  subset_data = occ[occ$grid_id == g, ]
  species_thinned = subset_data %>%
    filter(detection == 1)
  bg = subset_data %>%
    filter(detection == 0)
  sp = subset_data
  sp$species = species
  
  # Crop raster to grid
  pred_ras <- stack("./input_data/pred_ras.tif")
  pred_ras = rast(pred_ras)
  pred_ras_grid = crop(pred_ras, subset_data)
  
  form = paste0("detection~",paste(pred_ras_names , collapse = "+"))
  # Prepare training and testing data
  sdm_data = sdmData(formula = as.formula(form), train = sp)
  
  # Train SDM models
  assign(paste0("m", species), 
         sdm(data = sdm_data,
             methods = c("gam", "brt", "mars", "rf", "svm", "maxent"),
             replication = 'cv',
             test.percent = 30,
             n = 5,
             parallelSettings = list(ncore = 5, method = 'parallel'),
             modelSettings = list(gam=list(method="REML"),
                                  brt = list(
               n.trees = tune_brt_par$n_trees, 
               bag.fraction = tune_brt_par$bag_rate, 
               interaction.depth = tune_brt_par$tc, 
               shrinkage = tune_brt_par$lrt),
               mars = list(nprune = tune_mars_par$nprune),
               rf = list(mtry = tune_rf_par$mtry, sampsize = nrow(sp[sp$detection == 1,])),
               svm = list(sigma = tune_svm_par$sigma, C = tune_svm_par$C),
               maxent = list(fc = tune_maxent_par$fc, rm = tune_maxent_par$rm))))

  # Initialize lists for storing results
  thresholds_list <- list()
  stats_list <- list()
  var_imp_list <- list()
  
  # Retrieve the corresponding model object for the species
  model_object <- get(paste0("m", species))
  
  # Get the model names
  model_names <- names(model_object@models$detection)
  
  # Loop through each model type (BRT, RF, SVM, etc.)
  for (model_index in seq_along(model_object@models$detection)) {
    
    # Loop through each replication of the model
    for (replication_index in seq_along(model_object@models$detection[[model_index]])) {
      
      # Extract the model
      model <- model_object@models$detection[[model_index]][[replication_index]]
      
      # Ensure the model has variable importance results
      if (length(model@varImportance$test.dep) != 0) {
        
        # Extract threshold-based evaluation metrics
        thresholds_list[[length(thresholds_list) + 1]] <- data.frame(
          Species = species,
          Model = model_names[model_index],
          Model_no = model_index,
          Replication = replication_index,
          Train = model@evaluation$training@threshold_based,
          Test = if (length(model@evaluation$test.dep) != 0) {
            model@evaluation$test.dep@threshold_based
          } else {
            data.frame(criteria = NA, threshold = NA, sensitivity = NA, specificity = NA, 
                       TSS = NA, Kappa = NA, NMI = NA, phi = NA, ppv = NA, npv = NA, 
                       ccr = NA, prevalence = NA)
          }
        )
        
        # Extract model evaluation statistics
        stats_list[[length(stats_list) + 1]] <- data.frame(
          Species = species,
          Model = model_names[model_index],
          Model_no = model_index,
          Replication = replication_index,
          Train_AUC = model@evaluation$training@statistics$AUC,
          Test_AUC = ifelse(length(model@evaluation$test.dep) != 0, 
                            model@evaluation$test.dep@statistics$AUC, NA),
          Train_COR = model@evaluation$training@statistics$COR,
          Test_COR = ifelse(length(model@evaluation$test.dep) != 0, 
                            model@evaluation$test.dep@statistics$COR, NA),
          Train_Prevalence = model@evaluation$training@statistics$Prevalence,
          Test_Prevalence = ifelse(length(model@evaluation$test.dep) != 0, 
                                   model@evaluation$test.dep@statistics$Prevalence, NA),
          Train_Deviance = model@evaluation$training@statistics$Deviance,
          Test_Deviance = ifelse(length(model@evaluation$test.dep) != 0, 
                                 model@evaluation$test.dep@statistics$Deviance, NA)
        )
        
        # Extract variable importance values
        var_importance_values <- as.data.frame(model@varImportance$test.dep@varImportance)
        var_imp_list[[length(var_imp_list) + 1]] <- data.frame(
          Species = species,
          Model = model_names[model_index],
          Model_no = model_index,
          Replication = replication_index,
          Variable = rownames(var_importance_values),
          Importance = var_importance_values
        )
      }
    }
  }

  # Convert lists to data frames
  thresholds <- do.call(rbind, thresholds_list)
  stats <- do.call(rbind, stats_list)
  var_imp <- do.call(rbind, var_imp_list)

  #####Saving & summarizing results######
  modelname = paste0("./output_data/", species, "_grid", g, "_modelrun_summary.RData")
  save(thresholds, stats, var_imp, file = modelname)
  
  # Convert stats columns to numeric and calculate AUC difference
  stats[,-c(1,2)] = sapply(stats[,-c(1,2)], as.numeric)
  stats$Diff_AUC = abs(stats$Train_AUC - stats$Test_AUC)
  
  # Group by species and model, then summarize all statistics
  stats_mean = stats %>% group_by(Species, Model) %>% summarise_all(mean, na.rm = TRUE)
  
  # Remove Test.criteria column and calculate mean thresholds
  thresholds = thresholds[, !colnames(thresholds) %in% c("Test.criteria")]
  thresholds_mean = thresholds %>% group_by(Species, Model, Train.criteria) %>% summarise_all(mean, na.rm = TRUE)
  
  # Calculate mean variable importance values
  var_imp_mean = var_imp %>% group_by(Species, Model, Variable) %>% summarise_all(mean, na.rm = TRUE)
  
  
  write.csv(stats_mean, paste0("./output_data/", species, "_grid", g, "_ModelStats.csv"), row.names = F)
  write.csv(thresholds_mean, paste0("./output_data/", species, "_grid",g,"_ModelThresholds.csv"), row.names = F)
  write.csv(var_imp_mean, paste0("./output_data/", species, "_grid", g, "_ModelVarImpMean.csv"), row.names = F)
  
  ####Predicting distributions using top models####
  
  model_name = paste0("m", species)
  
  # Get best models for each species, selecting top 3 models
  best_models = stats_mean %>%
    arrange(desc(Test_AUC)) %>%
    group_by(Species) %>%
    slice(1:3)
  
  # Test.TSS,
  all_models = stats_mean %>%
    select(Species, Model, Test_AUC, Diff_AUC)
  
  # Predict using all models for the species
  pred_ras <- stack("./input_data/pred_ras.tif")
  pred_ras = rast(pred_ras)
  pred_ras_grid = crop(pred_ras, subset_data)
  for (model in all_models$Model) {
    
    pred = predict(get(model_name), newdata = pred_ras_grid, method = model, mean = TRUE, overwrite = TRUE, nc = 2)
    pred_mean = mean(stack(pred))
    writeRaster(pred_mean, paste0("./output_data/prediction/", species, "_grid", g, "_", model, ".tif"),overwrite = TRUE)
    }
    }
  
  # Ensemble predictions with all models
  ensemble_pred = ensemble(get(model_name), newdata = pred_ras_grid, setting = list(method = 'weighted', stat = 'AUC'))
  writeRaster(ensemble_pred, paste0("./output_data/prediction/", species, "_grid", g, "_ensemble_all", ".tif"),overwrite = TRUE)

  # Ensemble predictions with top 3 models
  
  #For top3 models
   for(j in 1:nrow(best_models))
   {
     mod_pos = c(which(names(get(model_name)@models$detection) %in% best_models$Model))
     for(k in 1:length(mod_pos))
     {
       str = ((mod_pos[k]-1)*25)+1
       stp = (mod_pos[k]*25)
       for(l in seq(str,stp,1))
       {
         if(!exists("ss")){
           ss = get(model_name)[[l,drop=F]]
         }else{
           ss = ss + get(model_name)[[l,drop=F]]
         }
       }
     }
   }
  ensemble_t3 = ensemble(ss, newdata=pred_ras_grid, setting=list(method='weighted',stat='AUC'))
  writeRaster(ensemble_t3, paste0("./output_data/prediction/", species, "_grid", g, "_ensemble_t3", ".tif"),overwrite = TRUE)

}

t4 = Sys.time()

##### Threshold evaluation #####

# List of species
species_list = c("SB", "FC", "DH", "GJ", "IW", "JC", "LC", "LP", "RSC", "SH", "TG")

# Create an empty dataframe to store results
threshold_results <- data.frame(Species = character(),
                                Grid = numeric(),
                                Model = character(),
                                MaxSS_Threshold = numeric(),
                                stringsAsFactors = FALSE)

# Loop over each species
for(species in species_list) {
  
  # Define file paths dynamically based on species
  occ_file <- paste0("./input_data/", species, "_occ_covariate.csv")
  grid_file <- paste0("./input_data/", species, "_selected_grids.rds")
  
  # Read the data files
  occ <- read.csv(occ_file)
  grid <- readRDS(grid_file)
  
  # Looping over grids for each species to generate model predictions
  for (g in grid$grid_id) {
    
    # Read tuned parameters for each model type
    subset_data = occ[occ$grid_id == g, ]
    species_thinned = subset_data %>%
      filter(detection == 1)
    bg = subset_data %>%
      filter(detection == 0)
    sp = subset_data
    sp$species = species
    
    # For ensemble_all
    ras = raster(paste0("./output_data/prediction/", species,"_grid", g, "_ensemble_all.tif"))
    sp_HS = raster::extract(ras, species_thinned[,c("lon","lat")])
    bg_HS = raster::extract(ras, bg[, c("lon", "lat")])
    
    # Create presence dataset
    sp_df = data.frame(HS = sp_HS, PA = 1)
    
    # Create background (absence) dataset
    bg_df = data.frame(HS = bg_HS, PA = 0)
    
    # Combine both into one dataset
    eval_data = rbind(sp_df, bg_df)
    
    ens_eval = sdm::evaluates(x = eval_data$PA, p = eval_data$HS)
    
    threshold_all = ens_eval@threshold_based[["threshold"]][ens_eval@threshold_based[["criteria"]] == "max(se+sp)"]
    
    threshold_results <- rbind(threshold_results, 
                               data.frame(Species = species, Grid = g, Model = "ensemble_all", 
                                          MaxSS_Threshold = threshold_all))
    
    # For ensemble_t3
    ras = raster(paste0("./output_data/prediction/", species,"_grid", g, "_ensemble_t3.tif"))
    sp_HS = raster::extract(ras, species_thinned[,c("lon","lat")])
    bg_HS = raster::extract(ras, bg[, c("lon", "lat")])
    
    # Create presence dataset
    sp_df = data.frame(HS = sp_HS, PA = 1)
    
    # Create background (absence) dataset
    bg_df = data.frame(HS = bg_HS, PA = 0)
    
    # Combine both into one dataset
    eval_data = rbind(sp_df, bg_df)
    
    ens_eval = sdm::evaluates(x = eval_data$PA, p = eval_data$HS)
    
    threshold_t3 = ens_eval@threshold_based[["threshold"]][ens_eval@threshold_based[["criteria"]] == "max(se+sp)"]
    
    threshold_results <- rbind(threshold_results, 
                               data.frame(Species = species, Grid = g, Model = "ensemble_t3", 
                                          MaxSS_Threshold = threshold_t3))
  }
  
}

write.csv(threshold_results, "./output_data/Model_ensemble_thresholds.csv")


