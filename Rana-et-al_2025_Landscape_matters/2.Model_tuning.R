options(java.parameters = c("-XX:+UseConcMarkSweepGC", "-Xmx8g"))
gc()

library(rJava)
library(caret)
library(doParallel)
library(earth)
library(ecospat)
library(raster)
library(sdm)
library(ENMeval)
library(tidyverse)
library(sf)
library(dismo)
library(terra)
library(randomForest)


# Read and process species occurrence data
species = "SB"
occ = read.csv("./input_data/SB_occ_covariate.csv")
grid = readRDS("./input_data/SB_selected_grids.rds")
pred_ras = stack("./input_data/pred_ras.tif")
pred_ras = rast(pred_ras)


#####Model tuning:Maxent########
# Initializing empty dataframes
tune_maxent_par = tune_maxent_table = data.frame()

# Loop over each grid_id in the "grid" dataset for model tuning based on occ data 

for (g in grid$grid_id) {
  
  subset_data = occ[occ$grid_id == g, ]
  species_thinned = subset_data %>%
    filter(detection == 1)
  bg = subset_data %>%
    filter(detection == 0)
  sp = subset_data
  sp$species = species

  # Crop raster to grid
  pred_ras_grid = crop(pred_ras, subset_data)
  
  # Convert to matrix (columns = layers, rows = pixel values)
  ras_matrix = as.matrix(pred_ras_grid, na.rm=TRUE)
  
  # Compute correlation matrix and find correlated rasters
  cor_matrix = cor(ras_matrix, use="pairwise.complete.obs")
  highly_correlated = findCorrelation(cor_matrix, cutoff=0.7, names=TRUE)
  
  # Drop correlated layers
  filtered_ras = pred_ras_grid[[!names(pred_ras_grid) %in% highly_correlated]]
  
  
  tune_maxent = ENMevaluate(
    occs = species_thinned[, c("lon", "lat")], 
    envs = filtered_ras, 
    bg = bg[, c("lon", "lat")],
    tune.args = list(fc = c("L", "Q", "LQ", "LQT"), rm = c(0.5, 1, 2)),
    partitions = "randomkfold", 
    algorithm = "maxent.jar", 
    doClamp = TRUE,
    parallel = TRUE, 
    numCores = 5
  )
  
  if (!is.null(tune_maxent)) {
    tune_maxent_table_temp = eval.results(tune_maxent)
    tune_maxent_table_temp = arrange(tune_maxent_table_temp, desc(auc.val.avg))
    
    # Define unique filename for each grid
    filename = paste0("./output_data/", species, "_grid", g, "_tunemaxentpar.csv")
    write.csv(cbind("grid_id" = g, "species" = species, tune_maxent_table_temp), 
              filename, row.names = FALSE)
    message("Saved results to: ", filename)
  } else {
    message("ENMevaluate failed for grid: ", g)
  }
  gc()
  gc()
}
  
#########Parallelization########
#Sys.setenv(JAVA_HOME = "C:/Program Files/Java/jdk-22/bin/")

#Define a function to unregister parallel processing - declutters cache memory
unregister_dopar = function() {
env = foreach:::.foreachGlobals
rm(list = ls(name = env), pos = env)
}

####Model tuning: Others#####
tune_mars_par = tune_brt_par = tune_rf_par = tune_svm_par = data.frame()

unregister_dopar()

t1 = Sys.time()

for (g in grid$grid_id) {
  
  subset_data = occ[occ$grid_id == g, ]
  species_thinned = subset_data %>%
    filter(detection == 1)
  bg = subset_data %>%
    filter(detection == 0)
  sp = subset_data
  sp$species = species
  
  # Crop raster to grid
  #pred_ras_grid = crop(pred_ras, subset_data)
  
  # Convert to matrix (columns = layers, rows = pixel values)
  #ras_matrix = as.matrix(pred_ras_grid, na.rm=TRUE)
  
  # Compute correlation matrix and find correlated rasters
  #cor_matrix = cor(ras_matrix, use="pairwise.complete.obs")
  #highly_correlated = findCorrelation(cor_matrix, cutoff=0.7, names=TRUE)
  
  # Drop correlated layers
  #filtered_ras = pred_ras_grid[[!names(pred_ras_grid) %in% highly_correlated]]
  
  training <- subset_data
  
  ####Model tuning: MARS####
  training$detection <- as.factor(training$detection)
  levels(training$detection) <- c("c0", "c1")
  training <- training %>%
    dplyr::select(-c("X","grid_id","lat","lon")) %>%
    na.omit()
  
  mytuneGrid <- expand.grid(nprune = 2:20, degree = 1) #setting up tuning hypervariables
  mycontrol <- trainControl(method = "cv", number = 10, classProbs = TRUE,
                          summaryFunction = twoClassSummary, allowParallel = TRUE, p = 0.7)

  tune_mars <- caret::train(form = detection ~ ., data = training, method = "earth", metric = "ROC",
                          trControl = mycontrol, tuneGrid = mytuneGrid, thresh = 0.00001)
  if (!is.null(tune_mars)) {
    tune_mars_par <- rbind(tune_mars_par, tune_mars$bestTune)
    
    # Define unique filename for each grid
    filename = paste0("./output_data/", species, "_grid", g, "_tunemarspar.csv")
    write.csv(cbind("grid_id" = g, "species" = species, tune_mars_par), 
              filename, row.names = FALSE)
    message("Saved results to: ", filename)
  } else {
    message("ENMevaluate failed for grid: ", g)
  }
  
  ####Model tuning: SVM####
  tune_svm <- caret::train(detection ~ ., data = training, method = "svmRadial", metric = "ROC",
                           tuneLength = 5, trControl = mycontrol)
  if (!is.null(tune_svm)) {
    tune_svm_par <- rbind(tune_svm_par, tune_svm$bestTune)
    
    # Define unique filename for each grid
    filename = paste0("./output_data/", species, "_grid", g, "_tunesvmpar.csv")
    write.csv(cbind("grid_id" = g, "species" = species, tune_svm_par), 
              filename, row.names = FALSE)
    message("Saved results to: ", filename)
  } else {
    message("ENMevaluate failed for grid: ", g)
  }
  
  ####Model tuning: BRT####
  # Prepare the training data
  levels(training$detection) = c(0, 1)
  prNum = as.numeric(table(training$detection)["1"]) # number of presences
  bgNum = as.numeric(table(training$detection)["0"]) # number of backgrounds
  wt = ifelse(training$detection == 1, 1, prNum / bgNum)
  training$detection = as.numeric(training$detection)
  if (max(as.numeric(training$detection)) == 2)
    training$detection = as.numeric(training$detection - 1)
  
  # Define tuning grid
  tune_settings = expand.grid(lrt = c(0.001, 0.005, 0.01), tc = 1:3, bag_rate = c(0.7))
  
  # Setup parallel processing
  cl = makeCluster(detectCores() - 1)
  registerDoParallel(cl)
  clusterExport(cl, list("training", "wt", "tune_settings"))
  
  # Perform model tuning with parallel processing
  tune_brt = foreach(i = 1:nrow(tune_settings), .combine = rbind, .errorhandling = "remove") %dopar% {
    test = dismo::gbm.step(data = training, gbm.x = 2:ncol(training), gbm.y = 1, family = "bernoulli",                              tree.complexity = tune_settings$tc[i], learning.rate = tune_settings$lrt[i],
                           bag.fraction = tune_settings$bag_rate[i], n.folds = 10, site.weights = wt)
    print(c(i, test$n.trees, test$cv.statistics$deviance.mean))
  }
  
  stopCluster(cl)
  tune_brt = data.frame(tune_brt)
  if (!is.null(tune_brt)) {
  
  if (ncol(tune_brt) > 1) {
    colnames(tune_brt) = c("row", "n.trees", "dev_mean")
    tune_settings[c(tune_brt$row), c('n_trees', 'dev_mean')] <- tune_brt[, c("n.trees", "dev_mean")]
    tune_brt_par = rbind(tune_brt_par, tune_settings)
  } else {
    tune_brt_par = rbind(tune_brt_par, "lrt" = NA, "tc" = NA, "bag_rate" = NA, "n_trees" = NA, "dev_mean" = NA)
  }
    #Define unique filename for each grid
    filename = paste0("./output_data/", species, "_grid", g, "_tunebrtpar.csv")
    write.csv(cbind("grid_id" = g, "species" = species, tune_brt_par), 
              filename, row.names = FALSE)
    message("Saved results to: ", filename)
  } else {
    message("ENMevaluate failed for grid: ", g)
  }
  
  ####Model tuning: RF#####
  unregister_dopar()
  training$detection = as.factor(training$detection)
  control = trainControl(method = "cv", number = 10, search = "grid", allowParallel = TRUE, p = 0.7)
  tunegrid = expand.grid(.mtry = c(1:8))
  
  tune_rf = caret::train(detection ~ ., data = training, method = "rf", metric = "Accuracy",
                         tuneGrid = tunegrid, trControl = control, weights = wt)
  
  if (!is.null(tune_rf)) {
    tune_rf_par = rbind(tune_rf_par, tune_rf$bestTune)
    
    # Define unique filename for each grid
    filename = paste0("./output_data/", species, "_grid", g, "_tunerfpar.csv")
    write.csv(cbind("grid_id" = g, "species" = species, tune_rf_par), 
              filename, row.names = FALSE)
    message("Saved results to: ", filename)
  } else {
    message("ENMevaluate failed for grid: ", g)
  }

}

t2 = Sys.time()
modelname = paste0("./output_data/", species, "_modeltuning.RData")
save.image(modelname)



######Data exploration --> finding NAs######
colSums(is.na(occ))
colSums(is.na(occ[occ$detection == 1, ]))

# Extract relevant data
plot_data = occ[, c("lon", "lat", "bio01_Annual_mean_temp")]
colnames(plot_data) = c("x", "y", "Value")  # Rename for clarity

# Round coordinates to create larger tiles
plot_data$x_rounded = round(plot_data$x, 2)  # Adjust rounding as needed
plot_data$y_rounded = round(plot_data$y, 2)

ggplot(plot_data, aes(x = x_rounded, y = y_rounded, fill = is.na(Value))) +
  geom_tile() +
  scale_fill_manual(values = c("FALSE" = "gray", "TRUE" = "red"), name = "NA") +
  theme_minimal() +
  labs(title = "NA Values in Water Classification", x = "Longitude", y = "Latitude")

#####Plotting & Saving summarized stats######
# Filter and merge stats_mean for plotting
stats_mean = thresholds_mean %>%
  filter(Train.criteria == "max(se+sp)") %>% #Maximizing sensitivity and specificity
  select(Species, Model, Train.criteria, Test.TSS) %>%
  merge(stats_mean, by = c("Species", "Model"))
stats_mean = stats_mean[,-3]

# Plot Test.AUC vs Test.TSS
ggplot(stats_mean, aes(y = Test_AUC, x = Test.TSS, col = Species)) +
  geom_point() + geom_smooth(method = "lm")

# Plot Test.TSS and Test.AUC by Model
stats_mean %>%
  ggplot(aes(x = Model, y = Test.TSS, col = "TSS")) +
  geom_point() +
  facet_wrap(~Species) +
  theme(axis.text.x = element_text(angle = 45)) +
  geom_point(aes(x = Model, y = Test_AUC, col = "AUC"))