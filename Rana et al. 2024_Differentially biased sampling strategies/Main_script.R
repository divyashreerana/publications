###---------------------------------------------------------Global model GLM-----------------------------------------------------------------
setwd("I:/My Drive/WildCru_Comparative_HSM202/Data/Rasters/")

####################Scale selection################
##
library(dplyr)
library(usdm)
library(MuMIn)
library(PresenceAbsence)
library(Hmisc)
library(sf)
library(ecospat)
library(randomForest)
library(rfUtilities)
#models_list = list()

###############Creating raster stack########## 
# wd = "C:/Divya/Rasters/Carol250k/all_filtered_rasters/resampled/" #Changed as per run
# ras = list.files("C:/Divya/Rasters/Carol250k/all_filtered_rasters/trial", pattern=".tif", full.names=T)
# e <- as(extent(67, 101, 7, 38), 'SpatialPolygons')
# crs(e) <- CRS('+init=EPSG:4326')
# base = raster("C:/Divya/Rasters/Carol250k/all_filtered_rasters/cti500.tif")
# 
# #Matching extent - cropped with e (remember mask shouldn't have decimal, creates issues later)
# for(filename in ras){
#   input = raster(filename)
#   output = setExtent(crop(input, e), e)
#   output = resample(output, base)
#   writeRaster(output, filename = paste0(wd, names(input), ".tif"), overwrite = T)
# }
# 
# #Matching resolution. Best to resample everything just to avoid decimal errors
# ras1 = list.files("C:/Divya/Rasters/Carol250k/all_filtered_rasters/cropped1", pattern=".tif", full.names=T)
# for(filename in ras1){
#   output = resample(raster(filename), base)
#   writeRaster(output, filename = paste0(wd, names(raster(filename)), ".tif"), overwrite = T)
# }


#################Data input and processing############
#Models for each species run separately
# spp_PA = read.csv("cats_PresenceAbsence_all.csv") #Master file

# #Filtering variables with more than 90% 0s to check for biased sampling
# colno500 = grep("500",colnames(spp_PA),fixed=TRUE) #filtering out raw variables
# rawvar = restdf[,colno500] #subsetting the data for raw variables
# prop0 = function (x) length(which(x==0))/length(x) #function to calculate prop of zero
# filtercolnames = colnames(rawvar[which(sapply(rawvar,prop0)<0.9)]) #variable names filtered with less than 90% zeros   

#Manually read filtered csv with scales for filtercolnames - filtered for more than 90% zeros
#spp_PA_filtered = read.csv("data/RSC_PO_vals.csv")
spp_PA_filtered = read.csv("final/InputData/cats_PresenceAbsence_all_filtered90.csv")

#Remove NAs - messes up the dredge
con = rowSums(is.na(spp_PA_filtered)) > 0
subdf <- subset.data.frame(spp_PA_filtered, subset = con)
restdf <- subset.data.frame(spp_PA_filtered, subset = !con) 

# #Within range selection
# restdf = subset(restdf, FC_range == 1) #Change species name
# restdf =cbind(restdf$FC, restdf[,c(11:194)]) #Only Presence + covariate value should be there
# names(restdf)[1] = "FC"
# colnames(restdf)

presence_glm <- subset(restdf, FC == 1) #Change species name
absence_glm <- subset(restdf, FC == 0) #Change species name

# # presence training and testing datasets
smp_size.p_glm <- floor(0.8 * nrow(presence_glm))
set.seed(666)
train_ind.p_glm <- sample(seq_len(nrow(presence_glm)), smp_size.p_glm)
train.p_glm <- presence_glm[train_ind.p_glm,]
test.p_glm <- presence_glm[-train_ind.p_glm,]
#
# # absence training and testing datasets
smp_size.a_glm <- floor(0.8 * nrow(absence_glm))
set.seed(666)
train_ind.a_glm <- sample(seq_len(nrow(absence_glm)), smp_size.a_glm)
train.a_glm <- absence_glm[train_ind.a_glm,]
test.a_glm <- absence_glm[-train_ind.a_glm,]
#
# #combine datasets
df.train_glm <- rbind(train.p_glm, train.a_glm)
df.test_glm <- rbind(test.p_glm, test.a_glm)
#
df.train_glm
df.test_glm
table(df.train_glm$RSC) #Change species name
table(df.test_glm$RSC) #Change species name
df.train_glm$S.No.= rownames(df.train_glm)
df.test_glm$S.No.= rownames(df.test_glm)


#########Running GLM using train dataset##########                              

PA_spp_sites = cbind(df.train_glm$S.No.,df.train_glm$JC, df.train_glm[, c(6:189)]) #Subsetting for species; only SNo retained for now, no coords


  scale_glm = data.frame() # create data frame to store results
    for (k in 3:ncol(PA_spp_sites)) { #just take the variables columns
    # if (length(unique(PA_spp_sites[[colnames(PA_spp_sites)[k]]]))>5){
      tmp = as.formula(paste0("df.train_glm$JC~",colnames(PA_spp_sites)[k]))
      fit = glm(tmp, family=binomial(link="logit"), data = PA_spp_sites)
      
      ## capture summary stats
      intercept = coef(summary(fit))[1]
      slope = coef(summary(fit))[2]
      p.value = coef(summary(fit))[8]
      AIC = AIC(fit)
      Deviance = deviance(fit)
      
      # get coefficents of fit
      cfit = coef(summary(fit))
      
      # create temporary data frame
      df = data.frame(var = colnames(PA_spp_sites)[k], intercept = cfit[1],
                       slope = cfit[2], p.value = cfit[8],
                       AIC = AIC(fit), Deviance = deviance(fit), stringsAsFactors = F)
      
      # bind rows of temporary data frame to the results data frame
      scale_glm = rbind(scale_glm, df)
    } 


  #scale_glm = scale_glm[-1,]
  scale_glm2 = scale_glm %>%   ###add group column
    mutate(group = case_when(grepl("ndvi", var) ~ "NDVI",
                             grepl("svvi", var) ~ "SVVI",
                             grepl("gpp", var) ~ "GPP",
                             grepl("water", var) ~ "Water_percent",
                             grepl("tcover", var) ~ "Tree_Cover_percent",
                             grepl("hpop", var) ~ "Human_Population",
                             grepl("gdp", var) ~ "GDP",
                             grepl("fire", var) ~ "HFire",
                             grepl("hloss", var) ~ "HLOss",
                             grepl("degr", var) ~ "HDegradation",
                             grepl("prept", var) ~ "Precipitation",
                             grepl("prepse", var) ~ "Precipitation_seasonality",
                             grepl("tempt", var) ~ "Temperature",
                             grepl("tempse", var) ~ "Temperature_seasonality",
                             grepl("cti", var) ~ "CTI",
                             grepl("rough", var) ~ "Roughness",
                             grepl("tpi", var) ~ "TPI",
                             grepl("ele", var) ~ "Elevation",
                             grepl("modi", var) ~ "Global_Modification_Index",
                             grepl("scec", var) ~ "Soil_CEC",
                             grepl("sclay", var) ~ "Soil_clay",
                             grepl("ssand", var) ~ "Soil_sand",
                             grepl("soiln", var) ~ "Soil_nitrogen",
                             grepl("ssoc", var) ~ "Soil_SOC",
                             grepl("sbdod", var) ~ "Soil_BDOD",
                             grepl("road", var) ~ "Roads",
                             grepl("lulcbuilt", var) ~ "LULC_Builtup",
                             grepl("lulccrop", var) ~ "LULC_Cropland",
                             grepl("lulcgrass", var) ~ "LULC_Grassland",
                             grepl("lulcshrub", var) ~ "LULC_Shrubland",
                             grepl("lulctree", var) ~ "LULC_TreeCover",
                             grepl("lulcwetl", var) ~ "LULC_Wetland",
                             grepl("swc", var) ~ "SoilWaterContent"))
                             
  table(scale_glm2$group)
  
  ##Ordering variables by group
  scale_glm2_ord = scale_glm2[order(scale_glm2[,'group']),]  ##order variables
  
  
  ##select each variable and the best AIC
  scale_glm2_ord2 = scale_glm2_ord %>% group_by(group) %>% slice_min(n = 1, order_by=AIC) ###n is the number top/lowest rows
  scale_glm2_ord2$sd = scale_glm2_ord %>% group_by(group)%>% summarise_at(vars(AIC), list(name=sd))
  
  scale_glm2_ord2 = as.data.frame(scale_glm2_ord2)
  
 
  write.table("RSC_scale_selection", "final/Results_GLM_scale_selection_PO_range.csv", append=TRUE)
  write.table(scale_glm2_ord2, "final/Results_GLM_scale_selection_PO_range.csv", append=TRUE)
  
  #Storing scale optimization result separately for each species
  rsc_scale_glm = scale_glm2_ord2

  ####create a data frame with PA column + selected scales
  rownames(rsc_scale_glm) = rsc_scale_glm$var ##to keep the location_ids, change species
  scale_glm3 = as.data.frame(t(rsc_scale_glm)) #change species
  all.df_glm<- cbind(df.train_glm$RSC, PA_spp_sites[,names(PA_spp_sites) %in% names(scale_glm3)])#change species ##keep variables with only selected scales
  colnames(all.df_glm)
  
  ##
  #################Variables correlation analysis################
  ##
  threshold = 0.7
  
  #order variables by AIC from high to low
  var.SO_aic1 = rsc_scale_glm[order(rsc_scale_glm$AIC, decreasing=T),] #Change species 
  sel.var = var.SO_aic1$var
  
  # response variable should be on the 1st column
  svar = all.df_glm
  cor = cor(svar[,sel.var],method="pearson", use = "complete.obs") ##take only the explanatory variables
  
  # remove max corr
  for (l in 1:ncol(cor)) { ##give 0 for the correlation pair
    cor[l,l] <- 0
  }
  cor <- data.frame(cor)
  
  while (max(abs(cor)) > threshold) {
    breakloop <- FALSE
    for (l in 1:ncol(cor)) {
      for (m in 1:ncol(cor)) {
        if (abs(cor)[l,m] >= threshold) {
          imp.x <- var.SO_aic1[m,5] #based on least AIC
          imp.y <-var.SO_aic1[l,5]
          if (imp.x > imp.y) { 
            drop <- colnames(cor)[m]
          }
          else {
            drop <- rownames(cor)[l]
          }
          breakloop <- TRUE
          break
        }
      }
      if (breakloop) break
    }
    cor <- cor[row.names(cor) != drop, colnames(cor) != drop]
  }
  print(names(cor))
  corprint = print(names(cor))
  #create a data frame with response variable and the best cov for later analyses
  cor.df_glm = cbind(all.df_glm[1], all.df_glm[,names(all.df_glm) %in% names(cor)])  
  colnames(cor.df_glm)
  rsc_uncorr = cor.df_glm #Change species name
  
  
  if (ncol(cor.df_glm)>2){
    print("yes - correlation")
    write.table("RSC_uncorrelated_variables", "final/Results_glm_variables_PearsonCorr_PO_range.csv", append=TRUE) #Change species name
    write.table(corprint, "final/Results_glm_variables_PearsonCorr_PO_range.csv", append=TRUE)
  }else{
    print("no - correlation")
    write.table("no variables after correlation", "final/Results_glm_variables_PearsonCorr_PO_range.csv", append=TRUE)
  }

  #################GLM Multiscale models################
  ##
  all.df_glm2 = jc_uncorr #Change species name
  
  if (ncol(all.df_glm2)>2){
    print("yes - multimodel")
    var_glm <- all.df_glm2[,-1]
    var_names <- colnames(var_glm)
    var_fml<-paste(var_names,collapse="+")  ###name of variables
  }else{
    print("no - multimodel")
    var_names <- colnames(all.df_glm2)
    var_fml<-var_names[2]  ###name of variables
  }
  
  m_fml<-as.formula(paste("df.train_glm$JC~",var_fml)) # # PresenceAbsence data vs variables
  options(na.action="na.fail") #important for dredge
  
  start_time_dredge = Sys.time()
  glm.fit <- glm(m_fml,data=all.df_glm2,family="binomial") #global model with all variables. 
  glm.d<-dredge(glm.fit)#all subsets from global model - perform glm subseting all variables with all combinations
  end_time_dredge = Sys.time()
  
  ###Saving dredge results for species
  glm.d.jc = glm.d
  
  #Model averaging?
  #glm.fc.best<-subset(glm.d.fc, delta==0) #best model (top - delta==0) otherwise if you want all glm.d should have that. You can define any subset you want within given delta AIC...you probably want that
  
  m.sum<-summary(get.models(glm.d.jc,1)[[1]])$coefficient #this is a full table for best model! Change species name
  m.sum<- as.data.frame(m.sum)
  coef.p<-m.sum[,c(1,4)]
  names(coef.p)[1]<-"Coefficient"
  names(coef.p)[2]<-"p"

  glm.final1 = get.models(glm.d.jc,1)[[1]] #Best model, change species name
  
  #Loading raster stack for prediction
  ras_crop = list.files("C:/Divya/Rasters/Carol250k/all_filtered_rasters/predict", pattern=".tif", full.names=T)
  xvars <- raster::stack(ras_crop)
  names(xvars) 
  
  # all.df_glm2 = rsc_uncorr
  # glm.final = get.models(glm.d.rsc,1)[[1]] #Best model, change species name
  
  mod <- terra::predict(xvars, glm.final1, type="response", na.rm=TRUE, overwrite=TRUE)
  plot(mod)
  
  writeRaster(mod,"final/RSC_PO_range_predicted_raster.tif")
  
  #Saving all data (raw data, scale glm, uncorrelated variables, dredge result, PO data) for each species
  save(df.train_glm, df.test_glm, rsc_scale_glm, rsc_uncorr, glm.d.rsc, glm.final, file = "final/RSC_PO_range_dat.rda")
  
  
  
  #saving validation data with different name
  df.train_rsc_PO_range = df.train_glm
  df.test_rsc_PO_range = df.test_glm
  glm.final_rsc_PO_range = glm.final
  save(df.train_rsc_PO_range, df.test_rsc_PO_range, glm.final_rsc_PO_range, file = "final/rsc_PO_range_val.rda")
  #rm(df.test_glm, df.train_glm, fc_uncorr, glm.final, glm.d.fc, fc_scale_glm)
  
  
  #################Model evaluation################
  
    #Declare train and test data based on required validation
    #df.train_glm = df.train_PO_fc
    df.test_glm = df.test_fc_PA_india
    glm.final = glm.final_fc_PA_india

    #df.train_glm$location_id <- rownames(df.train_glm)
    df.test_glm$location_id <- rownames(df.test_glm)
    #train and test (test/validation is the most important)
    colnames(df.test_glm)
    
 
    ###TEST DATA###
    sdata.v<-as.data.frame(cbind(df.test_glm$location_id, df.test_glm$FC, predict(glm.final,df.test_glm,type='response'))) #second column should be the presence-absence data
    #change the column names of sdata
    names(sdata.v)<-c('ID','observed','fitted')
    
    #Assessing model accuracy (Sensitivity Specificity, PCC and Kappa)
    saccu.v<-presence.absence.accuracy(sdata.v,threshold=30,st.dev=FALSE)
    saccu.v[,-c(1,2)]<-signif(saccu.v[,-c(1,2)],digits=2)
    saccu.v # this is a table with all the values you need for different thresholds. Normally I would select the row with the highest Kappa value. AUC is threshold independent so you will get only one value
    
    saccu_best.v <- saccu.v %>% slice_max(n = 1, order_by=Kappa) ###n is the number top/lowest rows
    saccu_best.v$TSS <- (saccu_best.v$sensitivity + saccu_best.v$specificity)-1    
    saccu_best.v
    
    write.table("RSC_PO_PAtest_evaluation", "final/Results_GLM_model_evaluation_Range.csv", append=TRUE)
    write.table(saccu_best.v, "final/Results_GLM_model_evaluation_Range.csv", append=TRUE)
    rm(sdata.v,saccu.v,saccu_best.v, df.test_glm, glm.final)
    
    #save(df.test_PA_fc, df.train_PA_fc, glm.final_PA_fc, fc_uncorr_PA, file = "data/FC_PA_val.rda")
    ##
    
    #Estimating AUC values
    library(pROC)
    library(raster)
    dat = read.csv("final/InputData/FC_PO_Range_vals.csv")
    raster = raster::raster("final/Results/Georeferenced_predicted_rasters/FC_avg_range_predicted_raster_modified.tif")
    plot(raster)
    #dat = subset(dat, LC_range==1) # use this to subset according to conditions
    test = dat
    test$SDM_vals = raster::extract(raster, cbind(dat$Lon, dat$Lat))
    test$SDM_vals
    obs = test$FC[-which(is.na(test$SDM_vals))]
    pred = test$SDM_vals[-which(is.na(test$SDM_vals))]
    
    roc_obj = roc(obs, pred)
    auc(roc_obj)
    roc_df = data.frame(
      TPR=rev(roc_obj$sensitivities), 
      FPR=rev(1 - roc_obj$specificities))
    # labels=roc_obj$response, 
    # scores=roc_obj$predictor)
    plot(roc_df$FPR, roc_df$TPR)
    
    ##Boyce's index
    mod = raster::raster("final/Results/Georeferenced_predicted_rasters/fc_PO_range_predicted_raster_modified.tif")
    plot(mod)
    df.test_glm = df.test_fc_PO_range
    table(df.test_glm$FC)
    df.test_glm = subset(df.test_glm, FC == 1)
    obs = cbind(df.test_glm$Lon, df.test_glm$Lat)
    colnames(obs) = c('x', 'y')
    points(obs)
    ecospat.boyce (na.omit(getValues(mod)), na.omit(extract(mod, obs)), nclass=0, window.w="default", res=100, PEplot = TRUE, rm.duplicate = TRUE, method = 'spearman')
    
#################Response curves################
    ##
    coef.var <- coef.p[-1,]
    coef.var$var <-  rownames(coef.var)
    coef.6var <- coef.var %>% slice_min(n = 6, order_by=p) ###n is the number top/lowest rows
    coef.6var <- coef.6var[,3]
    
    par(mfrow=c(2,3))
    for(n in coef.6var) {
      var_name <- paste(n)
      df_var <- all.df_glm2 %>% dplyr::select(contains(var_name))
      plot(df_var[,1], all.df_glm2$`restdf$RSC`, ylab="Probability", xlab=var_name, title("RSC_global"))
      plsmo(df_var[,1], y=all.df_glm2$`restdf$RSC`, method="lowess", add=T, trim=0, col="blue")
    }
    dev.print(pdf, "RSC_responsecurve_global.pdf")
  


#####################Rasterr averaging##############
library(maptools) # For geometry
library(terra) # Performs the crop and mask
library(sf)
library(rgdal)
library(raster)
    
#Raster avergaing
ras_PA = raster::raster("final/Results/Georeferenced_predicted_rasters/LC_PO_india_predicted_raster_modified.tif")
plot(ras_PA)    
ras_PO = raster::raster("final/Results/Georeferenced_predicted_rasters/LC_PO_range_predicted_raster_modified.tif")
plot(ras_PO)
ras_avg = raster::raster("final/Results/Georeferenced_predicted_rasters/RSC_avg_india_predicted_raster_modified.tif")
plot(ras_avg)

#avg = (ras_PA*0.45) + (ras_PO*0.55)
#plot(avg)
#writeRaster(avg, "final/Results/Georeferenced_predicted_rasters/RSC_avg_range_predicted_raster_modified.tif")

#Masking and cropping rasters

india= readOGR(dsn = "I:/My Drive/WildCru_Comparative_HSM202/Data/Rasters/final/InputData/", layer = "India_boundary")
range= readOGR(dsn = "I:/My Drive/WildCru_Comparative_HSM202/Data/Rasters/final/InputData/", layer = "LC_Range")


raster_PA = crop(rast(ras_PA), vect(india), mask= T)
raster_PO = crop(rast(ras_PO), vect(india), mask= T)
#raster_avg = crop(rast(ras_avg), vect(india), mask= T)

plot(raster_avg)
plot(range, lwd=1, add=T)

#Raster stack of different predictions
pred <- stack(raster(raster_PA), raster(raster_PO))#, raster(raster_avg))
names(pred)

#layerCor(pred, "pearson", na.rm =T)
jnk=layerStats(pred, 'pearson', na.rm=T)
corr_matrix=jnk$'pearson correlation coefficient'
corr_matrix

#########Checking predictions and training data#############
india= readOGR(dsn = "C:/Divya/Species/India_boundary/", layer = "India_boundary")
mod = raster::raster("final/Results/Georeferenced_predicted_rasters/JC_PA_india_predicted_raster_modified.tif")
clip = crop(rast(mod), vect(india), mask= T)
plot(clip)

df.train_glm = df.train_lc_PA_india
pre = subset(df.train_glm, LC == 1)
abs = subset(df.train_glm, LC == 0)
points(pre$Lon, pre$Lat, col ="blue")
points(abs$Lon, abs$Lat, col = "red", cex = 0.1)

#########Inspecting covariate space covered by data##########
#Plotting PCAs
library(ggfortify)
library(ggplot2)
library(gridExtra)
library(ggpubr)
library(bioconductor)
library(PCAtools)
library(cluster)
library(ggConvexHull)
library(sf)

#Loading data
main = data.frame()

dat = df.train_rsc_PO_range[, grepl("500" , names(df.train_rsc_PO_range))] 
dat = cbind(dat, df.train_rsc_PO_range$RSC)
names(dat)[names(dat) == "df.train_rsc_PO_range$RSC"] = "PA"
dat$Record = ifelse(dat$PA == 1, "PO - Presence.Range", "PO - Absence.Range")
dat$Species = "RSC"
colnames(dat)

main = rbind(main, dat)
colnames(main)

save(main, file = "final/OutputData/all_train.rda")

PA_ind = df.train_lc_PA_india[, grepl("500" , names(df.train_lc_PA_india))]
PA_ind = cbind(PA_ind, df.train_lc_PA_india$LC)
names(PA_ind)[names(PA_ind) == "df.train_lc_PA_india$LC"] = "LC"
PA_ind$Record = ifelse(PA_ind$LC == 1, "PA - Presence.India", "PA - Absence.India")
colnames(PA_ind)


PO_ind = df.train_lc_PO_india[, grepl("500" , names(df.train_lc_PO_india))]
PO_ind = cbind(PO_ind, df.train_lc_PO_india$LC)
names(PO_ind)[names(PO_ind) == "df.train_lc_PO_india$LC"] = "LC"
PO_ind$Record = ifelse(PO_ind$LC == 1, "PO - Presence.India", "PO - Absence.India")
colnames(PO_ind)

PA_ran = df.train_lc_PA_range[, grepl("500" , names(df.train_lc_PA_range))]
PA_ran = cbind(PA_ran, df.train_lc_PA_range$LC)
names(PA_ran)[names(PA_ran) == "df.train_lc_PA_range$LC"] = "LC"
PA_ran$Record = ifelse(PA_ran$LC == 1, "PA - Presence.Range", "PA - Absence.Range")
colnames(PA_ran)


PO_ran = df.train_lc_PO_range[, grepl("500" , names(df.train_lc_PO_range))]
PO_ran = cbind(PO_ran, df.train_lc_PO_range$LC)
names(PO_ran)[names(PO_ran) == "df.train_lc_PO_range$LC"] = "LC"
PO_ran$Record = ifelse(PO_ran$LC == 1, "PO - Presence.Range", "PO - Absence.Range")
colnames(PO_ran)

dat = PO_ran

dat = rbind(PA_ind, PO_ind, PA_ran, PO_ran)

#Subsetting for specific PCAs
dat = subset(main, Species == "JC"& (Record == "PA - Presence.India" | Record == "PO - Presence.India"))
dat$Record = as.factor(dat$Record)
pca = prcomp(dat[,grepl("500" , names(dat))], # need to remove categorical variables
                          center = TRUE,
                          scale. = TRUE)


#temp = as.data.frame(pca_trial$rotation)
#temp$Species = "FC"
#pca = rbind (pca_jc, pca_rsc, pca_fc, pca_lc)

##Creating ellipses using correlation matrix

# library(ellipse)
# library(sf)
# polygon(ellipse(c1*(max(abs(pca$rotation))*1), centre=colMeans(tab[1:2980,]), level=0.9999999), col=adjustcolor("skyblue2", alpha.f=0.25), border="skyblue")
# polygon(ellipse(c2*(max(abs(pca$rotation))*1), centre=colMeans(tab[2981:3251,]), level=0.9999999), col=adjustcolor("gold", alpha.f=0.25), border="gold2")
# pt1 = data.frame(ellipse(c1*(max(abs(pca$rotation))*1), centre=colMeans(tab[1:2980,]), level=0.9999999), col=adjustcolor("skyblue2", alpha.f=0.25), border="skyblue")
# pt2 = data.frame(ellipse(c2*(max(abs(pca$rotation))*1), centre=colMeans(tab[2981:3251,]), level=0.9999999))


plot1 <- autoplot(pca, data = dat, colour = 'Record', frame = T, frame.type = "norm", size = 0.01, level = .50, conf.int=TRUE) #can make ellipses or hulls using frame = T
plot2 <- autoplot(pca, data = dat, colour = 'Record') + theme(legend.position = c(0.3, 0.8)) + scale_y_continuous(limits = c(-0.1, 0.1))
plot3 <- autoplot(pca, data = dat, colour = 'Record') + theme(legend.position = c(0.3, 0.8)) + scale_y_continuous(limits = c(-0.1, 0.1))
plot4 <- autoplot(pca, data = dat, colour = 'Record') + theme(legend.position = c(0.3, 0.8)) + scale_y_continuous(limits = c(-0.1, 0.1))
#plot_final = grid.arrange(plot1, plot2, plot3, plot4, ncol=2, nrow=2)
plot_final = ggarrange(plot1, plot2, plot3, plot4, nrow=2, ncol=2, common.legend = T)
annotate_figure(plot_final, top = text_grob("Presences", 
                                      color = "black", face = "bold", size = 14))

##Final PCA and convex hull measurements
Species = unique(main$Species)
Record = unique(main$Record)
results = data.frame()
ind_plots = list()
sp_plots = list()
c1 = c(1,2,6,2,4)
c2 = c(3,4,8,6,8)
for(i in 1:length(Species)){
  #Subsetting data for pca
  dat_sp = subset(main, main$Species == Species[i])
  dat_sp = main[which(main$Species == Species[i]),]
  for(j in 1:length(c1))
  {
    dat = dat_sp[which(dat_sp$Record ==  Record[c1[j]] | dat_sp$Record == Record[c2[j]]),]
    #Calculating pc axis with numerical columns
    pca = prcomp(dat[,grepl("500" , names(dat))], center = TRUE, scale. = TRUE)
    #New dataframe to plot pc1 vs pc2
    newdf = data.frame("pc1" = pca$x[,1], "pc2" = pca$x[,2], "grp" = dat$Record)
    #Plotting with convex hulls
    #ind_plots[[((i-1)*length(c1))+j]] = ggplot(newdf, aes(pc1, pc2, fill=grp, col = grp)) + geom_point(size = 0.1) + geom_convexhull(alpha = 0.3) 
    #plot(plot1) 
    #Creating convex hull polygons
    pg1 = newdf %>% filter(grp == unique(newdf$grp)[1]) %>% 
      st_as_sf(coords = c("pc1", "pc2")) %>% summarise() %>% 
      concaveman::concaveman(concavity = 10)
    pg2 = newdf %>% filter(grp == unique(newdf$grp)[2]) %>% 
      st_as_sf(coords = c("pc1", "pc2")) %>% summarise() %>% 
      concaveman::concaveman(concavity = 10)
    pgi = st_intersection(pg1,pg2)
    #Calculating area under convex hulls and intersection
    area_inta_prop = st_area(pgi)/(st_area(pg1) + st_area(pg2) - st_area(pgi))
    area_int1_prop = st_area(pgi)/st_area(pg1)
    area_int2_prop = st_area(pgi)/st_area(pg2)
    area_int = st_area(pgi)
    area_1 = st_area(pg1)
    area_2 = st_area(pg2)
    results = rbind(results, cbind("Species" = Species[i],
                                   "Cat1" = Record[c1[j]],
                                   "Cat2" = Record[c2[j]],
                                   "Area_int" = area_int,
                                   "Area_1" = area_1,
                                   "Area_2" = area_2,
                                   "Area_inta_prop" = area_inta_prop,
                                   "Area_int1_prop" = area_int1_prop,
                                   "Area_int2_prop" = area_int2_prop))
  }
}


#######Plotting differences##########

##PCA plots of covariate space and species distributions

ind = read_sf("I:/My Drive/WildCru_Comparative_HSM202/Data/Rasters/final/InputData/India_boundary.shp")
ran = read_sf("I:/My Drive/WildCru_Comparative_HSM202/Data/Rasters/final/InputData/FC_Range.shp")
ran = st_make_valid(ran) #like fix geometries
ind = st_make_valid(ind)
sf_use_s2(FALSE)
ran_clip = st_intersection(ran, ind)
dist = ggplot() + geom_sf(data = ind) + geom_sf(data = ran_clip, fill = "green", alpha=0.2) + ggtitle("FC distribution") + theme(plot.title = element_text(hjust = 0.5))

##ggarrange plots per species
ggarrange(ind_plots[[6]], ind_plots[[7]], ind_plots[[8]], ind_plots[[9]], ind_plots[[10]], dist, legend = "top", nrow = 2, ncol = 3, labels = c("a","b","c","d","e","f"))


#Reformatting results for plots
temp = data.frame(results$Species)
temp$cat1.model = unlist(strsplit(results$Cat1, split = "[-|.]"))[seq(1,20*3,3)]
temp$cat1.PA = unlist(strsplit(results$Cat1, split = "[-|.]"))[seq(2,20*3,3)]
temp$cat1.extent = unlist(strsplit(results$Cat1, split = "[-|.]"))[seq(3,20*3,3)]
temp$cat2.model = unlist(strsplit(results$Cat2, split = "[-|.]"))[seq(1,20*3,3)]
temp$cat2.PA = unlist(strsplit(results$Cat2, split = "[-|.]"))[seq(2,20*3,3)]
temp$cat2.extent = unlist(strsplit(results$Cat2, split = "[-|.]"))[seq(3,20*3,3)]
temp$overlap = results$Area_inta_prop #used only one measure

write.csv(temp, "final/OutputData/MCP_overlap_values.csv")

#Comparison of sampling extent - India vs range
y = temp %>% filter(cat1.extent == "India" & cat2.extent == "Range")

ggplot(y, aes(x = results.Species, y = as.numeric(overlap), fill = cat1.model)) + 
  geom_bar(stat="identity", position=position_dodge()) + 
  labs(x = "Species", y = "Proportion of Overlap", fill = "Dataset") + 
  theme_minimal() + 
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 25),  # Increasing text size
        axis.text = element_text(face = "bold"),
        legend.text = element_text(size = 20)) +  # Increasing legend text size
  geom_hline(yintercept = mean(as.numeric(y$overlap), na.rm=TRUE), color='red', lty='dashed', lwd=1) + 
  scale_fill_manual(labels = c("PA", "PB"), values = c("#fdae61","#abd9e9"))


#Comparison of datasets - PA vs PO
x = temp %>% filter(cat1.extent != "India" | cat2.extent != "Range") %>% 
  mutate(cat = paste0(cat1.model, cat1.PA, cat1.extent, cat2.model, cat2.PA, cat2.extent))   
ggplot(x, aes(x = cat, y = as.numeric(overlap), fill = results.Species)) +
  geom_bar(stat="identity", position=position_dodge()) +
  labs(x = "Sampling category", y = "Proportion of Overlap", fill = "Species") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    text = element_text(size = 25),
    axis.text = element_text(face = "bold"),
    legend.text = element_text(size = 20)
  ) +
  scale_x_discrete(labels = c('Absence-India', 'Absence-Range', 'Presence')) +
  geom_hline(yintercept = mean(as.numeric(x$overlap), na.rm = TRUE), color = 'red', lty = 'dashed', lwd = 1) +
  scale_fill_manual(values = c("#66c2a5", "#fc8d62", "#bebada", "#b3de69"))+
  ylim(0, 1)


#"#fdb462","#8dd3c7",,"#bebada","#80b1d3","#fdbf6f""#fb8072""#8da0cb",

