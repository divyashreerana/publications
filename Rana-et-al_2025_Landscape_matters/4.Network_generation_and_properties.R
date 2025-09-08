library(graph4lg)
library(raster)
library(sf)
library(dplyr)
library(igraph)
library(tibble)

species_trait = read.csv("./occurrence/species_traits.csv")
model = "ensemble_all"
# List of species
species_list = c("SB", "FC", "DH", "GJ", "IW", "JC", "LC", "LP", "RSC", "SH", "TG")

#######Node and linkset creation: Graphab###########
error_log <- list()
conversion_log <- data.frame() ##euc to cost, cost = slope*euc + intercept) based on edge weights of both linksets

for (species in species_list) {
  sp <- species
  
  grid_file <- paste0("./input_data/", species, "_selected_grids.rds")
  grid <- readRDS(grid_file)
  
  threshold <- read.csv("./output_data/Model_ensemble_thresholds.csv")
  home_range <- species_trait %>%
    filter(species == sp) %>%
    pull(Home_range)
  dispersal_dist <- species_trait %>%
    filter(species == sp) %>%
    pull(Dispersal_km)
  
  for (g in grid$grid_id) {
    threshold_value <- threshold %>%
      filter(Species == species, Grid == g, Model == model) %>%
      pull(MaxSS_Threshold)
    
    proj_name <- paste0("graphab_", species, "_grid", g, "_model_", model)
    proj_path <- "D:/Professional/PhD/NCBS/SDM/Analysis/SDM_chap1/graphab/"
    
    # Project creation
    graphab_project(proj_name = proj_name,
                    raster = paste0("D:/Professional/PhD/NCBS/SDM/Analysis/SDM_chap1/network/", species, "_grid", g, "_model_", model, "_utm_bin_int.tif"),
                    habitat = 1,
                    minarea = home_range * 100,
                    proj_path = proj_path)
    
    # Linkset - COST distance
    tryCatch({
      graphab_link(proj_name = proj_name,
                   distance = "cost",
                   cost = paste0("D:/Professional/PhD/NCBS/SDM/Analysis/SDM_chap1/network/", species, "_grid", g, "_model_", model, "_rs.tif"),
                   name = "lkst_rs",
                   topo = "complete",
                   proj_path = proj_path)
    }, error = function(e) {
      error_log[[length(error_log) + 1]] <<- list(species = species, grid = g, step = "cost_link", error = e$message)
    })
    
    # Linkset - EUCLIDEAN distance
    tryCatch({
      graphab_link(proj_name = proj_name,
                   distance = "euclid",
                   name = "lkst_euclid",
                   topo = "complete",
                   proj_path = proj_path)
    }, error = function(e) {
      error_log[[length(error_log) + 1]] <<- list(species = species, grid = g, step = "euclid_link", error = e$message)
    })
    
    # Estimate conversion factor
    tryCatch({
      g_cost <- graphab_to_igraph(proj_name = proj_name,
                                  linkset = "lkst_rs",
                                  nodes = "patches",
                                  weight = "cost",
                                  proj_path = proj_path)
      
      g_euc <- graphab_to_igraph(proj_name = proj_name,
                                 linkset = "lkst_euclid",
                                 nodes = "patches",
                                 weight = "euclid",
                                 proj_path = proj_path)
      
      df_cost <- as_data_frame(g_cost, what = "edges") %>%
        dplyr::select(from, to, cost = weight)
      df_euc <- as_data_frame(g_euc, what = "edges") %>%
        dplyr::select(from, to, euclid = weight)
      
      df_both <- inner_join(df_cost, df_euc, by = c("from", "to"))
      
      fit <- lm(cost ~ euclid, data = df_both)
      
      conversion_log <- rbind(conversion_log,
                              data.frame(species = species,
                                         grid = g,
                                         slope = coef(fit)["euclid"],
                                         intercept = coef(fit)["(Intercept)"],
                                         r2 = summary(fit)$r.squared))
    }, error = function(e) {
      error_log[[length(error_log) + 1]] <<- list(species = species, grid = g, step = "conversion", error = e$message)
    })
  }
}

# Save logs
write.csv(conversion_log, "./graphab/cost_to_euclid_conversion.csv", row.names = FALSE)
saveRDS(error_log, "./graphab/link_creation_errors.rds")

# Print or save the error log
if (length(error_log) > 0) {
  print("Some link sets:")
  print(error_log)
  error_df <- do.call(rbind, lapply(error_log, as.data.frame))
  write.csv(error_df, "./graphab/linkset_errors.csv", row.names = FALSE)
}

#######Graph creation, pruning and network metric calculation: igraph#########

species_trait = read.csv("./occurrence/species_traits.csv")
model = "ensemble_all"
conversion_factors = read.csv("./graphab/cost_to_euclid_conversion.csv")
# List of species
species_list = c("SB", "FC", "DH", "GJ", "IW", "JC", "LC", "LP", "RSC", "SH", "TG")

all_metrics <- list()

for(species in species_list) {
  sp = species
  # Define file paths dynamically based on species
  grid_file = paste0("./input_data/", species, "_selected_grids.rds")
  grid = readRDS(grid_file)
  
  dispersal_dist = species_trait %>%
    filter(species == sp) %>%
    pull(Dispersal_km)
  
  # Looping over grids for each species to generate graphs
  for (g in grid$grid_id[1:length(grid$grid_id)]) {  
    
    conv_row <- conversion_factors %>%
      filter(species == sp, grid == g)
    if (nrow(conv_row) == 0) {
      message(paste("Skipping", species, "grid", g, ": no conversion factor found"))
      next
    }
    slope <- conv_row$slope
    
    # Convert dispersal distance (in km) to meters, then to cost distance
    thr_euclid <- dispersal_dist * 1000  # in meters
    thr_cost <- slope * thr_euclid
    thr_cost_0.25xup = thr_cost + (0.25*thr_cost)
    thr_cost_0.25xdown = thr_cost - (0.25*thr_cost)
    
    # Proceed with graph creation and pruning
    proj_name <- paste0("graphab_", species, "_grid", g, "_model_", model)
    proj_path <- "D:/Professional/PhD/NCBS/SDM/Analysis/SDM_chap1/graphab"
    
    land_graph <- tryCatch({
      graphab_to_igraph(
        proj_name = proj_name,
        linkset = "lkst_rs",
        nodes = "patches",
        weight = "cost",
        fig = TRUE,
        crds = TRUE,
        proj_path = proj_path
      )
    }, error = function(e) {
      message(paste("Skipping", species, "grid", g, ":", e$message))
      return(NULL)
    })
    
    if (is.null(land_graph)) next
    
    crds_patches <- land_graph[[2]]  # patch centroid coordinates
    land_graph <- land_graph[[1]]    # igraph object
    
    if (ecount(land_graph) > 1) {
      g_pruned <- delete_edges(land_graph, E(land_graph)[weight > thr_cost])
      g_0.25xup <- delete_edges(land_graph, E(land_graph)[weight > thr_cost_0.25xup])
      g_0.25xdown <- delete_edges(land_graph, E(land_graph)[weight > thr_cost_0.25xdown])
    } else {
      message(paste("Skipping", species, "grid", g, ": no edges in graph"))
      next
    }
    #plot_graph_lg(g_0.25xdown,
    #crds = crds_patches,
    #mode = "spatial",
    #node_size = "Area")
    
    graph_list <- list(
      g_pruned = g_pruned,
      g_0.25xup = g_0.25xup,
      g_0.25xdown = g_0.25xdown
    )
    
    for (type in names(graph_list)) {
      graph_obj <- graph_list[[type]]
      
      ##Flux calculation###
      # Extract patch area
      patch_areas <- setNames(V(graph_obj)$Area, V(graph_obj)$name)
      
      # Get edge list with weights
      edges_df <- as_data_frame(graph_obj, what = "edges")
      
      # Compute flux-like values
      edges_df$A_i <- patch_areas[as.character(edges_df$from)]
      edges_df$A_j <- patch_areas[as.character(edges_df$to)]
      edges_df$flux_like <- with(edges_df, ifelse(weight == 0, NA, (A_i * A_j) / weight))
      
      
      metrics_row <- tibble(
        species = species,
        grid = g,
        model = model,
        prune_type = type,  # Label the pruning type
        
        ## GLOBAL (NETWORK-LEVEL) METRICS
        g_num_nodes = gorder(graph_obj),  # Number of habitat patches (nodes) in the graph
        g_num_edges = gsize(graph_obj),  # Number of connections (links) between patches
        
        g_density = edge_density(graph_obj),  # Proportion of realized links out of all possible links
        
        g_diameter = diameter(graph_obj, directed = FALSE, unconnected = TRUE),  
        # Longest shortest path between any two patches (can indicate overall extent or fragmentation)
        
        g_avg_path_length = mean_distance(graph_obj, directed = FALSE, unconnected = TRUE),  
        # Mean of shortest paths between all pairs of patches (lower = more connected)
        
        g_clustering_coefficient = transitivity(graph_obj, type = "global"),  
        # Tendency for patches to form tightly connected groups (triangles)
        
        g_efficiency = global_efficiency(graph_obj),  
        # Overall efficiency of movement through the network (higher = easier connectivity)
        
        g_modularity = modularity(cluster_fast_greedy(graph_obj)),  
        # Degree to which the network is divided into modules or communities (higher = more fragmented)
        
        g_components = components(graph_obj)$no,  
        # Number of disconnected subnetworks (components); >1 indicates fragmentation
        
        g_assortativity_degree = assortativity_degree(graph_obj, directed = FALSE),  
        # Whether high-degree patches tend to connect to other high-degree patches (positive = yes)
        
        ## NODE-LEVEL AVERAGES (LOCAL METRICS)
        l_avg_degree = mean(degree(graph_obj)),  
        # Average number of links per patch (degree); indicates patch connectedness
        
        l_avg_strength = mean(strength(graph_obj)),  
        # Average sum of link weights per patch; in cost-based networks, lower = better
        
        l_avg_betweenness = mean(betweenness(graph_obj, directed = FALSE, normalized = TRUE)),  
        # Average number of shortest paths passing through a patch (importance for movement)
        
        l_avg_closeness = mean(closeness(graph_obj, normalized = TRUE), na.rm = TRUE),  
        # Average inverse distance from each patch to all others (centrality measure)
        
        l_avg_eigenvector = mean(eigen_centrality(graph_obj)$vector, na.rm = TRUE),  
        # Influence of a patch based on being connected to other central patches
        
        l_avg_local_clustering = mean(transitivity(graph_obj, type = "local", isolates = "zero"), na.rm = TRUE),         # Average of local clustering coefficient (how interconnected a patch’s neighbors are)
        
        l_flux_mean = mean(edges_df$flux_like, na.rm = TRUE)
        # Ease of movement across patch incorportaing node and edge property
      )
      all_metrics[[length(all_metrics) + 1]] <- metrics_row
    }
  }
  
}

all_metrics <- bind_rows(all_metrics)

write.csv(all_metrics, "./results/all_network_properties.csv")
