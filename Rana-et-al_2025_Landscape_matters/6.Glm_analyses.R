library(betareg)
library(MASS)
library(dplyr)
library(broom)
library(purrr)
library(forcats)
library(performance)  # for r2()
library(ggplot2)
library(corrplot)

final = read.csv("./results/final_metrics_cropped.csv")

######### Checking for correlation between variables########
pred_vars <- final %>%
  dplyr::select(c(2:7, 28:32))

net_metrics <- final %>%
  dplyr::select(c(11:27))


# Compute correlation matrix
cor_matrix <- cor(pred_vars, use = "pairwise.complete.obs")

corrplot(cor_matrix, method = "color", type = "upper", tl.cex = 0.7)

##### Data exploration ######
response = final[,11:25]
response_long <- response %>%
  pivot_longer(cols = everything(), names_to = "metric", values_to = "value") %>%
  filter(!is.na(value))

### Density distributions for 15 network metrics 
ggplot(response_long, aes(x = value)) +
  geom_density(fill = "lightgreen", alpha = 0.6) +
  facet_wrap(~ metric, scales = "free", ncol = 4) +
  theme_minimal() +
  labs(title = "Density Plots of Network Metrics", x = "Value", y = "Density")

response_long %>%
  group_by(metric) %>%
  summarise(p_value = shapiro.test(value)$p.value)

##None of the network metrics are normal

######Finding appropriate distribution models######

network = colnames(final[,11:25])
landscape = colnames(final[,2:7])
species = colnames(final[,28:32])

# Define your response metrics
response_vars <- c("g_num_nodes", "g_num_edges", "g_density", "g_diameter", "g_avg_path_length",
                   "g_efficiency", "g_modularity", "g_components", "l_avg_degree", "l_avg_strength",
                   "l_avg_betweenness", "l_avg_closeness", "l_avg_eigenvector", "l_avg_local_clustering",
                   "l_flux_mean")

# Define your predictors
predictors <- c("ed", "np", "pd", "shdi", "lpi", "pland", "Adult_body_mass_g",
                "density_n_km2", "habitat_openess", "anthro_adaptability", "iucn_status")


# Initialize classification list
model_family <- list()

for (var in response_vars) {
  # Log-transform response
  log_var <- paste0("log_", var)
  final[[log_var]] <- log(final[[var]] + 1e-6)  # small offset to avoid log(0)
  
  # Test for normality
  sw_test <- shapiro.test(final[[log_var]])
  if (sw_test$p.value > 0.05) {
    model_family[[var]] <- "Gaussian (log-transformed)"
    next
  }
  
  # Check if original is count data --> checks for non-negative whole numbers
  if (all(final[[var]] %% 1 == 0) && all(final[[var]] >= 0)) {
    model_family[[var]] <- "Poisson"
    next
  }
  # Fit Gamma and Beta models and compare AIC
  formula_str <- paste(var, "~", paste(predictors, collapse = " + "))
  gamma_fit <- try(glm(as.formula(formula_str), data = final, family = Gamma(link = "log")), silent = TRUE)
  
  # Beta requires response in (0,1)
  scaled <- (final[[var]] * (nrow(final) - 1) + 0.5) / nrow(final)
  beta_fit <- try(betareg(as.formula(paste("scaled ~", paste(predictors, collapse = " + "))), data = final), silent = TRUE)
  
  # Compare AICs
  if (inherits(gamma_fit, "glm") && inherits(beta_fit, "betareg")) {
    if (AIC(gamma_fit) < AIC(beta_fit)) {
      model_family[[var]] <- "Gamma"
    } else {
      model_family[[var]] <- "Beta"
    }
  } else if (inherits(gamma_fit, "glm")) {
    model_family[[var]] <- "Gamma"
  } else if (inherits(beta_fit, "betareg")) {
    model_family[[var]] <- "Beta"
  } else {
    model_family[[var]] <- "Check manually"
  }
}

# View results
print(model_family)

#####Running appropriate GLMs#####

# Initialize results list
model_results <- list()

for (response in names(model_family)) {
  family <- model_family[[response]]
  
  for (predictor in predictors) {
    df <- final
    
    # Scale predictor
    df[[paste0("scaled_", predictor)]] <- scale(df[[predictor]], center = TRUE, scale = TRUE)
    scaled_predictor <- paste0("scaled_", predictor)
    
    fit <- tryCatch({
      if (family == "Gaussian (log-transformed)") {
        # Create log-transformed response on the fly
        df$log_response <- log(df[[response]] + 1e-6)
        glm(as.formula(paste("log_response ~", scaled_predictor)), data = df, family = gaussian())
        
      } else if (family == "Poisson") {
        glm(as.formula(paste(response, "~", scaled_predictor)), data = df, family = poisson())
        
      } else if (family == "Gamma") {
        glm(as.formula(paste(response, "~", scaled_predictor)), data = df, family = Gamma(link = "log"))
        
      } else if (family == "Beta") {
        scaled <- (df[[response]] * (nrow(df) - 1) + 0.5) / nrow(df)
        scaled <- pmin(pmax(scaled, 1e-6), 1 - 1e-6)
        df$scaled_response <- scaled
        betareg(as.formula(paste("scaled_response ~", scaled_predictor)), data = df)
        
      } else {
        NULL
      }
    }, error = function(e) NULL)
    
    if (is.null(fit)) next
    
    # Extract R² using performance
    r2_val <- tryCatch({
      r2(fit)$R2
    }, error = function(e) NA)
    
    tidy_fit <- broom::tidy(fit) %>%
      filter(term == scaled_predictor) %>%
      mutate(
        response = response,
        predictor = predictor,
        direction = case_when(
          p.value < 0.05 & estimate > 0 ~ "positive",
          p.value < 0.05 & estimate < 0 ~ "negative",
          TRUE ~ "undetermined"
        ),
        is_significant = p.value < 0.05,
        r2 = r2_val
      ) %>%
      dplyr::select(response, predictor, estimate, p.value, direction, is_significant, r2)
    
    tidy_fit$model_family <- family
    
    model_results[[paste(response, predictor, sep = "_")]] <- tidy_fit
  }
}

# Combine all results
final_model_summary <- bind_rows(model_results)
write.csv(final_model_summary, "./results/final_model_summary.csv", row.names = F)

#####Visualizing model outputs########

final = read.csv("./results/final_metrics_cropped.csv")

# Define metric classification
metric_info <- tibble::tibble(
  response = c(
    "g_num_nodes", "g_num_edges", "g_density", "g_diameter", "g_avg_path_length",
    "g_efficiency", "g_modularity", "g_components",
    "l_avg_degree", "l_avg_strength", "l_avg_betweenness",
    "l_avg_closeness", "l_avg_eigenvector", "l_avg_local_clustering", "l_flux_mean"
  ),
  response_scale = c(rep("global", 8), rep("local", 7)),
  response_functional = c(
    "structural complexity", "structural complexity", "structural complexity", "connectivity", "connectivity","connectivity", "structural complexity", "structural complexity", "structural complexity",
    "structural complexity", "centrality", "centrality", "centrality", "structural complexity", "connectivity")
)
# Define predictor groups
network = colnames(final[,11:25])
landscape = colnames(final[,2:7])
species = colnames(final[,28:32])

# Join metric classification to model results
glm_plot_df <- final_model_summary %>%
  left_join(metric_info, by = "response") %>%
  mutate(
    predictor_type = case_when(
      predictor %in% species ~ "species",
      predictor %in% landscape ~ "landscape",
      TRUE ~ "other"
    ),
    predictor_type = factor(predictor_type, levels = c("species", "landscape", "other")),
    response = fct_reorder(response, as.numeric(factor(response_functional)))
  )

####### A. Significant observations with estimate and r2

# Plot heatmap
p <- ggplot(glm_plot_df, aes(x = predictor, y = response, fill = direction)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(r2, 2)), size = 4) +
  scale_fill_manual(
    values = c("positive" = "#237194", "negative" = "#B98C88", "undetermined" = "#F4E4BA"),
    labels = c("positive" = "positive", "negative" = "negative", "undetermined" = "non-significant"),
    name = "Effect") +
  labs(x = "Predictor Traits", y = "Network Metrics") +
  facet_grid(response_functional ~ predictor_type, scales = "free", space = "free") +
  theme(
    axis.title = element_text(face = "bold", size = 20),
    axis.text = element_text(size = 15),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank(),
    legend.title = element_text(face = "bold", size = 15),
    legend.text = element_text(size = 15),
    strip.text = element_text(face = "bold", size = 20),
    legend.position = "bottom"
  )

# Save plot
ggsave("./graphs/glm_significance_heatmap_revised_with_r2.png", plot = p, width = 14, height = 14, dpi = 500)

####### B. Significant observations against predictions
prediction = read.csv("./graphs/network_predictions_summary.csv")

# 1. Pivot prediction file to long format
prediction_long <- prediction %>%
  pivot_longer(
    cols = starts_with("g_") | starts_with("l_"),
    names_to = "response",
    values_to = "expected_dir"
  )

# 2. Join with glm_plot_df
plot_df <- glm_plot_df %>%
  left_join(prediction_long, by = c("predictor", "response")) %>%
  mutate(
    mismatch = case_when(
      direction == "undetermined" ~ FALSE,  
      expected_dir == "undetermined" ~ FALSE,  
      direction != expected_dir ~ TRUE,
      TRUE ~ FALSE
    )
  )

# 3. Plot with mismatch outlined
p = ggplot(plot_df, aes(x = predictor, y = response)) +
  geom_tile(aes(fill = direction), color = "white") +
  geom_tile(data = filter(plot_df, mismatch),
            color = "black", linewidth = 1.2, fill = NA) +  # add outline to mismatches
  scale_fill_manual(
    values = c("positive" = "#237194", "negative" = "#B98C88", "undetermined" = "#F4E4BA"),
    name = "Effect"
  ) +
  labs(x = "Predictor Traits", y = "Network Properties") +
  facet_grid(response_functional ~ predictor_type, scales = "free", space = "free") +
  theme_minimal(base_size = 15) +
  theme(
    axis.title = element_text(face = "bold", size = 20),
    axis.text = element_text(size = 15),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank(),
    legend.title = element_text(face = "bold", size = 15),
    legend.text = element_text(size = 15),
    strip.text = element_text(face = "bold", size = 15),
    legend.position = "bottom")

ggsave("./graphs/glm_significance_with_prediction_revised.png", plot = p, width = 14, height = 10, dpi = 500)


######## Relative importance #########

# General model fitting + R2 extraction
fit_model <- function(response, predictors, data, family) {
  df <- data
  
  # Build formula string
  formula_str <- paste(response, "~", paste(predictors, collapse = " + "))
  
  # Fit model depending on family
  fit <- tryCatch({
    if (family == "Gaussian (log-transformed)") {
      df$log_response <- log(df[[response]] + 1e-6)
      lm(as.formula(paste("log_response ~", paste(predictors, collapse = " + "))), data = df)
      
    } else if (family == "Poisson") {
      glm(as.formula(formula_str), data = df, family = poisson())
      
    } else if (family == "Gamma") {
      glm(as.formula(formula_str), data = df, family = Gamma(link = "log"))
      
    } else if (family == "Beta") {
      # Smithson & Verkuilen rescaling
      scaled <- (df[[response]] * (nrow(df) - 1) + 0.5) / nrow(df)
      scaled <- pmin(pmax(scaled, 1e-6), 1 - 1e-6)
      df$scaled_response <- scaled
      betareg(as.formula(paste("scaled_response ~", paste(predictors, collapse = " + "))), data = df)
      
    } else {
      stop("Unsupported family")
    }
  }, error = function(e) NULL)
  
  # Extract R2
  r2_val <- tryCatch({
    r2(fit)$R2
  }, error = function(e) NA)
  
  list(fit = fit, r2 = r2_val)
}

# Manual relaimpo generalized
manual_relaimpo_mc <- function(response, predictors, data, family, nperm = 1000) {
  contrib <- setNames(rep(0, length(predictors)), predictors)
  
  # Monte Carlo: sample predictor orderings
  for (i in 1:nperm) {
    order <- sample(predictors, length(predictors))  # random permutation
    prev_r2 <- 0
    
    for (j in 1:length(order)) {
      current_vars <- order[1:j]
      res <- fit_model(response, current_vars, data, family)
      r2 <- res$r2
      if (!is.na(r2)) {
        contrib[order[j]] <- contrib[order[j]] + (r2 - prev_r2)
        prev_r2 <- r2
      }
    }
  }
  
  # Average across sampled permutations
  contrib <- contrib / nperm
  
  # Normalize to sum = 1
  contrib_norm <- contrib / sum(contrib, na.rm = TRUE)
  
  list(raw = contrib, normalized = contrib_norm)
}

### df: Dataframe with scaled predictors
# List of responses
responses <- c(
  "g_num_nodes", "g_num_edges", "g_density", "g_diameter", "g_avg_path_length",
  "g_efficiency", "g_modularity", "g_components",
  "l_avg_degree", "l_avg_strength", "l_avg_betweenness",
  "l_avg_closeness", "l_avg_eigenvector", "l_avg_local_clustering", "l_flux_mean"
)

all_rel_importance <- do.call(rbind, lapply(responses, function(resp) {
  fam <- model_family[[resp]]  # fetch the right family for the response
  
  res <- manual_relaimpo_mc(resp, scaled_predictors, df, fam)
  
  data.frame(
    response   = resp,
    family     = fam,
    predictor  = names(res$normalized),
    importance = res$normalized,
    stringsAsFactors = FALSE
  )
}))

all_rel_importance$predictor <- sub("^scaled_", "", all_rel_importance$predictor)

write.csv(all_rel_importance, "./results/scaled_predictor_variable_importance.csv", row.names = F)


########## Plotting relative importance ########

rel_importance = read.csv("./results/scaled_predictor_variable_importance.csv")

metric_info <- tibble::tibble(
  response = c(
    "g_num_nodes", "g_num_edges", "g_density", "g_diameter", "g_avg_path_length",
    "g_efficiency", "g_modularity", "g_components",
    "l_avg_degree", "l_avg_strength", "l_avg_betweenness",
    "l_avg_closeness", "l_avg_eigenvector", "l_avg_local_clustering", "l_flux_mean"
  ),
  response_scale = c(rep("global", 8), rep("local", 7)),
  response_functional = c(
    "structural complexity", "structural complexity", "structural complexity", "connectivity", "connectivity","connectivity", "structural complexity", "structural complexity", "structural complexity",
    "structural complexity", "centrality", "centrality", "centrality", "structural complexity", "connectivity")
)

# Join model results with metric information
summary_contrib <- rel_importance %>%
  left_join(metric_info, by = "response") %>%
  group_by(predictor, response_scale) %>%
  summarise(mean_importance = mean(importance, na.rm = TRUE),
    .groups = "drop") %>%
  mutate(predictor_type = case_when(
      predictor %in% species ~ "species",
      predictor %in% landscape ~ "landscape",
      TRUE ~ "other"
    )) 

# Pivot wider to compare local vs global
diff_summary <- summary_contrib %>%
  pivot_wider(names_from = response_scale, values_from = mean_importance) %>%
  mutate(
    importance_diff = ((global - local) / global) * 100,
    term = factor(predictor, levels = predictor_order)
  )

# Define desired predictor order
predictor_order <- c(
  "habitat_openess", "density_n_km2", "Adult_body_mass_g", "anthro_adaptability", "iucn_status",
  "pd", "pland", "ed", "lpi", "np", "shdi")

# Apply order to predictor variable
diff_summary$predictor <- factor(diff_summary$predictor, levels = predictor_order)


# Plot
ggplot(diff_summary, aes(x = predictor, y = importance_diff, fill = predictor_type)) +
  geom_col() +
  scale_fill_manual(values = c("landscape" = "#FAA32B", "species" = "#F4E4BA")) +
  labs(
    x = "Predictor variable",
    y = "Difference (Global - Local) in Mean Importance (%)",
    fill = "Predictor Type"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.background = element_blank(),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

# Join model results with metric information
summary_contrib <- rel_importance %>%
  left_join(metric_info, by = "response") %>%
  group_by(predictor, response_functional) %>%
  summarise(mean_importance = mean(importance, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(predictor_type = case_when(
    predictor %in% species ~ "species",
    predictor %in% landscape ~ "landscape",
    TRUE ~ "other"
  )) 

# Normalize mean_importance so each response category sums to 1
summary_contrib <- summary_contrib %>%
  group_by(response_functional) %>%
  mutate(prop_importance = mean_importance / sum(mean_importance))

# Calculate mean contribution for each response category, grouped by predictor type
mean_contribution_by_category <- summary_contrib %>%
  group_by(response_functional, predictor_type) %>%
  summarize(mean_importance = mean(prop_importance, na.rm = TRUE), .groups = "drop")

# Define desired predictor order
predictor_order <- c(
  "habitat_openess", "density_n_km2", "Adult_body_mass_g", "anthro_adaptability", "iucn_status",
  "pd", "pland", "ed", "lpi", "np", "shdi")

# Apply order to predictor variable
summary_contrib$predictor <- factor(summary_contrib$predictor, levels = predictor_order)

# Plot stacked bars with equal height
ggplot(summary_contrib, aes(x = predictor, y = prop_importance*100, fill = response_functional)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_manual(values = c("centrality" = "#FAA32B", "connectivity" = "#B98C88", "structural complexity" = "#F4E4BA")) + 
  labs(
    x = "Predictor variable",
    y = "Proportion of variable contribution",
    fill = "Response category",
    title = "Stacked bar plot of predictor importance"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.background = element_blank(),  
    plot.background = element_blank(),   
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )+
  coord_fixed(ratio = 8)   # makes it taller relative to width






