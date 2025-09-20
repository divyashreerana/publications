library(dplyr)
library(ggplot2)
library(raster)
library(sf)
library(ggspatial)    # for scale bar and north arrow
library(scatterpie)
library(cowplot)

####### Indetifying genotypes and recaptures ######

d <- diss.dist(dat)  # dissimilarity matrix
heatmap(as.matrix(d))     # quick heatmap to spot clones

mlg.table(dat)  # lists multilocus genotypes

## King kinship results
kin <- read.csv("./figure/admixture/allsb_sunbear_964x66_king_kinship.csv", header=TRUE)

kin$ID1_clean <- sub("^(([^_]+_[^_]+))_.*", "\\1", kin$ID1)
kin$ID2_clean <- sub("^(([^_]+_[^_]+))_.*", "\\1", kin$ID2)

ids <- unique(c(kin$ID1_clean, kin$ID2_clean))

mat <- matrix(NA, nrow=length(ids), ncol=length(ids),
              dimnames = list(ids, ids))

for(i in 1:nrow(kin)) {
  id1 <- kin$ID1_clean[i]
  id2 <- kin$ID2_clean[i]
  kinval <- kin$Kinship[i]
  mat[id1, id2] <- kinval
  mat[id2, id1] <- kinval  # symmetrical
}

diag(mat) <- 1

longmat <- melt(mat, na.rm=TRUE)

# classify the relationship category
longmat$category <- cut(
  longmat$value,
  breaks = c(-Inf, 0.0442, 0.0884, 0.177, 0.354, Inf),
  labels = c("Unrelated", "3rd-degree", "2nd-degree", "1st-degree", "Twin/Duplicate")
)

# plot
ggplot(longmat, aes(Var1, Var2, fill=category)) + 
  geom_tile() + 
  scale_fill_manual(values = c(
    "Unrelated" = "lightgrey",
    "3rd-degree" = "green",
    "2nd-degree" = "yellow",
    "1st-degree" = "orange",
    "Twin/Duplicate" = "red"
  )) + 
  theme(axis.text.x = element_text(angle=90, hjust=1, size = 4),
        axis.text.y = element_text(hjust=1, size = 4)) +
  labs(x="", y="", fill="Relationship")

ggsave("./figure/admixture/allsb_sunbear_964x66_relatedness.png", width = 6, height = 5, dpi = 300)

##### Identifying optimal K #####

#Add row header in the csv obtained from linux "Run	K	cv"

cvdata <- read.csv("./output/admixture/sunbear_964x66_admixture_cv.csv", header = TRUE)

# Summarize by K
cv_summary <- cvdata %>%
  group_by(K) %>%
  summarise(
    mean_cv = mean(cv),
    sd_cv   = sd(cv),
    n       = n()
  )

# Plot mean ± sd for each K
ggplot(cv_summary, aes(x = K, y = mean_cv)) +
  geom_line(color = "#BECF5F", size = 1.2) +
  geom_point(size = 3, color = "#BECF5F") +
  geom_errorbar(aes(ymin = mean_cv - sd_cv, ymax = mean_cv + sd_cv),
                width = 0.3, color = "#CC6677", size = 1) +
  scale_x_continuous(breaks = 1:10) +
  theme_minimal(base_size = 14) +
  labs(title = "ADMIXTURE CV error across K",
       x = "K (Number of clusters)",
       y = "Mean CV error ± SD")

ggsave("./figure/admixture/allsb_brownbear_584x52_admixture_cv.png", width = 6, height = 5, dpi = 300)

##### Plotting admixture results for optimal K (4) on landscape ####

ind.list = rownames(dat@tab)

#Load csv with sample, population, coordinates and membership probabilities
meta = read.csv("./figure/admixture/sunbear_964x66_membership_probability_k4.csv")
str(meta)

# Load data
shape <- st_read("./figure/Central_terai_PA.shp")
india <- st_read("D:/Professional/PhD/Permits/Shapefiles/India_boundary.shp")

# Add jittered coordinates
set.seed(123)
meta$Lat_jitter <- meta$Lat + runif(nrow(meta), -0.01, 0.01)
meta$Lon_jitter <- meta$Lon + runif(nrow(meta), -0.01, 0.01)

# --- Main map ---
main_map <- ggplot() +
  geom_sf(data = shape, fill = "antiquewhite", color = "grey60") +
  geom_scatterpie(
    aes(x = Lon_jitter, y = Lat_jitter, group = ind, r = 0.01),
    data = meta,
    cols = c("K1", "K2", "K3","K4"),
    color = "black"
  ) +
  scale_fill_manual(
    values = c(
      "#117733",  # deep forest green
      "#88CCEE",   # pale sky blue
      "#BECF5F",  # your lime green
      "#CC6677"  # # muted rose/earthy red for contrast
    ),
    name = "Cluster"
  ) +
  annotate("text", x = 80.81, y = 28.6,  label = "Dudhwa NP\nn=10",      size = 3, fontface = "bold") +
  annotate("text", x = 81.09, y = 28.1,  label = "Katarniaghat WLS\nn=0", size = 3, fontface = "bold") +
  annotate("text", x = 80.35, y = 28.13, label = "Kishanpur WLS\nn=15",   size = 3, fontface = "bold") +
  annotate("text", x = 80.25, y = 28.7,  label = "Pilibhit TR\nn=41",     size = 3, fontface = "bold") +
  annotation_scale(location = "br", width_hint = 0.15,
                   pad_x = unit(0.7, "in"), pad_y = unit(0.2, "in")) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = c(0.1, 0.2),
    legend.background = element_rect(fill = "white", color = "white"),
    legend.title = element_text(size = 12),
    legend.text  = element_text(size = 10),
    axis.ticks   = element_blank(),
    axis.title   = element_blank()
  )


# --- Inset map (India with highlight) ---
# You can highlight study area extent using st_bbox(shape)
study_bbox <- st_bbox(shape)

inset_map <- ggplot() +
  geom_sf(data = india, fill = "#DDCC77", color = "black") +
  geom_rect(
    aes(xmin = study_bbox["xmin"], xmax = study_bbox["xmax"],
        ymin = study_bbox["ymin"], ymax = study_bbox["ymax"]),
    color = "red", fill = NA, size = 1
  ) +
  theme_void()

# --- Combine inset and main map ---
final_plot <- ggdraw() +
  draw_plot(main_map) +
  draw_plot(inset_map, x = 0.65, y = 0.65, width = 0.3, height = 0.3)

final_plot

ggsave("./figure/sunbear_964x66_admixturek4_landscape.png", plot = final_plot, width = 6, height = 4.5, dpi = 300)

###### Plotting custom admixture like plot (k=3,4,5) ########

# read data
meta <- read.csv("./figure/admixture/sunbear_964x66_membership_probability_k5.csv")

# convert from wide to long
meta_long <- pivot_longer(
  meta,
  cols = starts_with("K"),
  names_to = "Cluster",
  values_to = "Proportion"
)

# keep the individual names as factors in the same order
meta_long$ind <- factor(meta_long$ind, levels = meta$ind)

# define cluster colours
cluster_cols <- c(
  "K1" = "#117733", 
  "K2" = "#88CCEE", 
  "K3" = "#BECF5F",
  "K4" = "#CC6677",
  "K5" = "#DDCC77")

#" # # muted rose/earthy red for contrast
# plot with individual names on x-axis
ggplot(meta_long, aes(x = ind, y = Proportion, fill = Cluster)) +
  geom_bar(stat = "identity", width = 1) +
  scale_fill_manual(values = cluster_cols) +
  theme_classic(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size=5)
  ) +
  labs(x = "Individual", y = "Membership proportion", fill = "Cluster")

###### Making supervised DAPC ######
vcf_filtered = read.vcfR("./output/vcf/allsb_sunbear_mac3_964x66_60indmiss_40snpmiss.vcf", verbose = F)
dat <- vcfR2genind(vcf_filtered)

pop.list <- sub("^[^_]+_([A-Z]+)[0-9]+_.*$", "\\1", rownames(dat@tab))
dat@pop = as.factor(pop.list)

dapc2 = dapc(dat, dat$pop, n.pca = 25, n.da = 6) 
myCol <- c("DDW" = "#117733", "KIS" = "#CC6677", "PBT" = "#BECF5F")

scatter(dapc2, posi.da = "bottomleft", scree.da = F, scree.pca = T, pch = 19, col = myCol, bg = "white",
        clab = TRUE)


########Population genetic measures#######

dat_hier <- genind2hierfstat(dat)
stats <- basic.stats(dat_hier)

# Overall FST
overall_fst <- stats$overall["Fst"]

pw_fst <- pairwise.WCfst(dat_hier)
print(pw_fst)

# Mean Fis across all loci and populations
mean_Fis <- mean(stats$Fis, na.rm = TRUE)

# Mean observed and expected heterozygosity
mean_Ho <- mean(stats$Ho, na.rm = TRUE)
mean_He <- mean(stats$Hs, na.rm = TRUE)

# Output
cat("Mean FIS:", round(mean_Fis, 3), "\n")
cat("Mean Ho :", round(mean_Ho, 3), "\n")
cat("Mean He :", round(mean_He, 3), "\n")

fis_by_pop <- colMeans(stats$Fis, na.rm = TRUE)
print(fis_by_pop)

#inbreeding(dat, res.type = "estimate")

boxplot(stats$Fis, main = "Per-locus FIS by Population", ylab = "FIS")
abline(h = 0, lty = 2, col = "red")

ho_by_pop <- colMeans(stats$Ho, na.rm = TRUE)
print(round(ho_by_pop, 3))

he_by_pop <- colMeans(stats$Hs, na.rm = TRUE)
print(round(he_by_pop, 3))
