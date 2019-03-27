# Install and load needed packages
needed_packages <- c("tidyverse",
                     "xtable",
                     "Jmisc",
                     "reshape2",
                     "rfm",
                     "cowplot",
                     "plotly",
                     "fpc",
                     "dbscan",
                     "factoextra",
                     "StatMatch",
                     "ggdendro",
                     "cluster",
                     "ggforce")
for (package in needed_packages) {
  if (!require(package, character.only=TRUE)) {install.packages(package, character.only=TRUE)}
  library(package, character.only=TRUE)
}
rm("needed_packages", "package")

# Load scripts and functions
source("exploratory_data_analysis.R")
Jmisc::sourceAll(file.path(getwd(), "Helpers", fsep="/"))

# PREPARE DATA FOR CLUSTERING

# Remove outliers and select variables
target_data = rfm_df %>%
  dplyr::filter(outlier_elbow == FALSE) %>%
  dplyr::select(recency, frequency, monetary)

# Scale data
target_data_scaled = target_data %>%
  scale()

## NUMBER OF CLUSTERS

# Elbow method
plot_elbow = number_of_clusters(data        = target_data_scaled,
                                max         = 15,
                                plot_breaks = seq(0, 16, by = 2),
                                scale       = FALSE)

plot_elbow +
  geom_point(aes(x = 4, y = 0.73), size = 5, shape = 1, color="gold4")

# dev.copy2pdf(file = "../Paper/kmeans_tve.pdf")
# dev.off()

# Silhouette
plot_sil = kmeans_parameter_validation(data = target_data_scaled,
                                       min = 2, max = 15, scale = FALSE,
                                       iter_max = 10, plot_breaks = seq(2, 15, 2))

plot_sil +
  geom_point(aes(x = 4, y = 0.545), size = 5, shape = 1, color="gold4")

# dev.copy2pdf(file = "../Paper/kmeanssilhouette.pdf")
# dev.off()

## INITIAL CENTROIDS
n_clusters = 4
initial_centroids = kmeans_initial_centroids(data  = target_data_scaled,
                                             scale = FALSE,
                                             k     = n_clusters)

initial_centroids %>%
  t() %>%
  xtable::xtable()

## CLUSTERING
kmeans = kmeans(target_data_scaled, centers = initial_centroids)

rfm_df = rfm_df %>%
  dplyr::mutate(cluster_kmeans = ifelse(outlier_elbow, NA, kmeans$cluster))

# Plot clusters
fviz_cluster(object = kmeans, data = target_data_scaled,
             geom = "point", main = "", shape = 19) +
  theme_bw() +
  theme(axis.title.x = element_text(size = rel(1.2)),
        axis.text.x  = element_text(size = rel(1.2)),
        axis.title.y = element_text(size = rel(1.2)),
        axis.text.y  = element_text(size = rel(1.2)),
        legend.position = "bottom") 
  
# dev.copy2pdf(file = "../Paper/kmeansclusterscaled.pdf")
# dev.off()

# Silhouette index
kmeans_silhouette = cluster::silhouette(x    = kmeans$cluster,
                                        dist = dist(target_data_scaled))

# Silhouette plot
fviz_silhouette(kmeans_silhouette) +
  theme(legend.position = "bottom")

# dev.copy2pdf(file = "../Paper/kmeanssilplot.pdf")
# dev.off()

## REMOVE UNNECESSARY OBJECTS
rm("kmeans", "plot_elbow", "plot_sil", "n_clusters", "initial_centroids",
   "target_data", "target_data_scaled")
