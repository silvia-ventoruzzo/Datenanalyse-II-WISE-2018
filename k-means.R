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
plot_elbow = number_of_clusters(scaled_df   = target_data_scaled,
                                max         = 15,
                                plot_breaks = seq(0, 16, by = 2))

plot_elbow +
  geom_point(aes(x = 4, y = 0.73), size = 5, shape = 1, color="gold4")

# dev.copy2pdf(file = "../Paper/kmeans_tve.pdf")
# dev.off()

# Silhouette
plot_sil = kmeans_parameter_validation(data = target_data_scaled,
                                       min = 2, max = 15, scale = FALSE,
                                       iter_max = 10, plot_breaks = seq(2, 15, 2))

plot_sil +
  geom_point(aes(x = 4, y = 0.544), size = 5, shape = 1, color="gold4")

# dev.copy2pdf(file = "../Paper/kmeanssilhouette.pdf")
# dev.off()

# Clustering
n_clusters = 4
set.seed(900114)
kmeans = kmeans(target_data_scaled, centers = n_clusters)

rfm_df = rfm_df %>%
  dplyr::mutate(cluster_kmeans = ifelse(outlier_elbow, NA, kmeans$cluster))

# Analyze cluster structure
rfm_df %>%
  dplyr::filter(!is.na(cluster_kmeans)) %>%
  dplyr::group_by(cluster_kmeans) %>%
  summarize(count = n(),
            perc  = count/nrow(rfm_df)*100) %>%
  xtable::xtable() %>%
  print(include.rownames = FALSE)

rfm_df %>%
  dplyr::filter(cluster_kmeans == 1) %>%
  dplyr::select(recency, frequency, monetary, first_purchase) %>%
  descriptive_statistics() %>%
  xtable::xtable() %>%
  print(include.rownames = FALSE)
rfm_df %>%
  dplyr::filter(cluster_kmeans == 2) %>%
  dplyr::select(recency, frequency, monetary, first_purchase) %>%
  descriptive_statistics() %>%
  xtable::xtable() %>%
  print(include.rownames = FALSE)
rfm_df %>%
  dplyr::filter(cluster_kmeans == 3) %>%
  dplyr::select(recency, frequency, monetary, first_purchase) %>%
  descriptive_statistics() %>%
  xtable::xtable() %>%
  print(include.rownames = FALSE)
rfm_df %>%
  dplyr::filter(cluster_kmeans == 4) %>%
  dplyr::select(recency, frequency, monetary, first_purchase) %>%
  descriptive_statistics() %>%
  xtable::xtable() %>%
  print(include.rownames = FALSE)


## REMOVE UNNECESSARY OBJECTS
rm("kmeans", "plot_elbow", "plot_sil", "n_clusters")