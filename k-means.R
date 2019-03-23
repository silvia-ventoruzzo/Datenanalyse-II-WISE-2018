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
                     "cluster")
for (package in needed_packages) {
  if (!require(package, character.only=TRUE)) {install.packages(package, character.only=TRUE)}
  library(package, character.only=TRUE)
}
rm("needed_packages", "package")

# Load scripts and functions
source("exploratory_data_analysis.R")
Jmisc::sourceAll(file.path(getwd(), "Helpers", fsep="/"))


# Find number of clusters
n_clusters_kmeans = NbClust(target_data_scaled, method = "kmeans", index = "all")

# dev.copy2pdf(file = "../Paper/kmeans_nclustersindexes.pdf")
# dev.off()



number_of_clusters(scaled_df   = target_data_scaled,
                   max         = 15,
                   plot_breaks = seq(0, 15, by = 1))



# Clustering
n_clusters = 5
set.seed(1953647536)
kmeans = kmeans(target_data_scaled, centers = n_clusters)

clustered = clustered %>%
  mutate(cluster_kmeans = as.factor(kmeans$cluster))