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
source("k-means.R")
Jmisc::sourceAll(file.path(getwd(), "Helpers", fsep="/"))

# PREPARE DATA FOR CLUSTERING

# Remove outliers and select variables
target_data = rfm_df %>%
  dplyr::filter(outlier_elbow == FALSE) %>%
  dplyr::select(recency, frequency, monetary)

# Scale data
target_data_scaled = target_data %>%
  scale()

## HCLUST
distance = mahalanobis_distance(x = target_data_scaled)
hclust = hclust(distance, method = "average")

# Dendogramm data
dend <- as.dendrogram(hclust)
dend_data <- dendro_data(dend, type = "rectangle")

ggplot(dend_data$segments) + 
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_text(data = dend_data$labels, aes(x, y, label = label),
            nudge_y = -1, check_overlap = TRUE,
            hjust = 1, angle = 90, size = 2, color = "red") +
  scale_y_continuous(limits = c(-3, 400)) +
  annotate("text", x = 100, y = 200, label = "1", color = "darkgreen", size = 5) +
  annotate("text", x = 3900, y = 100, label = "2", color = "darkgreen", size = 5) +
  annotate("text", x = 4400, y = 100, label = "3", color = "darkgreen", size = 5) +
  annotate("text", x = 3410, y = 38, label = "4", color = "blue", size = 5) +
  annotate("text", x = 4400, y = 38, label = "5", color = "blue", size = 5) +
  theme_bw() +
  labs(x = "Distance",
       y = "Height")

# dev.copy2pdf(file = "../Paper/dendogramm.pdf")
# dev.off()

## TESTING DIFFERENT VALUES OF K

# Variation A: 3 clusters
hier_clust_A = stats::cutree(hclust, k = 3)

silhouette_A = cluster::silhouette(x    = hier_clust_A,
                                   dist = distance)

fviz_silhouette(silhouette_A)

# dev.copy2pdf(file = "../Paper/hierarcAsilhouette.pdf")
# dev.off()

# Variation B: 5 clusters
hier_clust_B = stats::cutree(hclust, k = 5)

silhouette_B = cluster::silhouette(x    = hier_clust_B,
                                   dist = distance)

fviz_silhouette(silhouette_B)

# dev.copy2pdf(file = "../Paper/hierarcBsilhouette.pdf")
# dev.off()

# Comparison
stats_A = data.frame(
  n_clusters                = max(hier_clust_A),
  silhouette_index          = summary(silhouette_A)$avg.width,
  silhouette_all_positive   = ifelse(all(summary(silhouette_A)$clus.avg.width > 0),
                                     TRUE, FALSE))
stats_B = data.frame(
  n_clusters                = max(hier_clust_B),
  silhouette_index          = summary(silhouette_B)$avg.width,
  silhouette_all_positive   = ifelse(all(summary(silhouette_B)$clus.avg.width > 0),
                                     TRUE, FALSE))
stats_hclust = rbind(stats_A, stats_B) %>%
  mutate(variation = c("A", "B"))

hclust_clusters = rfm_df %>%
  dplyr::filter(outlier_elbow == FALSE) %>%
  dplyr::select(customer_id) %>%
  dplyr::mutate(cluster_A = hier_clust_A,
                cluster_B = hier_clust_B) %>%
  dplyr::mutate(both_1 = ifelse(cluster_A == 1 & cluster_B == 1, TRUE,
                                ifelse(cluster_A == 1 | cluster_B == 1, FALSE,
                                       NA)))

hclust_clusters %>%
  summarize(count_1_A = sum(cluster_A == 1),
            count_1_B = sum(cluster_B == 1),
            count_1_both = sum(both_1, na.rm = TRUE))

# Keep only variation A
rfm_df = rfm_df %>%
  dplyr::mutate(cluster_hclust = ifelse(outlier_elbow, NA, hier_clust_A))

# Silhouette Index
hclust_silhouette = silhouette_A

## REMOVE UNNECESSARY OBJECTS
rm("dend", "dend_data", "hclust", "hclust_clusters", "stats_A", "stats_B",
   "stats_hclust", "distance", "hier_clust_A", "hier_clust_B",
   "silhouette_A", "silhouette_B")
