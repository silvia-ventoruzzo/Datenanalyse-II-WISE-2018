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
source("hierarchical_clustering.R")
Jmisc::sourceAll(file.path(getwd(), "Helpers", fsep="/"))

# Prepare dataframe for clustering
target_data = rfm_df %>%
  dplyr::select(recency, frequency, monetary)

# Scale data
target_data_scaled = target_data %>%
  scale()

# FINDING PARAMETERS

# Test different values of k = MinPts and Eps
dbscan_parameters = dbscan_parameter_validation(
  data   = target_data_scaled,
  minpts = c(4, 6),
  eps    = seq(0.05, 0.2, by = 0.01),
  scale  = FALSE) %>%
  arrange(desc(silhouette_index))

dbscan_parameters %>%
  filter(silhouette_index > 0) %>%
  dplyr::select(-silhouette_all_positive) %>%
  mutate(noise_points = noise_points*100) %>%
  xtable::xtable(digits = c(0, 0, 2, 0, 2, 3)) %>%
  print(include.rownames = FALSE)

# k-dist Graph
knndist = dbscan::kNNdist(target_data_scaled, k = 6) %>%
  as.data.frame()

knndist_ordered = order_to_var(df   = knndist, 
                               vars  = as.character(seq(4, 6)),
                               desc  = TRUE)
ggplot(knndist_ordered) +
  geom_line(aes(x = order_4, y = `4`, color = "4"), size = 1.2) +
  geom_line(aes(x = order_6, y = `6`, color = "6"), size = 1.2) +
  scale_y_continuous(limits = c(0, 0.6),
                     breaks = seq(0, 0.6, 0.1)) +
  theme_bw() +
  labs(x = "Points",
       y = "k-distance",
       color = "Values of k") +
  theme(axis.title.x = element_text(size = rel(1.2)),
        axis.text.x  = element_text(size = rel(1.2)),
        axis.title.y = element_text(size = rel(1.2)),
        axis.text.y  = element_text(size = rel(1.2)),
        legend.position = "bottom")

# dev.copy2pdf(file = "../Paper/kdistgraph.pdf")
# dev.off()

# Parameters selected

parameters = dbscan_parameters %>%
  dplyr::slice(c(1, 7)) %>%
  dplyr::select(minpts, eps) %>%
  dplyr::mutate(variation = c("A", "B"))

# VARIATION A

# Clustering
fpc_A = fpc::dbscan(target_data_scaled, 
                  eps = parameters %>% dplyr::filter(variation == "A") %>% dplyr::select(eps) %>% t() %>% c(), 
                  MinPts = parameters %>% dplyr::filter(variation == "A") %>% dplyr::select(minpts)  %>% t() %>% c(), 
                  scale = FALSE)
dbscan_A = dbscan::dbscan(target_data_scaled, 
                        eps = parameters %>% dplyr::filter(variation == "A") %>% dplyr::select(eps) %>% t() %>% c(), 
                        minPts = parameters %>% dplyr::filter(variation == "A") %>% dplyr::select(minpts) %>% t() %>% c())
all(fpc_A$cluster == dbscan_A$cluster)

# Silhouette Plot
silhouette_plot(data = target_data_scaled,
                parameters = parameters %>%
                  dplyr::filter(variation == "A") %>%
                  dplyr::select(-variation) %>%
                  t() %>% c(),
                cluster_method = "DBSCAN",
                scale = FALSE)

# dev.copy2pdf(file = "../Paper/dbscanAsilhouette.pdf")
# dev.off()

fviz_cluster(fpc_A, target_data, geom = "point") +
  theme_bw()

# dev.copy2pdf(file = "../Paper/dbscanAclustrplot.pdf")
# dev.off()

# VARIATION B

# Clustering
fpc_B = fpc::dbscan(target_data_scaled, 
                  eps = parameters %>% dplyr::filter(variation == "B") %>% dplyr::select(eps) %>% t() %>% c(), 
                  MinPts = parameters %>% dplyr::filter(variation == "B") %>% dplyr::select(minpts)  %>% t() %>% c(), 
                  scale = FALSE)
dbscan_B = dbscan::dbscan(target_data_scaled, 
                        eps = parameters %>% dplyr::filter(variation == "B") %>% dplyr::select(eps) %>% t() %>% c(), 
                        minPts = parameters %>% dplyr::filter(variation == "B") %>% dplyr::select(minpts) %>% t() %>% c())
all(fpc_B$cluster == dbscan_B$cluster)

# Silhouette Plot
silhouette_plot(data = target_data_scaled,
                parameters = parameters %>%
                  dplyr::filter(variation == "B") %>%
                  dplyr::select(-variation) %>%
                  t() %>% c(),
                cluster_method = "DBSCAN",
                scale = FALSE)

# dev.copy2pdf(file = "../Paper/dbscanBsilhouette.pdf")
# dev.off()

fviz_cluster(fpc_B, target_data, geom = "point") +
  theme_bw()

# dev.copy2pdf(file = "../Paper/dbscanBclustrplot.pdf")
# dev.off()

# Comparison of variations
dbscan_clusters = rfm_df %>%
  dplyr::select(customer_id) %>%
  dplyr::mutate(cluster_A = fpc_A$cluster,
                cluster_B = fpc_B$cluster) %>%
  dplyr::mutate(both_1 = ifelse(cluster_A == 1 & cluster_B == 1, TRUE,
                         ifelse(cluster_A == 1 | cluster_B == 1, FALSE,
                                NA)),
                both_0 = ifelse(cluster_A == 0 & cluster_B == 0, TRUE,
                                ifelse(cluster_A == 0 | cluster_B == 0, FALSE,
                                       NA)))

dbscan_clusters %>%
  summarize(count_1_A = sum(cluster_A == 1),
            count_1_B = sum(cluster_B == 1),
            count_1_both = sum(both_1, na.rm = TRUE),
            count_0_A = sum(cluster_A == 0),
            count_0_B = sum(cluster_B == 0),
            count_0_both = sum(both_0, na.rm = TRUE),
            count_0_1 = sum(cluster_A == 0 & cluster_B == 1),
            count_0_2 = sum(cluster_A == 0 & cluster_B == 2),
            count_0_3 = sum(cluster_A == 0 & cluster_B == 3))

# Keep only variation A
rfm_df = rfm_df %>%
  dplyr::mutate(cluster_dbscan = fpc_A$cluster)

# Plot clusters
fviz_cluster(object = fpc_A, data = target_data_scaled,
             geom = "point", main = "", shape = 19) +
  theme(axis.title.x = element_text(size = rel(1.2)),
        axis.text.x  = element_text(size = rel(1.2)),
        axis.title.y = element_text(size = rel(1.2)),
        axis.text.y  = element_text(size = rel(1.2)),
        legend.position = "bottom")

# dev.copy2pdf(file = "../Paper/dbscanclusterscaled.pdf")
# dev.off()

# Silhouette Index
dbscan_silhouette = cluster::silhouette(x    = fpc_A$cluster,
                                        dist = dist(target_data_scaled))

## REMOVE UNNECESSARY OBJECTS
rm("dbscan_A", "dbscan_B", "dbscan_clusters", "dbscan_parameters", "fpc_A", "fpc_B",
   "knndist", "knndist_ordered", "parameters")
