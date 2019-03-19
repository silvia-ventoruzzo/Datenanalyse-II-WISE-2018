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
                     "qpcR")
for (package in needed_packages) {
  if (!require(package, character.only=TRUE)) {install.packages(package, character.only=TRUE)}
  library(package, character.only=TRUE)
}
rm("needed_packages", "package")

# Load scripts and functions
source("rfm.R")
Jmisc::sourceAll(file.path(getwd(), "Helpers", fsep="/"))

# Prepare dataframe for clustering
target_data = rfm_df %>%
  select(recency, frequency, monetary)

# Scale data
target_data_scaled = target_data %>%
  scale()

########################
###### DBSCAN ##########
########################

# FINDING PARAMETERS

# Test different values of k = MinPts

dbscan_parameters = dbscan_parameter_validation(
  data   = target_data_scaled,
  minpts = seq(40, 60),
  eps    = seq(0.11, 0.2, by = 0.01),
  scale  = FALSE) %>%
  arrange(desc(silhouette_index))

parameters = dbscan_parameters %>%
  filter(n_clusters > 2)



# Eps
# k-dist plot
knndist = dbscan::kNNdist(target_data_scaled, k = 60) %>%
  as.data.frame()

knndist_ordered = order_to_var(df   = knndist, 
                               vars  = as.character(seq(40, 60)),
                               desc  = TRUE)


ggplot(knndist_ordered) +
  geom_line(aes(x = order_40, y = `40`, color = "40")) +
  geom_line(aes(x = order_50, y = `50`, color = "50")) +
  geom_line(aes(x = order_60, y = `60`, color = "60")) +
  scale_y_continuous(limits = c(0, 1),
                     breaks = seq(0, 1, 0.2)) +
  theme_bw() +
  labs(x = "Points",
       y = "k-distance",
       color = "Values of k")

minpts = 60
eps = 0.12

fpc = fpc::dbscan(target_data_scaled, eps = eps, MinPts = minpts, scale = FALSE)
dbscan = dbscan::dbscan(target_data_scaled, eps = eps, minPts = minpts)
all(fpc$cluster == dbscan$cluster)
fviz_cluster(fpc, target_data, geom = "point") +
  theme_bw()

target_data %>%
  ggpairs(mapping = ggplot2::aes(color = as.factor(fpc$cluster))) +
  theme_bw() 

clustered = rfm_df %>%
  mutate(cluster_dbscan = as.factor(fpc$cluster))

clustered %>% summarize(count = sum(cluster_dbscan == 0))

########################
###### K-MEANS #########
########################

# Find number of clusters
number_of_clusters(scaled_df   = target_data_scaled,
                   max         = 15,
                   plot_breaks = seq(0, 15, by = 1))

# Clustering
n_clusters = 5
set.seed(1953647536)
kmeans = kmeans(target_data_scaled, centers = n_clusters)

clustered = clustered %>%
  mutate(cluster_kmeans = as.factor(kmeans$cluster))