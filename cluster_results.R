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
source("dbscan.R")
Jmisc::sourceAll(file.path(getwd(), "Helpers", fsep="/"))

# Size of clusters
summary(as.factor(rfm_df$cluster_kmeans))
#   1    2    3    4   NA's 
# 1101 2708  451   51   20 

summary(as.factor(rfm_df$cluster_hclust))
#   1     2    3  NA's 
# 4292   16    3   20 

summary(as.factor(rfm_df$cluster_dbscan))
#  0    1    2 
# 183 4127   21 

# Silhouette Indexes
summary(kmeans_silhouette)$avg.width # 0.5444295
summary(hclust_silhouette)$avg.width # 0.9656451
summary(dbscan_silhouette)$avg.width # 0.5095059

# Compare Chi-Square outliers with DBSCAN outliers
rfm_df %>%
  summarize(outliers_chisq  = sum(outlier_chisq),
            outliers_dbscan = sum(cluster_dbscan == 0),
            dbscan_chisq    = outliers_dbscan/outliers_chisq)

# Compare sizes of clusters
rfm_df %>%
  group_by(cluster_kmeans, cluster_hclust, cluster_dbscan) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  mutate(percentage = count/nrow(rfm_df)) %>%
  arrange(desc(count)) %>%
  replace(is.na(.), 0)

# Biggest cluster of all
rfm_df %>%
  dplyr::filter(cluster_kmeans == 2 & cluster_hclust == 1 & cluster_dbscan == 1) %>%
  dplyr::group_by(segment) %>%
  dplyr::summarize(count = n()) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(sum  = sum(count),
                perc = count/sum) %>%
  dplyr::select(segment, perc) %>%
  dplyr::arrange(desc(perc))

rfm_df %>%
  dplyr::filter(cluster_kmeans == 2 & cluster_hclust == 1 & cluster_dbscan == 1) %>%
  dplyr::select(recency, frequency, monetary) %>%
  descriptive_statistics() %>%
  xtable::xtable() %>%
  xtable::xtable() %>%
  print(include.rownames = FALSE)

# Compare hclust and dbscan biggest cluster
rfm_df %>%
  group_by(cluster_hclust, cluster_dbscan) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  replace(is.na(.), 0)

rfm_df %>%
  dplyr::filter(cluster_hclust == 1 & cluster_dbscan == 1) %>%
  dplyr::select(recency, frequency, monetary) %>%
  descriptive_statistics() %>%
  xtable::xtable() %>%
  print(include.rownames = FALSE)

## ANALYZE K-MEANS CLUSTERS

# Frequencies
rfm_df %>%
  dplyr::filter(!is.na(cluster_kmeans)) %>%
  dplyr::group_by(cluster_kmeans) %>%
  summarize(count = n(),
            perc  = count/nrow(rfm_df)*100) %>%
  xtable::xtable() %>%
  print(include.rownames = FALSE)

# Outliers
rfm_df %>%
  dplyr::filter(!is.na(cluster_kmeans),
                outlier_chisq) %>%
  dplyr::group_by(cluster_kmeans, outlier_chisq) %>%
  dplyr::summarize(count = n())

# Descriptive statistics
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

# Contingency table with segments
contingency_table = rfm_df %>%
  dplyr::filter(!is.na(cluster_kmeans)) %>%
  dplyr::group_by(cluster_kmeans, segment) %>%
  dplyr::summarize(count = n()) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(cluster_kmeans) %>%
  dplyr::mutate(perc = count/sum(count)*100) %>%
  dplyr::select(-count) %>%
  tidyr::spread(key = "cluster_kmeans", value = "perc") %>%
  replace(is.na(.), 0) %>%
  dplyr::mutate_if(is.numeric, function(x) {round(x, 2) %>% as.character() %>% paste("%", sep = "")})

contingency_table %>%
  xtable::xtable() %>%
  print(include.rownames = FALSE)

## CLUSTERING TENDENCY
cluster_tendency = get_clust_tendency(data = rfm_df %>%
                                                dplyr::select(recency, frequency, monetary) %>%
                                                scale(),
                                      n    = 1800,
                                      seed = 900114)