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

# Compare composition of clusters
summary(as.factor(rfm_df$cluster_kmeans))
#   1    2    3    4    NA's 
#  453 1101 2706   51   20 

summary(as.factor(rfm_df$cluster_hclust))
#   1     2    3  NA's 
# 4292   16    3   20 

summary(as.factor(rfm_df$cluster_dbscan))
#  0    1    2 
# 183 4127   21 

rfm_df %>%
  group_by(cluster_kmeans, cluster_hclust, cluster_dbscan) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

# Compare Chi-Square outliers with DBSCAN outliers
rfm_df %>%
  summarize(outliers_chisq  = sum(outlier_chisq),
            outliers_dbscan = sum(cluster_dbscan == 0),
            dbscan_chisq    = count_noise/count_outliers)
