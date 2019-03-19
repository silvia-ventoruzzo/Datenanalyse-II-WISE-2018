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




###########################
# HIERARCHICAL CLUSTERING #
###########################

## HCLUST

distance = mahalanobis_distance(x = target_data_scaled)
hclust = hclust(distance, method = "average")
plot(hclust, hang = -1) # 4
ggdendrogram(hclust)

# Dendogramm data
dend <- as.dendrogram(hclust)
dend_data <- dendro_data(dend, type = "rectangle")
ggplot(dend_data$segments) + 
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend))+
  geom_text(data = dend_data$labels, aes(x, y, label = label),
            hjust = 1, angle = 90, size = 3)+
  ylim(-3, 15) +
  theme_bw()


# DIANA

cutree = cutree(hclust, k = 4)
plot(target_data_scaled, col = cutree)

diana = diana(target_data_scaled, diss = FALSE)
