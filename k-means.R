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