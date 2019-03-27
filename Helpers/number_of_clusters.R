# Calculate plot of tve to choose number of clusters for k-means clustering
number_of_clusters = function(data, min = 2, max,
                              iter_max = 10, plot_breaks,
                              scale = TRUE) {
  
  # Scale data if necessary
  if (scale) {
    scaled_data = scale(data)
  } else {
    scaled_data = data
  }
  # Set seed for reproducibility
  set.seed(900114)
  # Values to test
  k_values = min:max
  # Derive data sample
  sample = scaled_data[sample(nrow(scaled_data), round(0.25*nrow(scaled_data))), ]
  # Calculate distance
  distance = dist(sample)
  # Perform hierarchical clustering
  hclust = stats::hclust(distance, method = "average")
  # Empty dataframe for the total variance explained
  tve = data.frame(clusters = k_values,
                   tve      = rep(NA, length(k_values)))
  # Empty list for the kmeans objects
  clk = list()
  # Loop through the possible values of k
  for (k in k_values) {
    # Extract clusters from hclust
    clusters = sample %>%
      as.data.frame() %>%
      dplyr::mutate(cluster = stats::cutree(hclust, k = k))
    # Calculate hclust cluster centroids
    centroids = clusters %>%
      dplyr::group_by(cluster) %>%
      dplyr::summarize_all(mean) %>%
      dplyr::select(-cluster)
    # Calculate k-means for each k using hclust centroids
    clk[[k-1]] = kmeans(scaled_data, centers = centroids, iter.max = iter_max)
    # Save the number of clusters
    names(clk)[k-1] = paste(k, "clusters", sep = " ")
    # Calculate percentage of total variance explained
    tve$tve[k-1] = 1-clk[[k-1]]$tot.withinss/clk[[k-1]]$totss
    # Print process
    print(paste("k-means with", k, "clusters done", sep = " "))
  }
  
  # Plot tve against k values
  plot = ggplot(data = tve, aes(x = clusters, y = tve)) +
    geom_line(color = "grey") +
    geom_point(color = "red") +
    scale_x_continuous(breaks = plot_breaks) +
    labs(x     = "Number of clusters",
         y     = "% of total variance explained") +
    theme_bw() +
    theme(axis.text.x = element_text(size = rel(1.2)),
          axis.text.y = element_text(size = rel(1.2)),
          axis.title.x = element_text(size = rel(1.2)),
          axis.title.y = element_text(size = rel(1.2)))
  
  return(plot)
}