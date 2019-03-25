kmeans_parameter_validation = function(data, min = 2, max, scale = TRUE,
                                       iter_max = 10, plot_breaks) {
  
  # Scale data if necessary
  if (scale) {
    
    data_scaled = scale(data)
    
  } else {
    
    data_scaled = data
  }
  
  distance = dist(data_scaled)
  
  # Set seed for reproducibility
  set.seed(900114)
  
  # Values to test
  k_values = min:max
  # Empty dataframe for the total variance explained
  sil = data.frame(clusters = k_values,
                   sil      = rep(NA, length(k_values)))
  # Empty list for the kmeans objects
  clk = list()
  # Loop through the possible values of k
  for (k in k_values) {
    # Calculate k-means for each k
    clk[[k-1]] = kmeans(data_scaled, centers = k, iter.max = iter_max)
    # Save the number of clusters
    names(clk)[k-1] = paste(k, "clusters", sep = " ")
    # Save cluster values
    clusters = clk[[k-1]]$cluster
    # Calculate silhouette values
    silhouette = cluster::silhouette(x = clusters, dist = distance)
    sil_index = summary(silhouette)$avg.width
    # Update dataframe
    sil = sil %>%
      dplyr::mutate(sil = ifelse(clusters == k, sil_index, sil))
    # Print process
    print(paste("k-means with", k, "clusters done", sep = " "))
  }
  
  # Plot silhouette index against k values
  plot = ggplot(data = sil, aes(x = clusters, y = sil)) +
    geom_line(color = "grey") +
    geom_point(color = "blue") +
    scale_x_continuous(breaks = plot_breaks) +
    labs(x     = "Number of clusters",
         y     = "Silhouette Index") +
    theme_bw() +
    theme(axis.text.x = element_text(size = rel(1.2)),
          axis.text.y = element_text(size = rel(1.2)),
          axis.title.x = element_text(size = rel(1.2)),
          axis.title.y = element_text(size = rel(1.2)))
  
  return(plot)
  
}