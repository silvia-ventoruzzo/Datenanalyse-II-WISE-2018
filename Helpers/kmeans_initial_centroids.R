kmeans_initial_centroids = function(data, k,
                                    sample_size = round(0.25*nrow(scaled_data)),
                                    scale = TRUE) {
  # Scale data if necessary
  if (scale) {
    scaled_data = scale(data)
  } else {
    scaled_data = data
  }
  # Derive data sample
  sample = scaled_data[sample(nrow(scaled_data), round(0.25*nrow(scaled_data))), ]
  # Calculate distance
  distance = dist(sample)
  # Perform hierarchical clustering
  hclust = stats::hclust(distance, method = "average")
  # Extract clusters from hclust
  clusters = sample %>%
    as.data.frame() %>%
    dplyr::mutate(cluster = stats::cutree(hclust, k = k))
  # Calculate hclust cluster centroids
  centroids = clusters %>%
    dplyr::group_by(cluster) %>%
    dplyr::summarize_all(mean) %>%
    dplyr::select(-cluster)
  
  return(centroids)
}