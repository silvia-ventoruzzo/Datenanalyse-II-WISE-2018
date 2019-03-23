silhouette_plot = function(data, distance = "euclidean", parameters, cluster_method, scale = TRUE) {
  
  # Scale data if necessary
  if (scale) {
    
    data_scaled = scale(data)
    
  } else {
    
    data_scaled = data
  }
  
  # Calculate distance
  if (distance == "Mahalanobis") {
    distance = mahalanobis_distance(data_scaled)
  } else {
    distance = dist(data_scaled, method = distance)
  }
  
  # Clustering
  if (cluster_method == "DBSCAN") {
    
    if (length(parameters) != 2) {stop("DBSCAN needs 2 parameters: MinPts and Eps (in this order).")}
    
    # DBSCAN
    cluster_obj = fpc::dbscan(data_scaled, MinPts = parameters[1], eps = parameters[2], scale = FALSE)
    
  } else if (cluster_method == "Hierarchical Clustering") {
    
    if (length(parameters) != 2) {stop("Hierarchical clustering needs 2 additional parameters: Linkage and number of clusters.")}
    
    hclust = stats::hclust(distance, method = parameters[1])

    cluster_obj = data.frame(
      clusters = stats::cutree(hclust, k = parameters[2])
    )
    
  } else if (cluster_method == "k-Means") {
    
    if (length(parameters) != 1) {stop("Hierarchical clustering needs 1 additional parameter: Number of clusters.")}
    
    cluster_obj = stats:kmeans(data_scaled, centers = parameters[1], iter.max = 20)
    
  } else {
    
    stop("At the moment only DBSCAN, Hierarchical Clustering and k-Means are supported.
         Choose one of these and check uppercase and lower case letters.")
  }

  # Extract points' clusters
  clusters = cluster_obj$cluster
  
  # Calculate silhouette values
  silhouette = cluster::silhouette(x = clusters, dist = distance)
  
  # Produce plot
  plot = fviz_silhouette(silhouette)
  
  return(plot)
}
