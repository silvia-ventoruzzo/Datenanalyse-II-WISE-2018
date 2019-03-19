dbscan_parameter_validation = function(data, minpts, eps, scale = TRUE) {
  
  # Scale data if necessary
  if (scale) {
    
    data_scaled = scale(data)
    
  } else {
    
    data_scaled = data
  }
  
  distance = dist(data_scaled)
  
  # Getting same distance vectors
  n <- max(length(minpts), length(eps))
  length(minpts) <- n                      
  length(eps) <- n
  
  # Bind vectors by column
  parameters = cbind(minpts, eps) %>%
    as.data.frame() %>%
    expand.grid() %>%
    drop_na()
  
  cluster_stats = c()
  
  for (i in 1:nrow(parameters)) {
    
    print(paste("DBSCAN with MinPts =", parameters$minpts[i], "and Eps =", parameters$eps[i], sep = " "))
    
    # Cluster with DBSCAN
    dbscan = fpc::dbscan(data_scaled, MinPts = parameters$minpts[i], eps = parameters$eps[i], scale = FALSE)
    
    # Extract points' clusters
    clusters = dbscan$cluster
    
    # Calculate silhouette values
    silhouette = cluster::silhouette(x = clusters, dist = distance)
    
    # Produce statistics' dataframe
    stats = data.frame(
      minpts                    = parameters$minpts[i],
      eps                       = parameters$eps[i],
      n_clusters                = max(clusters),
      noise_points              = sum(clusters == 0),
      silhouette_index          = summary(silhouette)$avg.width,
      silhouette_all_positive   = ifelse(all(summary(silhouette)$clus.avg.width > 0),
                                         TRUE, FALSE)
      )
    
    cluster_stats = cluster_stats %>% rbind(stats)
    
  }
  
  return(cluster_stats)
  
}