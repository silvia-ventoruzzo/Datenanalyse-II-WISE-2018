# x - data frame
# cx - covariance matrix; if not provided, 
#      it will be estimated from the data
mahalanobis_distance <- function(x, cx = NULL) {
  if(is.null(cx)) cx <- cov(x)
  out <- lapply(1:nrow(x), function(i) {
    mahalanobis(x = x, 
                center = sapply(FUN = "c", X = x[i, ]),
                cov = cx)
  })
  return(as.dist(do.call("rbind", out)))
}