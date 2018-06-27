# this script use to select the best cluster
# method include kmeans, pam , fanny

kBest <- function(data, dist , method = "kmeans"){
  
  nclusters=NULL
  sil = NULL
  out <- list()
  res <- matrix(NA, 19, ncol(data))
	for (k in 2:20) { 
		  switch (method,
		          kmeans = { data.cluster_temp <-kmeans(dist, k)$cluster} ,
			  pam = { data.cluster_temp <-pam(dist, k)$clustering},
		          fanny = {  data.cluster_temp <- fanny(dist, k)$clustering}
		                               )
		res[k-1,] <- data.cluster_temp
		nclusters[k-1] <- index.G1(t(data) , data.cluster_temp,  d = dist,
		centrotypes = "medoids")
		sil[k-1] <- mean(silhouette(data.cluster_temp, dist = dist)[,3])
	}
  
  best <- which.max(nclusters)+1
  kCluster <- c(2:20)
  CH_index <- nclusters
  Silhouette <- sil
  cluster <- data.frame(kCluster,  CH_index, Silhouette)
  cluster <- melt(cluster, id = "kCluster")
  colnames(cluster) <- c("kCluster", "Index", "value")
  figure <- ggplot(cluster, aes(kCluster, value, fill = Index)) +
        geom_bar(stat = "identity", position = "dodge")+coord_flip()+ facet_grid(.~Index, scales = "free")
  #figure <- ggplot(cluster,aes(x = kCluster, y = CH_index))+geom_point(size = 3)
  #figure2 <- ggplot(cluster,aes(x = kCluster, y = sil))+geom_point(size = 3)
  
  out <- list(res[best-1,], best, figure)
  return(out)
}
