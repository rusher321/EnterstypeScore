# this script use the new data to generate the cluster
# here use fanny, pam, hierachical, kmeans, gmm
# library(cluster)



mycluster <- function(dist, method, k){
	
	```
        dist: distance based profile
	method: cluster method
	k: cluster number
	
	here method is based on the distance
	```
	if(attribute(dist)$class != "dist"){
		stop("the input must be dist")
	}
	
	if(is.na(pmatch(method, c("fanny", "pam", "hierachical", "kmeans")))){
		stop("mycluster don't include the cluster method ")

	}

	index <- pmatch(method, c("fanny", "pam", "hierachical", "kmeans"))

	if(index == 1)
		cluster <- fanny(dist, k)$cluster
	if(index == 2)
		cluster <- pam(dist, k)$cluster
	if(index == 3)
		cluster <- hclust(dist, method = "median")
	if(index == 4)
		cluster <- kmeans(dist, k)$cluster
	
	out <- data.frame(cluster)
	rownames(out) <- rownames(dist)
	return(out)

}



mygmm <- function(pro, k ){

	```
	here use the Mclust packages based on the EM to clust
	the parametre is default
	we estimate the result used this gmm model, find that the clust is gradient

	pro: profile , row is observe, col is variable
	k : cluster number
	```
	model <- Mclust(pro, k)
	cluster <- summary(model)$classification
	out <- data.frame(cluster)
	return(out)

}

