# pam, based on the relative abundance profile



# dmn, based on the genus count 

library(DirichletMultinomial)
library(vegan)

my_dmn <- function(count, method = "min", max.iter = 10){
	
	```
	this script need to use the count profile

	```
	pr <- count[ ,colSums(count)!=0] 
	set.seed(0)
	fit <- mclapply(1:max.iter, dmn, count = pr, verbose = TRUE)
	lplc <- sapply(fit, laplace)
	id.min <- which.min(lplc)
	id.lower <- which(lplc <= c(lplc[-1], lplc[gn]))[1]
	id <- ifelse(method == "min", id.min, id.lower)
	best <- fit[[id]]
	comonents <- mixture(best)
	group = apply(comonents, 1, which.max)
	out <- data.frame(group)
	colnames(out) <- "cluster"
	pdf("best_cluster.pdf")
	plot(lplc, type = "b", 
     	xlab = "Number of Dirichlet Components", ylab = "Model Fit")
	dev.off()
	return(out)

}


my_pam <- function(data, method = "bray", max.iter=10){
	
	```
	this script use the relative abundance 

	```
	source("distance.R")	
	Methods <-  c("jsd", "average", "manhattan", "euclidean", "canberra", "bray",
        "kulczynski", "gower", "morisita", "horn", "mountford",
        "jaccard", "raup", "binomial", "chao", "altGower", "cao",
        "mahalanobis")


	index <- pmatch(method, Methods)

	if(index == 1)
        	pro <- as.matrix(pro)
        	dis <- dist.JSD(pro)
	if(index == 2)
        	dis <- average_dis(pro)
	if(index != 2 & index != 1)
        	dis <- vegdis(pro, method = args[3])

	res <- matrix(NA, 19, ncol(data))
	for (k in 2:max.iter) {
                 
		data.cluster_temp <-pam(dist, k)$clustering   
                res[k-1,] <- data.cluster_temp
                nclusters[k-1] <- index.G1(t(data) , data.cluster_temp,  d = dist,
                centrotypes = "medoids")
                sil[k-1] <- mean(silhouette(data.cluster_temp, dist = dist)[,3])
        }
	
	best <- which.max(nclusters)+1
	out  <- data.frame(res[best-1, ])
	colnames(out) <- "cluster"
	pdf("best_cluster.pdf")
	plot(nclusters, type = "b", 
     	xlab = "Number of pam cluster", ylab = "index.G1")
	dev.off()

	return(out)

}


