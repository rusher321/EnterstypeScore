# this script use to get the gbm model based on the original data
# x is genus profiel , y is enterstype score

args <- commandArgs(T)
if(length(args) != 5){
	stop("Rscript model.R data_base distance cluster_num[2/3] prefix")
}


library(caret)
library(vegan)

source("distance.R")
source("gbm.R")

# data
pro <- t(read.table(args[1], header=T, row.names=1, sep="\t"))

# distance
Methods <-  c("jsd", "average", "manhattan", "euclidean", "canberra", "bray",
        "kulczynski", "gower", "morisita", "horn", "mountford",
        "jaccard", "raup", "binomial", "chao", "altGower", "cao",
        "mahalanobis")

method <- args[2]

index <- pmatch(method, Methods)

if(index == 1)
        pro <- as.matrix(pro)
        dis <- dist.JSD(pro)
if(index == 2)
        dis <- average_dis(pro)
if(index != 2 & index != 1)
        dis <- vegdis(pro, method = args[2])


score <- fanny(dis, args[3])

if(args[3]==2)
	cluster1 <- rbind(pro, value=score[,1])
	cluster2 <- rbind(pro, value=score[,2])
	c1_model <- mygbm(data = cluster1)
	c2_model <- mygbm(data = cluster2)
	

if(args[3]==3)
	cluster1 <- rbind(pro, value=score[,1])
	cluster2 <- rbind(pro, value=score[,2])
	cluster3 <- rbind(pro, value=score[,3])
	c1_model <- mygbm(data = cluster1)
	c2_model <- mygbm(data = cluster2)
	c2_model <- mygbm(data = cluster3)
if(args[3]>3)
	stop("cluster number is only 2 or 3 here.")


# figure 

c1_model <- mygbm(data = cluster)





