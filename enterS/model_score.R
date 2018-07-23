# cluster[2/3] data_base

args <- commandArgs(T)
if(length(args) != 5){
        stop("Rscript model_score.R new_data data_base distance cluster_num[2/3] prefix")
}


model<- paste0(args[2],"_",args[3],"_", args[4])

if(!file.exists(model)){
	stop("can't find the model;
	     need generare the model before executing the script")
}

source(model)

# here need to confirm the newdata & data_base have same format

newdata <- read.table(args[1], header=T, row.names=1, sep="\t")

# predict
if(args[4] == 2)
	mode11 <- out[[1]]
	cluster1_score <- predict(model1, newdata)
	model2 <- out[[2]]
	cluster2_score <- predict(model2, newdata)
	score <- data.frame(cluster1_score, cluster2_score)
if(args[4] == 3)
	model1 <- out[[1]]
	cluster1_score <- predict(model1, newdata)
	model2 <- out[[2]]
	cluster2_score <- predict(model2, newdata)
	model3 <- out[[3]]
	cluster3_score <- predict(model3, newdata)
	score <- data.frame(cluster1_score, cluster2_score, cluster3_score)
if(args[4] > 3)
	stop("here cluster number only is set to 2 or 3")

rownames(score) <- rownames(newdata)

write.table(score, paste0(args[1], ".score.tab"), sep="\t", quote=F)

