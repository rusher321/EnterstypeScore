#-------------------------------------------------------------------------------#
# Copyright (c) 2018 Huahui Ren (BGI-shenzhen). Allrights reserved.             #
# Created by Huahui Ren (BGI-shenzhen) on 21/6/2018                             #
# This R program is using to visualize the new samples on database or on its    #
# own space									#
# Args:                                                                         #
#   prof: column is sample, row is genus                                        #
#   Cohort:"china.igc","china.matephlan2","hmp.igc","hmp.matephlan2","methit.igc"
#   distance.method: bray/euclidean/jsd/average                                 #
#   reduction.method: pca/pcoa/tsne 		                                #
#   prefix: the result prefix                                                   #
#   option: new data's config                                                   #   
# output:                                                                       #
#   dis                                                                         #
#   figure	                                                                # 
#-------------------------------------------------------------------------------#

# options  
rm(list = ls())
options(digits = 10)

workdir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(workdir)
if(file.exists("result")){next}else{dir.create("result")}

# load packages

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
    sapply(pkg, require, character.only = TRUE)
}

# ----Load Required packages----
packages <- c("ggplot2","ape","RcolorBrewer","Rtsne","vegan")
ipak(packages)

source("../scripts/distance.R")
source("../scripts/distance_space.R")


# ----main script----

args <- commandArgs(T)


if(length(args) <=  5){
        stop("Rscript visual_sample.R [pro]  [cohort] [distance] [reduction] [prefix] [option]")
}

# load data
data <- t(read.table(args[1], header = T, row.names = 1, check.names = F, sep = "\t"))
base <- t(read.table(paste0(args[2], ".tab"), header = T, row.names = 1, check.names = F, sep = "\t"))

pro <- rbind(data, base)
config <- c(rep(args[5], nrow(data)), rep("base", nrow(base)))


# compute the distance matrix 

Methods <-  c("jsd", "average", "manhattan", "euclidean", "canberra", "bray", 
        "kulczynski", "gower", "morisita", "horn", "mountford", 
        "jaccard", "raup", "binomial", "chao", "altGower", "cao", 
        "mahalanobis")

method <- args[3]

index <- pmatch(method, Methods)

if(index == 1)
	pro <- as.matrix(pro)
	dis <- dist.JSD(pro)
if(index == 2)
	dis <- average_dis(pro)
if(index != 2 & index != 1)
	dis <- vegdis(pro, method = args[3])


write.table(dis, paste0("./result/",prefix, "_", method), quote =F, col.names=NA, sep="\t")
write.table(pro, paste0("./result/",prefix,".relative.tab"), quote =F, col.names=NA, sep="\t")

config <- data.frame(config)
rownames(config) <- rownames(pro)
write.table(config, paste0("./result/",prefix,".relative.tab"), quote =F, col.names=NA, sep="\t")

# figure

Reduction <- c("pcoa", "rpca", "tsne", "nmds")

index2 <- pmatch(args[4], Reduction)

if(index2 == 1)
	space <- mypcoa(dis, method = method, config)
if(index2 == 2)
	space <- myrpca(pro, method = method, config) 
if(index2 == 3)
	space <- mytsne(dis, method = method, config)
if(index2 == 4)
	space <- mynmds(pro, method = method, config)


pdf(paste0("./result/",prefix,"_", args[4], ".pdf"))
space
dev.off()




