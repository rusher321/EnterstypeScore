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
# output:                                                                       #
#   figure	                                                                # 
#-------------------------------------------------------------------------------#

# options  
rm(list = ls())
options(digits = 10)

workdir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(workdir)
if(file.exists("result")){dir.create("result")}

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


if(length(args) != 5){
        stop("Rscript visual_sample.R [pro]  [cohort] [distance] [reduction] [prefix]")
}

# load data
data <- t(read.table(args[1], header = T, row.names = 1, check.names = F, sep = "\t"))
base <- t(read.table(paste0(args[2], ".tab"), header = T, row.names = 1, check.names = F, sep = "\t"))





# compute the distance matrix 

Methods <-  c("jsd", "average", "manhattan", "euclidean", "canberra", "bray", 
        "kulczynski", "gower", "morisita", "horn", "mountford", 
        "jaccard", "raup", "binomial", "chao", "altGower", "cao", 
        "mahalanobis")

index <- pmathch(args[3], Methods)

if(index == 1)
	pro <- as.matrix(pro)
	dis <- dist.JSD(pro)
if(index == 2)
	dis <- vegdis(pro, method = args[3])
if(index == 3)
	dis <- average_dis(pro)

# 








