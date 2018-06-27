#-------------------------------------------------------------------------------#
# Copyright (c) 2018 Huahui Ren (BGI-shenzhen). Allrights reserved.             #
# Created by Huahui Ren (BGI-shenzhen) on 21/6/2018                             #
# here use the fanny function to compute the entertype score			#
# the method is fuzzy c-mean, hereafter can use the its variant			#
# Args:                                                                         #
#   input : dist or profile                                                     #
#   config : config lable                                                       #
#   distance.method: bray/euclidean/jsd/average                                 #
#   cluster: 2,3,4                                                              #
#   prefix: the result prefix                                                   #
# output:                                                                       #
#   entertypeScore                                                              #
#   figure                                                                      #
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
packages <- c("ggplot2","cluster")
ipak(packages)
source("../distance.R")
# ----main script----

pro <- read.table(args[1], header=T, row.names=1, sep="\t")
config <- read.table()

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
        dis <- vegdis(pro, method = args[3])

result_fanny <- fanny(dis, args[3], diss = T, maxit = 1000)
score <- result_fanny$membership

write.table(score, paste0("./result/",prefix,".fanny.tab"), quote =F, col.names=NA, sep="\t")
# ---- figure -----
