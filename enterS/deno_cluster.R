
#-------------------------------------------------------------------------------#
# Copyright (c) 2018 Huahui Ren (BGI-shenzhen). Allrights reserved.             #
# Created by Huahui Ren (BGI-shenzhen) on 21/6/2018                             #
# This R program is using to deno cluster on newdata			        #
# Args:                                                                         #
#   pro:  column is sample, row is genus                                        #
#   method: dmn/pam			                                        #
#   max.iter: the max group number                                              #
#   prefix: the result prefix                                                   #
# output:                                                                       #
#   cluster                                                                     #
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
packages <- c("ggplot2","ape","RcolorBrewer","Rtsne","vegan")
ipak(packages)

source("./script/cluster.R")


args <- commandArgs(T)


if(length(args) <=  3){
        stop("Rscript deno_cluster.R [pro] [method] [max.iter] [prefix]")
}

pro <- read.table(args[1], header=T, row.names=1, sep="\t")
method <- args[2]










