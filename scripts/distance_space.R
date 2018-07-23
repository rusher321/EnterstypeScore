# this script to visualize the distance space ,and highlight the new samples   
# library(ggplot2)
# library(ape) 
# library(RcolorBrewer)
# library(Rtsne)



# here the most color is 12

paired <- brewer.pal(9, "Set1")

mypcoa <- function(dis, method = "bray", config){
   
   out <- list()
   if(method != "euclidean"){
   	pco <- pcoa(dis)
   }else{
   	pco <- pcoa(dis, correction= "lingoes")
   }

   eig <- pco$value[,1]
   pc1 <- eig[1]/sum(eig)*100
   pc2 <- eig[2]/sum(eig)*100
   pc1 <- paste0("pcoa1(",round(pc1,2),"%)")
   pc2 <- paste0("pcoa2(",round(pc2,2),"%)")
   dat2 <- data.frame(pco$vector[,1:2])
   colnames(dat2) <- c("PC1", "PC2")
   dat2$group <- as.factor(config)
   plot <- ggplot(dat2,aes(PC1,PC2,color=group))+geom_point()+theme_bw()+
	   ggtitle(pasteo("Pcoa","_",method))+theme(plot.title = element_text(hjust = 0.5,size = 14),
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 13),
        panel.grid = element_blank(),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 13))+xlab(pc1)+ylab(pc2)+scale_color_manual(values = c("#999999", paired))
  out <- list(plot, dat2)
  return(out)

}


# tsne 

mytsne <- function(dis, config, method="bray"){
    out <- list()
    #dis <- vegdist(data, method = method)
    tsne <- Rtsne(dis, dims = 2, perplexity=30, verbose=T, max_iter = 500)

    pc1 <- "t-SNE1"
    pc2 <- "t-SNE2"
    dat2 <- data.frame(tsne$Y)
   
    colnames(dat2) <- c("y1", "y2")
    rownames(dat2) <- rownames(dis)
    dat2$group <- as.factor(config[rownames(dat2),1])
    plot <- ggplot(dat2,aes(y1,y2,color=group))+geom_point()+theme_bw()+ggtitle(paste0("Tsne", "_", method))+
	    theme(plot.title = element_text(hjust = 0.5, size = 14),
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 13),
        panel.grid = element_blank(),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 13))+xlab(pc1)+ylab(pc2)+scale_color_manual(values = c("#999999", paired))
  
    out <- list(dat2, plot)
    return(plot)


}


# This function decomposes a rectangular matrix M into a low-rank component, and a sparse component,
# by solving a convex program called Principal Component Pursuit.

myrpca <- function(data, config, scale=F){
 
  out <- list()
  #data <- data[,colSums(data)!=0]
  #if(scale==F){
  #data <- scale(data, center=T, scale=F)
  pc <- rpca(data)

  rpc <- pc$L.svd$u%*%diag(pc$L.svd$d)
#  eig <- pc$L.svd$d  # here not sure
#  pc1 <- eig[1]/sum(eig)*100
#  pc2 <- eig[2]/sum(eig)*100
  pc1 <- paste0("Rpc1")
  pc2 <- paste0("Rpc2")
  dat2 <- data.frame(rpc[,1:2])
  colnames(dat2) <- c("PC1", "PC2")
  rownames(dat2) <- rownames(dat)
  dat2$group <- as.factor(config[rownames(dat2),1])
    plot <- ggplot(dat2,aes(PC1,PC2,color=group))+geom_point()+theme_bw()+ggtitle("Rpca")+theme(plot.title = element_text(hjust = 0.5,size = 14),
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 13),
        panel.grid = element_blank(),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 13))+xlab(pc1)+ylab(pc2)+scale_color_manual(values = c("#999999", paired))
  out <- list(plot, dat2)
  return(plot)
}


# NMDS, here can use any distance

mynmds <- function(data, method = "bray", config){
  
  out <- list()
  mds <- metaMDS(data, distance = method, k = 2, trymax = 50)

  dat2 <- data.frame(mds$point[,1:2])
  colnames(dat2) <- c("NMDS1", "NMDS2")
  dat2$group <- as.factor(config)
  plot <- ggplot(dat2,aes(NMDS1,NDMS2,color=group))+geom_point()+theme_bw()+
    ggtitle(paste0("NMDS","_",method))+theme(plot.title = element_text(hjust = 0.5,size = 14),
                                             axis.title = element_text(size = 10),
                                             axis.text = element_text(size = 13),
                                             panel.grid = element_blank(),
                                             legend.title = element_text(size = 15),
                                             legend.text = element_text(size = 13))+xlab("NMDS1")+ylab("NMDS2")+scale_color_manual(values = c("#999999", paired))
  out <- list(plot, dat2)
  return(out)
  
}







