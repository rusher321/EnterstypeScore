# this script to visualize the score
# libray(ggplot2)
# library(reshape2)
vis_score <- function(score, config ){
	
	```
	score:  score matrix
	config: base / sample

	```
	if(all(rownames(score)==rownames(config))){
		stop("sample names don't match")
	}	

	dat <- rbind(score, config)
	col_num <- ncol(dat)

	colnames(dat) <- c(paste0(rep("cluster_", col_num-1), "_",1:(col_num-1)), "lable")
	
	sample_num <- dat$lable
	if()
	plot <- ggplot(dat, aes(x = , fill = config)) +
		geom_density(position = "identity", alpha = 0.4)+
		theme_bw()+facet_wrap(~lable)
	
	return(plot)

}

