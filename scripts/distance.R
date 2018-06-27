# to compute the tax profile distance , the distance method is a option, here recommend average distance
# library(vegan)

vegdis <- function (x, method = "bray", binary = FALSE, diag = FALSE, upper = FALSE, 
    na.rm = FALSE, ...) 
{
    ZAP <- 1e-15
    if (!is.na(pmatch(method, "euclidian"))) 
        method <- "euclidean"
    METHODS <- c("manhattan", "euclidean", "canberra", "bray", 
        "kulczynski", "gower", "morisita", "horn", "mountford", 
        "jaccard", "raup", "binomial", "chao", "altGower", "cao", 
        "mahalanobis")
    method <- pmatch(method, METHODS)
    inm <- METHODS[method]
    if (is.na(method)) 
        stop("invalid distance method")
    if (method == -1) 
        stop("ambiguous distance method")
    if (!method %in% c(1, 2, 6, 16) && any(rowSums(x, na.rm = TRUE) == 
        0)) 
        warning("you have empty rows: their dissimilarities may be meaningless in method ", 
            dQuote(inm))
    if (!method %in% c(1, 2, 3, 6, 16) && any(x < 0, na.rm = TRUE)) 
        warning("results may be meaningless because data have negative entries in method ", 
            dQuote(inm))
    if (method == 11 && any(colSums(x) == 0)) 
        warning("data have empty species which influence the results in method ", 
            dQuote(inm))
    if (method == 6) 
        x <- decostand(x, "range", 2, na.rm = TRUE, ...)
    if (method == 16) 
        x <- veganMahatrans(scale(x, scale = FALSE))
    if (binary) 
        x <- decostand(x, "pa")
    N <- nrow(x <- as.matrix(x))
    if (method %in% c(7, 13, 15) && !identical(all.equal(as.integer(x), 
        as.vector(x)), TRUE)) 
        warning("results may be meaningless with non-integer data in method ", 
            dQuote(inm))
    d <- .C("veg_distance", x = as.double(x), nr = N, nc = ncol(x), 
        d = double(N * (N - 1)/2), diag = as.integer(FALSE), 
        method = as.integer(method), NAOK = na.rm, PACKAGE = "vegan")$d
    if (method == 10) 
        d <- 2 * d/(1 + d)
    d[d < ZAP] <- 0
    if (any(is.na(d))) 
        warning("missing values in results")
    attr(d, "Size") <- N
    attr(d, "Labels") <- dimnames(x)[[1]]
    attr(d, "Diag") <- diag
    attr(d, "Upper") <- upper
    attr(d, "method") <- paste(if (binary) 
        "binary ", METHODS[method], sep = "")
    attr(d, "call") <- match.call()
    class(d) <- "dist"
    d
}


dist.JSD <- function(inMatrix, pseudocount=0.0000000001, ...) {
	
	inMatrix <- t(inMatrix)
	KLD <- function(x,y) sum(x *log(x/y))
	JSD<- function(x,y) sqrt(0.5 * KLD(x, (x+y)/2) + 0.5 * KLD(y, (x+y)/2))
	matrixColSize <- length(colnames(inMatrix))
	matrixRowSize <- length(rownames(inMatrix))
	colnames <- colnames(inMatrix)
	resultsMatrix <- matrix(0, matrixColSize, matrixColSize)
        
  inMatrix = apply(inMatrix,1:2,function(x) ifelse (x==0,pseudocount,x))

	for(i in 1:matrixColSize) {
		for(j in 1:matrixColSize) { 
			resultsMatrix[i,j]=JSD(as.vector(inMatrix[,i]),
			as.vector(inMatrix[,j]))
		}
	}
	colnames -> colnames(resultsMatrix) -> rownames(resultsMatrix)
	as.dist(resultsMatrix)->resultsMatrix
	attr(resultsMatrix, "method") <- "dist"
	return(resultsMatrix)

}


# get bray , jsd , euclibean 's genometric mean 

average_dis <- function(data){

    dist.bray <- vegdist(data, method = "bray")
    dist.euclibean <- vegdist(data, method = "euclidean")
    dist.jsd <- dist.JSD(as.matrix(data))
    dist.av <- (dist.bray*dist.euclibean*dist.jsd)^1/3

    return(dist.av)

}




