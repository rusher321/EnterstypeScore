# this script use the fuzzy cluster to get the sample the entertypes score
# library(cluster)

fanny <- function (x, k, diss = inherits(x, "dist"), memb.exp = 2, metric = c("euclidean", 
    "manhattan", "SqEuclidean"), stand = FALSE, iniMem.p = NULL, 
    cluster.only = FALSE, keep.diss = !diss && !cluster.only && 
        n < 100, keep.data = !diss && !cluster.only, maxit = 500, 
    tol = 1e-15, trace.lev = 0) 
{
    if ((diss <- as.logical(diss))) {
        if (anyNA(x)) 
            stop("NA values in the dissimilarity matrix not allowed.")
        if (data.class(x) != "dissimilarity") {
            if (!is.null(dim(x))) {
                x <- as.dist(x)
            }
            else {
                if (!is.numeric(x) || is.na(n <- sizeDiss(x))) 
                  stop("'x' is not and cannot be converted to class \"dissimilarity\"")
                attr(x, "Size") <- n
            }
            class(x) <- dissiCl
            if (is.null(attr(x, "Metric"))) 
                attr(x, "Metric") <- "unspecified"
        }
        n <- attr(x, "Size")
        dv <- as.double(c(x, 0))
        jp <- 1
        mdata <- FALSE
        ndyst <- 0L
        x2 <- double(n)
        jdyss <- 1
    }
    else {
        x <- data.matrix(x)
        if (!is.numeric(x)) 
            stop("x is not a numeric dataframe or matrix.")
        x2 <- if (stand) 
            scale(x, scale = apply(x, 2, meanabsdev))
        else x
        metric <- match.arg(metric)
        ndyst <- which(metric == eval(formals()$metric))
        n <- nrow(x2)
        jp <- ncol(x2)
        if ((mdata <- any(inax <- is.na(x2)))) {
            jtmd <- as.integer(ifelse(apply(inax, 2, any), -1, 
                1))
            valmisdat <- 1.1 * max(abs(range(x2, na.rm = TRUE)))
            x2[inax] <- valmisdat
        }
        dv <- double(1 + (n * (n - 1))/2)
        jdyss <- 0
    }
    if ((k <- as.integer(k)) < 1 || k > n%/%2 - 1) 
        stop("'k' (number of clusters) must be in {1,2, .., n/2 -1}")
    if (length(memb.exp) != 1 || (memb.exp <- as.double(memb.exp)) < 
        1 || memb.exp == Inf) 
        stop("'memb.exp' must be a finite number > 1")
    if ((maxit <- as.integer(maxit)[1]) < 0) 
        stop("'maxit' must be non-negative integer")
    computeP <- is.null(iniMem.p)
    if (computeP) 
        iniMem.p <- matrix(0, n, k)
    else {
        dm <- dim(iniMem.p)
        if (length(dm) != 2 || !all(dm == c(n, k)) || !is.numeric(iniMem.p) || 
            any(iniMem.p < 0) || !isTRUE(all.equal(unname(rowSums(iniMem.p)), 
            rep(1, n)))) 
            stop("'iniMem.p' must be a nonnegative n * k matrix with rowSums == 1")
        if (!is.double(iniMem.p)) 
            storage.mode(iniMem.p) <- "double"
    }
    stopifnot(length(cluster.only) == 1)
    stopifnot(length(trace.lev) == 1)
    storage.mode(x2) <- "double"
    res <- .C(cl_fanny, as.integer(n), as.integer(jp), k, x2, 
        dis = dv, ok = as.integer(jdyss), if (mdata) rep(valmisdat, 
            jp) else double(1), if (mdata) jtmd else integer(jp), 
        ndyst, integer(n), integer(n), integer(n), double(n), 
        p = iniMem.p, dp = matrix(0, n, k), avsil = double(k), 
        integer(k), double(k), double(k), double(n), ttsil = as.double(0), 
        obj = as.double(c(cluster.only, trace.lev, computeP, 
            0)), clu = integer(n), silinf = if (cluster.only) 0 else matrix(0, 
            n, 4), memb.exp = memb.exp, tol = as.double(tol), 
        maxit = maxit)
    if (!(converged <- res$maxit > 0)) {
        warning(gettextf("FANNY algorithm has not converged in 'maxit' = %d iterations", 
            maxit))
    }
    if (!cluster.only) 
        sildim <- res$silinf[, 4]
    if (diss) {
        if (keep.diss) 
            disv <- x
        labs <- attr(x, "Labels")
    }
    else {
        if (res$ok == -1) 
            stop("No clustering performed, NA-values in the dissimilarity matrix.")
        labs <- dimnames(x)[[1]]
        if (keep.diss) {
            disv <- res$dis[-(1 + (n * (n - 1))/2)]
            disv[disv == -1] <- NA
            class(disv) <- dissiCl
            attr(disv, "Size") <- nrow(x)
            attr(disv, "Metric") <- metric
            attr(disv, "Labels") <- labs
        }
    }
    if (length(labs) != 0) {
        if (!cluster.only) 
            sildim <- labs[sildim]
        dimnames(res$p) <- list(labs, NULL)
        names(res$clu) <- labs
    }
    coeff <- if (memb.exp == 2) 
        res$obj[3:4]
    else {
        cf <- sum(res$p^2)/n
        c(cf, (k * cf - 1)/(k - 1))
    }
    names(coeff) <- c("dunn_coeff", "normalized")
    if (abs(coeff["normalized"]) < 1e-07) 
        warning("the memberships are all very close to 1/k. Maybe decrease 'memb.exp' ?")
    k.crisp <- res$obj[1]
    res$obj <- c(objective = res$obj[2])
    r <- list(membership = res$p, coeff = coeff, memb.exp = memb.exp, 
        clustering = res$clu, k.crisp = k.crisp, objective = c(res$obj, 
            tolerance = res$tol), convergence = c(iterations = res$maxit, 
            converged = converged, maxit = maxit), diss = if (keep.diss) disv, 
        call = match.call())
    if (k != 1 && !cluster.only) {
        dimnames(res$silinf) <- list(sildim, c("cluster", "neighbor", 
            "sil_width", ""))
        r$silinfo <- list(widths = res$silinf[, -4], clus.avg.widths = res$avsil[1:k], 
            avg.width = res$ttsil)
    }
    if (keep.data && !diss) {
        if (mdata) 
            x2[x2 == valmisdat] <- NA
        r$data <- x2
    }
    class(r) <- c("fanny", "partition")
    r
}
