##
## p3d:ellipsoid.R
## 2011-12-22
##


# from heplots; modified to work with p3d
ellipsoid <- function(center, shape, radius=1, segments=60, warn.rank=FALSE){
	# adapted from the shapes3d demo in the rgl package and from the Rcmdr package
	degvec <- seq(0, 2*pi, length=segments)
	ecoord2 <- function(p) c(cos(p[1])*sin(p[2]), sin(p[1])*sin(p[2]), cos(p[2]))
	v <- t(apply(expand.grid(degvec,degvec), 1, ecoord2))
	if (!warn.rank){
		warn <- options(warn=-1)
		on.exit(options(warn))
	}
	Q <- chol(shape, pivot=TRUE)
	order <- order(attr(Q, "pivot"))
	v <- center + radius * t(v %*% Q[, order])
	v <- rbind(v, rep(1,ncol(v))) 
	e <- expand.grid(1:(segments-1), 1:segments)
	i1 <- apply(e, 1, function(z) z[1] + segments*(z[2] - 1))
	i2 <- i1 + 1
	i3 <- (i1 + segments - 1) %% segments^2 + 1
	i4 <- (i2 + segments - 1) %% segments^2 + 1
	i <- rbind(i1, i2, i4, i3)
	x <- asEuclidean(t(v))
	ellips <- qmesh3d(v, i)
	return(ellips)
}
