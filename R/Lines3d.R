##
## p3d:Lines3d.R
## 2011-12-22
##
## merged Lines3d.R and Lines3d.default.R  10-30-2011 (MF)
#  fixed args for Lines3d.default as an S3 method
# TODO: obj serves no useful role and should be deleted if no other Linesed methods contemplated -- removed 11-04-2011 (GM)

Lines3d <-
function( ... ) {
    "
    Lines3d allows arguments to be vectors or matrices whose corresponding
    axes as specified with names of the form 'x', 'xy', 'yxz', etc.
    "
    UseMethod("Lines3d")
}

Lines3d.default <-
		function( ... )  {
	a <- args3d(...)
	# disp(a)
	xyz <- a$x
	if (nrow(xyz) == 0) return( invisible(0))
	nas <- apply(xyz, 1, function(x) any(is.na(x)))
	inds <- 1:nrow(xyz)
	inds[nas] <- NA
	inds <-  cbind(inds[-length(inds)],inds[-1])
	inds <- na.omit(inds)
	inds <- c(t(inds))
	xyz <- xyz[inds,]
	a$x <- xyz
	# names(a) <- sub('^lwd$','size', names(a))
	# do.call("rgl.lines", a)
	do.call("segments3d", a)
}


# Lines3d( y = 1, z = 1.1, x =c(0,2), col = 'red')
# Lines3d( y = 1.1, z = 1.1, x =c(0,2), color = 'red')

# Lines3d( y = 1, z = 1.2, x =c(0,2))
# Lines3d( y = 1.1, z = 1.2, x =c(0,2))


# Lines3d( y = 1, z = 1.5, x =c(0,2), col = 'red3', lwd = 3)
# Lines3d( y = 1.1, z = 1.5, x =c(0,2), color = 'red3', lwd = 3)
# Lines3d( y = 1.1, z = 1.8, x =c(0,2),col='red',lwd = 3)
# material3d()