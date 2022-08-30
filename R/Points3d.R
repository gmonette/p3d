##
## p3d:Points3d.R
## 2011-12-22
##

# merged Points.*.R (MF)


#' @export
Points3d <-
function(x,...) UseMethod("Points3d")

# Points3d.default <-
# 		function(x,...) scat3d( x,..., clear =FALSE)

#' @export
Points3d.formula <-
		function(x,...) Plot3d(x, ... , clear = FALSE)


#' @export
Points3d.default <-
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
    do.call("points3d", a)
  }


#' @export
Points3d_ <-
  function(x,...) UseMethod("Points3d_")

#' @export
Points3d_.default <-
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
    do.call("spheres3d", a)
  }
