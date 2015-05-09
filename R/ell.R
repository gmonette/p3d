##
## p3d:ell.R
## 2011-12-22
##


# Modified by GM 2013-10-27:
#    added ellpts and ellptsc to generate 9 points
#    of tangent parallelogram
#
## Last modified by Georges Monette 2010-12-02
## Consolidated ell.R, dell.R, ell.conj.R, center.R and center.ell.F
## Added function ellpt, ellptc, elltan, elltanc, ellbox to generate points on ellipses,
## axes of ellipses and tangents.


ell <-
function( center = c(0,0), shape = diag(2) , radius  = 1, n =100) {
          fac <- function( x )  {
              # fac(M) is a 'right factor' of a positive semidefinite M
              # i.e. M = t( fac(M) ) %*% fac(M)
              # similar to chol(M) but does not require M to be PD.
              xx <- svd(x)
          t(xx$v) * sqrt(pmax( xx$d,0))
          }
         angles = (0:n) * 2 * pi/n
         if ( length(radius) > 1) {
            ret <- lapply( radius, function(r) rbind(r*cbind( cos(angles), sin(angles)),NA))
            circle <- do.call( rbind, ret)
         }
         else circle = radius * cbind( cos(angles), sin(angles))
         ret <- t( c(center) + t( circle %*% fac(shape)))
         attr(ret,"parms") <- list ( center = rbind( center), shape = shape, radius = radius)
         class(ret) <- "ell"
         ret
    }
    
dell <-
function( x, y, radius = 1, ...) {
        if ( (is.matrix(x) && (ncol(x) > 1))|| is.data.frame(x)) mat <- as.matrix(x[,1:2])
        else if (is.list(x)) mat <- cbind(x$x, x$y)
        else mat <- cbind( x,y)
        ell( apply(mat,2,mean), var(mat), radius = radius, ...)
    }

ell.conj <-
function( center, shape, dir, radius = 1, len = 1) {
            # returns conjugate axes or tangent lines to ellipse
            vecs <- uv( shape, dir, radius)
            list( u = list( center-len*vecs$u, center+len*vecs$u),
                  v = list( center-len*vecs$v, center+len*vecs$v),
                  tan1 = list( center + vecs$u - len*vecs$v,center + vecs$u+ len*vecs$v),
                  tan2 = list( center - vecs$u - len*vecs$v,center - vecs$u+ len*vecs$v),
                  tan3 = list( center + vecs$v - len*vecs$u,center + vecs$v+ len*vecs$u),
                  tan4 = list( center - vecs$v - len*vecs$u,center - vecs$v+ len*vecs$u),
                  center = center)
    }


center <-
function( obj, ... ) UseMethod("center")


center.ell <-
function( obj, ...) attr(obj, 'parms') $ center



ConjComp <- function( X , Z = diag( nrow(X)) , ip = diag( nrow(X)), tol = 1e-07 ) {
                  # also in package spida:
                  # keep versions consistent
                     # ConjComp returns a basis for the conjugate complement of the
                     # conjugate projection of X into span(Z) with respect to inner product with
                     # matrix ip.
                     # Note: Z is assumed to be of full column rank but not necessarily X.
                     xq <- qr(t(Z) %*% ip %*% X, tol = tol)
                     if ( xq$rank == 0 ) return( Z )
                     a <- qr.Q( xq, complete = TRUE ) [ ,-(1:xq$rank)]
                     Z %*% a
            }

uv <- function(object,...) UseMethod('uv')

uv.ell <- function( object, u, radius = 1, ...){
        p <- attr(object,"parms")
        uv( p$shape, u=u, radius=radius)
}
uv.default <-
function( object, u , radius = 1, ...) {
       # returns 'unit' u and conjugate v
            u <- u / sqrt( sum( u*solve(object,u)))   # 'unit' vector in direction of dir
            v <- c(ConjComp( u, diag(2) , solve(object)))  # conjugate
            v <- v / sqrt( sum( v * solve( object, v)))
       list(u = radius * u, v= radius * v)
    }



ellpt <- function(ell, dir = c(0,1) , radius = 1 ) {
   # point on an ellipse in a particular direction
   p <- attr(ell,'parms')
   dir <- cbind(dir)
   dn <- sum(dir* solve(p$radius[1]^2 * p$shape, dir))
   ax <- dir / sqrt(dn)
   #disp(ax)
   #disp( rbind(radius))
   #disp( p$center)
   t( ax %*% rbind(radius) + as.vector(p$center))               # returns a row for plotting
}

ellptc <- function(ell, dir = c(0,1) , radius = 1 ) {
   # point on an ellipse in a conjugate direction
   p <- attr(ell,'parms')
   dir <- cbind(dir)
   ax <- Null( solve(p$shape, dir) )
   dn <- sum(ax* solve( p$shape, ax))
   ax <- ax* p$radius[1] /sqrt(dn)
   t( ax %*% rbind(radius) + as.vector(p$center))               # returns a row for plotting
}

elltanc <- function( ell, dir = c(0,1), radius = 1, len = 1, v = c(-1,1)) {
       p <- attr(ell,'parms')
       ax <- ellptc( ell, dir = dir, radius = len * v)
       if ( is.null(radius) ) return( t( t(ax) + dir))
       pt <- ellpt( ell, dir = dir, radius = radius)
       t( t(ax) -as.vector(p$center) + as.vector(pt))
}

elltan <- function( ell, dir = c(0,1), radius = 1, len = 1, v = c(-1,1)) {
       p <- attr(ell,'parms')
       ax <- ellpt( ell, dir = dir, radius =  len * v)
       pt <- ellptc( ell, dir = dir, radius = radius)
       t( t(ax) -as.vector(p$center) + as.vector(pt))
}

ellbox <- function( ell, dir = c(0,1) , radius = 1 ){
      rbind(
 elltan(  ell, dir = dir, radius = radius) , NA,
 elltanc( ell, dir = dir, radius = radius) , NA,
 elltan(  ell, dir  = dir, radius = -radius), NA,
 elltanc( ell, dir = dir, radius = -radius) )
}

ellplus <-
function (center = rep(0, 2), shape = diag(2), radius = 1, n = 100,
    angles = (0:n) * 2 * pi/n, fac = chol, ellipse = all, diameters = all,
    box = all, all = FALSE)
{
    help <- "\n        ellplus can produce, in addition to the points of an ellipse, the\n        conjugate axes corresponding to a chol or other decomposition\n        and the surrounding parallelogram.\n        "
    rbindna <- function(x, ...) {
        if (nargs() == 0)
            return(NULL)
        if (nargs() == 1)
            return(x)
        rbind(x, NA, rbindna(...))
    }
    if (missing(ellipse) && missing(diameters) && missing(box))
        all <- TRUE
    circle <- function(angle) cbind(cos(angle), sin(angle))
    Tr <- fac(shape)
    ret <- list(t(c(center) + t(radius * circle(angles) %*% Tr)),
        t(c(center) + t(radius * circle(c(0, pi)) %*% Tr)), t(c(center) +
            t(radius * circle(c(pi/2, 3 * pi/2)) %*% Tr)), t(c(center) +
            t(radius * rbind(c(1, 1), c(-1, 1), c(-1, -1), c(1,
                -1), c(1, 1)) %*% Tr)))
    do.call("rbindna", ret[c(ellipse, diameters, diameters, box)])
}

ellpts <- function( ell, dir = c(0,1), radius = 1, len = 1, v = c(-1,1)) {
  rbind(
    ellpt( ell, dir = dir, radius = radius * c(-1,0,1)),
    elltan( ell, dir = dir, radius = radius,
            v = radius * c(-1,0,1)),
    elltan( ell, dir = dir, radius = -radius,
            v = radius * c(-1,0,1)))
}

ellptsc <- function( ell, dir = c(0,1), radius = 1, len = 1, v = c(-1,1)) {
  rbind(
    ellptc( ell, dir = dir, radius = radius * c(-1,0,1)),
    elltanc( ell, dir = dir, radius = radius,
            v = radius * c(-1,0,1)),
    elltanc( ell, dir = dir, radius = -radius,
            v = radius * c(-1,0,1)))
}








