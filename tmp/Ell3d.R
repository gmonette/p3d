## Modified 2013-08-29 by GM
##   fix problem with Ell3d with missing data
##   and with nearly singular variances.
##   The ideal fix which would have involved improving
##   on the use of 'chol' to factorize the variance
##   in ellipse3d.default didn't work well because
##   replacing ellipse3d.default with Ellipse3d herein
##   messed up the colours, presumably because of
##   namespace issues.
##   So the current fix involved fattening up the
##   variance matrices a bit but this might cause
##   unexpected problems.
##
## p3d:Ell3d.R
## 2011-12-22
##

##  Added 'partial ellipse' from p3d.beta 2011-11-05 (GM)


#' @export
Ellipse3d <-
function (x, scale = c(1, 1, 1), centre = c(0, 0, 0), level = 0.95,
          t = sqrt(qchisq(level, 3)), which = 1:3, subdivide = 3,
          smooth = TRUE,
          ...)
{
  # This fixes the factorization problem in ellipse3d.default
  # but is not currently used because of namespace problems
  #
  # From rgl:::ellipse3d.default that uses
  #  unpivoted chol to factor the variance matrix
  #  with resulting errors when the variance is nearly
  #  singular
  # Here we use a method based on the eigenvalue factorization
  # but This doesn't work because of namespace problems
  # and Ellipse3d not having access to colors in rgl.
  #
  fac <- function( vv ) {
      sv <- eigen(vv,symmetric=TRUE)
      t(sv$vectors) * sqrt(sv$values)
  }
  stopifnot(is.matrix(x))
  cov <- x[which, which]
  chol <- fac(cov)
  sphere <- subdivision3d(cube3d(...), subdivide)
  norm <- sqrt(sphere$vb[1, ]^2 + sphere$vb[2, ]^2 + sphere$vb[3,
                                                               ]^2)
  for (i in 1:3) sphere$vb[i, ] <- sphere$vb[i, ]/norm
  sphere$vb[4, ] <- 1
  if (smooth)
    sphere$normals <- sphere$vb
  result <- scale3d(transform3d(sphere, chol), t, t, t)
  if (!missing(scale))
    result <- scale3d(result, scale[1], scale[2], scale[3])
  if (!missing(centre))
    result <- translate3d(result, centre[1], centre[2], centre[3])
  return(result)
}

#' @export
Ell3d <- function(x , ...) {
#   Adds a data ellipse(s) to a 3D plot using the data frame"
#   for the plot"
      UseMethod("Ell3d")
}

#' @export
Ell3d.default <- function( x,  radius = 1, col,
                           alpha = 0.5,
                           ellipsoid = TRUE,
                           partial = NULL,
                           partial.col = "black",
                           partial.lwd = 1,
                           partial.alpha = 1,
                           partial.offset = 0,
                           use.groups = pars$has.groups,
                           verbose = 0,
                           mean = 0,
                           variance = NULL,
                           ...) {
  fatten <- function(vv){
      # ellipses that are almost flat cause a problem
      # with 'chol' in ellipsoid3d
      # so we fatten them up a bit:
      d <- diag(sqrt(diag(vv)))
      dinv <- diag(1/sqrt(diag(vv)))
      e <- eigen(dinv%*%vv%*%dinv,symmetric=TRUE)
      lam <- e$values
#      lam <- pmax(lam, max(lam)/100000)
      lam <- pmax(lam, max(lam)/100000)
      d%*%(e$vectors%*%diag(lam)%*%t(e$vectors))%*%d
  }
#
#   fatten <- function(vv) {
#     vv + .0000001 * diag(diag(vv))
#   }
#  fatten <- function(vv) vv
  Rebind <- function( mat, i ) {
    if ( i > ncol(mat)) return( cbind(mat,0))
    if ( i == 1 ) return( cbind(0,mat))
    cbind( mat[, 1:(i-1)], 0, mat[,i:(ncol(mat)-1)])
  }

  ##
  ## TO DO: modify to take center and shape
  ##

  condvar <- function( vv, i ) {
    vv[-i,-i] -
      vv[-i,i,drop=FALSE]%*%solve( vv[i,i,drop=FALSE],vv[i,-i,drop=FALSE])
  }
  Levels <- function(x) {
    if (is.factor(x))
      levels(x)
    else unique(x)
  }

  ellp <- function(partial, partial.offset, cc, vv, radius,partial.col,partial.lwd,partial.alpha){
    for ( jj in partial ){
      for ( off in partial.offset ){
        center <- cc[-jj] + radius * (off / sqrt(vv[jj,jj])) * vv[-jj,jj]
        radius.factor = sqrt( 1 - off^2)
        ell.lines <- ell( center = center,
                          shape = condvar(vv,jj),
                          radius = radius * radius.factor)
        ell.lines <- Rebind( ell.lines, jj)
        ell.lines[,jj] <- cc[jj] + radius * off * sqrt(vv[jj,jj])
        Lines3d(ell.lines, col = partial.col, lwd = partial.lwd, alpha = partial.alpha)
      }
    }
  }

  pars <- Plot3d.par()
  if (missing(col)) col <- pars$col
  if ((!missing(x))||(!missing(variance))){
    if( !missing(x) ) {
      if ( is.data.frame(x) ) {
        xmat <- as.matrix(x[,pars$names[c('x','y','z')]] )
      } else {
        xmat <- x
      }
      vv <- fatten(var(na.omit(xmat)))
      cc <- apply(na.omit(xmat),2,mean)
      if (ellipsoid) plot3d( ellipse3d(vv,centre = cc, t = radius),
                             col=col, alpha = alpha, add = TRUE)
      if ( !is.null(partial) ){
        ellp(partial, partial.offset, cc, vv, radius,partial.col,partial.lwd,partial.alpha)
      }
    } else {  # x is missing but variance is not
      # use variance and mean
      vv <- variance
      cc <- mean
      if (ellipsoid) plot3d( ellipse3d(vv,centre = cc, t = radius),
                             col=col, alpha = alpha, add = TRUE)
      if ( !is.null(partial) ){
        ellp(partial, partial.offset, cc, vv, radius,partial.col,partial.lwd,partial.alpha)
      }
    }
  } else {  # use displayed data
    if (verbose > 1)
      disp(col)
    xmat <- as.matrix(pars$data[,pars$names[c('x','y','z')]] )
    if( !use.groups ) {
      if (nrow( xmat)>1){
        vv <- fatten(var(na.omit(xmat)))
        cc <- apply(na.omit(xmat),2,mean)
        plot3d( ellipse3d(vv,centre = cc, t = radius),
                col=col[1], alpha = alpha, add = TRUE)
        if ( !is.null(partial) ){
          ellp(partial, partial.offset, cc, vv, radius,partial.col,partial.lwd,partial.alpha)
        }

      }
    } else {
      inds <- split( 1:nrow(xmat), pars$data[,pars$names['g']])
      lapply ( seq_along(inds), function( ii ) {
        gmat <- xmat[inds[[ii]],,drop = FALSE]
        if( nrow(na.omit(gmat)) > 1) {
          vv <- fatten(var(na.omit(gmat)))
          #disp(vv)
          #disp(gmat)
          if(FALSE){
          ev <- eigen(vv,symmetric=TRUE, only.values = TRUE)

          # fatten up nearly singular matrix to avoid crash in rgl
          if ( (max(ev$values)/min(ev$values)) > 1e07 )
            vv <- vv + (max(ev$values)*1e-07) *diag(nrow(vv))
          }
          cc <- apply(na.omit(gmat), 2, mean)
          plot3d( ellipse3d(vv,centre = cc, t = radius),
                  col=col[ii], alpha=0.5, add = TRUE)

          if ( !is.null(partial) ){
            ellp(partial, partial.offset, cc, vv, radius,partial.col,partial.lwd,partial.alpha)
          }

        }
      }
      )
    }
  }
  invisible(0)
}
