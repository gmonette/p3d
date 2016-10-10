##
## p3d:Axes3d.R
## 2011-12-22
##


xAxes3d <-
function(n=5, len= .05,col = 'black', ...) {
    #
    # Draws axes
    #
    gen.axes <- function( pvals, orig, ext , len=.05)  {
        ticks <- rbind( orig, orig*(1-len) + len* (orig -( ext - orig)))
        # disp(ticks)
        ps <- c(apply(cbind( pvals, pvals, NA),1,c))
        # disp(ps)
        inds <- rep( c(1,2,NA), length(pvals))
        # disp(inds)
        ret <- cbind( ps, ticks[inds,])
        ret
    }
    Pretty <- function(x,n=5,...) {
        ret <- pretty(x,n=n,...)
        drop <- ret < min(x) | ret > max(x)
        ret [!drop]
    }
    abox <- Plot3d.par()$abox

    mat <- gen.axes( labs <- Pretty( abox[1:2],n=n), abox[c(3,5)], abox[c(4,6)],len = len)
    Lines3d( xyz = mat,col=col,lwd=1,...)
    Text3d( xyz = mat[seq(2, nrow(mat), 3),], text = as.character(labs),col = col,...)

    mat <- gen.axes( labs <- Pretty( abox[3:4],n=n), abox[c(1,5)], abox[c(2,6)],len = len)
    Lines3d( yxz = mat , col = col,lwd=1,...)
    Text3d( yxz = mat[seq(2, nrow(mat), 3),], text = as.character(labs),col = col,...)

    mat <- gen.axes( labs <- Pretty( abox[5:6],n=n), abox[c(1,3)], abox[c(2,4)],len = len)
    Lines3d( zxy = mat , col = col,lwd=1,...)
    Text3d( zxy = mat[seq(2, nrow(mat), 3),], text = as.character(labs),col = col,...)

}




#' @export
Axes3d <-
  function(n=5, len= .05,col = 'black', ...) {
    #
    # Draws axes
    #
    gen.axes <- function( pvals, orig, ext , len=.05)  {
      ticks <- rbind( orig, orig*(1-len) + len* (orig -( ext - orig)))
      # disp(ticks)
      ps <- c(apply(cbind( pvals, pvals, NA),1,c))
      # disp(ps)
      inds <- rep( c(1,2,NA), length(pvals))
      # disp(inds)
      ret <- cbind( ps, ticks[inds,])
      ret
    }
    Pretty <- function(x,n=5,...) {
      ret <- pretty(x,n=n,...)
      drop <- ret < min(x) | ret > max(x)
      ret [!drop]
    }
    pars <- Plot3d.par()
    abox <- pars$abox
    dat <- pars$data
    if (is.factor(v <- dat[[2]])) {
      mat <- gen.axes(1:length(levels(v)),
                      abox[c(3,5)], abox[c(4,6)], len = len)
      Lines3d(xyz = mat, col = col, lwd = 1,...)
      Text3d( xyz = mat[seq(2, nrow(mat), 3),],
              text = levels(v), col = col, ...)

    } else {
      mat <- gen.axes( labs <- Pretty( abox[1:2],n=n),
                       abox[c(3,5)], abox[c(4,6)],len = len)
      Lines3d( xyz = mat,col=col,lwd=1,...)
      Text3d( xyz = mat[seq(2, nrow(mat), 3),],
              text = as.character(labs),col = col,...)
    }
    if (is.factor(v <- dat[[1]])) {
      mat <- gen.axes(1:length(levels(v)),
                      abox[c(1,5)], abox[c(2,6)], len = len)
      Lines3d(yxz = mat, col = col, lwd = 1,...)
      Text3d( yxz = mat[seq(2, nrow(mat), 3),],
              text = levels(v), col = col, ...)

    } else {
      mat <- gen.axes( labs <- Pretty( abox[3:4],n=n),
                       abox[c(1,5)], abox[c(2,6)],len = len)
      Lines3d( yxz = mat , col = col,lwd=1,...)
      Text3d( yxz = mat[seq(2, nrow(mat), 3),],
            text = as.character(labs),col = col,...)
    }
    if( is.factor(v <- dat[[3]])) {
      cat('z')
      mat <- gen.axes(1:length(levels(v)),
                       abox[c(1,3)], abox[c(2,4)], len = len)
      Lines3d(zxy = mat, col = col, lwd = 1,...)
      Text3d( zxy = mat[seq(2, nrow(mat), 3),],
              text = levels(v), col = col, ...)
    } else {
      mat <- gen.axes( labs <- Pretty( abox[5:6],n=n),
                       abox[c(1,3)], abox[c(2,4)],len = len)
      Lines3d( zxy = mat , col = col,lwd=1,...)
      Text3d( zxy = mat[seq(2, nrow(mat), 3),],
              text = as.character(labs),col = col,...)
    }

  }
