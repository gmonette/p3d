##
## p3d:ab.R
## 2011-12-22
##

#' @export
ab <-
function( p1, p2 ) {
       # prepare arguments to plot a line with
       # do.call('abline', ab( p1, p2))
       d <- p2 - p1
       if ( d[1] == 0) {
          ret <- list( v = p1[1])
       }
       else {
          b = (p2[2]-p1[2])/(p2[1]-p1[1])
          ret <- list( a = p2[2] - b*p2[1], b =b)
       }
       ret
    }

