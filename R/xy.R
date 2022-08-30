##
## p3d:Long3d.R
## 2011-12-22
##

# fixed buglet using plist in do.call


#' @export
xy <-
function( p1, p2 ) {
          # turn list of points into matrix for plotting
          if( is.list( p1) ) do.call( rbind, p1)
          else rbind( p1, p2)
    }

