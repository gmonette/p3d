##
## p3d:abline2.R
## 2011-12-22
##

#' @export
abline2 <-
function( p1, p2, ...) {
            if ( is.list(p1)) abline2( p1[[1]], p1[[2]], ...)
            else do.call( abline, c(ab(p1,p2),...))
    }

