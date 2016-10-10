##
## p3d:Surf3d.R
## 2011-12-22
##


#' @export
Surf3d <-
function( x ,...) {
  help = "
    Surf3d methods not yet written
  "
  UseMethod("Surf3d")
}

#' @export
Surf3d.default <-
		function( x, z, y, ...) {
	g <- expand.grid( x = sort(unique(x)), z = sort(unique(z)))
	stop("Surf3d methods not yet written")
}
