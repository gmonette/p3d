##
## p3d:data.Ell3d.R
## 2011-12-22
##

#' @export
data.Ell3d <-
function(  ... ) {
    #
    # NEEDS WORK:
    # should work like 'ellipsoid = TRUE' when called with no arguments
    #
      a <- args3d(...)
      xyz <- a$x
      oargs <- a[-1]
      center <- apply( xyz, 2, mean, na.rm = TRUE)
      shape <-  var( xyz, na.rm = TRUE, use = "complete")
      do.call( Ell3d, c( center= center, shape = shape, oargs))
}

