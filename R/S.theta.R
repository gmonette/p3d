##
## p3d:S.theta.R
## 2011-12-22
##


#' @export
S.theta <-
function( theta ) {
    # rotate through vertical axis through phi degrees
    theta = theta *2*pi/360
    cbind( c(cos(theta), 0,sin(theta),0), c(0,1,0,0), c(-sin(theta), 0, cos(theta),0), c(0,0,0,1))
}

