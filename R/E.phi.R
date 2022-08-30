##
## p3d:E.phi.R
## 2011-12-22
##


#' @export
E.phi <-
function( phi ) {
    # incline horizontal plane by phi degrees
    # note that userMatrix for spinning has form E.phi %*% S.theta
    phi = phi *2*pi/360
    cbind(c(1,0,0,0), c(0,cos(phi),sin(phi),0),  c(0,-sin(phi), cos(phi),0), c(0,0,0,1))
}

