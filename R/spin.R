##
## p3d:spin.R
## 2011-12-22
##


#' @export
spin <-
function( theta=0, phi=15,
          fov = par3d("FOV"), zoom = par3d("zoom"),
          scale = par3d("scale"), stay = FALSE) {

          # Like rgl viewpoint but starts with defaults except
          # for theta (rotation around vertical axis)
          # and phi (angular height above the horisontal plane)
          # PLAN:
          # without arguments it should just start spinning
          # USAGE: see rgl.viewpoint and rgl.snapshot
          # rgl.bringtotop(stay = FALSE)
          rgl.viewpoint( theta=theta, phi = phi,
                fov = fov, zoom = zoom, scale = scale)

}


#' @export
bend <- function(x,linear = .8) {
  if ( linear >= 1) return(x)
  low <- min(x,na.rm=T)
  high <- max(x,na.rm=T)
  if ( low ==high) return(x)
  knot1 <- (1-linear)/2
  knot2 <- 1 - knot1
  psi <- .5/(knot1-knot1^2)
  z <- (x - low)/(high-low)
  ret <- ifelse( z < knot1, psi*z^2,
                 ifelse( z < knot2, psi*knot1^2 + 2*psi*knot1*(z-knot1),
                         1 - psi*(1-z)^2))
  low + (high - low) *ret
}


#' @export
qseq <- function( ... ,linear = .8) {
  if(linear >= 1) seq(...) else bend( seq(...), linear = linear)
}


#' @export
spinto <-
  function (theta = NULL, phi = NULL, fov = NULL, dpf = .5, n , type = "quad",
            linear = .85,
           snap = NULL,...)
  {
    # started programming  GM 2013-09-05
    #  Spins the current rgl window with a constant increment in theta and phi.
    #
    um <- par3d("userMatrix")
    Acos <- function(x) 360 * acos(x)/(2 * pi)
    Asin <- function(x) 360 * asin(x)/(2 * pi)
    Atan2 <- function(s, c) atan2(s, c) * (180/pi)
    theta.phi <- function() {
      par3d()
      um <- par3d("userMatrix")
      list(theta = Atan2(-um[1, 3], um[1, 1]), phi = Atan2(um[3,
                                                              2], um[2, 2]))
    }
    tp <- theta.phi()
    theta.start <- tp$theta
    phi.start <- tp$phi
    fov.start <- par3d("FOV")
    if ( missing(theta) && missing(phi) && missing(fov)) {
      theta <- 0
      phi <- 0
      fov <- 0
    } else {
      if (is.null(theta)) theta <- theta.start
      if (is.null(phi)) phi <- phi.start
      if (is.null(fov)) fov <- fov.start
    }
    delta <- sqrt( (theta-theta.start)^2 + (phi-phi.start)^2 + (fov-fov.start)^2)
    if (missing(n)) n <- max(ceiling( delta/dpf),1)
    #if( type != 'quad') qseq <- seq
    theta <- qseq(theta.start, theta, length.out = n, linear = linear)
    phi <- qseq(phi.start, phi, length.out = n, linear = linear)
    fov <- qseq(fov.start, fov, length.out = n, linear = linear)
    for( ii in 1:n) {
      spin(theta = theta[ii], phi = phi[ii], fov = fov[ii])
      if(!is.null(snap)) snap(...)
    }
    invisible(NULL)
  }
