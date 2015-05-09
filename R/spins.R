##
## p3d:spins.R
## 2011-12-22
##


spins <-
function( inc.theta = 1/4, inc.phi = 0, theta = NULL, phi = NULL) {
 help = "
    Spins the current rgl window with a constant increment in theta and phi.
    Use ESC in R window to stop.
    BUG: - Respects some mouse motions but behaves unpredictably.
         - Should have a better way to stop
 "
  cat("\nUse ESC in R window to stop spinning\n")
      um <- par3d('userMatrix')
      Acos = function(x) 360*acos(x)/(2*pi)
      Asin = function(x) 360*asin(x)/(2*pi)
      Atan2 = function( s, c) atan2( s, c)*(180/pi)
      theta.phi = function( ) {

          par3d()  # it may be necessary to call par3d first to get the right values on the next call
          um = par3d('userMatrix')
          list(theta = Atan2(-um[1,3], um[1,1]), phi = Atan2 (um[3,2],um[2,2]))
      }

      tp = theta.phi()
      if(is.null(theta)) theta = tp$theta
      if(is.null(phi)) phi = tp$phi
      while(TRUE) {
           theta = (theta + inc.theta) %% 360
           phi = phi + inc.phi
           spin( theta = theta , phi = phi)
           tp = theta.phi()
           # print( tp )
           theta = tp$theta
           phi = tp$phi
      }
      invisible(NULL)
}

