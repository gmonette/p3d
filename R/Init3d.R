##
## p3d:Init3d.R
## 2011-12-22
##
##
##  GM: mod 2011-03-07: added ... to rgl.open to allow:

# From: Carrie Smith:
# Also, I remembered why I used open3d() from rgl instead of Init3d() in the demo in class.
# Init3d() on Mac opens a small window but it can't be resized.
# I was able to specify a larger window using open3d(windowRect=c(x,x,x,x)),
# but I don't think windowRect is a valid argument for Init3d().
# Don't know if that's an easy thing to change, but if it is I think Mac users would appreciate it.

##

#' @export
Init3d <-
function( par3d = list(),
      family = c('sans','serif', "mono", "symbol"),
      mouseMode = c('polar','fov','zoom'),
      cex = .8,
      font = 2,
      left = 700, top = 50, size = 750,
      ...) {
    family = match.arg(family)
    mod = list(family=family, cex = cex, font = font,
               mouseMode = mouseMode,
               windowRect = c(left,top,left + size, top + size))
    par3d [ names(mod)] <- mod
    open3d(...)
    do.call('par3d', par3d)
    fg()
}

## Alternative version that uses open3d

#' @export
Init3d_ <-
  function( par3d = list(),
            family = c('sans','serif', "mono", "symbol"),
            # mouseMode = c('polar','fov','zoom'),
            mouseMode =
              c(none="none", left="polar", right="zoom", middle="fov", wheel="pull"), # Thanks to John Fox, 2022_08_30
            cex = .8,
            font = 1,
            ...) {
    library(p3d)
    family = match.arg(family)
    mod = list(family=family, cex = cex, font = font,mouseMode = mouseMode)
    par3d [ names(mod)] <- mod
    rgl.open(...)
    do.call('par3d', par3d)
  }


