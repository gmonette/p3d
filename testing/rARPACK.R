m <- matrix(rnorm(1000^2), 1000)
library(rARPACK)
system.time(eigen(m, only.values=T))
system.time(eigs(m, 1000))

library(misc3d)
library(rgl)
rgl
local({
  haveRGL <- suppressWarnings(requireNamespace(rgl,quietly=TRUE))

  ## Example 1: Bivariate quadratic
  zz<-surfaceTriangles(seq(-1,1,len=30), seq(-1,1,len=30),
                       function(x, y) (x^2 + y^2), color2 = "green")
  drawScene(zz)
  drawScene(updateTriangles(zz, material = "metal"),
            screen=list(z=45, y=110),light=c(.3,.3,1))

  ## Example 2: Bivariate normal density
  zz<-surfaceTriangles(seq(-2,2,len=30), seq(-2,2,len=30),
                       function(x, y) 2 * exp(-0.5 * (x^2 + y^2)))
  drawScene(zz)
  drawScene(zz, light=c(.5,.5,1))

  drawScene(zz, lighting=perspLighting, light=c(.5,.5,1))
  drawScene(updateTriangles(zz, material = "dull"), light=c(.5,.5,1))
  drawScene(updateTriangles(zz, material = "shiny"), light=c(.5,.5,1))
  drawScene(updateTriangles(zz, material = "metal"), light=c(.5,.5,1))

  ## Example 3: Volcano
  z <- 2 * volcano
  x <- 10 * (1:nrow(z))
  y <- 10 * (1:ncol(z))
  vtri <- surfaceTriangles(x, y, z, color="green3")
  vtriDull <- updateTriangles(vtri,material="dull")
  vtriMetal <- updateTriangles(vtri,material="metal")
  vtriShiny <- updateTriangles(vtri,material="shiny")

  drawScene(vtri, screen=list(x=40, y=-40, z=-135), scale = FALSE)
  drawScene(vtriShiny, screen=list(x=40, y=-40, z=-135), scale = FALSE)
  drawScene(vtri,lighting=perspLighting,
            screen=list(x=40, y=-40, z=-135), scale = FALSE)

  drawScene(vtri, light=c(1, 1.5, 0),screen=list(x=40, y=-40, z=-135),
            scale=FALSE)
  drawScene(vtri,lighting=perspLighting, light=c(1, 1.5, 0),
            screen=list(x=40, y=-40, z=-135), scale = FALSE)

  drawScene(vtriDull, light=c(1, 1.5, 0),
            screen=list(x=40, y=-40, z=-135), scale = FALSE)
  drawScene(vtriMetal, light=c(1, 1.5, 0),
            screen=list(x=40, y=-40, z=-135), scale = FALSE)
  drawScene(vtriShiny, light=c(1, 1.5, 0),
            screen=list(x=40, y=-40, z=-135), scale = FALSE)

  drawScene(vtriDull, light=c(1, 1.5, 0),
            screen=list(x=40, y=-40, z=-135), scale = FALSE, engine = "grid")
  drawScene(vtriMetal, light=c(1, 1.5, 0),
            screen=list(x=40, y=-40, z=-135), scale = FALSE, engine = "grid")
  drawScene(vtriShiny, light=c(1, 1.5, 0),
            screen=list(x=40, y=-40, z=-135), scale = FALSE, engine = "grid")

  drawScene(list(vtri,
                 translateTriangles(vtriMetal, y = 650),
                 translateTriangles(vtriDull, x=900),
                 translateTriangles(vtriShiny, x=900,y = 650)),
            light = c(1, 1.5, 0), screen = list(x=40, y=-40, z=-135),
            scale = FALSE)

  ## based on an example from lattice wireframe()
  vv <- parametric3d(fx = function(u, v) cos(u)*cos(v),
                     fy = function(u,v) sin(u) * cos(v),
                     fz = function(u,v) sin(v),
                     umin = -pi, umax = pi,
                     vmin = -pi/2, vmax = pi/2,
                     n = 50, draw = FALSE)

  dv <- function(vv, cmap = terrain.colors, ...) {
    cf <- function(x, y, z) {
      w <- sin(3 * x) + cos(5 * y) + sin(7 * z)
      cmap(length(w))[rank(w)]
    }
    drawScene(updateTriangles(vv, color = cf, ...))
  }
  dv(vv)
  dv(vv, cmap = rainbow)
  dv(vv, cmap = rainbow, col.mesh="black")

  if (suppressWarnings(require(maps,quietly=TRUE))) {
    m <- map(plot = F)
    drawScene(updateTriangles(vv,color="lightblue"))
    i <- which(m$x > 0)
    m$x[i] <- NA
    m$y[i] <- NA
    m$x <- m$x * pi / 180
    m$y <- m$y * pi / 180
    lines(sin(m$x+pi/2)*cos(m$y), sin(m$y))
  }

  vvv <- local({
    u <- seq(-pi, pi, len = 50)
    v <- seq(-pi/2, pi/2, len = 50)
    v[(1:12) * 4] <- NA
    parametric3d(fx = function(u, v) cos(u)*cos(v),
                 fy = function(u,v) sin(u) * cos(v),
                 fz = function(u,v) sin(v),
                 u = u, v = v, draw = FALSE)
  })
  dv(vvv)
  dv(vvv, cmap = rainbow)
  dv(vvv, cmap = rainbow, col.mesh="black")

  drawScene(updateTriangles(vtri, smooth = 1),
            screen = list(x = 40,  y= -40, z = -135), scale = FALSE)
  drawScene(updateTriangles(vtri, smooth = 2),
            screen = list(x = 40, y = -40, z = -135), scale = FALSE)

  drawScene(updateTriangles(vtri, smooth = 2,
                            color = function(x,y,z) {
                              cols <- terrain.colors(diff(range(z)))
                              cols[z - min(z) + 1]}),
            screen = list(x = 40, y = -40, z = -135), scale = FALSE,
            persp = TRUE, depth = 0.6)

  svtri <- local({
    z <- 2 * volcano
    x <- 10 * (1:nrow(z))
    y <- 10 * (1:ncol(z))
    i <- 1 : nrow(z)
    z[ i %% 4 == 0] <- NA
    surfaceTriangles(x, y, z, color="green3")
  })

  drawScene(updateTriangles(svtri, smooth=2,
                            color = function(x,y,z) {
                              cols <- terrain.colors(diff(range(z)))
                              cols[z - min(z) + 1]}),
            screen=list(x=40, y=-40, z=-135), scale = FALSE)
})





library(misc3d)

local({
  data(teapot)

  haveRGL <- suppressWarnings(requireNamespace("rgl",quietly=TRUE))

  ttri <- makeTriangles(teapot$vertices, teapot$edges,
                        color = "red", color2 = "green")
  edges <- teapot$edges

  ttriDull <- updateTriangles(ttri,material="dull")
  ttriShiny <- updateTriangles(ttri,material="shiny")
  ttriMetal <- updateTriangles(ttri,material="metal")

  ## teapots with varying materials
  drawScene(ttri,screen=list(y=-30,x=40), scale = FALSE)
  drawScene(ttriDull,screen=list(y=-30,x=40), scale = FALSE)
  drawScene(ttriShiny,screen=list(y=-30,x=40), scale = FALSE)
  drawScene(ttriMetal,screen=list(y=-30,x=40), scale = FALSE)
  drawScene(ttri,screen=list(y=-30,x=40),lighting=perspLighting,
            scale = FALSE)
  if (haveRGL) drawScene.rgl(ttri)

  ## teapots with varying colors
  drawScene(updateTriangles(ttriShiny,color2=grey.colors(ncol(edges))),
            screen=list(y=-30,x=40), scale = FALSE)
  drawScene(updateTriangles(ttriMetal, color2=heat.colors(ncol(edges))),
            screen=list(y=-30,x=40), scale = FALSE)
  drawScene(updateTriangles(ttriMetal,color2=heat.colors(ncol(edges))),
            screen=list(y=-30,x=40), scale = FALSE, engine="grid")

  ## two teapots side by side
  hc <- heat.colors(ncol(edges))
  drawScene(list(updateTriangles(ttri, color2 = hc),
                 translateTriangles(ttri,z=4)),
            screen=list(y=-30,x=40), scale = FALSE)
  drawScene(list(updateTriangles(ttri, color2 = hc),
                 translateTriangles(ttri,z=4)),
            screen=list(y=-30,x=40), scale = FALSE, engine="grid")
  drawScene(list(updateTriangles(ttriShiny,color2=hc),
                 translateTriangles(ttriMetal,z=4)),
            screen=list(y=-30,x=40), scale = FALSE, engine="grid")
  if (haveRGL)
    drawScene.rgl(list(updateTriangles(ttri, color=hc),
                       translateTriangles(ttri,z=4)))

  ## nested teapots
  drawScene(list(updateTriangles(ttri,color="blue", fill=FALSE,
                                 col.mesh="blue"),
                 scaleTriangles(updateTriangles(ttriMetal, color2="red"),
                                0.6)),
            screen=list(y=-30,x=20,y=-140), scale = FALSE)
  if (haveRGL)
    drawScene.rgl(list(updateTriangles(ttri, alpha = 0.5, color="blue"),
                       scaleTriangles(ttriMetal, 0.6)))

  ## teapot with smoothing (Phong shading)
  drawScene(updateTriangles(ttriMetal, color2 = hc, smooth = 1),
            screen=list(y=-30,x=40), scale = FALSE)
  drawScene(updateTriangles(ttriMetal, color2 = hc, smooth = 2),
            screen=list(y=-30,x=40), scale = FALSE)
  drawScene(updateTriangles(ttriMetal, color2 = hc, smooth = 3),
            screen=list(y=-30,x=40), scale = FALSE)
})
