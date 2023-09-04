#' ---
#' title: Using contour3d to plot quadrics
#' subtitle: confocal pencils of quadrics and tangent planes
#' ---
#'
#'
#' **** COPIED Projects/Suppresssion/Quadrics.R 2023-06-29 ****
#' 
#' For p3d: incorporate countour3d from misc3d
#' 
#' 
##' # Representation using homegeneous coordinates
#'
#' ![](Files/Quadrics_in_homogeneous_coordinates.pdf)
#'
library(spida2)
library(p3d)
library(misc3d)

init2 <- function() {
  plot(rbind(c(0,0)), xlim = c(-4,4), asp = 1, type = 'n')
}
c2d <- function(    # draw contour(s), i.e. level sets or a function in 3d
  fun,        # a function of 2 arguments 
  at = 0,
  levels = at,  # levels of fun for implicit surface
  n = 100,     # grid resolution
  bbox = par('usr'),
  labels = '',
  labcex = 0.1,
  scale = 1,
  add = TRUE, draw = T,
  x = Seq(bbox[1],bbox[2], n, xs), 
  y = Seq(bbox[3],bbox[4], n, ys), 
  xs = 1, ys = 1,                       # expand beyond bounding box                 
  ...) {
  # 
  # contour3d using bounding box parameters
  # 
  # 
  Seq <- function(from,to,n, expand) {
    mid <- (to + from)/2
    ran <- (to - from)/2
    seq(mid - expand*ran, mid + expand*ran, length.out = round(n*expand))
    # seq(mid - expand*ran, mid + expand*ran, length.out = n)
  }
  means <- c(bbox[1] + bbox[2], bbox[3] + bbox[4])[c(1,1,2,2)]/2
  bbox <- means + scale * (bbox - means)
  # if x,y,z not provided as arguments then bbox is used
  # 
  # Use vectorized function
  # 
  grid <- expand.grid(x = x, y = y)
  z <- with(grid, fun(x, y))
  z <- matrix(z, nrow = length(x))
  # 
  # z <- matrix(NA, length(x), length(y))
  # for(i in seq_along(x)) {
  #   for(j in seq_along(y)) {
  #     z[i,j] <- fun(x[i], y[j])
  #   }
  # }
  contour(x, y , z, levels = levels, add = add, draw = draw, labels = labels, labcex = labcex, ...)
}

init2()


init <- function(scale = 4, new = F, cex = 1.2){
  if(new||(cur3d()==0)) Init3d(cex=cex)
  di <- function(scale=4) { 
    data.frame(x=scale*c(-1,1), y=scale*c(-1,1),z=scale*c(-1,1))
  }
  Plot3d(y ~ x + z, di(scale))
  Pop3d()
}
init3 <- init

#
#
Plane <- function(L=c(1,1,1), const = 1) {
  ret <- function(x,y,z) {
    X <- rbind(x,y,z,1)
    colSums(X * c(L, const))
  }
  attr(ret, 'parms') <- list(L = c(L,const))
  ret
} 
Tplane <- function(V, L, at = NULL) {
  # tangent plane to V perpendicular to L, or at intersection of L and V
  if(is.null(at)) Plane(-L, sqrt(rbind(c(L)) %*% V %*% cbind(c(L))))
  else Plane(-solve(V,at), sqrt(rbind(c(at)) %*% cbind(c(solve(V,at)))))
}
IntVL <- function(V,L) {
  # V shape of ellipse
  # L is a line as a vector or row vector
  d2 <- rbind(L)%*%solve(V,L)
  L/sqrt(c(d2))
}

Quad <- function(        # function implicitly defining quadric surface
    mu = c(0,0,0),       # center
    shape = diag(3),     # inverse of quadratic form coefficients
    radius = 1,          #
    M11 = solve(shape),  # 
    a12 = M11 %*% cbind(mu),  # bounding column in homogeneous coordinates
    M = rbind(            # Matrix in homogeneous representation
      cbind(M11, a12),
      c(a12, - radius^2 + sum(a12*mu) ))
) {
  M
  ret <- function(x,y,z) {
    X <- cbind(x,y,z,1)
#    sapply(seq_len(nrow(X)), function(i) X[i,,drop=F]%*% M %*% t(X[i,,drop=F]))
    rowSums((X%*%M)*X)
  }
  attr(ret,'parms') <- list(M=M,shape = solve(M[1:3,1:3]))
  ret
}

Qm <- function(M, const = -1){
  M <- rbind(cbind(M,0),0)
  M[4,4] <- const
  ret <- function(x,y,z) {
    X <- cbind(x,y,z,1)
    rowSums((X%*%M)*X) 
  }
  attr(ret,'parms') <- list(Mh = M)
  ret
} 
Qv <- function(V, const = -1){
  M <- rbind(cbind(solve(V),0),0)
  M[4,4] <- const
  ret <- function(x,y,z) {
    X <- cbind(x,y,z,1)
    rowSums((X%*%M)*X) 
  }
  attr(ret,'parms') <- list(V = V)
  ret
} 

Quadh <- function(d = c(1,1,1), od = c(0,0,0), v = c(0,0,0), const =  -1) {
  M <- matrix(0,4,4)
  M[4,-4] <- v
  M[-4,4] <- v
  diag(M) <- c(d,const)
  M[rbind(c(2,1),c(3,1),c(3,2),c(1,2),c(1,3),c(2,3))] <- c(od,od)
  ret <- function(x,y,z) {
    X <- cbind(x,y,z,1)
    rowSums((X%*%M)*X) 
  }
  attr(ret,'parms') <- list(Mh = M)
  ret
}

Quadlam <- function(a = c(1,1,1), d = 1/(a+lam), od = c(0,0,0), v = c(0,0,0), const =  -1, lam = 0) {
  M <- matrix(0,4,4)
  M[4,-4] <- v
  M[-4,4] <- v
  diag(M) <- c(d,const)
  M[rbind(c(2,1),c(3,1),c(3,2),c(1,2),c(1,3),c(2,3))] <- c(od,od)
  ret <- function(x,y,z) {
    X <- cbind(x,y,z,1)
    rowSums((X%*%M)*X) 
  }
  attr(ret,'parms') <- list(Mh = M)
  ret
}

c3d <- function(    # draw contour(s), i.e. level sets or a function in 3d
  fun,        # a function of 3 arguments 
  level = 0,  # levels of fun for implicit surface
  n = 30,     # grid resolution
  bbox = par3d()$bbox,
  scale = 1,
  add = TRUE, draw = T,
  x = Seq(bbox[1],bbox[2], n, xs), 
  y = Seq(bbox[3],bbox[4], n, ys), 
  z = Seq(bbox[5],bbox[6], n, zs),
  xs = 1, ys = 1, zs = 1,                       # expand beyond bounding box                 
  ...) {
  # 
  # contour3d using bounding box parameters
  # 
  # 
  Seq <- function(from,to,n, expand) {
    mid <- (to + from)/2
    ran <- (to - from)/2
    seq(mid - expand*ran, mid + expand*ran, length.out = round(n*expand))
    # seq(mid - expand*ran, mid + expand*ran, length.out = n)
  }
  means <- c(bbox[1] + bbox[2], bbox[3] + bbox[4] , bbox[5] + bbox[6])[c(1,1,2,2,3,3)]/2
  bbox <- means + scale * (bbox - means)
  # if x,y,z not provided as arguments then bbox is used
  contour3d(fun, level, x,y,z ,add = add, draw = draw, ...)
} 

c3d_ <- function(   # older version ????
  fun, level = 0,  n=30,
  bbox = par3d()$bbox,
  density = 30,
  scale = 1,
  xext = scale,
  yext = scale,
  zext = scale,
  add = TRUE, draw = T,
  x.inc = (bbox[2] - bbox[1])/density, 
  y.inc = (bbox[4] - bbox[3])/density, 
  z.inc = (bbox[6] - bbox[5])/density,
  x = ext(bbox[1:2], xext, x.inc),
  y = ext(bbox[3:4], yext, y.inc),
  z = ext(bbox[5:6], zext, z.inc),
  ...) {
  ext <- function(range, scale, inc) {
    range <- mean(range) + scale*(range - mean(range))
    seq(from=range[1], to = range[2], by = inc)
  }
  # if x,y,z not provided as arguments then bbox is used
  contour3d(fun, level, x,y,z ,add = add, draw = draw, ...)
} 




Quadlam <- function(a = c(1,1,1), d = 1/(a-lam), od = c(0,0,0), v = c(0,0,0), const =  -1, lam = 0) {
  Quadh(d, od, v, const)
}
Pop3d()
init()
Quadlam(2*(1:3), lam=0) %>% c3d(color='red', alpha = .2)
Quadlam(2*(1:3), lam=1) %>% c3d(color='red', alpha = .2)
Quadlam(2*(1:3), lam=1.95) %>% c3d(color='red', alpha = .2, n = 50)
Quadlam(2*(1:3), lam=2.01) %>% c3d(color='red', alpha = .2, n = 50)
Quadlam(2*(1:3), lam=2.01) %>% c3d(color='red', alpha = .2, n = 50)
Quadlam(2*(1:3), lam=3.9) %>% c3d(color='red', alpha = .2, n = 50)
Quadlam(2*(1:3), lam=4.1) %>% c3d(color='red', alpha = .2, n = 50)

Quadlam(2*(1:3), lam=5.99) %>% c3d(color='red', alpha = .2, n = 50)
Quadlam(2*(1:3), lam=6.01) %>% c3d(color='red', alpha = .2, n = 50)
Quadlam(2*(1:3), lam=5) %>% c3d(color='red', alpha = .2, n = 50)


Quadlam(2*(1:3), lam=0) %>% c3d(color='blue', alpha = .1)
for(lam in seq(0,7,.1)+.05){
  Quadlam(2*(1:3), lam=lam) %>% c3d(color='red', alpha = .8)
  Sys.sleep(1)
  Pop3d()
}
#'
##' ## Confocal quadrics ####
#'
#' Test tangent planes
init(); Axes3d()
lam <- 2.5
d <- c(.5,1,2)^2   
# Qm(diag(1/d)) %>% c3d(color='red', alpha = .2, n = 50)
Qv(diag(d)) %>%  c3d(color='red', alpha = .2, n = 50)
Tplane(diag(d), c(1,1,1)) %>% c3d
Tplane(diag(d), at= c(1,1,1)) %>% c3d
rbind(0, 10*c(1,1,1)) %>% Lines3d(lwd = 2)

#'
#' Confocal quadrics
#'
#' Start with data ellipse
#' 
init(); Axes3d()

# linear pencil

d <- 1:3
Qm(diag(d)) %>%  c3d(color='red', alpha = .2, n = 50)
lams <- c(1, .1, -.1, -.9, -1.1)
lams <- c(-1.1, -1.9, -2.1, -2.9, -3.1)

for( lam in lams ) {
  readline(lam)
  Qm(diag(d)+lam*diag(3)) %>%  c3d(color='blue', alpha = .1, n = 50)
}
# confocal pencil
# 
d <- 1:3
Qv(diag(d)) %>%  c3d(color='red', alpha = .2, n = 50)
for( lam in lams ) {
  readline(lam)
  Qv(diag(d)+lam*diag(3)) %>%  c3d(color='blue', alpha = .1, n = 50)
}



Qm(diag(3)) %>% c3d(color='grey', alpha = .1, n = 50)
Qm(diag(d)) %>% c3d(color='blue', alpha = .2, n = 50)
Lines3d(rbind(c(0,0,0), c(10,10,10)), lwd =3, color = 'black')
Plane(c(1,1,1), 2) %>% c3d(color='red', alpha = .1)
Plane(c(1,1,1), 2) %>% c3d(color='red', alpha = .1)

# 
# - Given an ellipse variance V
# - a direction L
# - find 
#   - the plane tangent to V at L int V   
#   - the plane orthogonal to L that is tangent to V

# in 2 dimenstions
# 
shape <- cbind(c(2,1),c(1,2))
shape <- diag(c(2,.2))
ell(shape=shape) %>% plot(type='l', xlim = c(-3,3) , ylim = c(-3,3), asp = 1) 
lam <- 0
ell(shape=solve(shape)) %>% plot(type='l', xlim = c(-3,3) , ylim = c(-3,3), asp = 1) 
{
ell(shape=solve((1-lam)*shape + lam * diag(2))) %>% lines 
  lam <- lam + .1
}

lam <- 0
ell(shape=(shape)) %>% plot(type='l', xlim = c(-3,3) , ylim = c(-3,3), asp = 1) 
{
  ell(shape=((1-lam)*shape + lam * diag(2))) %>% lines 
  lam <- lam + .1
}



lam <- 0
ell(shape=solve(shape)) %>% plot(type='l', xlim = c(-3,3) , ylim = c(-3,3), asp = 1) 
{
  ell(shape=solve((1-lam)*shape + lam * diag(2))) %>% lines 
  lam <- lam + .1
}



#Pop3d()
IntVL(diag(1/d), c(1,1,1)) %>% rbind %>% spheres3d(radius = .05)
IntVL(diag(1/d), c(1,1,1)) %>% rbind %>% {. %*% diag(1/d)} %>% spheres3d(radius = .05)
IntVL(diag(1/d), c(1,1,1)) %>% rbind %>% {. %*% diag(1/d)} %>% 
  {rbind(0, .)} %>% Lines3d
c(2,3,1) %>% Lines3d(lwd =1)

c(1,3,2) %>% Points3d
-3*c(1,1,1) %>% points3d(color = 'black')
# dual
{
  init(); Axes3d()
  lam <- 2.5
  d <- (1:3)^2 - lam  
  Qm(diag(d)) %>% c3d(color='red', alpha = .2, n = 50)
  Qm(diag(1/d)) %>% c3d(color='blue', alpha = .2, n = 50)
  Qm(diag(3)) %>% c3d(color = 'grey', alpha = .2, n = 50)
  Lines3d(rbind(c(0,0,0), c(10,10,10)))
}
   
{
  init(); Axes3d()
  lam <- 0
  d <- (1:3)^2 - lam  
  Qm(diag(d)) %>% c3d(color='red', alpha = .2, n = 50)
  Qm(diag(1/d)) %>% c3d(color='blue', alpha = .2, n = 50)
  Qm(diag(3),const = -2) %>% c3d(color = 'grey', alpha = .2, n = 50)
  Lines3d(rbind(c(0,0,0), c(10,10,10)))
}

# pencil of quadrics

{
  init(); Axes3d()
  lam <- 0
  {
    d <- (1:3)^2 - 2.9  
  Qm(diag(3),const = -2) %>% c3d(color = 'grey', alpha = .2, n = 50)
    Qm(diag(d)) %>% c3d(color='red', alpha = .2, n = 50)
    Qm(diag(1/d)) %>% c3d(color='blue', alpha = .2, n = 50)
    lam <- lam + .3
  Lines3d(rbind(c(0,0,0), c(10,20,30)),lwd=3)
  }
}

   

{
  init()
  lam <- 0
  K <- diag(1:3)^2 + lam * diag(3)
  Quadlam(  K , lam=0) %>% c3d(color='blue', alpha = .2, n = 50)
  Quadlam(1/((diag(1:3))^2 + lam * diag(3)), lam=0) %>% c3d(color='red', alpha = .2, n = 50)
}


##  Stack exchange example ####

#' [Definition of an ellipsoid based on its focal points - Mathematics Stack Exchange](https://math.stackexchange.com/questions/19849/definition-of-an-ellipsoid-based-on-its-focal-points)
#' 


# Confocal central quadric ####
# 
init(); Axes3d()

M <- diag(1/(3:1))
Qm(M) %>% c3d(color='pink', alpha = .2, n = 50)
lam <-0
{
  lam <- lam - .1
M <- diag(1/(1:3) + lam)
Qm(M) %>% c3d(color='pink', alpha = .2, n = 50)
}
xyell <- ell(shape=diag(3:2 - 1))

Lines3d(xy=xyell, z = 0, lwd = 2 , color = 'black')

plot(c(0,0), xlim = c(-3,3), asp = 1, type = 'n')
lam <- 0
ell(shape=solve(diag(c(3,.5)))) %>% lines(col='blue')
ell(shape=solve(diag(2))) %>% lines(col='red')
while(TRUE){
  readline(lam)
  xyell <- ell(shape=solve((1-lam)*diag(c(3,.5)) + lam *diag(2)))
  xyell %>% lines
  lam <- lam + .2
}

?contour
methods(contour)
Pop3d()

## Methods for R^2 using contour ####



conic <- function(M, constant= -1) {
  M <- rbind(cbind(M,0),0)
  M[3,3] <- constant
  ret <- function(x,y) {
    X <- cbind(x,y,1)
    rowSums((X%*%M)*X) 
  }
  attr(ret,'parms') <- list(Mh = M)
  ret
}

{  # pencil
  init2()
  conic(diag(c(.5,2))) %>% c2d
  lap <- lam <- 0
  conic(diag(c(1,2))+lam * diag(2)) %>% c2d
  while(TRUE){
    lap <- lap + .1 
    lam <- lam - .1
    readline(lam)
    conic(diag(c(1,2)) + lam * diag(2)) %>% c2d(col = 'red')
    conic(diag(c(1,2)) + lap * diag(2)) %>% c2d(col = 'blue')
  }
}

{  # pencil of confocal conics
  init2()
  lap <- lam <- 0
  conic(solve(diag(c(1,2))+lam * diag(2))) %>% c2d
  while(TRUE){
    lap <- lap + .3 
    lam <- lam - .3
    readline(lam)
    conic(solve(diag(c(1,2)) + lam * diag(2))) %>% c2d(col = 'red')
    conic(solve(diag(c(1,2)) + lap * diag(2))) %>% c2d(col = 'blue')
  }
}

{  # pencil of confocal conics
  init2()
  lap <- lam <- 0
  conic(solve(diag(c(1,2))+lam * diag(2))) %>% c2d
  while(TRUE){
    lap <- lap + .3 
    lam <- lam - .3
    readline(lam)
    conic(solve(diag(c(1,2)) + lam * diag(2))) %>% c2d(col = 'red')
    conic(solve(diag(c(1,2)) + lap * diag(2))) %>% c2d(col = 'blue')
  }
}

{  # pencil of confocal conics
  init2()
  lap <- lam <- 0
  conic(solve(diag(c(1,2))+lam * diag(2))) %>% c2d
  while(TRUE){
    lap <- lap + .3 
    lam <- lam - .3
    readline(lam)
    conic(solve(diag(c(1,2)) + lam * diag(2))) %>% c2d(col = 'red')
    conic(solve(diag(c(1,2)) + lap * diag(2))) %>% c2d(col = 'blue')
  }
}

{  # pencil of confocal conics where second conic contain first
  init2()
  lap <- lam <- 0
  d <- c(.8,.3)
  conic(solve(diag(d)+lam * diag(2))) %>% c2d
  while(TRUE){
    lap <- lap + .1 
    lam <- lam - .1
    readline(lam)
    conic(solve(diag(d) + lam * diag(2))) %>% c2d(col = 'red')
    conic(solve(diag(d) + lap * diag(2))) %>% c2d(col = 'blue')
  }
}

{  # affine combination of confocal conics of intersecting conics
  init2()
  lap <- lam <- 0
  d <- c(1.6,.6)
  conic(solve(diag(d)+lam * diag(2))) %>% c2d(col = 'green')
  conic(solve(lam * diag(d)+ (1-lam) * diag(2))) %>% c2d(col='cyan')
  
  while(TRUE){
    lap <- lap + .1 
    lam <- lam - .1
    readline(lam)
    # conic(solve((1-lam) * diag(d) + lam * diag(2))) %>% c2d(col = 'red')
    conic(solve((1-lap) * diag(d) + lap * diag(2))) %>% c2d(col = 'blue')
  }
}

{  # affine combination of lines conics of intersecting conics
   # They do intersect
  init2()
  lap <- lam <- 0
  d <- c(1.6,.6)
  conic((diag(d)+lam * diag(2))) %>% c2d(col = 'green')
  conic((lam * diag(d)+ (1-lam) * diag(2))) %>% c2d(col='cyan')
  
  while(TRUE){
    
    lap <- lap + .1 
    lam <- lam - .1
    readline(lam)
    #conic(solve((1-lam) * diag(d) + lam * diag(2))) %>% c2d(col = 'red')
    conic((lam * diag(d) + (1-lam) * diag(2))) %>% c2d(col = 'blue')
  }
}

{  # affine combination of lines conics of non- intersecting conics
  # don't intersect
  init2()
  lap <- lam <- 0
  d <- c(.8,.6)
  conic((diag(d)+lam * diag(2))) %>% c2d(col = 'green')
  conic((lam * diag(d)+ (1-lam) * diag(2))) %>% c2d(col='cyan')
  
  while(TRUE){
    
    lap <- lap + .1 
    lam <- lam - .1
    readline(lam)
    conic(solve((1-lam) * diag(d) + lam * diag(2))) %>% c2d(col = 'red')
    #conic((lam * diag(d) + (1-lam) * diag(2))) %>% c2d(col = 'blue')
  }
}

{  # affine combination of lines conics of non- intersecting conics
  # don't intersect
  init2()
  lap <- lam <- 0
  d <- c(.8,.6)
  conic((diag(d)+lam * diag(2))) %>% c2d(col = 'green')
  conic((lam * diag(d)+ (1-lam) * diag(2))) %>% c2d(col='cyan')
  
  while(TRUE){
    
    lap <- lap + .1 
    lam <- lam - .1
    readline(lam)
    conic(((1-lam) * diag(d) + lam * diag(2))) %>% c2d(col = 'red')
    #conic((lam * diag(d) + (1-lam) * diag(2))) %>% c2d(col = 'blue')
  }
}

{  # affine combination of lines conics of non- intersecting conics
  # don't intersect
  init2()
  lap <- lam <- 0
  d <- c(1.2,-.2)
  conic((diag(d)+lam * diag(2))) %>% c2d(col = 'green')
  conic((lam * diag(d)+ (1-lam) * diag(2))) %>% c2d(col='cyan')
  
  while(TRUE){
    
    lap <- lap + .1 
    lam <- lam + .1
    readline(lam)
    conic(((1-lam) * diag(d) + lam * diag(2))) %>% c2d(col = 'red')
    #conic((lam * diag(d) + (1-lam) * diag(2))) %>% c2d(col = 'blue')
  }
}

{  # pencil when V is bigger than I
  # don't intersect
  init2()
  lap <- lam <- 0
  d <- c(.5,2)
  conic((diag(d)+lam * diag(2))) %>% c2d(col = 'green')
  conic(diag(d) - diag(2)) %>% c2d(col = 'black')
   #conic((lam * diag(d)+ (1-lam) * diag(2))) %>% c2d(col='cyan')
  
  while(TRUE){
    lap <- lap + .1 
    lam <- lam + .1
    readline(lam)
    conic(diag(d) + lam * diag(2)) %>% c2d(col = 'red')
    #conic((lam * diag(d) + (1-lam) * diag(2))) %>% c2d(col = 'blue')
  }
}





conic(diag(c(1,2))+lam * diag(2)) %>% c2d
conic(diag(c(1,2))+lam * diag(2)) %>% c2d









init(new=T)
# confocal quadrics to an ellipsoid
ellh <- function(a) {
  
  Quadh(1/(a-lam))
    

}

ellfun <- ellh(1:3)

ellfun %>% c3d(color='red', alpha = .5)
M <- solve(diag(c(1,2,3,-1)))
fun1 <- Quad(M=M)
funh <- Quadh()

eij <- function(i,j) {
  ret <- matrix(0,4,4)
  ret[i,j] <- 1
  ret
}

funhp <- Quadh()


Axes3d()


c3d(funh)
Pop3d()
Quadh(c(1,-1,1),const = 1) %>% c3d(level = 0) # hyperboloid of 2 sheets
Quadh(c(1,-1,-1),const = 1) %>% c3d
Quadh(c(-1,-1,-1),const = 1) %>% c3d
Quadh(c(1,-1,1),const = 0) %>% c3d(color='red', xs=2, zs = 2, ys=1.5)
Pop3d(3)
Quadh(c(1,-1,0),v=c(0,0,1),const = 0) %>% c3d(color='blue', alpha = .4, xs=2, zs=3)
Quadh(c(1,-1,0),v=c(0,0,1),const = 0) %>% c3d(color='blue', alpha = .8)
Pop3d()
Quadh(c(1,-1,0),v=c(0,1,0),const = 0) %>% c3d(color='red', alpha = .8)
Quadh(c(1,-1,0),v=c(0,2,0),const = 0) %>% c3d(color='blue', alpha = .8)
Quadh(c(1,-1,0),v=c(0,-2,0),const = 0) %>% c3d(color='red', alpha = .8)
Quadh(c(1,1,0)) %>% c3d(color='red', alpha = .8)
Quadh(c(1,1,0),v= 1:3) %>% c3d(color='red', alpha = .8)
Pop3d()
{
  

Quadh(c(1,1,0), v=c(0,0,0*.01)) %>% c3d(color='red', alpha = .8)
for(i in 1:100) {
  Quadh(c(1,1,0), v=c(0,0,i*.01)) %>% c3d(color='red', alpha = .8)
  Sys.sleep(1)
  Pop3d()
}
}
Lines3d(rbind(c(0,0,0),1000*c(2,1,1)), lwd = 3, color = 'blue')


c3d(fun1, n = 60, color = '#990000' , alpha = .3)
Pop3d()
fun2 <- Quad(mu =c(1,2,1), shape = diag(c(1,2,3)),  radius = 1)
c3d(fun2, color = '#009900', alpha = .3,)
fun3 <- Quad(mu =c(1,2,1), shape = diag(c(1,-2,3)), radius = 1)
c3d_(fun3, color = 'red', alpha = .2)
c3d_(fun3, xext = 2, zext = 3, color = 'red', alpha = .2)

c3d(fun3, scale=2,n=50, color = 'blue', alpha = .3)


bgplot3d(plot(1:3,1:3),bg.color = '#999900')

debug(Quad)

Pop3d()

Pop3d()
c3d(fun1, n = 50)

spins()

par3d()
Par3d()
fun1(1:3,1,2)
Plotot3d(y ~ x + z, di())
Pop3d()

  
  
# 
# Quad <- function(M) {
#   function(x,y,z) {
#     X <- cbind(x,y,z,1)
#     disp(X)
#     sapply(seq_len(nrow(X)), function(i) X[i,,drop=F]%*% M %*% t(X[i,,drop=F]))
#   }
# }
M <- solve(diag(c(1,2,3,-1)))
fun1 <- Quad(M)
fun1(1:3,1,2)

funh <- Quadh()
c3d(funh)  
  
#'
#'
#'
#' Jacobi coordinates
# 

#'
#' Quad is a function that defines an ellipse in homogeneous coordinates
#'

  
#' 


#+
#+
#+
knitr::knit_exit()
 

library(p3d)
library(misc3d)
Init3d()

di <- function(scale=4) { 
  data.frame(x=scale*c(-1,1), y=scale*c(-1,1),z=scale*c(-1,1))
}

Quad <- function(M) {
  function(x,y,z) {
    X <- cbind(x,y,z,1)
    disp(X)
    sapply(seq_len(nrow(X)), function(i) X[i,,drop=F]%*% M %*% t(X[i,,drop=F]))
  }
}
M <- solve(diag(c(1,2,3,-1)))
fun1 <- Quad(M)
fun1(1:3,1,2)
Plotot3d(y ~ x + z, di())
Pop3d()

xs <- seq(-4,4,.1)
contour3d(fun1, 0, xs,xs,xs , add = T, color = 'pink')
Axes3d()

fun2 <- Quad(solve(diag(c(-1,2,3,-1))))
contour3d(fun2, 0, xs,xs,xs ,add = T, color = 'blue', alpha = .3)

fun3 <- Quad(solve(diag(c(1,-2,3,-1))))
contour3d(fun3, 0, xs,xs,xs ,add = T, color = 'blue', alpha = .3)
fun4 <- Quad(solve(diag(c(-1,-2,3,-1))))
contour3d(fun4, 0, xs,xs,xs ,add = T, color = 'green', alpha = .3)

fun3 <- Quad(diag(c(2,-1,3,-1)))
contour3d(fun3, 0, xs,xs,xs , add = TRUE, color = 'pink', alpha = .5)

fun4 <- Quad(diag(c(-2,-1,3,-1)))
contour3d(fun4, 0, xs,xs,xs , add = TRUE, color = 'blue', alpha = .2)

fun5 <- Quad(diag(c(2,-1,-3,-1)))
contour3d(fun5, 0, xs,xs,xs , add = TRUE, color = 'green', alpha = .2)

fun.cyl <- Quad(diag(c(2,0,1,-1)))
fun.cyl2 <- Quad(diag(c(2,0,0,-1)))

contour3d(fun.cyl, 0, xs,xs, xs, add=TRUE, color = 'red', alpha = .2)
contour3d(fun.cyl2, 0, xs,xs, xs, add=TRUE, color = 'red', alpha = .2)


spins()


#Example 1: Draw a ball
Init3d()

f <- function(x, y, z)x^2+y^2+z^2
x <- seq(-2,2,len=100)

Plot3d(y ~ x + z, di())
Axes3d()
contour3d(f,4,x,x,x)
Axes3d()
Pop3d()

# ball with one corner removed.
contour3d(f,4,x,x,x, mask = function(x,y,z) x > 0 | y > 0 | z > 0)
contour3d(f,4,x,x,x, mask = function(x,y,z) x > 0 | y > 0 | z > 0,
          engine="standard", screen = list(x = 290, y = -20),
          color = "red", color2 = "white")

# ball with computed colors
w <- function(x,y,z) {
  v <- sin(x) + cos(2 * y) * sin(5 * z)
  r <- range(v)
  n <- 100
  i <- pmax(pmin(ceiling(n * (v - r[1]) / (r[2] - r[1])), n), 1)
  terrain.colors(n)[i]
}
contour3d(f,4,x,x,x, color = w)

#Example 2: Nested contours of mixture of three tri-variate normal densities
nmix3 <- function(x, y, z, m, s) {
  0.4 * dnorm(x, m, s) * dnorm(y, m, s) * dnorm(z, m, s) +
    0.3 * dnorm(x, -m, s) * dnorm(y, -m, s) * dnorm(z, -m, s) +
    0.3 * dnorm(x, m, s) * dnorm(y, -1.5 * m, s) * dnorm(z, m, s)
}
f <- function(x,y,z) nmix3(x,y,z,.5,.5)
g <- function(n = 40, k = 5, alo = 0.1, ahi = 0.5, cmap = heat.colors) {
  th <- seq(0.05, 0.2, len = k)
  col <- rev(cmap(length(th)))
  al <- seq(alo, ahi, len = length(th))
  x <- seq(-2, 2, len=n)
  contour3d(f,th,x,x,x,color=col,alpha=al)
  rgl::bg3d(col="white")
}
g(40,5)
gs <- function(n = 40, k = 5, cmap = heat.colors, ...) {
  th <- seq(0.05, 0.2, len = k)
  col <- rev(cmap(length(th)))
  x <- seq(-2, 2, len=n)
  m <- function(x,y,z) x > .25 | y < -.3
  contour3d(f,th,x,x,x,color=col, mask = m, engine = "standard",
            scale = FALSE, ...)
  rgl::bg3d(col="white")
}
gs(40, 5, screen=list(z = 130, x = -80), color2 = "lightgray", cmap=rainbow)

## Not run: 
#Example 3: Nested contours for FMRI data.

library(AnalyzeFMRI)
a <- f.read.analyze.volume(system.file("example.img", package="AnalyzeFMRI"))
a <- a[,,,1]
contour3d(a, 1:64, 1:64, 1.5*(1:21), lev=c(3000, 8000, 10000),
          alpha = c(0.2, 0.5, 1), color = c("white", "red", "green"))

# alternative masking out a corner
m <- array(TRUE, dim(a))
m[1:30,1:30,1:10] <- FALSE
contour3d(a, 1:64, 1:64, 1.5*(1:21), lev=c(3000, 8000, 10000),
          mask = m, color = c("white", "red", "green"))
contour3d(a, 1:64, 1:64, 1.5*(1:21), lev=c(3000, 8000, 10000),
          color = c("white", "red", "green"),
          color2 = c("gray", "red", "green"),
          mask = m, engine="standard",
          scale = FALSE, screen=list(z = 60, x = -120))

## End(Not run)

#Example 4: Separate the triangles from the contours of
#           mixture of three tri-variate normal densities
nmix3 <- function(x, y, z, m, s) {
  0.3*dnorm(x, -m, s) * dnorm(y, -m, s) * dnorm(z, -m, s) +
    0.3*dnorm(x, -2*m, s) * dnorm(y, -2*m, s) * dnorm(z, -2*m, s) +
    0.4*dnorm(x, -3*m, s) * dnorm(y, -3 * m, s) * dnorm(z, -3*m, s) }
f <- function(x,y,z) nmix3(x,y,z,0.5,.1)
n <- 20
x <- y <- z <- seq(-2, 2, len=n)
contour3dObj <- contour3d(f, 0.35, x, y, z, draw=FALSE, separate=TRUE)
for(i in 1:length(contour3dObj))
  contour3dObj[[i]]$color <- rainbow(length(contour3dObj))[i]
drawScene.rgl(contour3dObj)



if(F){
  
  f <- function(x, y) 100 / (17/((x / 365) * 0.3)) * (100 - y)
  y <- Rain <- c(1:100)
  x <- Tourism <- c(1:100)
  z <- maxGiraffeNumber <- outer(Rain, Tourism, f)
  surface3d(Tourism, Rain, maxGiraffeNumber, col = "red")
  Axes3d()
  Ell3d %>% methods
  p3d:::Ell3d.default
  # generate a Quad mesh object
  
  vertices <- c( 
    -1.0, -1.0, 0,
    1.0, -1.0, 0,
    1.0,  1.0, 0,
    -1.0,  1.0, 0
  )
  indices <- c( 1, 2, 3, 4 )
  
  open3d()  
  wire3d( mesh3d(vertices = vertices, quads = indices) ,col = 'pink')
  vs <- matrix(rnorm(40), nrow = 4)
  shade3d( mesh3d(vertices = vs, quads = 1:4))
}

x <- rnorm(100)
y <- rnorm(100)
z <- rnorm(100)
open3d()
# Needs to be a bigger window than the default
par3d(windowRect = c(100, 100, 612, 612))
parent <- currentSubscene3d()
mfrow3d(2, 2)
plot3d(x, y, z)
next3d(reuse = FALSE)
bgplot3d(plot(y, z))
next3d(reuse = FALSE)
bgplot3d(plot(x, z))
next3d(reuse = FALSE)
legend3d("center", c("2D Points", "3D Points"), pch = c(1, 16))
useSubscene3d(parent)
